/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"

//#include "insieme/core/forward_decls.h"
//#include "insieme/core/ir_node.h"
//#include "insieme/core/ir_address.h"

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/frontend/frontend.h"
#include "insieme/machine_learning/database_utils.h"

#include "insieme/driver/handle_fetures.h"


using namespace insieme;
namespace bpo = boost::program_options;
namespace bfs = boost::filesystem;
namespace ft = insieme::analysis::features;

/**
 * A struct aggregating command line options.
 */
struct CmdOptions {
	bool valid;						/* < a flag determining whether the cmd options are valid or not. */
	string rootDir;					/* < the root directory of the measurement data */
	string databaseFile;			/* < the database file to store the extracted features. */
	vector<string> sFeatures;		/* < a list of static features to extract. */
	vector<string> dFeatures;		/* < a list of dynamic features to extract. */
};


CmdOptions parseCommandLine(int argc, char** argv) {
	CmdOptions fail;
	fail.valid = false;

	// -- parsing -------------------------------------------

	// define options
	bpo::options_description desc("Supported Parameters");
	desc.add_options()
			("help,h", "produce help message")
			("directory,d", bpo::value<string>(), "root directory where to read data from, required")
			("static-features,s", bpo::value<vector<string>>(), "features to extract")
			("dynamic-features,f", bpo::value<vector<string>>(), "features to extract")
			("database-file,o", bpo::value<string>(), "the file the sqlite database will be stored, default: data.db")
	;

	// define positional options (all options not being named)
	bpo::positional_options_description pos;
	pos.add("directory", -1);

	// parse parameters
	bpo::variables_map map;
	bpo::store(bpo::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
	bpo::notify(map);


	// -- processing -----------------------------------------

	// check whether help was requested
	if (map.count("help")) {
		std::cout << desc << "\n";
		return fail;
	}

	CmdOptions res;
	res.valid = true;

	// input files
	if (map.count("directory")) {
		res.rootDir = map["directory"].as<string>();
	} else {
		LOG(ERROR) << "Input root directory not set!\n";
		return fail;
	}

	// output file (optional)
	if (map.count("database-file")) {
		res.databaseFile = map["database-file"].as<string>();
	} else
		res.databaseFile = "data.db";

	// static features
	if (map.count("static-features")) {
		res.sFeatures = map["static-features"].as<vector<string>>();
	}

	// dynamic features
	if (map.count("dynamic-features")) {
		res.dFeatures = map["dynamic-features"].as<vector<string>>();
	}

	if (res.sFeatures.empty() && res.dFeatures.empty()) {
		LOG(ERROR) << "No features set!\n";
		return fail;
	}

	// create result
	return res;
}

void processDirectory(const CmdOptions& options) {

	// access root directory
	bfs::path dir(options.rootDir);
	std::cerr << "Processing directory: " << dir << "\n";

	if (!bfs::is_directory(dir)) {
		std::cerr << "Not a directory!" << std::endl;
		return;
	}

	analysis::features::FeatureCatalog catalog;
	catalog.addAll(ft::getFullCodeFeatureCatalog());

	// collect features
	vector<ft::FeaturePtr> features;

	// get all features not specified in the arguments
	for_each(options.sFeatures, [&](std::string feature) {
		auto f = catalog.getFeature(feature);
		if(f)
			features.push_back(f);
	});
	if(features.empty()) {
		LOG(ERROR) << "None of the specified features could be found\n";
		return;
	}

	// print head line
	std::cout << "Static features;" << join(";",features, print<deref<ft::FeaturePtr>>()) << "\n";

	vector<bfs::path> kernels;
	for (auto it = bfs::recursive_directory_iterator(dir);
			it != bfs::recursive_directory_iterator(); ++it) {

		auto kernelFile = it->path() / "kernel.dat";


		if (bfs::exists(kernelFile) && bfs::file_size(kernelFile) > 500000) {
			std::cerr << "Ignoring Large File: " << kernelFile << "\n";
			continue;
		}

		if (bfs::exists(kernelFile) && bfs::file_size(kernelFile) < 500000) {
			kernels.push_back(kernelFile);
		}
	}

	LOG(INFO) << "Found " << kernels.size() << " kernels!" << std::endl;


	// process all identifies kernels ...
	#pragma omp parallel
	{
		core::NodeManager manager;

		#pragma omp for schedule(dynamic,1)
		for (std::size_t i=0; i<kernels.size(); i++) {
			auto path = kernels[i];

			try {

				std::cout << "Processing Kernel " << path.string() << "\n";

				std::fstream in(path.string(), std::fstream::in);
				auto kernelCode = core::dump::binary::loadAddress(in, manager);

				auto version 	= path.parent_path();
				auto kernel 	= version.parent_path();
				auto benchmark 	= kernel.parent_path();

				utils::Timer timer("Simulation Time");
				vector<ft::Value> values = driver::extractFeatures(kernelCode, features);
				timer.stop();

				#pragma omp critical
				{
					std::cout << benchmark.filename()
							<< "; " << kernel.filename()
							<< "; " << version.filename()
							<< "; " << ::join(";", values)
//								<< "; " << num_allocs
//								<< "; " << reuseDistance
							<< "; " << (long)(timer.getTime()*1000)
							<< "\n";
				}

			} catch (const core::dump::InvalidEncodingException& iee) {
				std::cerr << "Invalid encoding within kernel file of " << path;
			}
		}
	}

}

/**
 * The data base generator entry point.
 */
int main(int argc, char** argv) {
	core::NodeManager manager;

	// set up logger
	Logger::get(std::cerr, LevelSpec<>::loggingLevelFromStr("ERROR"));
	//Logger::get(cerr, LevelSpec<>::loggingLevelFromStr("DEBUG"));

	std::cerr << " --- Insieme Feature Database Generator, Version 0.0..01beta ---- \n";

	// process handle command line arguments
	CmdOptions options = parseCommandLine(argc, argv);
	if (!options.valid) {
		return 1;
	}

	// create database to write the features to
	// create and open database
	Kompex::SQLiteDatabase *pDatabase = ml::createDatabase(options.databaseFile);

	processDirectory(options);
	return 0;

}
