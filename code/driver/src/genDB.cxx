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
//#include "insieme/utils/timer.h"

#include "insieme/core/forward_decls.h"
//#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

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

// checks if there are some collisions in the hashes (ids) added to the database during this call
#define CHECK_FOR_COLLISIONS 1

/**
 * A struct aggregating command line options.
 */
struct CmdOptions {
	bool valid;						/* < a flag determining whether the cmd options are valid or not. */
	string rootDir;					/* < the file or root directory of the measurement data */
	string databaseFile;			/* < the database file to store the extracted features. */
	vector<string> sFeatures;		/* < a list of static features to extract. */
//	vector<string> dFeatures;		/* < a list of dynamic features to extract. */
	string dumpCid;					/* < the file to dump the cid of all processed codes in plain text*/
	bool recursive;                 /* < evaluate rootDir recursively and search for files named kernel.dat in the folder hierarchy */
	bool clear;						/* < flag indicating if database (if existing) should be overwritten or data just appended */
};

class CodeEqualException : public std::exception {
    std::string err;
public :
	const char* what() const throw() {
		return err.c_str();
	}

	CodeEqualException() : err("") {}

	CodeEqualException(std::string path1, std::string path2, int64_t id) {
		std::stringstream errStream;
		errStream << "Collision of codes. Skipping \n\t" << path1 << "\ndue to equal hash with \n\t" << path2 << "\nHash: " << id << std::endl<< std::endl;
		err = errStream.str();
	}

    ~CodeEqualException() throw() {}
};

CmdOptions parseCommandLine(int argc, char** argv) {
	CmdOptions fail;
	fail.valid = false;

	// -- parsing -------------------------------------------

	// define options
	bpo::options_description desc("Supported Parameters");
	desc.add_options()
			("help,h",                                           "produce help message")
			("directory,d",        bpo::value<string>(),         "root directory where to read data from, required")
			("static-features,f",  bpo::value<vector<string>>(), "features to extract")
//			("dynamic-features,f", bpo::value<vector<string>>(), "features to extract")
			("database-file,o",    bpo::value<string>(),         "the file the sqlite database will be stored, default: data.db")
			("dump-cid,u",		   bpo::value<string>(), 		 "the file to dump the cid of all processed codes in plain text")
			("recursive,r",                                      "evaluate rootDir recursively and search for files named kernel.dat in the folder hierarchy")
			("clear-database,c",                                 "overwrites any database that might exist at the given path")
			("log-level,L",        bpo::value<string>(),         "Log level: DEBUG|INFO|WARN|ERROR|FATAL")
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
		std::cout << " --- Insieme Feature Database Generator, Version 0.0..01beta ---- \n";
		std::cout << desc << "\n";
		return fail;
	}

	CmdOptions res;
	res.valid = true;

	// log level
	if (map.count("log-level")) {
		Logger::get(std::cerr, LevelSpec<>::loggingLevelFromStr(map["log-level"].as<string>()));
	}

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

	// cid dump file
	if(map.count("dump-cid")) {
		res.dumpCid = map["dump-cid"].as<string>();
	}

	// dynamic features
/*	if (map.count("dynamic-features")) {
		res.dFeatures = map["dynamic-features"].as<vector<string>>();
	}
*/
	if (res.sFeatures.empty() /*&& res.dFeatures.empty()*/) {
		LOG(ERROR) << "No features set!\n";
		return fail;
	}

	if (map.count("recursive"))
		res.recursive = true;
	else
		res.recursive = false;

	if (map.count("clear-database"))
		res.clear = true;
	else
		res.clear = false;

	// create result
	return res;
}

void writeFeaturesTables(ml::Database& database, vector<ft::FeaturePtr>& staticFeatures, vector<int64_t>& staticFeatureIds/*,
		vector<std::string>& dynamicFeatures, vector<int64_t>& dynamicFeatureIds*/, bool checkBeforeInsert) {
	boost::hash<std::string> string_hash; // using the hash of the features' name as feature id, assuming it will be equal (true for static features on 13.03.2012)

#if CHECK_FOR_COLLISIONS
	boost::unordered_map<int64_t, int> cCheck;
#endif

	// write static features to table
	database.beginStaticFeaturesTransaction();
	for_each(staticFeatures, [&](ft::FeaturePtr feature) {
		staticFeatureIds.push_back(string_hash(feature->getName()));

#if CHECK_FOR_COLLISIONS
		if(cCheck.find(staticFeatureIds.back()) != cCheck.end()) {
			LOG(ERROR) << "Collision in static features: " << feature->getName() << ": " << staticFeatureIds.back() << std::endl;
			assert(false && "Collision in static features");
		} else
			cCheck[staticFeatureIds.back()] = 1;
#endif

		database.insertIntoStaticFeatures(staticFeatureIds.back(), feature->getName(), checkBeforeInsert);
	});
	database.commitStaticFeaturesTransaction();
/*
	// write dynamic features to table
	database.beginDynamicFeaturesTransaction();
	for_each(dynamicFeatures, [&](std::string feature) {
		dynamicFeatureIds.push_back(string_hash(feature));
		database.insertIntoDynamicFeatures(dynamicFeatureIds.back(), feature);
	});
	database.commitDynamicFeaturesTransaction();
*/
}

int64_t extractFeaturesFromAddress(core::NodeAddress kernelCode, const CmdOptions& options, ml::Database& database, vector<ft::FeaturePtr>& staticFeatures,
		vector<int64_t> staticFeatureIds, boost::unordered_map<int64_t, std::string> cCheck, const std::string& kernelName ) {
	try {
//core::printer::PrettyPrinter pp(kernelCode);
//std::cout << pp << std::endl;
//		utils::Timer timer("Write measurements to database time");
		vector<ft::Value> values = driver::extractFeatures(kernelCode, staticFeatures);
//		timer.stop();

		int64_t cid = (*kernelCode).hash();

		{
			if(cCheck.find(cid) != cCheck.end()) {
				throw(CodeEqualException(kernelName, cCheck[cid], cid));
			} else
				cCheck[cid] = kernelName;

			size_t j = 0;
			for_range(make_paired_range(staticFeatureIds, values), [&](std::pair<int64_t, ft::Value> value) {

				LOG(DEBUG) << kernelName << " " << options.sFeatures.at(j) << " \t" << analysis::features::getValue<double>(value.second);
				database.insertIntoCode(cid, value.first, analysis::features::getValue<double>(value.second));
				++j;
			});
		}

		return cid;
	} catch (const CodeEqualException& cee) {
		LOG(ERROR) << cee.what();
	}

	return 0;
}

void processFile(const CmdOptions& options, ml::Database& database, vector<ft::FeaturePtr>& staticFeatures,
		vector<int64_t> staticFeatureIds) {

	bfs::path kernelFile(options.rootDir);

	boost::unordered_map<int64_t, std::string> cCheck;

	LOG(INFO) << "Processing file: " << kernelFile << std::endl;

	try {
		bool dump = false;
		std::ofstream cidOut;
		if(options.dumpCid.size() > 0) {
			dump = true;
			cidOut.open(options.dumpCid);
		}


		if (bfs::exists(kernelFile) && bfs::file_size(kernelFile) > 500000) {
			LOG(WARNING) << "Ignoring Large File: " << kernelFile << "\n";
			return;
		}

		database.beginDataTransaction();

		core::NodeManager manager;

		std::fstream in(kernelFile.string(), std::fstream::in);
		core::NodeAddress kernelCode = core::dump::binary::loadAddress(in, manager);

		int64_t cid = extractFeaturesFromAddress(kernelCode, options, database, staticFeatures, staticFeatureIds, cCheck, kernelFile.string());
		if(dump) {
			cidOut << kernelFile.string() << " " << cid << std::endl;
			cidOut.close();
		}
		database.commitDataTransaction();

	} catch (const core::dump::InvalidEncodingException& iee) {
		LOG(ERROR) << "Invalid encoding within kernel file of " << kernelFile;
	} catch (boost::filesystem3::filesystem_error& bffe) {
		LOG(ERROR) << kernelFile << " is not a valid binary INSPIRE file";
	}

}

void processDirectory(const CmdOptions& options, ml::Database& database, vector<ft::FeaturePtr>& staticFeatures,
		vector<int64_t> staticFeatureIds/* vector<int64_t> dynamicFeatureIds*/) {

	// access root directory
	bfs::path dir(options.rootDir);
	LOG(INFO) << "Processing directory: " << dir << "\n";

	if (!bfs::is_directory(dir)) {
		LOG(ERROR) << "Not a directory!" << std::endl;
		return;
	}

	vector<bfs::path> kernels;
	for (auto it = bfs::recursive_directory_iterator(dir);
			it != bfs::recursive_directory_iterator(); ++it) {

		auto kernelFile = it->path() / "kernel.dat";

		if (bfs::exists(kernelFile) && bfs::file_size(kernelFile) > 500000) {
			LOG(ERROR) << "Ignoring Large File: " << kernelFile << "\n";
			continue;
		}

		if (bfs::exists(kernelFile) && bfs::file_size(kernelFile) < 500000) {
			kernels.push_back(kernelFile);
		}
	}


	LOG(INFO) << "Found " << kernels.size() << " kernels!" << std::endl;
	assert(kernels.size() > 0 && "No kernels found");

//	std::cout << "Static features;" << join(";",staticFeatures, print<deref<ft::FeaturePtr>>()) << "\n";

	boost::unordered_map<int64_t, std::string> cCheck;

	database.beginDataTransaction();
	// process all identifies kernels ...
	{
		core::NodeManager manager;

		bool dump = false;
		std::ofstream cidOut;
		if(options.dumpCid.size() > 0) {
			dump = true;
			cidOut.open(options.dumpCid);
		}

		for (std::size_t i=0; i<kernels.size(); i++) {
			auto path = kernels[i];

			try {
				std::cout << "Processing Kernel " << path.string() << "\n";

				std::fstream in(path.string(), std::fstream::in);
				core::NodeAddress kernelCode = core::dump::binary::loadAddress(in, manager);

				/*		auto version 	= path.parent_path();
						auto kernel 	= version.parent_path();
						auto benchmark 	= kernel.parent_path();
				*/

				size_t cid = extractFeaturesFromAddress(kernelCode, options, database, staticFeatures, staticFeatureIds, cCheck, path.string());
				if(dump) {
					cidOut << path.string() << " " << cid << std::endl;
				}

				/*
				std::cout << benchmark.filename()
						<< "; " << kernel.filename()
						<< "; " << version.filename()
						<< "; " << ::join(";", values)
	//								<< "; " << num_allocs
	//								<< "; " << reuseDistance
						<< "; " << (long)(timer.getTime()*1000)
						<< "\n";*/
			} catch (const core::dump::InvalidEncodingException& iee) {
				LOG(ERROR) << "Invalid encoding within kernel file of " << path;
			}

		}
		if(dump) {
			cidOut.close();
		}

	}

	database.commitDataTransaction();
}

/**
 * The data base generator entry point.
 */
int main(int argc, char** argv) {
	core::NodeManager manager;

	// process handle command line arguments
	CmdOptions options = parseCommandLine(argc, argv);
	if (!options.valid) {
		return 1;
	}

	LOG(INFO) << " --- Insieme Feature Database Generator, Version 0.0..01beta ---- \n";

	// load features
	analysis::features::FeatureCatalog catalog;
	catalog.addAll(ft::getFullCodeFeatureCatalog());

	// collect features
	vector<ft::FeaturePtr> staticFeatures;

	if(options.sFeatures.size() != 0) {
		// get all features not specified in the arguments
		for_each(options.sFeatures, [&](std::string feature) {
			auto f = catalog.getFeature(feature);
			if(f)
				staticFeatures.push_back(f);
			else
				LOG(ERROR) << "Could not find '" << feature << "'\n\tskipping it\n";
		});
		if(staticFeatures.empty()) {
			LOG(ERROR) << "None of the specified features could be found\n";
			return 2;
		}
	}

	// create database to write the features to
	// create and open database
	ml::Database database(options.databaseFile, options.clear);

	// vector containing the ids of all features (= hash of names) as they are inserted in the database
	vector<int64_t> staticFeatureIds;//, dynamicFeatureIds;

	writeFeaturesTables(database, staticFeatures, staticFeatureIds/*, options.dFeatures, dynamicFeatureIds*/, !options.clear);

	if(options.recursive)
		processDirectory(options, database, staticFeatures, staticFeatureIds/*, dynamicFeatureIds*/);
	else
		processFile(options, database, staticFeatures, staticFeatureIds);

	return 0;

}
