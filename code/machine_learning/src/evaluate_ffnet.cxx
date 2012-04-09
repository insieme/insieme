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
#include "insieme/machine_learning/database_utils.h"
#include "insieme/machine_learning/evaluator.h"


using namespace insieme;
namespace bpo = boost::program_options;
namespace bfs = boost::filesystem;

// checks if there are some collisions in the hashes (ids) added to the database during this call
#define CHECK_FOR_COLLISIONS 1

/**
 * A struct aggregating command line options.
 */
struct CmdOptions {
	bool valid;						    /* < a flag determining whether the cmd options are valid or not. */
	std::string modelPath;				/* < the path to an .mod and .fnp file to read the model from. */
	std::vector<std::string> sFeatures;	/* < a list of static features to use. */
	std::vector<std::string> dFeatures;	/* < a list of dynamic features to use. */
	std::vector<std::string> pFeatures; /* < a list of pca features to use. */
};


CmdOptions parseCommandLine(int argc, char** argv) {
	CmdOptions fail;
	fail.valid = false;

	// -- parsing -------------------------------------------

	// define options
	bpo::options_description desc("Supported Parameters");
	desc.add_options()
			("help,h",                                           			"produce help message")
			("model-path,d",       bpo::value<std::string>(),         		"the path to an .mod and .fnp file to read the model from.")
			("static-features,s",  bpo::value<std::vector<std::string>>(), 	"static features to use")
			("dynamic-features,d", bpo::value<std::vector<std::string>>(), 	"dynamic features to use")
			("pca-features,d",	   bpo::value<std::vector<std::string>>(), 	"principal compontent analysis features to use")
			("log-level,L",        bpo::value<std::string>(),        	 	"Log level: DEBUG|INFO|WARN|ERROR|FATAL")
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
		Logger::get(std::cerr, LevelSpec<>::loggingLevelFromStr(map["log-level"].as<std::string>()));
	}

	// output file (optional)
	if (map.count("model-path")) {
		res.modelPath = map["model-path"].as<std::string>();
	} else {
		LOG(ERROR) << "No model set!\n";
		return fail;
	}

	// static features
	if (map.count("static-features")) {
		res.sFeatures = map["static-features"].as<std::vector<std::string>>();
	}

	// dynamic features
	if (map.count("dynamic-features")) {
		res.dFeatures = map["dynamic-features"].as<std::vector<std::string>>();
	}

	// static features
	if (map.count("pca-features")) {
		res.pFeatures = map["pca-features"].as<std::vector<std::string>>();
	}

	if (res.sFeatures.empty() && res.dFeatures.empty() && res.pFeatures.empty()) {
		LOG(ERROR) << "No features set!\n";
		return fail;
	}


	// create result
	return res;
}


/**
 * The model/code evaluation entry point.
 */
int main(int argc, char** argv) {
	// process handle command line arguments
	CmdOptions options = parseCommandLine(argc, argv);
	if (!options.valid) {
		return 1;
	}

	LOG(INFO) << " --- Insieme Model Evaluation, Version 0.0..01beta ---- \n";


	return 0;

}
