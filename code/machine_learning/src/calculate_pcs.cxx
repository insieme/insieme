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

#include "KompexSQLitePrerequisites.h"
#include "KompexSQLiteDatabase.h"
#include "KompexSQLiteStatement.h"
#include "KompexSQLiteException.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
//#include "insieme/utils/timer.h"
#include "insieme/machine_learning/database_utils.h"
#include "insieme/machine_learning/pca_separate_ext.h"
#include "insieme/machine_learning/pca_combined_ext.h"
#include "insieme/machine_learning/machine_learning_exception.h"

#include "insieme/machine_learning/trainer.h" // only needed to get POS and NEG

using namespace insieme;
using namespace insieme::ml;
namespace bpo = boost::program_options;
namespace bfs = boost::filesystem;

// checks if there are some collisions in the hashes (ids) added to the database during this call
#define CHECK_FOR_COLLISIONS 1

/**
 * A struct aggregating command line options.
 */
struct CmdOptions {
	bool valid;						    /* < a flag determining whether the cmd options are valid or not. */
	std::string databaseFile;			/* < the database file to read the data from. */
	std::vector<std::string> sFeatures;	/* < a list of static features to use. */
	std::vector<std::string> dFeatures;	/* < a list of dynamic features to use. */
	size_t numPcaFeatures;				/* < number of principal components to calculate. */
	double pcaCoverage;					/* < percentage of variance to be covered by the principal components. */
	std::string mangling;				/* < a postfix that will be added to every feature name in order to distinguish this PCs from others. */
	std::string target;					/* < target name to evaluate on. */
	std::vector<std::string> exclude;   /* < cids to exclude from the principal components calculation */
	std::vector<std::string> filter;	/* < cids to use for evaluation. */
	bool combine;						/* < if set, static and dynamic features will be combined to form pca features. */
	std::string out;					/* < File where to write the output */
};


size_t nFeatures(CmdOptions options) {
	return options.sFeatures.size() + options.dFeatures.size();
}

CmdOptions parseCommandLine(int argc, char** argv) {
	CmdOptions fail;
	fail.valid = false;

	// -- parsing -------------------------------------------

	// define options
	bpo::options_description desc("Supported Parameters");
	desc.add_options()
			("help,h",                                           			"produce help message")
			("database-file,b", 	bpo::value<std::string>(),         		"the database file to read the data from")
			("static-features,s",	bpo::value<std::vector<std::string>>(), "static features to use")
			("dynamic-features,d",	bpo::value<std::vector<std::string>>(), "dynamic features to use")
			("num-pca-features,n",	bpo::value<size_t>(),  				    "number of principal components to calculate")
			("pca-coverage,p",		bpo::value<double>(), 				    "percentage of variance to be covered by the principal components")
			("mangling,m",			bpo::value<std::string>(),      		"a postfix that will be added to every feature name in order to distinguish this PCs from others")
			("target,t",			bpo::value<std::string>(),				"target name to evaluate on")
			("exclude-cid,e",		bpo::value<std::vector<std::string>>(),	"cids to exclude from the principal components calculation")
			("filter-cid,f",		bpo::value<std::vector<std::string>>(),	"cids to use to calculate the principal components")
			("combine,c",													"if set, static and dynamic features will be combined to form pca features")
			("log-level,L",     	bpo::value<std::string>(),        	 	"Log level: DEBUG|INFO|WARN|ERROR|FATAL")
			("output,o",			bpo::value<std::string>(),				"File where to write the output. Optional, default: std::out")
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

	// database path
	if (map.count("database-file")) {
		res.databaseFile = map["database-file"].as<std::string>();
	} else {
		LOG(ERROR) << "No SQLite database set!\n";
		return fail;
	}
	// static features
	if (map.count("static-features")) {
		for_each(map["static-features"].as<std::vector<std::string>>(), [&](std::string staticFeature) {
			res.sFeatures.push_back(staticFeature);
		});
	}

	// dynamic features
	if (map.count("dynamic-features")) {
		for_each(map["dynamic-features"].as<std::vector<std::string>>(), [&](std::string dynamicFeature) {
			res.dFeatures.push_back(dynamicFeature);
		});
	}

	// number of principal compontents to calculate
	if (map.count("num-pca-features")) {
		res.numPcaFeatures = map["num-pca-features"].as<size_t>();
		if(res.numPcaFeatures == 0) {
			LOG(ERROR) << "Number of PCA features must be bigger than 0!\n";
			return fail;
		}
		if(res.numPcaFeatures > nFeatures(res)) {
			LOG(ERROR) << "Cannot calculate " << res.numPcaFeatures << " principal compontents out of " << nFeatures(res) << " features!\n";
			LOG(ERROR) << "Number of PCA features must be lower than the number of input features\n";
			return fail;
		}
	} else res.numPcaFeatures = 0;

	// percentage of variance to be covered by the principal components
	if (map.count("pca-coverage")) {
		res.pcaCoverage = map["pca-coverage"].as<double>();
		if(res.numPcaFeatures != 0) {
			LOG(ERROR) << "Cannot set pca-coverage and num-pca-features. Choose one of those!";
			return fail;
		}
		if(res.pcaCoverage <= 0.0) {
			LOG(ERROR) << "PCA covergae must be bigger than 0.0!\n";
			return fail;
		}
	} else {
		if(res.numPcaFeatures == 0) {
			LOG(ERROR) << "Neither 'num-pca-features' nor 'pca-coverage' set.\n Set one of them to a value bigger 0!";
			return fail;
		}
		res.pcaCoverage = 0.0;
	}

	// mangling postfix for pca feature names
	if (map.count("mangling")) {
		res.mangling = map["mangling"].as<std::string>();
	} else {
		res.mangling = "";
	}

	// check if features are set at all
	if (res.sFeatures.empty() && res.dFeatures.empty()) {
		LOG(ERROR) << "No features set!\n";
		return fail;
	}

	// name of target
	if(map.count("target")) {
		res.target = map["target"].as<std::string>();
	} else {
		LOG(ERROR) << "No target specified!\n";
		return fail;
	}

	if(map.count("exclude-cid")) {
		res.exclude = map["exclude-cid"].as<std::vector<std::string>>();
	}

	if(map.count("filter-cid")) {
		res.filter = map["filter-cid"].as<std::vector<std::string>>();
	}

	if(map.count("output")) {
		res.out = map["output"].as<std::string>();
	} else {
		res.out = std::string("");
	}

	if(map.count("combine")) {
		res.combine = true;
	} else {
		res.combine = false;
	}

	// create result
	return res;
}

void printCmdOptions(CmdOptions options, std::ostream& out) {
	out << "database-file    " << options.databaseFile << std::endl;
	out << "static-features  ", for_each(options.sFeatures, [&](std::string sf) { out << sf << " "; }), out << std::endl;
	out << "dynamic-features ", for_each(options.dFeatures, [&](std::string df) { out << df << " "; }), out << std::endl;
	out << "num-pca-features " << options.numPcaFeatures << std::endl;
	out << "pca-coverage     " << options.pcaCoverage << std::endl;
	out << "mangling         " << options.mangling << std::endl;
	out << "target           " << options.target << std::endl;
	out << "exclude          ", for_each(options.exclude, [&](std::string e) { out << e << " "; }), out << std::endl;
	out << "filter           ", for_each(options.filter, [&](std::string f) { out << f << " "; }), out << std::endl;
	out << "combine          " << (options.combine ? "true\n" : "false\n");
}


typedef std::shared_ptr<PcaExtractor> PcaExtractorPtr;

void doPca(CmdOptions options, std::ostream& out) throw(MachineLearningException, Kompex::SQLiteException) {
	PcaExtractorPtr pse;

	if(options.combine)	pse = std::make_shared<PcaCombinedExt>(options.databaseFile);
	else pse = std::make_shared<PcaSeparateExt>(options.databaseFile);

	if(options.sFeatures.size() > 0) pse->setStaticFeaturesByName(options.sFeatures);
	if(options.dFeatures.size() > 0) pse->setDynamicFeaturesByName(options.dFeatures);

//	pse.setExcludeCodes(options.exclude);
//	pse.setFilterCodes(options.filter);

	pse->calcPca(options.numPcaFeatures,options.numPcaFeatures);
}

int main(int argc, char** argv) {
	//TODO add flag and functionality for output class generation, at the moment only keepInt is needed

	// process handle command line arguments
	CmdOptions options = parseCommandLine(argc, argv);
	if (!options.valid) {
		return 1;
	}

	LOG(INFO) << " --- Insieme Principal Component Calculator, Version 0.0..01beta ---- \n";

	try {
		std::ofstream out(options.out);
		if(out.is_open()) {
			// print cmdOptions when writing to a file
			printCmdOptions(options, out);
			doPca(options, out);
			out.close();
		} else {
			doPca(options, std::cout);
		}
	} catch(MachineLearningException& mle) {
		LOG(ERROR) << "Machine Learning Exception: " << mle.what();
	} catch(Kompex::SQLiteException& sle) {
		LOG(ERROR) << "Database Exception: " << sle.GetString();
	}


	return 0;

}
