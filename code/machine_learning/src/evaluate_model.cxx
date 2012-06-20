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
#include <omp.h>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#include "KompexSQLitePrerequisites.h"
#include "KompexSQLiteDatabase.h"
#include "KompexSQLiteStatement.h"
#include "KompexSQLiteException.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/timer.h"

#include "insieme/machine_learning/database_utils.h"
#include "insieme/machine_learning/evaluator.h"
#include "insieme/machine_learning/myModel.h"
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
	std::string modelPath;				/* < the path to an .mod and .fnp file to read the model from. */
	std::string databaseFile;			/* < the database file to read the data from. */
	std::vector<std::string> sFeatures;	/* < a list of static features to use. */
	std::vector<std::string> dFeatures;	/* < a list of dynamic features to use. */
	std::vector<std::string> pFeatures; /* < a list of pca features to use. */
	std::string target;					/* < target name to evaluate on. */
	std::vector<std::string> filter;	/* < cids to use for evaluation. */
	bool calcError;						/* < flag to calculate the error of the entire dataset */
	bool svm;							/* < flag use a svm instead of a neural network */
	std::string out;					/* < File where to write the output */
};


const std::string getFeatureIdByName(const std::string& featureName, const std::string& tableName, Kompex::SQLiteDatabase* database){
	Kompex::SQLiteStatement stmt(database);
	// build query for name
	std::string tmp;
	try {
		std::stringstream qss;
		qss << "SELECT id FROM " << tableName << " f WHERE f.name = \"" << featureName << "\"";

		// query for the index of that name
		tmp = stmt.GetSqlResultString(qss.str());

	} catch(Kompex::SQLiteException &exception)	{
		LOG(ERROR) << exception.GetString();
		tmp = "";
	}
	if(tmp == "") {
		std::string err = "\nCannot find feature " + featureName;
		LOG(ERROR) << err << std::endl;
		throw ml::MachineLearningException(err);
	}
	// return feature index
	return tmp;
}

CmdOptions parseCommandLine(int argc, char** argv, Kompex::SQLiteDatabase*& database) {
	CmdOptions fail;
	fail.valid = false;

	// -- parsing -------------------------------------------

	// define options
	bpo::options_description desc("Supported Parameters");
	desc.add_options()
			("help,h",                                           			"produce help message")
			("model-path,m",    	bpo::value<std::string>(),         		"the path to an .mod and .fnp file to read the model from")
			("database-file,b", 	bpo::value<std::string>(),         		"the database file to read the data from")
			("static-features,s",	bpo::value<std::vector<std::string>>(), "static features to use")
			("dynamic-features,d",	bpo::value<std::vector<std::string>>(), "dynamic features to use")
			("pca-features,p",		bpo::value<std::vector<std::string>>(), "principal compontent analysis features to use")
			("target,t",			bpo::value<std::string>(),				"target name to evaluate on")
			("filter-cid,f",		bpo::value<std::vector<std::string>>(),	"cids to use for evaluation")
			("calculate-error,e",											"flag to calculate the error of the entire dataset")
			("svm,v",														"flag use a svm instead of a neural network")
			("output,o",			bpo::value<std::string>(),				"File where to write the output. Optional, default: std::out")
			("log-level,L",     	bpo::value<std::string>(),        	 	"Log level: DEBUG|INFO|WARN|ERROR|FATAL")
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

	// model path
	if (map.count("model-path")) {
		res.modelPath = map["model-path"].as<std::string>();
	} else {
		LOG(ERROR) << "No model set!\n";
		return fail;
	}

	// database path
	if (map.count("database-file")) {
		res.databaseFile = map["database-file"].as<std::string>();
		database = new Kompex::SQLiteDatabase(res.databaseFile, SQLITE_OPEN_READONLY, 0);
	} else {
		LOG(ERROR) << "No SQLite database set!\n";
		return fail;
	}
	// static features
	if (map.count("static-features")) {
		for_each(map["static-features"].as<std::vector<std::string>>(), [&](std::string staticFeature) {
			res.sFeatures.push_back(getFeatureIdByName(staticFeature, "static_features", database));
		});
	}

	// dynamic features
	if (map.count("dynamic-features")) {
		for_each(map["dynamic-features"].as<std::vector<std::string>>(), [&](std::string dynamicFeature) {
			res.dFeatures.push_back(getFeatureIdByName(dynamicFeature, "dynamic_features", database));
		});
	}

	// principal component analysis features
	if (map.count("pca-features")) {
		for_each(map["pca-features"].as<std::vector<std::string>>(), [&](std::string pcaFeature) {
			res.pFeatures.push_back(getFeatureIdByName(pcaFeature, "pca_features", database));
		});
	}

	// check if features are set at all
	if (res.sFeatures.empty() && res.dFeatures.empty() && res.pFeatures.empty()) {
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

	if(map.count("filter-cid")) {
		res.filter = map["filter-cid"].as<std::vector<std::string>>();
	} else {
		LOG(WARNING) << "No filter-cid specified! Using all cids for evaluation";
	}

	if(map.count("calculate-error")) {
		res.calcError = true;
	} else {
		res.calcError = false;
	}

	if(map.count("svm")) {
		res.svm = true;
	} else {
		res.svm = false;
	}

	if(map.count("output")) {
		res.out = map["output"].as<std::string>();
	} else {
		res.out = std::string("");
	}


	// create result
	return res;
}

void printCmdOptions(CmdOptions options, std::ostream& out) {
	out << "model-path       " << options.modelPath << std::endl;
	out << "database-file    " << options.databaseFile << std::endl;
	out << "static-features  ", for_each(options.sFeatures, [&](std::string sf) { out << sf << " "; }), out << std::endl;
	out << "dynamic-features ", for_each(options.dFeatures, [&](std::string df) { out << df << " "; }), out << std::endl;
	out << "pca-features     ", for_each(options.pFeatures, [&](std::string pf) { out << pf << " "; }), out << std::endl;
	out << "target           " << options.target << std::endl;
	out << "filter           ", for_each(options.filter, [&](std::string f) { out << f << " "; }), out << std::endl;
}

size_t nFeatures(CmdOptions options) {
	return options.sFeatures.size() + options.dFeatures.size() + options.pFeatures.size();
}

std::string genQuery(CmdOptions options) {
	std::stringstream qss;
	qss << "SELECT \n";
	size_t s = options.sFeatures.size();
	size_t d = options.dFeatures.size();
	size_t p = options.pFeatures.size();
	for(size_t i = 0; i < s; ++i) {
		qss << " c" << i << ".value AS Feature" << i << ",\n";
	}
	for(size_t i = 0; i < d; ++i) {
		qss << " s" << i << ".value AS Feature" << i + s << ",\n";
	}
	for(size_t i = 0; i < p; ++i) {
		qss << " p" << i << ".value AS Feature" << i << ",\n";
	}
	qss << " m." << options.target << " AS target FROM measurement m \n";
	for(size_t i = 0; i < s; ++i) {
		qss << " JOIN code c" << i << " ON m.cid=c" << i << ".cid AND c" << i << ".fid=" << options.sFeatures[i] << std::endl;
	}
	for(size_t i = 0; i < d; ++i) {
		qss << " JOIN setup s" << i << " ON m.sid=s" << i << ".sid AND s" << i << ".fid=" << options.dFeatures[i] << std::endl;
	}
	for(size_t i = 0; i < p; ++i) {
		qss << " JOIN principal_component p" << i << " ON m.pid=p" << i << ".pid AND p" << i << ".fid=" << options.pFeatures[i] << std::endl;
	}

	// filter for cids
	if(options.filter.size() > 0) {
		qss << " AND (m.cid=" << options.filter[0];
		for(size_t i = 1; i < options.filter.size(); ++i)
			qss << " OR" << " m.cid=" << options.filter[i];
		qss << " )" << std::endl;
	}

//std::cout << qss.str() << std::endl;
	return qss.str();
}

bool evaluate(Evaluator& eval, Array<double>& in, size_t expected, std::ostream& out) throw(MachineLearningException) {
	double start = omp_get_wtime();

	size_t actual = eval.evaluate(in);

	bool correct = expected == actual;
	double end = omp_get_wtime();


	out << correct << " Expected: " << expected << "; Actual: " << actual << std::endl;

	LOG(DEBUG) << "Evaluated in " << (end - start) * 1000 << " msec\n";
	return correct;
}

bool evaluateError(Evaluator& eval, Array<double>& in, size_t expected, double& errorSum, std::ostream& out)
		throw(MachineLearningException) {
	Array<double> actualOut;
	size_t i = 0;

	double start = omp_get_wtime();

	size_t actual = eval.evaluate(in, actualOut);

	double end = omp_get_wtime();

	out << "Expected: " << expected << "; Actual: " << actual << " - ";
	for_each(actualOut, [&](double ao) {
		double exVal = expected==i ? POS : NEG;
		errorSum += (ao - exVal) * (ao - exVal);
		out << ao << " ";

		++i;
	});
	out << std::endl;

	LOG(DEBUG) << "Evaluated in " << (end - start) * 1000 << " msec\n";
	return expected == actual;
}

typedef std::shared_ptr<MyModel> ModelPtr;

size_t evaluateDatabase(CmdOptions options, Kompex::SQLiteDatabase* database, std::ostream& out) throw(MachineLearningException, Kompex::SQLiteException) {
	size_t num = nFeatures(options);
	insieme::utils::Timer evalTimer("MachineLearning.evaluation ");

	// declare Machine
	RBFKernel kernel(1.0);
	//LinearKernel kernel;
	ModelPtr model = options.svm ? (ModelPtr)std::make_shared<MyMultiClassSVM>(&kernel) : (ModelPtr)std::make_shared<MyFFNet>();

	Evaluator eval = Evaluator::loadEvaluator(*model, options.modelPath);

	assert((options.svm || num == model->getInputDimension()) && "The number of specified features does not match the input size of the loaded model");

	Kompex::SQLiteStatement stmt(database);

	stmt.Sql(genQuery(options));

	size_t nRows = stmt.GetNumberOfRows();
	LOG(INFO) << "Queried Rows: " << nRows << ", Number of features: " << options.sFeatures.size() << " + " << options.dFeatures.size()  <<
			" + " << options.pFeatures.size() << std::endl;
	if(nRows == 0)
		throw MachineLearningException("No dataset for the requested features could be found");

	Array<double> in(options.sFeatures.size() + options.dFeatures.size() + options.pFeatures.size());
	double mse;

	// fetch all results
	while(stmt.FetchRow()){
//std::cout << "Result: " << stmt.GetColumnName(1) << " " << stmt.GetColumnName(2) << " " << stmt.GetColumnName(3) << std::endl;
//std::cout << "Data:   " << stmt.GetColumnInt(1) << " " << stmt.GetColumnInt(2) << " " << stmt.GetColumnInt(3) << std::endl;

		// construct training vectors
		for(size_t j = 0; j < num; ++j) {
			in(j) = stmt.GetColumnDouble(j);
		}


		// translate index to one-of-n coding
		// TODO add something else than keep int
		size_t expected = size_t(stmt.GetColumnDouble(num));

		if(options.calcError)
			evaluateError(eval, in, expected, mse, out);
		else
			evaluate(eval, in, expected, out);
	}

	// reset the prepared statement
	stmt.Reset();

	if(options.calcError) {
		mse /= nRows;
		out << "MSE: " << mse << std::endl;
	}

	// do not forget to clean-up
	stmt.FreeQuery();

	evalTimer.stop();
	std::cout << evalTimer << std::endl;
	return nRows;
}

/**
 * The model/code evaluation entry point.
 */
int main(int argc, char** argv) {
	//TODO add flag and functionality for output class generation, at the moment only keepInt is needed

	Kompex::SQLiteDatabase* database;

	// process handle command line arguments
	CmdOptions options = parseCommandLine(argc, argv, database);
	if (!options.valid) {
		return 1;
	}

	LOG(INFO) << " --- Insieme Model Evaluation, Version 0.0..01beta ---- \n";

	try {
		std::ofstream out(options.out);
		if(out.is_open()) {
			// print cmdOptions when writitng to a file
			printCmdOptions(options, out);
			evaluateDatabase(options, database, out);
			out.close();
		} else {
			evaluateDatabase(options, database, std::cout);
		}

		database->Close();
		delete database;
	} catch(MachineLearningException& mle) {
		LOG(ERROR) << "Machine Learning Exception: " << mle.what();
	} catch(Kompex::SQLiteException& sle) {
		LOG(ERROR) << "Database Exception: " << sle.GetString();
	}


	return 0;

}
