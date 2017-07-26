/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

/**
 * Within this file a small, simple example of a compiler driver utilizing
 * the insieme compiler infrastructure is presented.
 *
 * This file is intended to provides a template for implementing new compiler
 * applications utilizing the Insieme compiler and runtime infrastructure.
 */

#include <string>
#include "insieme/driver/integration/test_result.h"
#include "insieme/driver/integration/test_framework.h"
#include <vector>
#include <boost/functional/hash.hpp>
#include <boost/lexical_cast.hpp>
#include "insieme/utils/config.h"


namespace itc = insieme::driver::integration;
typedef itc::IntegrationTestCase TestCase;

namespace insieme {
namespace driver {
namespace integration {
namespace metrics {

	// executes a command and returns its output
	std::string executeCommand(std::string command) {
		FILE* pipe;
		pipe = popen(command.c_str(), "r");
		char tmpString[2048];
		std::string returnString("");
		LOG(INFO) << "Execute command: " << command;
		if(!pipe) {
			LOG(WARNING) << "Error executing command \"" << command << "\"!" << std::endl;
			returnString = "undefined";
		} else
			while(fgets(tmpString, 2048, pipe) != NULL) {
				returnString += string(tmpString);
			}

		pclose(pipe);
		return returnString;
	}

	TestResult getStaticMetrics(TestCase testcase, vector<pair<TestStep, TestResult>> results) {
		map<string, string> metricResults;

		std::string filenames("");
		for(const auto& cur : testcase.getFiles()) {
			filenames += " " + cur.string();
		}

		// TODO get codeType
		std::string codeType("");
		metricResults["codeType"] = codeType;

		// get loc
		std::string loc("");
		loc = executeCommand(std::string(TEST_ROOT_DIR) + "/cloc.pl " + filenames + " --quiet --xml");

		//"parse" xml to find total lines of code
		try {
			int tmp = loc.find("<total");
			loc = loc.substr(tmp);

			tmp = loc.find("code=\"");
			loc = loc.substr(tmp);

			tmp = loc.find_first_of("\"");
			loc = loc.substr(tmp + 1);

			tmp = loc.find_first_of("\"");
			loc = loc.substr(0, tmp);
		} catch(out_of_range) { loc = "undefined"; }

		if(!std::all_of(loc.begin(), loc.end(), ::isdigit)) {
			LOG(WARNING) << "Unable to determine LOC of " << testcase.getName() << std::endl;
			loc = "-1";
		}
		metricResults["loc"] = loc;

		// get size
		std::string size("");
		size = executeCommand("du -cL " + filenames + " | grep \"total\" | awk '{{print $1;}}'");
		size.resize(size.length() - 1); // remove line break
		metricResults["size"] = size;

		// get omp pragams
		std::string pragmas("");
		pragmas = executeCommand("egrep '#pragma omp' " + filenames + " | wc -l");
		pragmas.resize(pragmas.length() - 1); // remove line break
		metricResults["ompPragmas"] = pragmas;

		// get parType
		std::string parType("");
		if(testcase.isEnableOpenMP()) {
			parType = "omp";
		} else if(testcase.isEnableOpenCL()) {
			parType = "ocl";
		} else {
			parType = "unknown/sequential";
		}
		metricResults["parType"] = parType;

		// get failState
		std::string failstate("");
		for(const auto& stepResult : results)
			if(!stepResult.second.wasSuccessful()) {
				failstate = stepResult.first.getName();
				break;
			}
		metricResults["failState"] = failstate;

		return StaticResult(metricResults);
	}


	// gets all results of a testcase and generates an SQL string to insert it into a DB
	std::string getSQLString(TestCase testcase, vector<pair<TestStep, TestResult>> results, bool deleteCase) {
		std::string sqlStatement("");

		std::string filenames("");
		for(const auto& cur : testcase.getFiles()) {
			filenames += " " + cur.string();
		}

		// testID
		std::string testID = std::to_string(boost::hash_value(testcase.getName()));

		// DELETE IF NECESSARY
		if(deleteCase) { sqlStatement = sqlStatement + "DELETE FROM Test where id=\"" + testID + "\";\n"; }

		// INSERT RUNCONFIGURATIONS

		std::string sqlRunconfigs("");
		std::string host = executeCommand("hostname");
		host.resize(host.size() - 1);
		std::string insiemeVersion = itc::testFramework::getGitVersion();
		std::map<string, string> knownBackendCompiler;
		std::set<string> knownMetrics;

		for(const auto& stepResult : results) {
			TestStep step = stepResult.first;
			TestResult result = stepResult.second;

			// insert static metrics
			if(step.getStepType() == STATIC_METRIC) {
				StaticResult* statRes = (StaticResult*)&result;

				map<string, string> metricResults = statRes->getStaticMetrics();

				// INSERT TEST
				sqlStatement = sqlStatement + "\n" + "INSERT OR IGNORE INTO Test (id,name,codeType,loc,size,ompPragmas,parType,failState) " + "VALUES(\""
				               + testID + "\",\"" + testcase.getName() + "\",\"" + metricResults["codeType"] + "\"," + metricResults["loc"] + ","
				               + metricResults["size"] + "," + metricResults["ompPragmas"] + ",\"" + metricResults["parType"] + "\",\""
				               + metricResults["failstate"] + "\");\n";

			} else if(step.getStepType() == RUN) {
				std::string stepBaseName = step.getName();
				// get name until the second occurrence of "_"
				stepBaseName = stepBaseName.substr(0, stepBaseName.find("_", stepBaseName.find("_") + 1));


				std::string numThreads("-1");
				if(result.getNumThreads()) { numThreads = std::to_string(result.getNumThreads()); }

				std::string scheduling(result.getSchedulingString());

				// get appropriate backend compiler
				std::string compileStep = step.getName().substr(0, step.getName().find("execute")) + "compile";
				std::string backendComp = testcase.getPropertiesFor(compileStep)["compiler"];
				if(backendComp == "") {
					LOG(WARNING) << "Unable to get backend compiler for step " << step.getName() << " in test " << testcase.getName();
				} else {
					if(knownBackendCompiler.count(backendComp) == 0) {
						// INSERT BACKEND COMPILER

						// get backend compiler version
						std::string backendVersion("");
						if(backendComp.find("insiemecc") != backendComp.npos) {
							backendVersion = itc::testFramework::getGitVersion();
						} else {
							backendVersion = executeCommand(backendComp + " -dumpversion");
							backendVersion.resize(backendVersion.size() - 1); // remove line break
						}

						std::string backendID = std::to_string(boost::hash_value(backendComp + backendVersion));

						sqlRunconfigs = sqlRunconfigs + "INSERT OR IGNORE INTO BackendCompiler(id,name,version) " + "VALUES(\"" + backendID + "\",\""
						                + backendComp + "\",\"" + backendVersion + "\");\n";

						knownBackendCompiler[backendComp] = backendID;
					}
					// only set id of backend comp
					backendComp = knownBackendCompiler[backendComp];
				}

				std::string runConfigID = std::to_string(boost::hash_value(testID + step.getName() + insiemeVersion + backendComp + host));

				sqlRunconfigs = sqlRunconfigs + "\n"
				                + "INSERT OR IGONORE INTO RunConfiguration(id,testID,step,numThreads,scheduling,insiemeVersion,backendCompiler,host) "
				                + "VALUES(\"" + runConfigID + "\",\"" + testID + "\",\"" + stepBaseName + "\"," + numThreads + ",\"" + scheduling + "\",\""
				                + insiemeVersion + "\",\"" + backendComp + "\",\"" + host + "\");\n";

				// INSERT METRICS AND RESULTS
				map<string, float> metricResults = result.getMetrics();

				for(auto metr = metricResults.begin(); metr != metricResults.end(); metr++) {
					// check if metric has to be inserted into DB
					if(knownMetrics.find(metr->first) == knownMetrics.end()) {
						sqlRunconfigs = sqlRunconfigs + "INSERT OR IGNORE INTO Metric(name) VALUES(\"" + metr->first + "\");\n";
						knownMetrics.insert(metr->first);
					}

					// insert value
					sqlRunconfigs = sqlRunconfigs + "INSERT INTO Result(metricID,runID,value) " + "VALUES(\"" + metr->first + "\",\"" + runConfigID + "\","
					                + std::to_string(metr->second) + ");\n";
				}
			}
		}

		return sqlStatement + sqlRunconfigs;
	}


	// returns an SQL String to create the database for the test results
	std::string getSQLCreate() {
		return std::string("\n"
		                   "PRAGMA foreign_keys = ON;\n" // enable foreign keys (for sqlite)
		                   "CREATE TABLE IF NOT EXISTS Test (\n"
		                   "	id varchar(20) PRIMARY KEY,\n"
		                   "	name varchar(50),\n"
		                   "	codeType varchar(10),\n"
		                   "	loc int,\n"
		                   "	size int,\n"
		                   "	ompPragmas int,\n"
		                   "	parType varchar(10),\n"
		                   "	failState varchar(30)\n"
		                   ");\n"
		                   "CREATE TABLE IF NOT EXISTS RunConfiguration(\n"
		                   "	id varchar(20) PRIMARY KEY,\n"
		                   "	testID varchar(20) REFERENCES Test(id) ON DELETE CASCADE,\n"
		                   "	step varchar(20),\n"
		                   "	numThreads int,\n"
		                   "	scheduling varchar(10),\n"
		                   "	insiemeVersion varchar(20) REFERENCES InsiemeVersion(name) ON DELETE CASCADE,\n"
		                   "	backendCompiler varchar(20) REFERENCES BackendCompiler(id) ON DELETE CASCADE,\n"
		                   "	host varchar(20) REFERENCES Host(name) ON DELETE CASCADE\n"
		                   ");\n"
		                   "CREATE TABLE IF NOT EXISTS Metric(\n"
		                   "	name varchar(20) PRIMARY KEY\n"
		                   ");\n"
		                   "CREATE TABLE IF NOT EXISTS Result(\n"
		                   "	id INTEGER PRIMARY KEY AUTOINCREMENT,"
		                   "	metricID varchar(20) REFERENCES Metric(name) ON DELETE SET NULL,\n"
		                   "	runID varchar(20) REFERENCES RunConfiguration(id) ON DELETE CASCADE,\n"
		                   "	value FLOAT\n,"
		                   "	executionDate timestamp DEFAULT CURRENT_TIMESTAMP\n"
		                   ");\n"
		                   "CREATE TABLE IF NOT EXISTS InsiemeVersion(\n"
		                   "	name varchar(20) PRIMARY KEY,\n"
		                   "	firstSeen timestamp DEFAULT CURRENT_TIMESTAMP\n"
		                   ");\n"
		                   "CREATE TABLE IF NOT EXISTS BackendCompiler(\n"
		                   "	id varchar(20) PRIMARY KEY,\n"
		                   "	name varchar(20),\n"
		                   "	version varchar(20)"
		                   ");\n"
		                   "CREATE TABLE IF NOT EXISTS Host(\n"
		                   "	name varchar(20) PRIMARY KEY,\n"
		                   "	kernelVersion varchar(20),\n"
		                   "	rooflinePoint float\n"
		                   ");\n");
	}

	// returns SQL String to prepare insertion of the test results
	std::string getSQLInit(bool createDB = false) {
		std::string initStr("");
		if(createDB) { initStr += getSQLCreate(); }

		// insert insieme version
		initStr += ("INSERT OR IGNORE INTO InsiemeVersion(name) "
		            "VALUES(\""
		            + insieme::driver::integration::testFramework::getGitVersion() + "\");\n");
		// insert host
		std::string hostname = executeCommand("hostname");
		hostname.resize(hostname.size() - 1);
		std::string kernelVersion = executeCommand("uname -r");
		kernelVersion.resize(kernelVersion.size() - 1);

		// TODO calculate roofline point
		std::string rooflinePoint("2.0");

		initStr += ("INSERT OR IGNORE INTO Host(name,kernelVersion,rooflinePoint) "
		            "VALUES(\""
		            + hostname + "\",\"" + kernelVersion + "\"," + rooflinePoint + ");\n");

		return initStr;
	}

} // end namespace metrics
} // end namespace integration
} // end namespace driver
} // end namespace insieme
