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

#pragma once

#include "insieme/driver/integration/test_step.h"
#include "insieme/driver/integration/test_framework.h"
#include "insieme/driver/integration/metrics.h"

typedef itc::IntegrationTestCase TestCase;

namespace insieme {
namespace driver {
namespace integration {
namespace metrics {

	class TestOutput {
	  protected:
		map<TestCase, vector<pair<TestStep, TestResult>>> allResults;
		TestOutput(map<TestCase, vector<pair<TestStep, TestResult>>> results) {
			allResults = results;
		}
		TestOutput(){};

	  public:
		virtual void writeOutput(bool overwrite) = 0;
	};

	// supported output formats
	class SQLOutput : public TestOutput {
	  public:
		SQLOutput(map<TestCase, vector<pair<TestStep, TestResult>>> results) : TestOutput(results){};

		void writeOutput(bool overwrite) {
			string sql = getSQLInit(true);
			for(auto it = allResults.begin(); it != allResults.end(); it++) {
				sql += getSQLString(it->first, it->second, overwrite);
			}

			ofstream sqlFile("metrics.sql");
			if(sqlFile.is_open()) {
				sqlFile << sql;
				sqlFile.close();
			} else {
				LOG(ERROR) << "Unable to open file metrics.sql for writing!" << std::endl;
			}
		}
	};

	class CSVOutput : public TestOutput {
	  public:
		CSVOutput(map<TestCase, vector<pair<TestStep, TestResult>>> results) : TestOutput(results){};

		void writeOutput(bool overwrite) {
			string header("");
			string staticheader("name,");
			string csv;

			// generate common step/value list, necessary if some tests have more steps than others
			//<step,map<metric (runtime,memory, etc.),value>>
			map<string, map<string, string>> stepVals;

			// not really performant, but i've no idea how to do it better
			for(auto it = allResults.begin(); it != allResults.end(); it++)
				for(auto step = it->second.begin(); step != it->second.end(); step++)
					if(step->first.getStepType() == RUN) {
						map<string, string> metricNames;
						map<string, float> results = step->second.getMetrics();
						for(auto metric = results.begin(); metric != results.end(); metric++) {
							metricNames[metric->first] = "undefined";
						}
						stepVals[step->first.getName()] = metricNames;
					} else if(step->first.getStepType() == STATIC_METRIC) {
						StaticResult* statRes = (StaticResult*)&(step->second);
						map<string, string> metricResults = statRes->getStaticMetrics();
						map<string, string> metricNames;
						for(auto metric = metricResults.begin(); metric != metricResults.end(); metric++) {
							metricNames[metric->first] = "undefined";
						}
						stepVals["static"] = metricNames;
					}

			// get real results and append it to the csv
			for(auto it = allResults.begin(); it != allResults.end(); it++) {
				map<string, map<string, string>> stepValsTest = stepVals;
				header = "";
				csv += "\n" + it->first.getName();
				for(auto step = it->second.begin(); step != it->second.end(); step++) {
					if(step->first.getStepType() == RUN) {
						map<string, float> results = step->second.getMetrics();
						map<string, string> txtResults;
						for(auto metric = results.begin(); metric != results.end(); metric++) {
							txtResults[metric->first] = std::to_string(metric->second);
						}
						stepValsTest[step->first.getName()] = txtResults;
					} else if(step->first.getStepType() == STATIC_METRIC) {
						StaticResult* statRes = (StaticResult*)&(step->second);
						map<string, string> metricResults = statRes->getStaticMetrics();
						map<string, string> txtResults;
						for(auto metric = metricResults.begin(); metric != metricResults.end(); metric++) {
							txtResults[metric->first] = metric->second;
						}
						stepValsTest["static"] = txtResults;
					}
				}

				for(auto step = stepValsTest.begin(); step != stepValsTest.end(); step++) {
					for(auto result = step->second.begin(); result != step->second.end(); result++) {
						if(step->first == "static") {
							header += result->first + ",";
						} else {
							header += step->first + "[" + result->first + "],";
						}
						csv += "," + result->second;
					}
				}
			}
			header = staticheader + header;                 // add static vals
			header = header.substr(0, header.length() - 1); // remove last ","

			ofstream csvFile("metrics.csv");
			if(csvFile.is_open()) {
				csvFile << header << csv;
				csvFile.close();
			} else {
				LOG(ERROR) << "Unable to open file metrics.csv for writing";
			}
		}
	};

	class DummyOutput : public TestOutput {
	  public:
		DummyOutput(){};
		void writeOutput(bool overwrite){};
	};

	TestOutput* createTestOutput(string key, map<TestCase, vector<pair<TestStep, TestResult>>> results) {
		if(boost::to_upper_copy(key) == "SQL") {
			return new SQLOutput(results);
		} else if(boost::to_upper_copy(key) == "CSV") {
			return new CSVOutput(results);
		} else {
			LOG(WARNING) << "Output format " << key << " not supported!";
			return new DummyOutput();
		}
	}

} // end namespace metrics
} // end namespace integration
} // end namespace driver
} // end namespace insieme
