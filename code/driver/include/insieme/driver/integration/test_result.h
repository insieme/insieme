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

#pragma once


#include <string>
#include <vector>

#include "insieme/utils/logging.h"
using namespace std;

namespace insieme {
namespace driver {
namespace integration {

	enum SchedulingPolicy { SCHED_UNDEFINED, STATIC, DYNAMIC, GUIDED };

	class TestResult {
	  public:
		enum class ResultType { SUCCESS, FAILURE, ABORT, OMITTED };

	  private:
		ResultType resType;

		string stepName;
		int retVal;
		map<string, float> metricResults;
		map<string, float> metricDeviation;

		string output;
		string errorOut;
		string cmd;
		std::vector<std::string> producedFiles;
		int numThreads;
		SchedulingPolicy sched;

		friend class boost::serialization::access;
		template <class Archive>
		void serialize(Archive& ar, const unsigned int version) {
			ar& resType;
			ar& metricResults;
			ar& metricDeviation;
			ar& output;
			ar& errorOut;
			ar& cmd;
			ar& producedFiles;
			ar& numThreads;
			ar& sched;
		}

	  protected:
		map<string, string> staticResults;

	  public:
		TestResult(const ResultType& resType = ResultType::SUCCESS, string stepName = "", int retVal = 0,
		           map<string, float> metricResults = map<string, float>(), const string& output = "", const string& errorOut = "", const string& cmd = "",
		           std::vector<std::string> producedFiles = std::vector<std::string>(), const int& numThreads = 0, SchedulingPolicy sched = SCHED_UNDEFINED)
		    : resType(resType), stepName(stepName), retVal(retVal), metricResults(metricResults), metricDeviation(map<string, float>()), output(output),
		      errorOut(errorOut), cmd(cmd), producedFiles(producedFiles), numThreads(numThreads), sched(sched) {}

		static TestResult userAborted(const string& stepName) {
			TestResult res(ResultType::ABORT, stepName, -1);
			return res;
		}

		static TestResult stepOmitted(const string& stepName) {
			TestResult res(ResultType::OMITTED, stepName, -1);
			return res;
		}

		bool wasSuccessful() const {
			return resType == ResultType::SUCCESS;
		}

		bool wasOmitted() const {
			return resType == ResultType::OMITTED;
		}

		bool wasAborted() const {
			return resType == ResultType::ABORT;
		}

		// deletes all produced files
		void clean() const {
			for(const auto& cur : producedFiles) {
				remove(cur.c_str());
			}
		}

		static TestResult returnAVG(vector<TestResult> t) {
			TestResult ret;
			TestResult front = t.front();
			map<string, float> frontResults = front.getMetrics();
			float sum = 0, avg, var;

			// iterate over all collected metrics
			for(auto metr = frontResults.begin(); metr != frontResults.end(); metr++) {
				vector<float> metricResults;
				sum = 0;

				// iterate over all testRuns
				for(auto result = t.begin(); result != t.end(); result++) {
					map<string, float> testResults = result->getMetrics();

					// check if results are comparable
					if(frontResults.size() != testResults.size() || front.getNumThreads() != result->getNumThreads()
					   || front.getScheduling() != result->getScheduling() || !front.wasSuccessful() || !result->wasSuccessful()) {
						LOG(WARNING) << "Test results of a metric not comparable, no median calculated!";
						return front;
					}
					sum += testResults[metr->first];
					metricResults.push_back(testResults[metr->first]);
				}

				// calc avg
				avg = sum / t.size();

				// calc stddev
				float tmp = 0;
				for(float val : metricResults) {
					tmp += (val - avg) * (val - avg);
				}
				tmp /= t.size();
				var = sqrt(tmp);

				// insert into ret
				ret.insertMetric(metr->first, avg, var);
			}
			ret.producedFiles = front.producedFiles;
			return ret;
		}


		static TestResult returnMedian(vector<TestResult> t) {
			TestResult ret;
			TestResult front = t.front();
			map<string, float> frontResults = front.getMetrics();

			// iterate over all collected metrics
			for(auto metr = frontResults.begin(); metr != frontResults.end(); metr++) {
				vector<float> metricResults;

				// iterate over all testRuns
				for(auto result = t.begin(); result != t.end(); result++) {
					map<string, float> testResults = result->getMetrics();

					// check if results are comparable
					if(frontResults.size() != testResults.size() || front.getNumThreads() != result->getNumThreads()
					   || front.getScheduling() != result->getScheduling() || !front.wasSuccessful() || !result->wasSuccessful()) {
						LOG(WARNING) << "Test results of a metric not comparable, no median calculated!";
						return front;
					}
					metricResults.push_back(testResults[metr->first]);
				}

				// calc median and insert into ret
				std::sort(metricResults.begin(), metricResults.end());
				int size = metricResults.size();
				if(size % 2 != 0) {
					ret.insertMetric(metr->first, metricResults[size / 2]);
				} else {
					ret.insertMetric(metr->first, (metricResults[(size + 1) / 2] + metricResults[(size - 1) / 2]) / 2);
				}
			}

			ret.producedFiles = front.producedFiles;
			return ret;
		}

		string getCmd() const {
			return cmd;
		}

		string getStepName() const {
			return stepName;
		}

		int getRetVal() const {
			return retVal;
		}

		int getNumThreads() const {
			return numThreads;
		}

		string getFullOutput() const {
			return output + errorOut;
		}

		map<string, float> getMetrics() const {
			return metricResults;
		}

		float getRuntime() const {
			return metricResults.find("walltime")->second;
		}

		float getMemory() const {
			return metricResults.find("mem")->second;
		}

		float getRuntimeDev() const {
			return metricDeviation.find("walltime")->second;
		}

		float getMemoryDev() const {
			return metricDeviation.find("mem")->second;
		}

		void insertMetric(string name, float val) {
			metricResults[name] = val;
		}

		void insertMetric(string name, float val, float dev) {
			metricResults[name] = val;
			metricDeviation[name] = dev;
		}

		SchedulingPolicy getScheduling() const {
			return sched;
		}

		bool deviationAvailable() const {
			return metricResults.size() == metricDeviation.size();
		}

		string getSchedulingString() const {
			if(sched == STATIC) { return "static"; }
			if(sched == DYNAMIC) { return "dynamic"; }
			if(sched == GUIDED) { return "guided"; }
			return "undefined";
		}
	};

	class StaticResult : public TestResult {
	  public:
		StaticResult(map<string, string> results) {
			staticResults = results;
		};

		map<string, string> getStaticMetrics() {
			return staticResults;
		}
	};

} // end namespace integration
} // end namespace driver
} // end namespace insieme
