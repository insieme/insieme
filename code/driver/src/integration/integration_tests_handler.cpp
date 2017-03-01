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
 *
 */
#include "insieme/driver/integration/integration_tests_handler.h"

#include <string>

#include <boost/format.hpp>
#include <boost/program_options.hpp>

#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_step.h"
#include "insieme/driver/integration/test_framework.h"
#include "insieme/driver/perf_reg/logging.h"



namespace bpo = boost::program_options;

namespace itc = insieme::driver::integration;
typedef itc::IntegrationTestCase TestCase;
using itc::TestStep;
using itc::TestResult;

using insieme::driver::perf_reg::TestResultsLogger;

namespace tf = itc::testFramework;


namespace insieme {
namespace driver {
namespace integration {

	namespace {
		tf::Options parseCommandLine(int argc, char** argv) {
			static const tf::Options fail(false);

			// -- parsing -------------------------------------------

			// define options
			bpo::options_description desc("Supported Parameters");
			// clang-format off
			desc.add_options()
					("help,h",              "produce help message")
					("config,c",            "print the configuration of the selected test cases")
					("mock,m",              "make it a mock run just printing commands not really executing those")
					("panic,p",             "panic on first sign of trouble and stop execution")
					("list,l",              "just list the targeted test cases")
					("worker,w",            bpo::value<int>()->default_value(1), "the number of parallel workers to be utilized")
					("cases",               bpo::value<vector<string>>(),        "the list of test cases to be executed")
					("step,s",              bpo::value<string>(),                "the test step to be applied")
					("repeat,r",            bpo::value<int>()->default_value(1), "the number of times the tests shell be repeated")
					("no-clean",            "keep all output files")
					("no-color",            "force-disable highlighting of output")
					("blacklisted-only",    "only run the blacklisted test cases")
					("long-tests-only",     "only run test cases which take long to execute")
					("long-tests-also",     "also run test cases which take long to execute")
					("preprocessing",       "perform all pre-processing steps")
					("postprocessing",      "perform all post-processing steps")
					("log-to-csv-file",     bpo::value<string>()->implicit_value("./integration_tests_results.csv"), "Log memory and runtime results to a CSV file")
					("csv-file-id",         bpo::value<string>(), "Use a special ID for the CSV log file, e.g. the build number or Git commit hash")
					;
			// clang-format on

			// define positional options (all options not being named)
			bpo::positional_options_description pos;
			pos.add("cases", -1);

			// parse parameters
			bpo::variables_map map;
			bpo::store(bpo::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
			bpo::notify(map);


			// check whether help was requested
			if(map.count("help")) {
				std::cout << desc << "\n";

				std::cout << " ------------- Steps -----------------\n";
				for(auto entry : insieme::driver::integration::getFullStepList()) {
					std::cout << "\t" << std::left << std::setw(30) << entry.first;
					auto deps = entry.second.getDependencies();
					if(!deps.empty()) {
						std::cout << " <- [ ";
						for(auto dep : deps) {
							std::cout << dep << " ";
						}
						std::cout << "]";
					}
					std::cout << "\n";
				}

				return fail;
			}

			tf::Options res;

			if(map.count("config")) { res.print_configs = true; }

			if(map.count("cases")) { res.cases = map["cases"].as<vector<string>>(); }

			res.mockrun = map.count("mock");
			res.no_clean = map.count("no-clean");
			res.color = !map.count("no-color");
			// if colored output was not disabled explicitely, disable anyway if there is no support
			if(res.color) { res.color = isatty(fileno(stdout)); }
			res.panic_mode = map.count("panic");
			res.num_threads = map["worker"].as<int>();
			res.num_repetitions = map["repeat"].as<int>();
			res.perf = false;

			res.list_only = map.count("list");

			if(map.count("step")) { res.steps.push_back(map["step"].as<string>()); }

			res.blacklistedOnly = map.count("blacklisted-only");
			res.longTestsOnly = map.count("long-tests-only");
			res.longTestsAlso = map.count("long-tests-also");

			res.preprocessingOnly = map.count("preprocessing");
			res.postprocessingOnly = map.count("postprocessing");

			res.logToCsvFile = map.count("log-to-csv-file");
			res.csvFile = res.logToCsvFile ? map["log-to-csv-file"].as<string>() : "";

			if(map.count("csv-file-id")) {
				res.csvFileIDisset = true;
				res.csvFileID = map["csv-file-id"].as<string>();
				if(!res.logToCsvFile) {
					LOG(WARNING) << "Ignoring CSV file ID, as logging has not been enabled";
				}
			}

			return res;
		}

		void printSummary(const int totalTests,
		                  const std::vector<TestCase>& okSteps, const bool blacklistedOnly,
		                  const int omittedCount,
		                  const std::map<TestCase, TestResult>& failedSteps,
		                  const int screenWidth,
		                  const tf::Colorize& col) {
			string footerSummaryFormat("%" + to_string(screenWidth - 12) + "d");
			string centerAlign("%|=" + to_string(screenWidth - 2) + "|");

			std::cout << "#" << string(screenWidth - 2, '-') << "#\n";
			std::cout << "#" << boost::format(centerAlign) % "INTEGRATION TEST SUMMARY"
					<< "#\n";
			std::cout << "#" << string(screenWidth - 2, '-') << "#\n";
			std::cout << "# TOTAL:  " << boost::format(footerSummaryFormat) % totalTests << " #\n";
			std::cout << "# PASSED: " << col.green() << boost::format(footerSummaryFormat) % okSteps.size() << col.reset() << " #\n";
			if (blacklistedOnly) {
				for(const auto& testCase : okSteps) {
					string footerSuccessListFormat("%" + to_string(screenWidth - 9 - testCase.getName().length()) + "s");
					std::cout << "#" << col.green() << "   - " << testCase.getName() << " " << col.reset() << boost::format(footerSuccessListFormat) % "" << " #\n";
				}
			}
			std::cout << "# OMITTED:" << (omittedCount != 0 ? col.yellow() : "") << boost::format(footerSummaryFormat) % omittedCount
					<< (omittedCount != 0 ? col.reset() : "") << " #\n";
			std::cout << "# FAILED: " << (failedSteps.size() != 0 ? col.red() : "") << boost::format(footerSummaryFormat) % failedSteps.size()
	          		<< (failedSteps.size() != 0 ? col.reset() : "") << " #\n";
			if (!blacklistedOnly) {
				for(const auto& cur : failedSteps) {
					TestCase testCase = cur.first;
					TestResult testResult = cur.second;
					string failedStepInfo(testResult.getStepName() + ": exit code " + to_string(testResult.getRetVal()));
					string footerFailedListFormat("%" + to_string(screenWidth - 9 - testCase.getName().length()) + "s");
					std::cout << "#" << col.red() << "   - " << testCase.getName() << " " << col.reset() << boost::format(footerFailedListFormat) % failedStepInfo
							<< " #\n";
				}
			}
			std::cout << "#" << string(screenWidth - 2, '-') << "#\n";
		}
	}

	int handleIntegrationTests(int argc, char** argv, const std::string& programName, const std::string& provramVersion,
	                           const IntegrationTestCaseDefaultsPaths& defaultPaths) {
		// TODO custom root config file
		// set OMP/IRT environment variables if not already set
		auto checkAndSetEnv = [](const char* identifier, const char* defaultValue){
			if(!getenv(identifier)) {
				std::cout << identifier << " environment variable not set, defaulting to " << defaultValue << "\n";
				setenv(identifier, defaultValue, 1);
			}
		};

		checkAndSetEnv("IRT_NUM_WORKERS", "3");
		checkAndSetEnv("OMP_NUM_THREADS", "3");

		// parse parameters
		tf::Options options = parseCommandLine(argc, argv);

		// output width in characters
		unsigned screenWidth = 120;
		// formatting string for center aligned output
		string centerAlign("%|=" + to_string(screenWidth - 2) + "|");

		// check whether the options have been valid
		if(!options.valid) {
			return 1; // fail otherwise
		}

		// get list of test cases
		auto cases = tf::loadCases(defaultPaths, options);

		string header = programName + " version: " + provramVersion;

		std::cout << "#" << string(screenWidth - 2, '-') << "#\n";
		std::cout << "#" << boost::format(centerAlign) % header << "#\n";
		std::cout << "#" << string(screenWidth - 2, '-') << "#\n";
		string benchmarkHeader("Running " + to_string(cases.size()) + " benchmark(s) " + to_string(options.num_repetitions) + " time(s)");
		std::cout << "#" << boost::format(centerAlign) % benchmarkHeader << "#\n";
		std::cout << "#" << string(screenWidth - 2, '-') << "#\n";

		// check whether only the configurations are requested
		if(options.print_configs) {
			int counter = 0;
			// print configurations
			for(const auto& cur : cases) {
				std::cout << (++counter) << "/" << cases.size() << ":\n";
				std::cout << "Test Case: " << cur.getName() << "\n";
				std::cout << "Directory: " << cur.getDirectory().string() << "\n";
				std::cout << cur.getProperties() << "\n";
			}

			return 0;
		}

		int totalTests = cases.size() * options.num_repetitions;
		int maxCounterStringLength = to_string(totalTests).length();
		string maxCounterStringLengthAsString(to_string(maxCounterStringLength));

		// only list test cases if requested so
		if(options.list_only) {
			int counter = 0;
			for(const auto& cur : cases) {
				string paddingWidth(to_string(screenWidth - (8 + maxCounterStringLength * 2)));
				std::cout << boost::format("# %" + maxCounterStringLengthAsString + "d/%" + maxCounterStringLengthAsString + "d - %-" + paddingWidth + "s")
												 % ++counter % cases.size() % cur.getName()
									<< " #\n";
			}
			std::cout << "#" << std::string(screenWidth - 2, '-') << "#\n";

			return 0;
		}

		// load list of test steps
		auto steps = tf::getTestSteps(options);

		itc::TestSetup setup;
		setup.mockRun = options.mockrun;
		setup.clean = !options.no_clean;
		setup.perf = options.perf;
		setup.executionDir = "";

		tf::Colorize colorize(options.color);

		// setup highlighted tests:
		std::set<std::string> highlight;
		highlight.insert(itc::TEST_STEP_INSIEMECC_SEQ_C_EXECUTE);
		highlight.insert(itc::TEST_STEP_INSIEMECC_SEQ_CPP_EXECUTE);
		highlight.insert(itc::TEST_STEP_INSIEMECC_RUN_C_EXECUTE);
		highlight.insert(itc::TEST_STEP_INSIEMECC_RUN_CPP_EXECUTE);
		highlight.insert(itc::TEST_STEP_REF_C_EXECUTE);
		highlight.insert(itc::TEST_STEP_REF_CPP_EXECUTE);

		// run test cases in parallel
		vector<TestCase> ok;
		vector<TestCase> failed;
		int omittedTestsCount = 0;
		map<TestCase, TestResult> failedSteps;
		#ifdef _OPENMP
		omp_set_num_threads(options.num_threads);
		#endif

		bool panic = false;
		int act = 0;

		// Setup logging facility if enabled via command line args
		if (options.logToCsvFile) {
			auto &logger = TestResultsLogger::getInstance();
			logger.addKey("RuntimeSec");
			logger.addKey("MemoryMB");
			if (options.csvFileIDisset)
				logger.setID(options.csvFileID);
		}

		for(int i = 0; i < options.num_repetitions; i++) {
			// get instance of the test runner
			itc::TestRunner& runner = itc::TestRunner::getInstance();
			// set lambda to print some execution summary at the end
			runner.attachExecuteOnKill([&]() { printSummary(totalTests, ok, options.blacklistedOnly, omittedTestsCount, failedSteps, screenWidth, colorize); });
			// set lambda to set panic mode before killing all the processes
			runner.attachExecuteBeforeKill([&]() { panic = true; });


			#pragma omp parallel for schedule(dynamic)
			for(auto it = cases.begin(); it < cases.end(); it++) { // GCC requires the ugly syntax for OpenMP
				const auto& cur = *it;

				if(panic) { continue; }

				vector<TestStep> list;

				if(options.preprocessingOnly) {
					list.push_back(itc::getStepByName(itc::TEST_STEP_PREPROCESSING));

				} else if(options.postprocessingOnly) {
					list.push_back(itc::getStepByName(itc::TEST_STEP_POSTPROCESSING));

				} else {
					// filter applicable steps based on test case
					list = itc::filterSteps(steps, cur);
					// schedule resulting steps
					list = itc::scheduleSteps(list, cur);
				}

				// if this test doesn't schedule any steps we count it as omitted
				if(list.empty()) { omittedTestsCount++; }

				// run steps
				vector<pair<string, TestResult>> results;
				bool success = true;
				for(const auto& step : list) {
					auto res = step.run(setup, cur, runner);
					results.push_back(std::make_pair(step.getName(), res));
					if(!res.wasOmitted() && !res.wasSuccessful()) {
						failedSteps[cur] = res;
						success = false;
						break;
					}
				}

				// all steps done - print summary
				#pragma omp critical
				{
					string paddingWidth(to_string(screenWidth - (8 + maxCounterStringLength * 2)));
					// print test info
					std::cout << "#" << std::string(screenWidth - 2, '-') << "#\n";
					std::cout << "#    " << boost::format("%" + maxCounterStringLengthAsString + "d") % ++act << "/"
										<< boost::format("%" + maxCounterStringLengthAsString + "d") % totalTests << " "
										<< boost::format("%-" + paddingWidth + "s") % cur.getName() << "#\n";
					std::cout << "#" << std::string(screenWidth - 2, '-') << "#\n";

					for(const auto& curRes : results) {
						// skip omitted steps
						if(curRes.second.wasOmitted()) { continue; }

						if(options.mockrun) {
							std::cout << colorize.blue() << curRes.first << std::endl;
							std::cout << colorize.green() << curRes.second.getCmd() << colorize.reset() << std::endl;
						} else {
							string colOffset;
							colOffset = string("%") + std::to_string(screenWidth - 4 - curRes.first.size()) + "s";
							std::stringstream line;

							// color certain passes:
							if(highlight.find(curRes.first) != highlight.end()) { line << colorize.bold() << colorize.blue(); }

							line << "# " << curRes.first
									 << boost::format(colOffset)
													% (boost::format("[%.3f secs, %7.3f MB]") % curRes.second.getRuntime() % (curRes.second.getMemory() / 1024))
									 << " #" << colorize.reset() << "\n";

							if(curRes.second.wasSuccessful()) {
								std::cout << line.str();
							} else {
								std::cout << colorize.red() << line.str();
								std::cout << "#" << std::string(screenWidth - 2, '-') << "#" << colorize.reset() << std::endl;
								std::cout << "Command: " << colorize.green() << curRes.second.getCmd() << colorize.reset() << std::endl << std::endl;
								std::cout << curRes.second.getFullOutput();
							}

							success = success && curRes.second.wasSuccessful();
						}
						if(!options.no_clean) { curRes.second.clean(); }

						if(curRes.second.wasAborted()) { panic = true; }
					}

					if(!options.mockrun) {
						if(success) {
							ok.push_back(cur);
							std::cout << colorize.green();
						} else {
							failed.push_back(cur);
							std::cout << colorize.red();
						}

						std::cout << "#" << std::string(screenWidth - 2, '-') << "#\n";
						int failedStringLength = to_string(failed.size()).length();
						if(options.blacklistedOnly) {
							failedStringLength = to_string(ok.size()).length();
						}

						if(success) {
							std::cout << "#    SUCCESS -- " << boost::format("%-" + to_string(screenWidth - 36 - failedStringLength) + "s") % cur.getName();
							if(!options.blacklistedOnly) {
								if(failed.size() > 0) { std::cout << colorize.red(); }
								std::cout << " (failed so far: " << failed.size() << ")";
								std::cout << colorize.green() << " #\n";
							} else {
								std::cout << "     (ok so far: " << ok.size() << ") #\n";
							}
						} else {
							std::cout << "#    FAILED  -- " << boost::format("%-" + to_string(screenWidth - 36 - failedStringLength) + "s") % cur.getName();
							if(!options.blacklistedOnly) {
								std::cout << " (failed so far: " << failed.size() << ") #\n";
							} else {
								if(ok.size() > 0) { std::cout << colorize.green(); }
								std::cout << "     (ok so far: " << ok.size() << ")";
								std::cout << colorize.red() << " #\n";
							}
						}
						std::cout << "#" << std::string(screenWidth - 2, '-') << "#\n";

						if(!success) {
							// trigger panic mode and graceful shutdown
							if(options.panic_mode) { panic = true; }
						}
					}

					// reset color to default value
					std::cout << colorize.reset();

					// save results if they should be logged to a file afterwards
					if(options.logToCsvFile) {
						auto &logger = TestResultsLogger::getInstance();
						for (const auto &result : results) {
							if (!result.second.wasSuccessful())
								continue;

							const auto &test = cur.getName();
							const auto &step = result.first;
							const auto &runtime = result.second.getRuntime();
							const auto &memory  = result.second.getMemory() / 1024;

							logger.addResult(test, step, "RuntimeSec", runtime);
							logger.addResult(test, step, "MemoryMB", memory);
						}
					}
				}

			} // end test case loop

		} // end repetition loop

		// write results to CSV file if enabled via command line args
		if(options.logToCsvFile) {
			cout << "#" << boost::format(centerAlign) % std::string("Logging results to file '" + options.csvFile + "'...") << "#\n";
			auto &logger = TestResultsLogger::getInstance();
			if (!logger.writeResultsToFile(options.csvFile)) {
				LOG(ERROR) << "Could not write to CSV file!";
			}
		}

		if(!panic) { printSummary(totalTests, ok, options.blacklistedOnly, omittedTestsCount, failedSteps, screenWidth, colorize); }

		// done
		return (failed.empty()) ? 0 : 1;
	}

} // end namespace integration
} // end namespace driver
} // end namespace insieme
