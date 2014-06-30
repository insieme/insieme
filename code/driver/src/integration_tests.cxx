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

/**
 * Within this file a small, simple example of a compiler driver utilizing
 * the insieme compiler infrastructure is presented.
 *
 * This file is intended to provides a template for implementing new compiler
 * applications utilizing the Insieme compiler and runtime infrastructure.
 */

#include <string>
#include <vector>
#include <iostream>
#include <iomanip>
#include <utility>
#include <signal.h>

#include <omp.h>


#include <boost/filesystem.hpp>
#include <boost/format.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"

#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_step.h"
#include "insieme/driver/integration/test_framework.h"

#include "insieme/utils/config.h"


using std::pair;
using std::string;
using std::vector;

namespace bpo = boost::program_options;
namespace itc = insieme::driver::integration;

typedef itc::IntegrationTestCase TestCase;
using itc::TestStep;
using itc::TestResult;

namespace tf = itc::testFramework;

bool running;
int totalTests;
vector<TestCase> ok;
vector<TestCase> failed;

namespace{
	tf::Options parseCommandLine(int argc, char** argv) {
		static const tf::Options fail(false);

		// -- parsing -------------------------------------------

		// define options
		bpo::options_description desc("Supported Parameters");
		desc.add_options()
				("help,h", 				"produce help message")
				("config,c", 			"print the configuration of the selected test cases")
				("mock,m", 				"make it a mock run just printing commands not really executing those")
				("panic,p", 			"panic on first sign of trouble and stop execution")
				("list,l", 				"just list the targeted test cases")
				("worker,w", 			bpo::value<int>()->default_value(1), 	"the number of parallel workers to be utilized")
				("cases", 				bpo::value<vector<string>>(), 			"the list of test cases to be executed")
				("step,s", 				bpo::value<string>(), 					"the test step to be applied")
				("repeat,r",				bpo::value<int>()->default_value(1), "the number of times the tests shell be repeated")
				("clean",				"remove all output files")
				("nocolor",				"no highlighting of output")
		;

		// define positional options (all options not being named)
		bpo::positional_options_description pos;
		pos.add("cases", -1);

		// parse parameters
		bpo::variables_map map;
		bpo::store(bpo::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
		bpo::notify(map);


		// check whether help was requested
		if (map.count("help")) {
			std::cout << desc << "\n";

			std::cout << " ------------- Steps -----------------\n";
			for(auto entry: insieme::driver::integration::getFullStepList()) {
				std::cout << "\t" << std::setw(20) << entry.first;
				auto deps = entry.second.getDependencies();
				if(!deps.empty()) {
					std::cout << " <- [ ";
					for(auto dep: deps) {
						std::cout << dep << " ";
					}
					std::cout << "]";
				}
				std::cout << "\n";
			}

			return fail;
		}

		tf::Options res;

		if (map.count("config")) {
			res.print_configs = true;
		}

		if (map.count("cases")) {
			res.cases = map["cases"].as<vector<string>>();
		}

		res.mockrun = map.count("mock");
		res.clean=map.count("clean");
		res.color=!map.count("nocolor");
		res.panic_mode = map.count("panic");
		res.num_threads = map["worker"].as<int>();
		res.num_repeditions = map["repeat"].as<int>();
		res.perf = false;

		res.list_only = map.count("list");

		if (map.count("step")) {
			res.steps.push_back(map["step"].as<string>());
		}

		return res;
	}
}


void printSummary() {
    // output width in characters
	unsigned screenWidth = 120;
    tf::Colorize colorize(true);

	// formatting string for center aligned output
	string centerAlign("%|=" + to_string(screenWidth-2) + "|");

	string footerSummaryFormat("%" + to_string(screenWidth - 12) + "d");
	string footerFailedListFormat("   - %-" + to_string(screenWidth-8) + "s");

	std::cout << "#" << string(screenWidth-2,'-') << "#\n";
	std::cout << "#" << boost::format(centerAlign) % "INTEGRATION TEST SUMMARY" << "#\n";
	std::cout << "#" << string(screenWidth-2,'-') << "#\n";
	std::cout << "# TOTAL:  " << boost::format(footerSummaryFormat) % totalTests << " #\n";
	std::cout << "# PASSED: " << colorize.green() << boost::format(footerSummaryFormat) % ok.size() << colorize.reset() << " #\n";
	std::cout << "# FAILED: " << colorize.red() << boost::format(footerSummaryFormat) % failed.size() << colorize.reset() << " #\n";
	for(const auto& cur : failed) {
		std::cout << "#" << colorize.red() << boost::format(footerFailedListFormat) % cur.getName() << colorize.reset() << " #\n";
	}
	std::cout << "#" << string(screenWidth-2,'-') << "#\n";
}


void signal_callback_handler(int signum) {
    #pragma omp master
    {
        if(running) {
            running=false;
            for(auto pid : itc::pids) {
                kill(pid, signum);
            }
            //print statistics
            printSummary();
            //exit
            exit(signum);
        }
    }
}

int main(int argc, char** argv) {
    //register custom signal handler
    signal(SIGINT, signal_callback_handler);
    //signal(SIGABRT, signal_callback_handler);
    running = true;

	//TODO custom root config file

	Logger::setLevel(WARNING);

	// parse parameters
	tf::Options options = parseCommandLine(argc, argv);

	// output width in characters
	unsigned screenWidth = 120;

	// formatting string for center aligned output
	string centerAlign("%|=" + to_string(screenWidth-2) + "|");

	// check whether the options have been valid
	if (!options.valid) return 1;		// fail otherwise

	// get list of test cases
	auto cases = tf::loadCases(options);

	string insiemeVersion("Insieme version: " + tf::getGitVersion());

	std::cout << "#" << string(screenWidth-2,'-') << "#\n";
	std::cout << "#" << boost::format(centerAlign) % insiemeVersion << "#\n";
	std::cout << "#" << string(screenWidth-2,'-') << "#\n";
	string benchmarkHeader("Running " + to_string(cases.size()) + " benchmark(s)");
	std::cout << "#" << boost::format(centerAlign) % benchmarkHeader << "#\n";
	std::cout << "#" << string(screenWidth-2,'-') << "#\n";

	// check whether only the configurations are requested
	if (options.print_configs) {
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

	int maxCounterStringLength = to_string(cases.size()).length();
	string maxCounterStringLengthAsString(to_string(maxCounterStringLength));

	// only list test cases if requested so
	if (options.list_only) {
		int counter = 0;
		for(const auto& cur : cases) {
			string paddingWidth(to_string(screenWidth-(8+maxCounterStringLength*2)));
			std::cout << boost::format("# %" + maxCounterStringLengthAsString + "d/%" + maxCounterStringLengthAsString + "d - %-" + paddingWidth + "s") % ++counter % cases.size() % cur.getName() << " #\n";
		}
		std::cout << "#" << std::string(screenWidth-2,'-') << "#\n";

		return 0;
	}

	// load list of test steps
	auto steps = tf::getTestSteps(options);

	itc::TestSetup setup;
	setup.mockRun = options.mockrun;
	setup.clean=options.clean;
	setup.perf=options.perf;

	tf::Colorize colorize(options.color);

	// setup highlighted tests:
	std::set<std::string> highlight;
	highlight.insert("main_seq_execute");
	highlight.insert("main_seq_c++_execute");
	highlight.insert("main_run_execute");
	highlight.insert("main_run_c++_execute");
	highlight.insert("ref_c_execute");
	highlight.insert("ref_c++_execute");
	highlight.insert("insiemecc_c_execute");
	highlight.insert("insiemecc_c++_execute");

	// run test cases in parallel
	omp_set_num_threads(options.num_threads);

	bool panic = false;
	totalTests = cases.size() * options.num_repeditions;
	int act = 0;

	for(int i=0; i<options.num_repeditions; i++) {

		#pragma omp parallel for schedule(dynamic)
		for(auto it = cases.begin(); it < cases.end(); it++) {			// GCC requires the ugly syntax for OpenMP
            if(running) {
                const auto& cur = *it;

                if (panic) continue;

                // filter applicable steps based on test case
                vector<TestStep> list = itc::filterSteps(steps, cur);

                // schedule resulting steps
                list = itc::scheduleSteps(list,cur);

                // run steps
                vector<pair<string, TestResult>> results;
                bool success = true;
                for(const auto& step : list) {
                    auto res = step.run(setup, cur);
                    results.push_back(std::make_pair(step.getName(), res));
                    if (!res || res.hasBeenAborted()) {
                        success = false;
                        break;
                    }
                }

                // all steps done - print summary
                #pragma omp critical
                {
                    string paddingWidth(to_string(screenWidth-(8+maxCounterStringLength*2)));
                    // print test info
                    std::cout << "#" << std::string(screenWidth-2,'-') << "#\n";
                    std::cout << "#    " << boost::format("%" + maxCounterStringLengthAsString + "d") % ++act << "/"<< boost::format("%" + maxCounterStringLengthAsString + "d") % totalTests
                            << " " << boost::format("%-" + paddingWidth + "s") % cur.getName() << "#\n";
                    std::cout << "#" << std::string(screenWidth-2,'-') << "#\n";

                    for(const auto& curRes : results) {
                        if(options.mockrun){
                            std::cout << colorize.blue() << curRes.first<< std::endl;
                            std::cout << colorize.green() << curRes.second.getCmd() << colorize.reset() <<std::endl;
                        } else {
                            string colOffset;
                            colOffset=string("%")+std::to_string(screenWidth-4-curRes.first.size())+"s";
                            std::stringstream line;

                            // color certain passes:
                            if (highlight.find (curRes.first) != highlight.end()) {
                                line <<  colorize.bold() << colorize.blue();
                            }

                            line << "# " << curRes.first << boost::format(colOffset) % (boost::format("[%.3f secs, %.3f MB]") % curRes.second.getRuntime() % (curRes.second.getMemory()/1024/1024))
                                    << " #" << colorize.reset() << "\n";

                            if(curRes.second.wasSuccessfull()){
                                std::cout << line.str();
                            } else {
                                std::cout << colorize.red() << line.str();
                                std::cout << "#" << std::string(screenWidth-2,'-') << colorize.reset() << std::endl;
                                std::cout << "Command: " << colorize.green() << curRes.second.getCmd()<< colorize.reset() << std::endl << std::endl;
                                std::cout << curRes.second.getFullOutput();
                            }

                            success = success && curRes.second.wasSuccessfull();
                        }
                        if(options.clean) {
                            curRes.second.clean();
                        }

                        if (curRes.second.hasBeenAborted()) {
                            panic = true;
                        }
                    }

                    if(!options.mockrun){

                        if(success) {
                            ok.push_back(cur);
                            std::cout << colorize.green();
                        } else {
                            failed.push_back(cur);
                            std::cout << colorize.red();
                        }

                        std::cout << "#" << std::string(screenWidth-2,'-') << "#\n";
                        int failedStringLength = to_string(failed.size()).length();

                        if(success)
                            std::cout << "#    SUCCESS -- " << boost::format("%-" + to_string(screenWidth-36-failedStringLength) + "s") % cur.getName() << " (failed so far: " << failed.size() << ") #\n";
                        else
                            std::cout << "#    FAILED  -- " << boost::format("%-" + to_string(screenWidth-36-failedStringLength) + "s") % cur.getName() << " (failed so far: " << failed.size() << ") #\n";
                        std::cout << "#" << std::string(screenWidth-2,'-') << "#\n";

                        if(!success) {
                            // trigger panic mode and graceful shutdown
                            if (options.panic_mode) {
                                panic = true;
                            }
                        }
                    }

                    //reset color to default value
                    std::cout << colorize.reset();
                }

            } // end test case loop
		}
	} // end repetition loop

    //print summary
    printSummary();

	// done
	return (failed.empty())?0:1;
}
