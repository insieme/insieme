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

#include <omp.h>


#include <boost/filesystem.hpp>

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
namespace{
	tf::Options parseCommandLine(int argc, char** argv) {
		static const tf::Options fail(false);

		// -- parsing -------------------------------------------

		// define options
		bpo::options_description desc("Supported Parameters");
		desc.add_options()
				("help,h", 				"produce help message")
				("config,c", 			"print the configuration of the selected test cases")
                                ("liststeps",                   "list all the available steps")
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

		// check whether step list was requested
		if (map.count("liststeps")) {
			std::cout << "Available steps:\n";
			for(auto entry: insieme::driver::integration::getFullStepList()) {
				std::cout << "\t" << std::setw(32) << entry.first;
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

		// -- processing -----------------------------------------

		// check whether help was requested
		if (map.count("help")) {
			std::cout << desc << "\n";
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
		// TODO: correct fix? no perf measurements for integration testing necessary
		res.perf = false;

		res.list_only = map.count("list");

		if (map.count("step")) {
			res.steps.push_back(map["step"].as<string>());
		}

		return res;
	}
}
int main(int argc, char** argv) {
	//TODO custom root config file

	Logger::setLevel(WARNING);

	// parse parameters
	tf::Options options = parseCommandLine(argc, argv);

	// check whether the options have been valid
	if (!options.valid) return 1;		// fail otherwise

	// get list of test cases
	auto cases = tf::loadCases(options);

	std::cout <<        "#------------------------------------------------------------------------------#\n";
	std::cout << format("#                 Insieme version: %-43s #\n", tf::getGitVersion());
	std::cout <<        "#------------------------------------------------------------------------------#\n";
	std::cout << format("#                           Running %3d benchmark(s)                           #\n", cases.size());
	std::cout <<        "#------------------------------------------------------------------------------#\n";

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

	// only list test cases if requested so
	if (options.list_only) {
		int counter = 0;
		for(const auto& cur : cases) {
			std::cout << format("| %4d/%4d - %-65s|\n", ++counter, cases.size(), cur.getName());
		}
		std::cout <<        "#------------------------------------------------------------------------------#\n";

		return 0;
	}



	// load list of test steps
	auto steps = tf::getTestSteps(options);

	itc::TestSetup setup;
	setup.mockRun = options.mockrun;
	setup.clean=options.clean;

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
	vector<TestCase> ok;
	vector<TestCase> failed;
	omp_set_num_threads(options.num_threads);

	bool panic = false;
	int totalTests=cases.size() * options.num_repeditions;
	int act=0;

	for(int i=0; i<options.num_repeditions; i++) {

		#pragma omp parallel for schedule(dynamic)
		for(auto it = cases.begin(); it < cases.end(); it++) {			// GCC requires the ugly syntax for OpenMP
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
				// print test info
				std::cout << "#------------------------------------------------------------------------------#\n";
				std::cout << "#\t" << ++act << "/"<< totalTests << "\t" << format("%-63s",cur.getName()) << "#\n";
				std::cout << "#------------------------------------------------------------------------------#\n";

				for(const auto& curRes : results) {
					if(options.mockrun){
						std::cout << colorize.blue() << curRes.first<< std::endl;
						std::cout << colorize.green() << curRes.second.getCmd() << colorize.reset() <<std::endl;
					} else {
						string colOffset;
						colOffset=string("%")+std::to_string(78-curRes.first.size())+"s";
						std::stringstream line;
					
						// color certain passes:
						if (highlight.find (curRes.first) != highlight.end()) {
							line <<  colorize.bold() << colorize.blue();
						}

						line << "# " << curRes.first <<
							format(colOffset.c_str(),format("[%.3f secs, %.3f MB]",curRes.second.getRuntime(), curRes.second.getMemory()/1024/1024)) << colorize.reset() << "\n";

						if(curRes.second.wasSuccessfull()){
							std::cout << line.str();
						} else {
							std::cout << colorize.red() <<line.str();
							std::cout << "#------------------------------------------------------------------------------#" << colorize.reset() << std::endl;
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

					if(success) 
						std::cout << colorize.green();
					else 
						std::cout << colorize.red();

					std::cout << "#------------------------------------------------------------------------------#\n";
					if(success)
						std::cout << "#\tSUCCESS -- "<<format("%-60s",cur.getName())<<"#\n";
					else
						std::cout << "#\tFAILED  -- "<<format("%-60s",cur.getName())<<"#\n";
					std::cout << "#------------------------------------------------------------------------------#\n";

					if (success) {
						ok.push_back(cur);
					} else {
						failed.push_back(cur);

						// trigger panic mode and graceful shutdown
						if (options.panic_mode) {
							panic = true;
						}
					}
					std::cout << colorize.reset();
				}

				//reset color to default value
				std::cout << colorize.reset();
			}

		} // end test case loop

	} // end repetition loop

	std::cout << "#~~~~~~~~~~~~~~~~~~~~~~~~~~ INTEGRATION TEST SUMMARY ~~~~~~~~~~~~~~~~~~~~~~~~~~#\n";
	std::cout << format("# TOTAL:          %60d #\n", cases.size()*options.num_repeditions);
	std::cout << "# PASSED:         " << colorize.green() << format("%60d", ok.size()) << colorize.reset() << " #\n";
	std::cout << "# FAILED:         " << colorize.red() << format("%60d", failed.size()) << colorize.reset() << " #\n";
	for(const auto& cur : failed) {
	std::cout << "#" << colorize.red() << format("   - %-63s          ", cur.getName()) << colorize.reset() << "#\n";
	}
	std::cout << "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#\n";

	// done
	return (failed.empty())?0:1;
}
