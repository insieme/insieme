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
#include <utility>

#include <omp.h>

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"

#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_step.h"

#include "insieme/utils/config.h"


using std::pair;
using std::string;
using std::vector;

namespace bpo = boost::program_options;
namespace itc = insieme::driver::integration;

typedef itc::IntegrationTestCase TestCase;
using itc::TestStep;
using itc::TestResult;

namespace {

	struct Options {
		bool valid;
		bool mockrun;
		int num_threads;
		int num_repeditions;
		bool print_configs;
		bool panic_mode;
		bool list_only;
		bool clean;
		vector<string> cases;
		vector<string> steps;

		Options(bool valid = true)
			: valid(valid), mockrun(false),
			  num_threads(1), num_repeditions(1), print_configs(false),
			  panic_mode(false), list_only(false), clean(false) {}
	};

	namespace fs = boost::filesystem;

	Options parseCommandLine(int argc, char** argv);

	vector<TestCase> loadCases(const Options& options);

	vector<TestStep> getTestSteps(const Options& options);
}

std::string getGitVersion(){
	string getGitVersionCmd=string("cd ")+SRC_ROOT_DIR+"; git describe --dirty";
	FILE* pipe=popen(getGitVersionCmd.c_str(),"r");
	char buff [50];
	fgets(buff,50,pipe);
	pclose(pipe);

	//remove line break
	buff[strlen(buff)-1]='\0';
	return string(buff);
}

int main(int argc, char** argv) {
	//TODO custom root config file

	Logger::setLevel(ERROR);

	// parse parameters
	Options options = parseCommandLine(argc, argv);

	// check whether the options have been valid
	if (!options.valid) return 1;		// fail otherwise

	// get list of test cases
	auto cases = loadCases(options);

	std::cout <<        "#------------------------------------------------------------------------------#\n";
	std::cout << format("#                 Insieme version: %-43s #\n", getGitVersion());
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
	auto steps = getTestSteps(options);

	itc::TestSetup setup;
	setup.mockRun = options.mockrun;
	setup.clean=options.clean;

	// setup highlighted tests:
	std::set<std::string> highlight;
	highlight.insert("main_seq_execute");
	highlight.insert("main_seq_c++_execute");
	highlight.insert("main_run_execute");
	highlight.insert("main_run_c++_execute");
	highlight.insert("ref_c_execute");
	highlight.insert("ref_c++_execute");

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
			list = itc::scheduleSteps(list);

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
						std::cout<<"\033[0;30m"<<curRes.first<<std::endl;
						std::cout<<"\033[0;32m"<<curRes.second.getCmd()<<std::endl;
					} else {
						string colOffset;
						colOffset=string("%")+std::to_string(78-curRes.first.size())+"s";
						std::stringstream line;
					
						// color certain passes:
						if (highlight.find (curRes.first) != highlight.end()) {
							line <<  "\033[1;94m";
						}

						line << "# " << curRes.first <<
							format(colOffset.c_str(),format("[%.3f secs, %.3f MB]",curRes.second.getRuntime(), curRes.second.getMemory()/1024/1024)) <<  "\033[0m\n";

						if(curRes.second.wasSuccessfull()){
							std::cout << line.str();
						} else {
							std::cout<<"\033[0;31m"<<line.str();
							std::cout << "#------------------------------------------------------------------------------#\n\033[0m";
							std::cout<<"Command: \033[0;32m"<<curRes.second.getCmd()<<"\033[0m"<<std::endl<<std::endl;
							std::cout<<curRes.second.getFullOutput();
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
						std::cout<<"\033[0;32m";
					} else {
						std::cout<<"\033[0;31m";
					}

					std::cout << "#------------------------------------------------------------------------------#\n";
					if(success)
						std::cout<<"#\tSUCCESS -- "<<format("%-60s",cur.getName())<<"#\n";
					else
						std::cout<<"#\tFAILED  -- "<<format("%-61s",cur.getName())<<"#\n";
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
				}
				//reset color to default value
				std::cout<<"\033[0m";
			}

		} // end test case loop

	} // end repetition loop

	std::cout << "#~~~~~~~~~~~~~~~~~~~~~~~~~~ INTEGRATION TEST SUMMARY ~~~~~~~~~~~~~~~~~~~~~~~~~~#\n";
	std::cout << format("# TOTAL:          %60d #\n", cases.size()*options.num_repeditions);
	std::cout << format("# PASSED:         \033[0;32m%60d\033[0m #\n", ok.size());
	std::cout << format("# FAILED:         \033[0;31m%60d\033[0m #\n", failed.size());
	for(const auto& cur : failed) {
		std::cout << format("#\033[0;31m   - %-63s          \033[0m#\n", cur.getName());
	}
	std::cout << "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#\n";

	// done
	return (failed.empty())?0:1;
}



namespace {


	Options parseCommandLine(int argc, char** argv) {
		static const Options fail(false);

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
		;

		// define positional options (all options not being named)
		bpo::positional_options_description pos;
		pos.add("cases", -1);

		// parse parameters
		bpo::variables_map map;
		bpo::store(bpo::command_line_parser(argc, argv).options(desc).positional(pos).run(), map);
		bpo::notify(map);


		// -- processing -----------------------------------------

		// check whether help was requested
		if (map.count("help")) {
			std::cout << desc << "\n";
			return fail;
		}

		Options res;

		if (map.count("config")) {
			res.print_configs = true;
		}

		if (map.count("cases")) {
			res.cases = map["cases"].as<vector<string>>();
		}

		res.mockrun = map.count("mock");
		res.clean=map.count("clean");
		res.panic_mode = map.count("panic");
		res.num_threads = map["worker"].as<int>();
		res.num_repeditions = map["repeat"].as<int>();

		res.list_only = map.count("list");

		if (map.count("step")) {
			res.steps.push_back(map["step"].as<string>());
		}

		return res;
	}

	vector<TestCase> loadCases(const Options& options) {

		// of no test is specified explicitly load all of them
		if (options.cases.empty()) {
				return itc::getAllCases();
		}

		// load selected test cases
		vector<TestCase> cases;
		for(const auto& cur : options.cases) {
			// load test case based on the location
			auto curSuite = itc::getTestSuite(cur);
			for(const auto& cur : curSuite) {
				if (!contains(cases, cur)) {		// make sure every test is only present once
					cases.push_back(cur);
				}
			}
		}
		return cases;
	}

	vector<TestStep> getTestSteps(const Options& options) {
		vector<TestStep> steps;

		// load steps selected by the options
		if (!options.steps.empty()) {
			const auto& all = itc::getFullStepList();

			for(const auto& cur : options.steps) {
				auto pos = all.find(cur);
				if (pos != all.end()) {
					steps.push_back(pos->second);
					continue;
				}
				std::cout << "WARNING: Unknown test step: " << cur << "\n";
			}

			return steps;
		}


		// TODO: filter them based on some options
		for(const auto& cur : itc::getFullStepList()) {
			steps.push_back(cur.second);
		}
		return steps;
	}

}
