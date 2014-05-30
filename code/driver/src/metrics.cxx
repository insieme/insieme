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


#include <boost/program_options.hpp>
#include <omp.h>
#include <fstream>

#include "insieme/utils/logging.h"
#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_step.h"
#include "insieme/driver/integration/test_framework.h"
#include "insieme/driver/integration/test_output.h"
#include "insieme/utils/config.h"


using std::pair;
using std::string;
using std::vector;

namespace bpo = boost::program_options;
namespace itc = insieme::driver::integration;
namespace tf = itc::testFramework;
namespace mr = insieme::driver::integration::metrics;

namespace {

	tf::Options parseCommandLine(int argc, char** argv);

}

int main(int argc, char** argv) {

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
	

	// load list of test steps
	auto steps = tf::getTestSteps(options);

	itc::TestSetup setup;
	setup.mockRun = options.mockrun;
	setup.clean=options.clean;
	setup.perf=options.perf;
	setup.load_miss=options.load_miss;
	setup.store_miss=options.store_miss;
	setup.flops=options.flops;
	setup.perf_metrics=options.perf_metrics;

	tf::Colorize colorize(options.color);

	// setup highlighted tests:
	std::set<std::string> highlight;
	highlight.insert("main_seq_execute");
	highlight.insert("main_seq_c++_execute");

	for(int i=1;i<options.statThreads;i*=2){
		highlight.insert(std::string("main_run_execute_")+std::to_string(i));
		highlight.insert(std::string("main_run_c++_execute_")+std::to_string(i));
		highlight.insert(std::string("ref_c_execute_")+std::to_string(i));
		highlight.insert(std::string("ref_c++_execute_")+std::to_string(i));
		highlight.insert(std::string("insiemecc_c_execute_")+std::to_string(i));
		highlight.insert(std::string("insiemecc_c++_execute_")+std::to_string(i));
	}
	highlight.insert(std::string("main_run_execute_")+std::to_string(options.statThreads));
	highlight.insert(std::string("main_run_c++_execute_")+std::to_string(options.statThreads));
	highlight.insert(std::string("ref_c_execute_")+std::to_string(options.statThreads));
	highlight.insert(std::string("ref_c++_execute_")+std::to_string(options.statThreads));
	highlight.insert(std::string("insiemecc_c_execute_")+std::to_string(options.statThreads));
	highlight.insert(std::string("insiemecc_c++_execute_")+std::to_string(options.statThreads));

	if(options.scheduling){
		highlight.insert(std::string("main_run_execute_dyn_")+std::to_string(options.statThreads));
		highlight.insert(std::string("main_run_c++_execute_dyn_")+std::to_string(options.statThreads));
		highlight.insert(std::string("main_run_execute_stat_")+std::to_string(options.statThreads));
		highlight.insert(std::string("main_run_c++_execute_stat_")+std::to_string(options.statThreads));
		highlight.insert(std::string("main_run_execute_guid_")+std::to_string(options.statThreads));
		highlight.insert(std::string("main_run_c++_execute_guid_")+std::to_string(options.statThreads));
	}	


	// run test cases in parallel
	vector<TestCase> ok;
	vector<TestCase> failed;

	bool panic = false;
	int totalTests=cases.size() * options.num_repeditions;
	int act=0;
	map<TestCase,vector<pair<TestStep, TestResult>>> allResults;

	for(auto it = cases.begin(); it < cases.end(); it++) {			
		const auto& cur = *it;

		if (panic) continue;

		// filter applicable steps based on test case
		vector<TestStep> list = itc::filterSteps(steps, cur);
		// schedule resulting steps
		list = itc::scheduleSteps(list,cur,options.statThreads,options.statistics);

		// run steps
		vector<pair<TestStep, TestResult>> results;
		bool success = true;
		for(const auto& step : list) {
			auto res = step.run(setup, cur);
			results.push_back(std::make_pair(step, res));
			if (!res || res.hasBeenAborted()) {
				success = false;
				break;
			}
		}
		
		// print test info
		std::cout << "#------------------------------------------------------------------------------#\n";
		std::cout << "#\t" << ++act << "/"<< totalTests << "\t" << format("%-63s",cur.getName()) << "#\n";
		std::cout << "#------------------------------------------------------------------------------#\n";

		for(const auto& curRes : results) {
			if(options.mockrun){
				std::cout << colorize.black() << curRes.first.getName()<< std::endl;
				std::cout << colorize.green() << curRes.second.getCmd() << colorize.reset() <<std::endl;
			} else {
				string colOffset;
				colOffset=string("%")+std::to_string(78-curRes.first.getName().size())+"s";
				std::stringstream line;
				
				// color certain passes:
				if (highlight.find (curRes.first.getName()) != highlight.end()) {
					line <<  colorize.bold() << colorize.blue();
				}

				line << "# " << curRes.first.getName() <<
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

		//execute static metrics
		results.push_back(std::make_pair(itc::StaticMetricsStep(),mr::getStaticMetrics(cur,results)));
		//save results into global map
		allResults[cur]=results;

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

	} // end test case loop

	// iterate over all output methods and execute them
	for(string s : options.outputFormats){
		mr::TestOutput* output= mr::createTestOutput(s,allResults);
		output->writeOutput(options.overwrite);
	}

	

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



namespace {


	tf::Options parseCommandLine(int argc, char** argv) {
		static const tf::Options fail(false);

		// -- parsing -------------------------------------------

		// define options
		bpo::options_description desc("Supported Parameters");
		desc.add_options()
				("help,h", 				"produce help message")
				("panic,p", 			"panic on first sign of trouble and stop execution")
				("mock,m", 				"make it a mock run just printing commands not really executing those")
				("step,s", 				bpo::value<string>(), 					"the test step to be applied")
				//("repeat,r",				bpo::value<int>()->default_value(1), "the number of times the tests shell be repeated")
				("no-perf",				"disable perf metrics")
				("scheduling,S",			"enable runs on all scheduling variants (static,dynamic,guided)")
				("no-overwrite",			"do not overwrite existing output data")
				("threads,t",				bpo::value<int>()->default_value(omp_get_max_threads()),"number of threads for statistic calculation")
				("cases", 				bpo::value<vector<string>>(), 			"the list of test cases to be executed")
				("load-miss",			bpo::value<string>(),"the perf code for the llc load misses")
				("store-miss",			bpo::value<string>(),"the perf code for the llc store misses")
				("flops",			bpo::value<string>(),"the perf code for the number of floating point operations")
				("perf-metric,P",		bpo::value<vector<string>>(),"a perf code to be measured")
				("output,o",			bpo::value<vector<string>>(),"output formats, currently supported: SQL,CSV")
				("force,f",			"force to execute all tests (even those uncommented with #)")
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

		tf::Options res;


		if (map.count("cases")) {
			res.cases = map["cases"].as<vector<string>>();
		}

		if(map.count("output")){
			res.outputFormats = map["output"].as<vector<string>>();
		}

		res.perf=!(map.count("no-perf"));
		if(res.perf){
			//if perf is enabled perf codes must be given
			if(map.count("load-miss")==0 || map.count("store-miss")==0 || map.count("flops")==0){
				std::cout << "To enable perf support perf metrics (load-miss,store-miss,flops) have to be given using the appropriate arguments!"<<std::endl;
				return fail;
			}
			res.load_miss=map["load-miss"].as<string>();
			res.store_miss=map["store-miss"].as<string>();
			res.flops=map["flops"].as<string>();
		
			if(map.count("perf-metric")){
				res.perf_metrics=map["perf-metric"].as<vector<string>>();
			}
		}

		if(!res.perf && map.count("perf-metric")){
			LOG(WARNING)<<"Requested perf metrics will not be executed!"<<std::endl;
		}
		res.scheduling=map.count("scheduling");
		res.mockrun = map.count("mock");
		res.clean=true;
		res.statistics=true;
		res.color=true;
		res.panic_mode = map.count("panic");
		res.num_threads = 1;
		res.force=map.count("force");
		//res.num_repeditions = map["repeat"].as<int>();
		res.statThreads=map["threads"].as<int>();
		res.overwrite=!map.count("no-overwrite");

		if (map.count("step")) {
			res.steps.push_back(map["step"].as<string>());
		}
		if(res.statThreads<1){
			LOG(WARNING)<<"Number of threads has to be bigger than 0! Setting numThreads to 1."<<std::endl;
			res.statThreads=1;
		}

		if(res.scheduling && res.statThreads<=1){
			LOG(WARNING)<<"Undefined behaviour if scheduling option is enabled but numThreads not set! Setting numThreads to 2."<<std::endl;
			res.statThreads=2;
		}

		return res;
	}
}
