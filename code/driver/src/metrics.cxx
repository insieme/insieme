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

#include <boost/serialization/vector.hpp>
#include <boost/serialization/set.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/program_options.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
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
	if(options.num_repeditions==1)
		std::cout << format("#                           Running %3d benchmark(s)                           #\n", cases.size());
	else
		std::cout << format("#                Running %3d benchmark(s) %3d repetitions                      #\n", cases.size(),options.num_repeditions);
	std::cout <<        "#------------------------------------------------------------------------------#\n";



	map<TestCase,vector<pair<TestStep, TestResult>>> allResults;
	map<string,string> conflictingSteps;

	// load list of test step
	auto steps = tf::getTestSteps(options);
	
	//define some conflicting steps, so either c or c++ is executed (not both of them)
	conflictingSteps["insiemecc_c++_check"]="insiemecc_c_check";
	conflictingSteps["insiemecc_c++_compile"]="insiemecc_c_compile";
	conflictingSteps["insiemecc_c++_execute"]="insiemecc_c_execute";
	conflictingSteps["main_c++_sema"]="main_c_sema";
	conflictingSteps["main_run_c++_check"]="main_run_check";
	conflictingSteps["main_run_c++_execute"]="main_run_execute";
	conflictingSteps["main_run_c++_compile"]="main_run_compile";
	conflictingSteps["main_run_c++_convert"]="main_run_convert";
	conflictingSteps["main_seq_c++_check"]="main_seq_check";
	conflictingSteps["main_seq_c++_compile"]="main_seq_compile";
	conflictingSteps["main_seq_c++_convert"]="main_seq_convert";
	conflictingSteps["main_seq_c++_execute"]="main_seq_execute";
	conflictingSteps["ref_c++_check"]="ref_c_check";
	conflictingSteps["ref_c++_compile"]="ref_c_compile";
	conflictingSteps["ref_c++_execute"]="ref_c_execute";

	conflictingSteps["insiemecc_c_check"]="insiemecc_c++_check";
	conflictingSteps["insiemecc_c_compile"]="insiemecc_c++_compile";
	conflictingSteps["insiemecc_c_execute"]="insiemecc_c++_execute";
	conflictingSteps["main_c_sema"]="main_c++_sema";
	conflictingSteps["main_run_check"]="main_run_c++_check";
	conflictingSteps["main_run_execute"]="main_run_c++_execute";
	conflictingSteps["main_run_compile"]="main_run_c++_compile";
	conflictingSteps["main_run_convert"]="main_run_c++_convert";
	conflictingSteps["main_seq_check"]="main_seq_c++_check";
	conflictingSteps["main_seq_compile"]="main_seq_c++_compile";
	conflictingSteps["main_seq_convert"]="main_seq_c++_convert";
	conflictingSteps["main_seq_execute"]="main_seq_c++_execute";
	conflictingSteps["ref_c_check"]="ref_c++_check";
	conflictingSteps["ref_c_compile"]="ref_c++_compile";
	conflictingSteps["ref_c_execute"]="ref_c++_execute";

	itc::TestSetup setup;
	setup.mockRun = options.mockrun;
	setup.clean=options.clean;
	setup.perf=options.perf;
	setup.load_miss=options.load_miss;
	setup.store_miss=options.store_miss;
	setup.flops=options.flops;
	setup.perf_metrics=options.perf_metrics;
	setup.executionDir=boost::filesystem::current_path().c_str();

	tf::Colorize colorize(options.color);

	// setup highlighted tests:
	std::set<std::string> highlight;
	highlight.insert("main_seq_execute");
	highlight.insert("main_seq_c++_execute");
	highlight.insert("ref_c++_execute");
	highlight.insert("ref_c_execute");

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

	//check if backup file exists, read results
        std::ifstream ifs("back.bin");
	if(ifs.good()){
		LOG(INFO)<<"Trying recovery from crashed run!";
		boost::archive::binary_iarchive ia(ifs);
		tf::Options opt;
		ia >> opt;
		//check if backup is compatible with current run
		if(opt==options)
		        ia >> allResults;
		else
			LOG(WARNING)<<"Backup not compatible, rerun required!";
	}
	ifs.close();

	// run test cases in parallel
	vector<TestCase> ok;
	vector<TestCase> failed;

	bool panic = false;
	int act=0;

    itc::TestRunner& runner = itc::TestRunner::getInstance();
	for(auto it = cases.begin(); it < cases.end(); it++) {
		const auto& cur = *it;

		

		bool execute=true;
		//check if already executed (results in backup file)
		if(allResults.count(cur)>0)
			execute=false;

		if (panic) continue;

		// filter applicable steps based on test case
		vector<TestStep> list = itc::filterSteps(steps, cur,conflictingSteps);
		// schedule resulting steps
		list = itc::scheduleSteps(list,cur,options.statThreads,options.statistics);
		// run steps
		vector<pair<TestStep, TestResult>> results;
		bool success = true;
        	string name=cur.getName();

	        if(execute) {
	            map<TestStep,vector<TestResult>> curRes;
	            for(int rep=0;rep<options.num_repeditions;rep++){
	                for(const auto& step : list) {
	                    auto res = step.run(setup, cur, runner);
	                    curRes[step].push_back(res);
	                    if (!res || res.hasBeenAborted()) {
	                        success = false;
	                        break;
	                    }
	                    if(!success)
	                   	 break;
	            	}	
			}
	                for(auto steps = curRes.begin(); steps != curRes.end(); steps++){
	                    TestResult res=steps->second.front();
	                    if(options.use_median){
	                        res=TestResult::returnMedian(steps->second);
	                    }
	                    else
	                        res=TestResult::returnAVG(steps->second);
	                        results.push_back(std::make_pair(steps->first,res));
	                    }
	                }
	        
		// get cached results
		else{
			results=allResults[cur];
			name=name+" -- CACHED";
		}

		// print test info
		std::cout << "#------------------------------------------------------------------------------#\n";
		std::cout << "#\t" << ++act << "/"<< cases.size() << "\t" << format("%-63s",name) << "#\n";
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

				if(curRes.second.deviationAvailable() && options.num_repeditions>1)
					line << "# " << curRes.first.getName() <<
						format(colOffset.c_str(),format("[%.3f secs (+/- %.3f), %.3f MB (+/- %.3f)]",curRes.second.getRuntime(), curRes.second.getRuntimeDev(),
						curRes.second.getMemory()/1024/1024,curRes.second.getMemoryDev()/1024/1024)) << colorize.reset() << "\n";
				else
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
			if(options.clean && execute) {
				curRes.second.clean();
			}

			if (execute && curRes.second.hasBeenAborted()) {
				panic = true;
			}
		}

		if(execute){
			//execute static metrics
			results.push_back(std::make_pair(itc::StaticMetricsStep(),mr::getStaticMetrics(cur,results)));
			//save results into global map
			allResults[cur]=results;
		}
		//save current state into file, for crash recovery
		remove("back.bin");
		std::ofstream ofs("back.bin");
		boost::archive::binary_oarchive oa(ofs);
		oa<<options;
		oa<<allResults;
		ofs.close();

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
	std::cout << format("# TOTAL:          %60d #\n", cases.size());
	std::cout << "# PASSED:         " << colorize.green() << format("%60d", ok.size()) << colorize.reset() << " #\n";
	std::cout << "# FAILED:         " << colorize.red() << format("%60d", failed.size()) << colorize.reset() << " #\n";
	for(const auto& cur : failed) {
	std::cout << "#" << colorize.red() << format("   - %-63s          ", cur.getName()) << colorize.reset() << "#\n";
	}
	std::cout << "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#\n";

	//delete backup file
	remove("back.bin");

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
				("repeat,r",				bpo::value<int>()->default_value(1), "the number of times the tests shall be repeated")
				("use-median",				"use median instead of avg if multiple runs are required")
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
			LOG(WARNING)<<"Requested perf metrics will not be executed!";
		}
		res.scheduling=map.count("scheduling");
		res.mockrun = map.count("mock");
		res.clean=true;
		res.statistics=true;
		res.color=true;
		res.panic_mode = map.count("panic");
		res.num_threads = 1;
		res.force=map.count("force");
		res.num_repeditions = map["repeat"].as<int>();
		res.statThreads=map["threads"].as<int>();
		res.overwrite=!map.count("no-overwrite");
		res.use_median=map.count("use-median");

		if (map.count("step")) {
			res.steps.push_back(map["step"].as<string>());
		}
		if(res.statThreads<1){
			LOG(WARNING)<<"Number of threads has to be bigger than 0! Setting numThreads to 1.";
			res.statThreads=1;
		}

		if(res.scheduling && res.statThreads<=1){
			LOG(WARNING)<<"Undefined behaviour if scheduling option is enabled but numThreads not set! Setting numThreads to 2.";
			res.statThreads=2;
		}

		return res;
	}
}
