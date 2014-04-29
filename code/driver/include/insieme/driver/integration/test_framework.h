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

#pragma once

#include <boost/program_options.hpp>
#include "insieme/utils/config.h"

namespace bpo = boost::program_options;
namespace itc = insieme::driver::integration;

typedef itc::IntegrationTestCase TestCase;
using itc::TestStep;
using itc::TestResult;

namespace insieme{
namespace driver{
namespace integration{
namespace testFramework{

	struct Options {
		bool valid;
		bool mockrun;
		int num_threads;
		int num_repeditions;
		bool statistics;
		bool scheduling;
		bool print_configs;
		int statThreads;
		bool panic_mode;
		bool list_only;
		bool clean;
		bool color;
		bool perf;
		bool overwrite;
		vector<string> cases;
		vector<string> steps;

		Options(bool valid = true)
			: valid(valid), mockrun(false),
			  num_threads(1), num_repeditions(1), statistics(false),scheduling(false), print_configs(false), statThreads(omp_get_max_threads()),
			  panic_mode(false), list_only(false), clean(false), color(true),perf(true),overwrite(false) {}
	};

	namespace fs = boost::filesystem;

	vector<TestCase> loadCases(const Options& options);

	vector<TestStep> getTestSteps(const Options& options);

	struct Colorize{ 
		enum Color { RED, GREEN, BLUE, BLACK, BOLD, RESET};
		bool color;
		Colorize(bool color) : color(color) {}
		string getColor(Color c) {
			if(color) {
				switch(c) {
					case RED: return "\033[31m";
					case GREEN: return "\033[32m";
					case BLUE: return "\033[34m";
					case BLACK: return "\033[30m";
					case RESET: return "\033[0m";
					case BOLD: return "\033[1m";
				}
			}

			return "";
		}

		string red() {return getColor(RED); }
		string green() {return getColor(GREEN); }
		string blue() {return getColor(BLUE); }
		string black() {return getColor(BLACK); }
		string reset() {return getColor(RESET); }
		string bold() {return getColor(BOLD); }
	};


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
		std::map<std::string,TestStep> all;

		if(options.statistics)
			all = itc::getFullStepList(options.statThreads,options.scheduling);
		else
			all = itc::getFullStepList();

		// load steps selected by the options
		if (!options.steps.empty()) {
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
		for(const auto& cur : all) {
			steps.push_back(cur.second);
		}
		return steps;
	}

} // end namespace testFramework
} // end namespace integration
} // end namespace driver
} // end namespace insieme
