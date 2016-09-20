/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#pragma once

#include <vector>

#ifdef _OPENMP
#include <omp.h>
#endif

#include <boost/program_options.hpp>
#include "insieme/utils/config.h"

#include "insieme/driver/integration/tests.h"
#include "insieme/driver/integration/test_step.h"

namespace bpo = boost::program_options;

namespace insieme {
namespace driver {
namespace integration {
namespace testFramework {

	typedef IntegrationTestCase TestCase;

	struct Options {
		bool valid;
		bool mockrun;
		int num_threads;
		int num_repetitions;
		bool use_median;
		bool statistics;
		bool scheduling;
		bool print_configs;
		#ifdef _OPENMP
		int statThreads = omp_get_max_threads();
		#else
		int statThreads = 0;
		#endif
		bool panic_mode;
		bool list_only;
		bool no_clean;
		bool color;
		bool overwrite;
		vector<string> cases;
		vector<string> steps;
		vector<string> outputFormats;
		bool blacklistedOnly;
		bool longTestsOnly;
		bool longTestsAlso;
		bool preprocessingOnly;
		bool postprocessingOnly;

		// perf metrics
		bool perf;
		string load_miss;
		string store_miss;
		string flops;
		vector<string> perf_metrics;

		Options(bool valid = true)
		    : valid(valid), mockrun(false), num_threads(1), num_repetitions(1), use_median(false), statistics(false), scheduling(false),
		      print_configs(false), panic_mode(false), list_only(false), no_clean(false), color(true), overwrite(false), blacklistedOnly(false),
		      longTestsOnly(false), longTestsAlso(false), preprocessingOnly(false), postprocessingOnly(false), perf(false), load_miss(""),
		      store_miss(""), flops("") {}

		bool operator==(Options a) const {
			return a.mockrun == mockrun && a.num_threads == num_threads && a.num_repetitions == num_repetitions && a.use_median == use_median
			       && a.statistics == statistics && a.scheduling == scheduling && a.statThreads == statThreads && a.cases == cases
			       && a.blacklistedOnly == blacklistedOnly && a.longTestsOnly == longTestsOnly && a.longTestsAlso == longTestsAlso
			       && a.preprocessingOnly == preprocessingOnly && a.postprocessingOnly == postprocessingOnly && a.perf == perf
			       && a.load_miss == load_miss && a.store_miss == store_miss && a.flops == flops && a.perf_metrics == perf_metrics && a.steps == steps;
		}

	  private:
		friend class boost::serialization::access;
		template <class Archive>
		void serialize(Archive& ar, const unsigned int version) {
			ar& mockrun;
			ar& num_threads;
			ar& num_repetitions;
			ar& use_median;
			ar& statistics;
			ar& scheduling;
			ar& statThreads;
			ar& cases;
			ar& perf;
			ar& load_miss;
			ar& store_miss;
			ar& flops;
			ar& perf_metrics;
			ar& steps;
			ar& blacklistedOnly;
			ar& longTestsOnly;
			ar& longTestsAlso;
			ar& preprocessingOnly;
			ar& postprocessingOnly;
		}
	};

	namespace fs = boost::filesystem;

	vector<TestCase> loadCases(const Options& options);

	vector<TestStep> getTestSteps(const Options& options);

	struct Colorize {
		enum Color { RED, YELLOW, GREEN, BLUE, BLACK, BOLD, RESET };
		bool color;
		Colorize(bool color) : color(color) {}
		string getColor(Color c) const {
			if(color) {
				switch(c) {
				case RED: return "\033[31m";
				case YELLOW: return "\033[33m";
				case GREEN: return "\033[32m";
				case BLUE: return "\033[34m";
				case BLACK: return "\033[30m";
				case RESET: return "\033[0m";
				case BOLD: return "\033[1m";
				}
			}

			return "";
		}

		string red() const {
			return getColor(RED);
		}
		string yellow() const {
			return getColor(YELLOW);
		}
		string green() const {
			return getColor(GREEN);
		}
		string blue() const {
			return getColor(BLUE);
		}
		string black() const {
			return getColor(BLACK);
		}
		string reset() const {
			return getColor(RESET);
		}
		string bold() const {
			return getColor(BOLD);
		}
	};


	std::string getGitVersion() {
		string getGitVersionCmd = string("cd ") + utils::getInsiemeSourceRootDir() + "; git describe --dirty";
		FILE* pipe = popen(getGitVersionCmd.c_str(), "r");
		char buff[50];
		auto success = fgets(buff, 50, pipe);
		if(!success) return "unknown version";
		pclose(pipe);

		// remove line break
		buff[strlen(buff) - 1] = '\0';
		return string(buff);
	}


	vector<TestCase> loadCases(const Options& options) {
		// if no test is specified explicitly load all of them
		LoadTestCaseMode loadMode = ENABLED_TESTS;
		if(options.blacklistedOnly) loadMode = BLACKLISTED_TESTS;
		else if(options.longTestsOnly) loadMode = LONG_TESTS;
		else if(options.longTestsAlso) loadMode = ENABLED_AND_LONG_TESTS;
		if(options.cases.empty()) {
			return getAllCases(loadMode);
		}

		// load selected test cases
		vector<TestCase> cases;
		for(const auto& cur : options.cases) {
			// load test case based on the location
			auto curSuite = getTestSuite(cur);
			for(const auto& cur : curSuite) {
				if(!contains(cases, cur)) { // make sure every test is only present once
					cases.push_back(cur);
				}
			}
		}
		return cases;
	}

	vector<TestStep> getTestSteps(const Options& options) {
		vector<TestStep> steps;
		std::map<std::string, TestStep> all;

		if(options.statistics) {
			all = getFullStepList(options.statThreads, options.scheduling);
		} else {
			all = getFullStepList();
		}


		// load steps selected by the options
		if(!options.steps.empty()) {
			for(const auto& cur : options.steps) {
				bool found = false;
				for(auto step : all) {
					if(step.first.find(cur) != std::string::npos) {
						steps.push_back(step.second);
						found = true;
					}
				}
				if(!found) { std::cout << "WARNING: Unknown test step: " << cur << "\n"; }
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
