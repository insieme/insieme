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
#include <gtest/gtest.h>

#include <cstdlib>
#include <fstream>
#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "insieme/core/annotations/source_location.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/inspyer/inspyer.h"

#include "insieme/driver/integration/tests.h"

#include "insieme/utils/gtest_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/analysis/cba/interface.h"

#ifdef INSIEME_ANALYSIS_DATALOG
#include "insieme/analysis/cba/datalog/interface.h"
#endif

#ifdef INSIEME_ANALYSIS_HASKELL
#include "insieme/analysis/cba/haskell/interface.h"
#endif


namespace insieme {
namespace driver {
namespace integration {

	using namespace utils;
	using namespace core;
	using namespace driver::integration;

	namespace an = insieme::analysis::cba;
	namespace fs = boost::filesystem;


	// a few configuration flags
	const char* PRINT_STATS_ENV     = "PRINT_STATS";
	const char* RUN_BLACKLISTED_ENV = "RUN_BLACKLISTED";
	const char* WALL_ENV            = "WALL";
	const char* PEDANTIC_ENV        = "PEDANTIC";
	const char* INSPYER_ENV         = "INSIEME_INSPYER";


	namespace {

		template<typename Engine>
		bool isBlackListed(const driver::integration::IntegrationTestCase& test) {

			// check kill switch
			if (std::getenv(RUN_BLACKLISTED_ENV)) {
				return false;
			}

			std::vector<string> blacklist;

			// get blacklist
			#ifdef INSIEME_ANALYSIS_HASKELL
				if (typeid(Engine) == typeid(an::HaskellEngine)) {
					blacklist = std::vector<string>{
						"seq/c/bots/sort",
						"cilk/nqueens",
						"cilk/pyramid",
						"seq/cpp/bugs/dowhile_this",
						"omp/c/pyramids",
						"seq/c/pyramids"
					};
				}
			#endif

			// check blacklist
			for(const auto& name : blacklist) {
				if (test.getName() == name) return true;
			}
			return false;

		}



		template<typename Engine>
		void run(const driver::integration::IntegrationTestCase& testCase) {

			bool wall = std::getenv(WALL_ENV);
			bool pedantic = std::getenv(PEDANTIC_ENV);
			bool dump_on_failure = std::getenv(INSPYER_ENV);

			bool fail_on_failure  = wall || pedantic;
			bool fail_on_universe = pedantic;


			// obtain test case
			SCOPED_TRACE("Testing Case: " + testCase.getName());
			LOG(INFO) << "Testing Case: " + testCase.getName();

			// check blacklist
			if (isBlackListed<Engine>(testCase)) {
				std::cout << "Skipping black-listed test case: " << testCase << "\n";
				return;
			}

			NodeManager manager;
			IRBuilder builder(manager);
			ProgramPtr code = testCase.load(manager);
			inspyer::MetaGenerator meta(code);


			const auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
			ExpressionPtr ref_deref  = refExt.getRefDeref();
			ExpressionPtr ref_assign = refExt.getRefAssign();

			TypePtr boolean = manager.getLangBasic().getBool();

			int narrow = 0;
			int failure = 0;
			int univers = 0;

			typename Engine::context_type ctxt;

			// locate all ref_deref and ref_assign symbols
			visitDepthFirst(NodeAddress(code), [&](const CallExprAddress& call) {
				auto trg = call->getFunctionExpr().as<ExpressionPtr>();
				if (*trg == *ref_deref || *trg == *ref_assign) {

					auto arg = call->getArgument(0);
					auto list = an::getReferencedMemoryLocations<Engine>(ctxt,arg);
					if (list.empty()) {

						// this is bad
						failure++;

						std::cout << "Presumably invalid empty reference obtained for: " << arg << "\n";
						std::cout << "   Source code location: " << core::annotations::getLocationString(arg) << "\n\n";

						meta.addBookmark(arg);
						meta.addLabel(arg, "Empty Set");

						// register failure
						if (fail_on_failure) {
							ADD_FAILURE() << "Invalid empty reference set obtained for " << core::annotations::getLocationString(arg);
						}

					} else if (list.isUniversal()) {

						// this is the ugly
						univers++;

						std::cout << "Inaccurate universal reference set obtained for: " << arg << "\n";
						std::cout << "   Source code location: " << core::annotations::getLocationString(arg) << "\n\n";

						//meta.addBookmark(arg);
						//meta.addLabel(arg, "Universe");

						// register failure
						if (fail_on_universe) {
							ADD_FAILURE() << "Invalid universal reference set obtained for " << core::annotations::getLocationString(arg);
						}

					} else {

						// this is good
						narrow++;

					}

				}
			});


			// performance data:
			if (std::getenv(PRINT_STATS_ENV)) {
				ctxt.dumpStatistics();

				std::cout <<
					"Checks: " << (failure + univers + narrow) << "\n" <<
					"Failures:  " << failure << "\n" <<
					"Universal: " << univers << "\n" <<
					"OK:        " << narrow  << "\n";
			}

			if(dump_on_failure && failure > 0) {
				fs::path dump_dir("dumps");
				if(!fs::exists(dump_dir)) fs::create_directory(dump_dir);

				std::string filename = testCase.getName();
				std::replace(filename.begin(), filename.end(), '/', '_');

				auto dump_file = (dump_dir / (filename + ".json")).string();
				auto dump_file_meta = (dump_dir / (filename + "_meta.json")).string();

				std::ofstream out_dump(dump_file);
				inspyer::dumpTree(out_dump, code);

				std::ofstream out_meta(dump_file_meta);
				meta.dump(out_meta);
			}
		}

	}



	// the type definition (specifying the parameter type)
	class StressTests : public ::testing::TestWithParam<IntegrationTestCase> {};

	TEST_P(StressTests, Dummy) {
		// one has to be here
	}

#ifdef INSIEME_ANALYSIS_DATALOG
	TEST_P(StressTests, Datalog) {
		// -- not implemented for datalog yet --
		// run<an::DatalogEngine>(GetParam());
	}
#endif

#ifdef INSIEME_ANALYSIS_HASKELL
	TEST_P(StressTests, Haskell) {
		run<an::HaskellEngine>(GetParam());
	}
#endif

	INSTANTIATE_TEST_CASE_P(OverallTest, StressTests, ::testing::ValuesIn(getAllCases()), TestCaseNamePrinter());


//	TEST(BuiltIn, Stats) {
//		core::NodeManager mgr;
//
//
//		std::map<string,int> stats;
//		for(const auto& cur : getAllCases()) {
//			std::cout << "Processing " << cur.getName() << " ...\n";
//			auto code = cur.load(mgr);
//			visitDepthFirstOnce(code, [&](const LambdaExprPtr& lambda) {
//				if (core::lang::isBuiltIn(lambda)) {
//					stats[core::lang::getConstructName(lambda)]++;
//				}
//			});
//		}
//
//		for(const auto& cur : stats) {
//			std::cout << cur.first << "\t" << cur.second << "\n";
//		}
//
//	}


} // end namespace integration
} // end namespace driver
} // end namespace insieme
