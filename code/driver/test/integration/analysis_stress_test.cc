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

#include <gtest/gtest.h>
#include <cstdlib>
#include <iostream>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include "insieme/utils/logging.h"
#include "insieme/core/ir_builder.h"
#include "insieme/driver/integration/tests.h"
#include "insieme/core/annotations/source_location.h"

#ifdef INSIEME_ANALYSIS_DATALOG
#include "insieme/analysis/datalog/interface.h"
#endif

#ifdef INSIEME_ANALYSIS_HASKELL
#include "insieme/analysis/haskell/interface.h"
#endif


namespace insieme {
namespace driver {
namespace integration {

	using namespace utils;
	using namespace core;
	using namespace driver::integration;

	namespace an = insieme::analysis;


	// a few configuration flags
	const char* PRINT_STATS_ENV     = "PRINT_STATS";
	const char* RUN_BLACKLISTED_ENV = "RUN_BLACKLISTED";
	const char* WALL_ENV            = "WALL";
	const char* PEDANTIC_ENV        = "PEDANTIC";


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
						"bots/sort",
						"cilk/pyramid",
						"cpp/bugs/dowhile_this",
						"omp/pyramids",
						"pyramids"
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
						std::cout << "   Source code location: " << *core::annotations::getLocation(arg) << "\n\n";

						// register failure
						if (fail_on_failure) {
							ADD_FAILURE() << "Invalid empty reference set obtained for " << *core::annotations::getLocation(arg);
						}

					} else if (list.isUniversal()) {

						// this is the ugly
						univers++;

						std::cout << "Inaccurate universal reference set obtained for: " << arg << "\n";
						std::cout << "   Source code location: " << *core::annotations::getLocation(arg) << "\n\n";

						// register failure
						if (fail_on_universe) {
							ADD_FAILURE() << "Invalid universal reference set obtained for " << *core::annotations::getLocation(arg);
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

	INSTANTIATE_TEST_CASE_P(OverallTest, StressTests, ::testing::ValuesIn(getAllCases()));


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
