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

#include <gtest/gtest.h>

#include "insieme/frontend/frontend.h"

#include "insieme/core/pattern/ir_pattern.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_program.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/driver/integration/tests.h"

namespace insieme {

using namespace core;
using namespace core::pattern;
using namespace driver::integration;


	// --------------- Some global utilities for this test program --------------------

	namespace {

		// a node manager bound to the life cycle of the entire program
		NodeManager testGlobalManager;

		// a cache for already loaded Integration tests
		std::map<IntegrationTestCase, ProgramPtr> loadedCodes;

		// a helper method for loading program code
		ProgramPtr load(NodeManager& manager, const IntegrationTestCase& testCase) {

			// check whether the code is already in the cache
			auto pos = loadedCodes.find(testCase);
			if (pos != loadedCodes.end()) {
				return manager.get(pos->second);
			}

			// not loaded yet => load and cache code
			core::ProgramPtr code = frontend::ConversionJob(testCase.getFiles(), testCase.getIncludeDirs()).execute(testGlobalManager);

			loadedCodes.insert(std::make_pair(testCase, code));
			return manager.get(code);
		}

		ProgramPtr load(NodeManager& manager, const string& name) {
			return load(manager, *getCase(name));
		}

		vector<Match<ptr_target>> findAllMatches(const TreePatternPtr& pattern, const NodePtr& tree) {
			vector<Match<ptr_target>> res;
			visitDepthFirstOnce(tree, [&](const NodePtr& cur){
				MatchOpt curMatch = pattern->matchPointer(cur);
				if (curMatch) {
					res.push_back(*curMatch);
				}
			}, true, true);
			return res;
		}

		void printMatch(const Match<ptr_target>& match) {
			std::cout << "\nMatching:\n";
			if (match.getRoot()) {
				std::cout << "    Structure: \n" << printer::PrettyPrinter(match.getRoot()) << "\n\n";
			}
			std::cout <<     "    Variables:\n";
			for(auto cur : match.getValueMap()) {
				std::cout << "          " << cur.first << " = ";
				if (cur.second.value.getDepth() == 0 && cur.second.value.getValue()) {
					std::cout << printer::PrettyPrinter(cur.second.value.getValue());
				} else {
					std::cout << cur.second.value;
				}
				std::cout << "\n\n";
			}
		}

		void runCheck(const TreePatternPtr& pattern, const string& testCase) {
			NodeManager manager;

			// load program
			core::ProgramPtr code = load(manager, testCase);

			std::cout << "Program: " << *code << "\n";

			std::cout << "-------------------------------------------------------------------------- \n";
			std::cout << "Pattern: " << pattern << "\n";

			vector<Match<ptr_target>> matches = findAllMatches(pattern, code);
			std::cout << "Number of matches: " << matches.size() << "\n";
			for_each(matches, [](const Match<ptr_target>& cur){
				printMatch(cur);
			});
			std::cout << "-------------------------------------------------------------------------- \n";
		}

	}


	// -------- testing patterns -----------------

	TEST(PatternTest, FindAllLiterals) {

		// Example: find all literals within code

		// create pattern
		TreePatternPtr pattern = irp::literal(var("value"),var("type"));

		// run checks
		runCheck(pattern, "hello_world");

	}


	TEST(PatternTest, FindForLoops) {

		// Example: find all for loops within code and get iterator / start / end / step / body

		// create pattern
		TreePatternPtr pattern = irp::forStmt(var("iterator"), var("begin"), var("end"), var("step"), var("body"));

		// run checks
		runCheck(pattern, "matrix_mul_static");
	}

	TEST(PatternTest, ContainsUsedVariable) {

		// Example: find all for loops within code and get iterator / start / end / step / body

		// create pattern
		auto x = var("x");
		auto use = !irp::declarationStmt() & step(x);
		auto pattern = aT(irp::declarationStmt(x)) & aT(use) & all(var("y", use));


		// load input code
		NodeManager mgr;
		core::ProgramPtr code = load(mgr, "matrix_mul_static");

		// goal: make this run for pendulum - now it still takes forever
//		core::ProgramPtr code = load(mgr, "pendulum");

		std::cout << "Running match ... \n";
		auto res = pattern->matchPointer(code);
		std::cout << "Done!\n";

		ASSERT_TRUE(res);
		std::cout << "x = " << res->getVarBinding("x").getValue() << "\n";
		std::cout << "y = " << res->getVarBinding("y").getList() << "\n";

	}

	TEST(PatternTest, ContainsUnusedVariable) {

		// Example: find all for loops within code and get iterator / start / end / step / body

		// create pattern
		auto x = var("x");
		auto use = !irp::declarationStmt() & step(x);
		auto pattern = aT(irp::declarationStmt(x)) & !aT(!irp::declarationStmt(x) & step(x));


		// load input code
		NodeManager mgr;
//		core::ProgramPtr code = load(mgr, "matrix_mul_static");
		core::ProgramPtr code = load(mgr, "pendulum");

		std::cout << "Running match ... \n";
		auto res = pattern->matchPointer(code);
		std::cout << "Done!\n";

		// there should not be a unused variable
		EXPECT_FALSE(res) << "x = " << res->getVarBinding("x").getValue();
	}


}
