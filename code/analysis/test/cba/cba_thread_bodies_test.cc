/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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


#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/analysis/thread_bodies.h"
#include "insieme/analysis/cba/analysis/merge_all_thread_bodies.h"

#include "insieme/core/ir_builder.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, Simple) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	decl ref<int> x = var(12);"
				"	"
				"	spawn x = 15;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		const auto& T = ThreadBodies;

		auto res = analysis.getValuesOf(code[1], T);
		ASSERT_EQ(1u, res.size());
		EXPECT_EQ("{body@0-1-2-2-2::[[0,0],[<1,[0,0],0>,<0,[0,0],0>]]}", toString(res));
		EXPECT_TRUE(res.begin()->getBody().isa<CallExprPtr>());
	}

	TEST(CBA, ParallelGroup) {

		// a simple test cases checking the handling of a spawned group
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	decl ref<int> x = var(12);"
				"	"
				"	parallel(job {"
				"		x = 15;"
				"	} );"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		// fix the label of the parallel call
		EXPECT_EQ(1,analysis.getLabel(code[1]));

		const auto& T = ThreadBodies;

		auto res = analysis.getValuesOf(code[1], T);
		EXPECT_EQ(2u, res.size());
		auto resStr = toString(res);

		EXPECT_PRED2(containsSubString, resStr, "body@0-1-2-2-2::[[0,0],[<1,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "body@0-1-2-2-2::[[0,0],[<1,[0,0],1>,<0,[0,0],0>]]");

//		createDotDump(analysis);
	}

	TEST(CBA, Uncertain) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);
		std::map<string, NodePtr> symbols;
		symbols["c"] = builder.variable(mgr.getLangBasic().getBool(), 100);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	decl ref<int> x = var(12);"
				"	"
				"	decl auto j1 = task { x = 1; };"
				"	decl auto j2 = task { x = 2; };"
				"	"
				"	decl auto t = parallel((c)?j1:j2);"
				"	sync t;"
				"}", symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		// just fix some labels (for the thread spawning calls)
		EXPECT_EQ(3,analysis.getLabel(code[3].as<DeclarationStmtAddress>()->getInitialization()));

		const auto& T = ThreadBodies;

		auto res = analysis.getValuesOf(code[3].as<DeclarationStmtAddress>()->getInitialization(), T);
		EXPECT_EQ(2u, res.size());
		auto resStr = toString(res);

		EXPECT_PRED2(containsSubString, resStr, "body@0-1-1-2-2::[[0,0],[<3,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "body@0-2-1-2-2::[[0,0],[<3,[0,0],0>,<0,[0,0],0>]]");


		auto res2 = analysis.getValuesOf(code[4], T);
		auto res2Str = toString(res2);

		EXPECT_EQ(2u, res2.size());
		EXPECT_EQ(resStr, res2Str);

//		createDotDump(analysis);
	}

	TEST(CBA, MergeAll) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);
		std::map<string, NodePtr> symbols;
		symbols["c"] = builder.variable(mgr.getLangBasic().getBool(), 100);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	decl ref<int> x = var(12);"
				"	"
				"	decl auto j1 = task { x = 1; };"
				"	decl auto j2 = task { x = 2; };"
				"	"
				"	parallel((c)?j1:j2);"
				"	syncAll;"
				"}", symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		// just fix some labels (for the thread spawning calls)
		EXPECT_EQ(3,analysis.getLabel(code[3]));

		const auto& T = MergeAllThreadBodies;

		auto res = analysis.getValuesOf(code[4], T);
		ASSERT_EQ(1u, res.size());
		EXPECT_EQ(2u, res.begin()->size());
		auto resStr = toString(res);

		EXPECT_PRED2(containsSubString, resStr, "body@0-1-1-2-2::[[0,0],[<3,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "body@0-2-1-2-2::[[0,0],[<3,[0,0],0>,<0,[0,0],0>]]");

//		createDotDump(analysis);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
