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


#include "insieme/analysis/cba/cba.h"
#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/analysis/thread_regions.h"
#include "insieme/core/ir_builder.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, SimpleSequential) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	decl auto a = var(0);"
				" 	a = 1;"
				"	a = 2;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		set<ThreadRegion<DefaultContext>> regions = analysis.getValuesOf(ThreadRegions);
		EXPECT_EQ(1, regions.size());
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - O0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(regions));

//		createDotDump(analysis);
	}

	TEST(CBA, SimpleSpawn) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	decl auto a = var(0);"
				" 	spawn a = 1;"
				"	a = 2;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		// fix some labels
		EXPECT_EQ(1, analysis.getLabel(code[1]));

		set<ThreadRegion<DefaultContext>> regions = analysis.getValuesOf(ThreadRegions);
		EXPECT_EQ(3, regions.size());

		auto resStr = toString(regions);

		EXPECT_PRED2(containsSubString, resStr, "I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]");				// main-thread from creation - spawn
		EXPECT_PRED2(containsSubString, resStr, "T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - O0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]");				// main-thread from spawn - end
		EXPECT_PRED2(containsSubString, resStr, "I0-1-2-3-2@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]] - O0-1-2-3-2@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]]");				// spawned thread

//		createDotDump(analysis);
	}

	TEST(CBA, MultiSpawn) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	decl auto a = var(0);"
				" 	spawn a = 1;"
				"	a = 2;"
				" 	spawn a = 3;"
				"	a = 4;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		// just fix some labels (for the thread spawning calls)
		EXPECT_EQ(1,analysis.getLabel(code[1]));
		EXPECT_EQ(2,analysis.getLabel(code[3]));

		set<ThreadRegion<DefaultContext>> regions = analysis.getValuesOf(ThreadRegions);
		EXPECT_EQ(5, regions.size());

		auto resStr = toString(regions);

		EXPECT_PRED2(containsSubString, resStr, "I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - O0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]");

		EXPECT_PRED2(containsSubString, resStr, "I0-1-2-3-2@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]] - O0-1-2-3-2@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "I0-3-2-3-2@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]] - O0-3-2-3-2@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]]");

//		createDotDump(analysis);
	}

	TEST(CBA, Channels) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	decl auto a = var(0);"
				"	decl auto c = channel.create(lit(int<4>),param(1));"
				" 	spawn {"
				"		a = 1;"
				"		channel.send(c,1);"
				"	};"
				" 	spawn {"
				"		channel.recv(c);"
				"		a = 2;"
				"	};"
				"	a = 3;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		// just fix some labels (for the thread spawning calls)
		EXPECT_EQ(1,analysis.getLabel(code[2]));
		EXPECT_EQ(2,analysis.getLabel(code[3]));

		set<ThreadRegion<DefaultContext>> regions = analysis.getValuesOf(ThreadRegions);
		EXPECT_EQ(7, regions.size());

		auto resStr = toString(regions);

		EXPECT_PRED2(containsSubString, resStr, "I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - T0-2@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "T0-2@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - O0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]");

		EXPECT_PRED2(containsSubString, resStr, "I0-2-2-3-2@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]] - T0-2-2-3-2-1-2-0-1-2-1@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "T0-2-2-3-2-1-2-0-1-2-1@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]] - O0-2-2-3-2@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]]");

		EXPECT_PRED2(containsSubString, resStr, "I0-3-2-3-2@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]] - T0-3-2-3-2-1-2-0-1-2-0@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]]");
		EXPECT_PRED2(containsSubString, resStr, "T0-3-2-3-2-1-2-0-1-2-0@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]] - O0-3-2-3-2@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]]");

//		I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - T0-2@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]
//		T0-2@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]
//		I0-2-2-3-2@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]] - T0-2-2-3-2-1-2-0-1-2-1@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]]
//		T0-2-2-3-2-1-2-0-1-2-1@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]] - O0-2-2-3-2@[[0,0],[<1,[0,0],0>,<0,[0,0],0>]]
//		T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]] - O0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]
//		I0-3-2-3-2@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]] - T0-3-2-3-2-1-2-0-1-2-0@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]]
//		T0-3-2-3-2-1-2-0-1-2-0@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]] - O0-3-2-3-2@[[0,0],[<2,[0,0],0>,<0,[0,0],0>]]

//		createDotDump(analysis);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
