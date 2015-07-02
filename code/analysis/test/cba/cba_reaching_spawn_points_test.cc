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


#include "insieme/analysis/cba/cba.h"
#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/analysis/reaching_spawn_points.h"
#include "insieme/core/ir_builder.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, NoSpawn) {

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

		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[0], ReachingSpawnPointsIn)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[0], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[0], ReachingSpawnPointsOut)));

		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[1], ReachingSpawnPointsIn)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[1], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[1], ReachingSpawnPointsOut)));

		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[2], ReachingSpawnPointsIn)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[2], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[2], ReachingSpawnPointsOut)));

//		createDotDump(analysis);
	}

	TEST(CBA, Spawns) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	decl auto a = var(0);"
				" 	spawn a = 1;"
				"	syncAll;"
				"	spawn a = 2;"
				"	spawn a = 3;"
				"	syncAll;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[0], ReachingSpawnPointsIn)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[0], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[0], ReachingSpawnPointsOut)));

		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[1], ReachingSpawnPointsIn)));
		EXPECT_EQ("{}", toString(analysis.getValuesOf(code[1], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], ReachingSpawnPointsOut)));

		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], ReachingSpawnPointsIn)));
		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{}", 									  toString(analysis.getValuesOf(code[2], ReachingSpawnPointsOut)));

		EXPECT_EQ("{}", 									  toString(analysis.getValuesOf(code[3], ReachingSpawnPointsIn)));
		EXPECT_EQ("{}", 									  toString(analysis.getValuesOf(code[3], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[3], ReachingSpawnPointsOut)));

		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[4], ReachingSpawnPointsIn)));
		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[4], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]],T0-4@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[4], ReachingSpawnPointsOut)));

		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]],T0-4@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[5], ReachingSpawnPointsIn)));
		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]],T0-4@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[5], ReachingSpawnPointsTmp)));
		EXPECT_EQ("{}", 									  toString(analysis.getValuesOf(code[5], ReachingSpawnPointsOut)));

//		createDotDump(analysis);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
