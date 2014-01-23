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
#include "insieme/analysis/cba/analysis/reaching_sync_points.h"
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
				"	auto a = var(0);"
				" 	a = 1;"
				"	a = 2;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[0], RSPin)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[0], RSPout)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], RSPin)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], RSPout)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], RSPin)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], RSPout)));

//		createDotDump(analysis);
	}

	TEST(CBA, SpawnSingle) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	auto a = var(0);"
				" 	spawn a = 1;"
				"	a = 2;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[0], RSPin)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[0], RSPout)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], RSPin)));
		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], RSPout)));
		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], RSPin)));
		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], RSPout)));

//		createDotDump(analysis);
	}

	TEST(CBA, SpawnMultiple) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	auto a = var(0);"
				" 	spawn a = 1;"
				"	a = 2;"
				" 	spawn a = 3;"
				"	a = 4;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[0], RSPin)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[0], RSPout)));

		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], RSPin)));
		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], RSPout)));

		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], RSPin)));
		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], RSPout)));

		EXPECT_EQ("{T0-1@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[3], RSPin)));
		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[3], RSPout)));

		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[4], RSPin)));
		EXPECT_EQ("{T0-3@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[4], RSPout)));

//		createDotDump(analysis);
	}

	TEST(CBA, SpawnAndChannels) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	auto a = var(0);"
				"	auto c = channel.create(lit(int<4>),param(2));"
				" 	spawn a = 1;"
				"	a = 2;"
				" 	channel.send(c,1);"
				"	a = 3;"
				"	channel.recv(c);"
				"	a = 4;"
				"}"
		).as<CompoundStmtPtr>();

		CompoundStmtAddress code(in);

		CBA analysis(code);

		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[0], RSPin)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[0], RSPout)));

		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], RSPin)));
		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[1], RSPout)));

		EXPECT_EQ("{I0@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], RSPin)));
		EXPECT_EQ("{T0-2@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[2], RSPout)));

		EXPECT_EQ("{T0-2@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[3], RSPin)));
		EXPECT_EQ("{T0-2@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[3], RSPout)));

		EXPECT_EQ("{T0-2@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[4], RSPin)));
		EXPECT_EQ("{T0-4@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[4], RSPout)));

		EXPECT_EQ("{T0-4@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[5], RSPin)));
		EXPECT_EQ("{T0-4@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[5], RSPout)));

		EXPECT_EQ("{T0-4@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[6], RSPin)));
		EXPECT_EQ("{T0-6@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[6], RSPout)));

		EXPECT_EQ("{T0-6@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[7], RSPin)));
		EXPECT_EQ("{T0-6@[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]}", toString(analysis.getValuesOf(code[7], RSPout)));

//		createDotDump(analysis);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
