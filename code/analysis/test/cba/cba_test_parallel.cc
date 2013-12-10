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


#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/framework/generator/reaching_definitions.h"
#include "insieme/analysis/cba/framework/generator/killed_definitions.h"

#include "insieme/analysis/cba/analysis/jobs.h"
#include "insieme/analysis/cba/analysis/references.h"

#include "insieme/core/ir_builder.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	// check reaching definitions

	TEST(CBA, Definitions_SequentialFlow) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		map<string,NodePtr> symbols;
		symbols["c"] = builder.variable(builder.parseType("bool"));

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	"
				"	ref<int> x = var(10);"		// def1
				"	ref<int> y = var(10);"		// def2
				"	c;"							// RD(x) = { def1 }, RD(y) = { def2 }, KD(x) = {}, KD(y) = {}
				"	x = 12;"					// def3
				"	c;"							// RD(x) = { def3 }, RD(y) = { def2 }, KD(x) = { def1 }, KD(y) = {}
				"	if (c) {"
				"		x = 14;"				// def4
				"		y = 16;"				// def5
				"	}"
				"	c;"							// RD(x) = { def3, def4 }, RD(y) = { def2, def5 }
				"	y = 12;"					// def4
				"	c;"							// RD(x) = { def3, def4 }, RD(y) = { def4 }
				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);
		CBA analysis(code);

		DefaultContext ctxt;

		// obtain location referenced by variable x
		set<Reference<DefaultContext>> refs = analysis.getValuesOf(code[0].as<DeclarationStmtAddress>()->getVariable(), R);
		EXPECT_EQ(1u, refs.size()) << refs;
		Location<DefaultContext> x = refs.begin()->getLocation();

		// also obtain location y
		refs = analysis.getValuesOf(code[1].as<DeclarationStmtAddress>()->getVariable(), R);
		EXPECT_EQ(1u, refs.size()) << refs;
		Location<DefaultContext> y = refs.begin()->getLocation();

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[2],  RDin, ctxt, x)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[2], RDout, ctxt, x)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[2],  RDin, ctxt, y)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[2], RDout, ctxt, y)));

		EXPECT_EQ("{}", 	toString(analysis.getValuesOf(code[2],  KDin, ctxt, x)));
		EXPECT_EQ("{}", 	toString(analysis.getValuesOf(code[2], KDout, ctxt, x)));
		EXPECT_EQ("{}", 	toString(analysis.getValuesOf(code[2],  KDin, ctxt, y)));
		EXPECT_EQ("{}", 	toString(analysis.getValuesOf(code[2], KDout, ctxt, y)));



		EXPECT_EQ("{(0-3,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 					toString(analysis.getValuesOf(code[4],  RDin, ctxt, x)));
		EXPECT_EQ("{(0-3,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 					toString(analysis.getValuesOf(code[4], RDout, ctxt, x)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[4],  RDin, ctxt, y)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[4], RDout, ctxt, y)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[4],  KDin, ctxt, x)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[4], KDout, ctxt, x)));
		EXPECT_EQ("{}", 														toString(analysis.getValuesOf(code[4],  KDin, ctxt, y)));
		EXPECT_EQ("{}", 														toString(analysis.getValuesOf(code[4], KDout, ctxt, y)));



		EXPECT_EQ("{(0-3,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-0,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 					toString(analysis.getValuesOf(code[6],  RDin, ctxt, x)));
		EXPECT_EQ("{(0-3,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-0,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 					toString(analysis.getValuesOf(code[6], RDout, ctxt, x)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[6],  RDin, ctxt, y)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[6], RDout, ctxt, y)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[6],  KDin, ctxt, x)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[6], KDout, ctxt, x)));
		EXPECT_EQ("{}", 														toString(analysis.getValuesOf(code[6],  KDin, ctxt, y)));
		EXPECT_EQ("{}", 														toString(analysis.getValuesOf(code[6], KDout, ctxt, y)));



		EXPECT_EQ("{(0-3,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-0,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 					toString(analysis.getValuesOf(code[7],  RDin, ctxt, x)));
		EXPECT_EQ("{(0-3,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-0,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 					toString(analysis.getValuesOf(code[7], RDout, ctxt, x)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[7],  RDin, ctxt, y)));
		EXPECT_EQ("{(0-7,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 																toString(analysis.getValuesOf(code[7], RDout, ctxt, y)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 												toString(analysis.getValuesOf(code[7],  KDin, ctxt, x)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 												toString(analysis.getValuesOf(code[7], KDout, ctxt, x)));
		EXPECT_EQ("{}", 																									toString(analysis.getValuesOf(code[7],  KDin, ctxt, y)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[7], KDout, ctxt, y)));



		EXPECT_EQ("{(0-3,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-0,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 		toString(analysis.getValuesOf(code[8],  RDin, ctxt, x)));
		EXPECT_EQ("{(0-3,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-0,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 		toString(analysis.getValuesOf(code[8], RDout, ctxt, x)));
		EXPECT_EQ("{(0-7,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 													toString(analysis.getValuesOf(code[8],  RDin, ctxt, y)));
		EXPECT_EQ("{(0-7,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 													toString(analysis.getValuesOf(code[8], RDout, ctxt, y)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 												toString(analysis.getValuesOf(code[8],  KDin, ctxt, x)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 												toString(analysis.getValuesOf(code[8], KDout, ctxt, x)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[8],  KDin, ctxt, y)));
		EXPECT_EQ("{(0-1-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-5-1-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[8], KDout, ctxt, y)));

//		createDotDump(analysis);
	}

	TEST(CBA, Definitions_SimpleParallel) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	"
				"	ref<int> x = var(10);"		// def1
				"	*x;"		// def1 should reach this point
				"	x = 12;"	// def2
				"	*x;"		// def2 should reach this point
				"	auto g = spawn x = 14;"		// def3
				"	*x;"		// def2 should reach this point
				"	sync g;"
				"	*x;"		// def2 and def3 should reach this point
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);
		CBA analysis(code);

		DefaultContext ctxt;

		// obtain location referenced by variable x
		set<Reference<DefaultContext>> refs = analysis.getValuesOf(code[0].as<DeclarationStmtAddress>()->getVariable(), R);
		EXPECT_EQ(1u, refs.size()) << refs;
		Location<DefaultContext> loc = refs.begin()->getLocation();


		// -- reaching definitions --

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[1], RDin, ctxt, loc)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[1], RDout, ctxt, loc)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[2], RDin, ctxt, loc)));
		EXPECT_EQ("{(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 					toString(analysis.getValuesOf(code[2], RDout, ctxt, loc)));

		EXPECT_EQ("{(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 				toString(analysis.getValuesOf(code[3], RDin, ctxt, loc)));
		EXPECT_EQ("{(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 				toString(analysis.getValuesOf(code[3], RDout, ctxt, loc)));

		EXPECT_EQ("{(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 				toString(analysis.getValuesOf(code[5], RDin, ctxt, loc)));
		EXPECT_EQ("{(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 				toString(analysis.getValuesOf(code[5], RDout, ctxt, loc)));

		EXPECT_EQ("{(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 				toString(analysis.getValuesOf(code[7], RDin, ctxt, loc)));
		EXPECT_EQ("{(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 				toString(analysis.getValuesOf(code[7], RDout, ctxt, loc)));


		// -- killed definitions --

		EXPECT_EQ("{}", 												toString(analysis.getValuesOf(code[1], KDin, ctxt, loc)));
		EXPECT_EQ("{}", 												toString(analysis.getValuesOf(code[1], KDout, ctxt, loc)));

		EXPECT_EQ("{}", 														toString(analysis.getValuesOf(code[2], KDin, ctxt, loc)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[2], KDout, ctxt, loc)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[3], KDin, ctxt, loc)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[3], KDout, ctxt, loc)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[5], KDin, ctxt, loc)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[5], KDout, ctxt, loc)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[7], KDin, ctxt, loc)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", 	toString(analysis.getValuesOf(code[7], KDout, ctxt, loc)));

//		createDotDump(analysis);
	}


	TEST(CBA, SimpelParallel) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	"
				"	ref<int> x = var(12);"
				"	*x;"		// should be 12
				"	auto g = spawn x = 14;"
				"	*x;"		// should still be 12
				"	sync g;"
				"	*x;"		// should be 14
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);
//dumpPretty(in);
		CBA analysis(code);

		EXPECT_EQ("{12}", toString(analysis.getValuesOf(code[1].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{12}", toString(analysis.getValuesOf(code[3].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{14}", toString(analysis.getValuesOf(code[5].as<ExpressionAddress>(), A)));

//		createDotDump(analysis);

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
