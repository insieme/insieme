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
#include "insieme/analysis/cba/analysis/uninterpreted_symbols.h"
#include "insieme/utils/set_utils.h"

#include "insieme/core/ir_builder.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, Symbols) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let a = expr lit(\"a\":A);"
				"	let b = expr lit(\"b\":B);"
				"	let f = expr lit(\"f\":(A,B)->C);"
				"	"
				"	decl auto x = var(a);"
				"	decl auto y = var(b);"
				"	"
				"	*x;"
				"	*y;"
				"	f(*x,*y);"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		EXPECT_EQ("{AP(a)}", toString(analysis.getValuesOf(code[2].as<ExpressionAddress>(), U)));
		EXPECT_EQ("{AP(b)}", toString(analysis.getValuesOf(code[3].as<ExpressionAddress>(), U)));
		EXPECT_EQ("{AP(f(a, b))}", toString(analysis.getValuesOf(code[4].as<ExpressionAddress>(), U)));

	}

	TEST(CBA, Symbols2) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);
		map<string,NodePtr> symbols;
		symbols["c"] = builder.variable(mgr.getLangBasic().getBool());

		auto in = builder.parseStmt(
				"{"
				"	let a = expr lit(\"a\":A);"
				"	let b = expr lit(\"b\":A);"
				"	let f = expr lit(\"f\":(A,A)->C);"
				"	"
				"	decl auto x = var(a);"
				"	decl auto y = var(b);"
				"	if (c) x = *y;"					// here we have a undefined choice
				"	"
				"	*x;"
				"	*y;"
				"	f(*x,*y);"
				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		auto litA = builder.parseExpr("lit(\"a\":A)");
		auto litB = builder.parseExpr("lit(\"b\":A)");
		auto litF = builder.parseExpr("lit(\"f\":(A,A)->C)");

		auto fAB = builder.callExpr(litF, litA, litB);
		auto fBB = builder.callExpr(litF, litB, litB);

		EXPECT_EQ(utils::set::toSet<set<ExpressionPtr>>(litA,litB), 	(const set<ExpressionPtr>&)analysis.getValuesOf(code[3].as<ExpressionAddress>(), U));
		EXPECT_EQ(utils::set::toSet<set<ExpressionPtr>>(litB), 			(const set<ExpressionPtr>&)analysis.getValuesOf(code[4].as<ExpressionAddress>(), U));
		EXPECT_EQ(utils::set::toSet<set<ExpressionPtr>>(fAB,fBB), 		(const set<ExpressionPtr>&)analysis.getValuesOf(code[5].as<ExpressionAddress>(), U));

	}

	TEST(CBA, Loops) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		map<string,NodePtr> symbols;
		symbols["cond"] = builder.variable(mgr.getLangBasic().getBool());

		auto code = builder.parseStmt(
				"{"
				"	let x = expr lit(\"x\":X);"
				"	let g = expr lit(\"g\":(X)->X);"
				"	"
				"	decl ref<X> a = var(x);"
				"	"
				"	*a;"
				"	"
				"	for(int<4> i = 0 .. 10 : 1) {"
				"		*a;"
				"		a = g(*a);"
				"		*a;"
				"	}"
				"	"
				"	*a;"
				"}",
				symbols
		).as<CompoundStmtPtr>();

		CompoundStmtAddress root(code);

		auto a1 = root[1].as<ExpressionAddress>();
		auto a3 = root[3].as<ExpressionAddress>();

		auto a21 = root[2].as<ForStmtAddress>()->getBody()[0];
		auto a22 = root[2].as<ForStmtAddress>()->getBody()[2];

		CBA analysis(root);

		set<ExpressionPtr> va21 = analysis.getValuesOf(a21, U);
		set<ExpressionPtr> va22 = analysis.getValuesOf(a22, U);
		set<ExpressionPtr> va3  = analysis.getValuesOf(a3,  U);

		// get all the values
		EXPECT_EQ("{AP(x)}", toString(analysis.getValuesOf(a1,  U)));

		auto isUndefined = [](const set<ExpressionPtr>& set)->bool {
			return contains(set, ExpressionPtr());
		};

		EXPECT_PRED1(isUndefined, va21);
		EXPECT_PRED1(isUndefined, va22);
		EXPECT_PRED1(isUndefined, va3);

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
