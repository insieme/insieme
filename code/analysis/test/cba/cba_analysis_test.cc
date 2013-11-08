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

#include "insieme/analysis/cba/analysis.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA_Analysis, BooleanConstants) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<string,core::NodePtr> symbols;
		symbols["c"] = builder.parseExpr("lit(\"c\":bool)");
		symbols["n"] = builder.parseExpr("lit(\"n\":int<4>)");
		symbols["m"] = builder.parseExpr("lit(\"m\":int<4>)");

		auto code = builder.parseStmt(
				"{"
				"	true;"
				"	false;"
				"	"
				"	true || false;"
				"	true && false;"
				"	"
				"	c;"
				"	true && c;"
				"	false && c;"
				"	"
				"	ref<bool> a = var(c);"
				"	*a;"
				"	a = true;"
				"	*a;"
				"	a = false;"
				"	*a;"
				"	"
				"	12 < 14;"
				"	7 + n < 12 + n;"
				"	7 + n < 12 + m;"
				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);
		auto root = CompoundStmtAddress(code);

		// just check all the expressions
		auto cur = root[0].as<core::ExpressionAddress>();
		EXPECT_TRUE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_FALSE(mayBeFalse(cur));

		cur = root[1].as<core::ExpressionAddress>();
		EXPECT_FALSE(isTrue(cur));
		EXPECT_FALSE(mayBeTrue(cur));
		EXPECT_TRUE(isFalse(cur));
		EXPECT_TRUE(mayBeFalse(cur));

		cur = root[2].as<core::ExpressionAddress>();
		EXPECT_TRUE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_FALSE(mayBeFalse(cur));

		cur = root[3].as<core::ExpressionAddress>();
		EXPECT_FALSE(isTrue(cur));
		EXPECT_FALSE(mayBeTrue(cur));
		EXPECT_TRUE(isFalse(cur));
		EXPECT_TRUE(mayBeFalse(cur));

		// c
		cur = root[4].as<core::ExpressionAddress>();
		EXPECT_FALSE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_TRUE(mayBeFalse(cur));

		// c && true
		cur = root[5].as<core::ExpressionAddress>();
		EXPECT_FALSE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_TRUE(mayBeFalse(cur));

		// c && false
		cur = root[6].as<core::ExpressionAddress>();
		EXPECT_FALSE(isTrue(cur));
		EXPECT_FALSE(mayBeTrue(cur));
		EXPECT_TRUE(isFalse(cur));
		EXPECT_TRUE(mayBeFalse(cur));

		// *a
		cur = root[8].as<core::ExpressionAddress>();
		EXPECT_FALSE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_TRUE(mayBeFalse(cur));

		// *a
		cur = root[10].as<core::ExpressionAddress>();
		EXPECT_TRUE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_FALSE(mayBeFalse(cur));

		// *a
		cur = root[12].as<core::ExpressionAddress>();
		EXPECT_FALSE(isTrue(cur));
		EXPECT_FALSE(mayBeTrue(cur));
		EXPECT_TRUE(isFalse(cur));
		EXPECT_TRUE(mayBeFalse(cur));


		// 12 < 14
		cur = root[13].as<core::ExpressionAddress>();
		EXPECT_TRUE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_FALSE(mayBeFalse(cur));

		// 7 + n < 12 + n
		cur = root[14].as<core::ExpressionAddress>();
		EXPECT_TRUE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_FALSE(mayBeFalse(cur));

		// 7 + n < 12 + m
		cur = root[15].as<core::ExpressionAddress>();
		EXPECT_FALSE(isTrue(cur));
		EXPECT_TRUE(mayBeTrue(cur));
		EXPECT_FALSE(isFalse(cur));
		EXPECT_TRUE(mayBeFalse(cur));
	}


	TEST(CBA_Analysis, AliasesSimple) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<string,core::NodePtr> symbols;
		symbols["extern"] = builder.parseExpr("lit(\"c\":bool)");
		symbols["e"] = builder.parseExpr("lit(\"e\":ref<int<4>>)");

		auto code = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	let point = struct { int a; int b; };"
				"	"
				"	ref<int> a = var(10);"
				"	ref<int> b = var(11);"
				"	ref<int> c = a;"
				"	ref<ref<int>> d = var(b);"
				"	"
				"	a;"
				"	b;"
				"	c;"
				"	*d;"
				"	e;"
				"	"
				"	d = a;"
				"	*d;"
				"	"
				"	if (extern) d = b; "
				"	*d;"
				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(code);
		auto root = CompoundStmtAddress(code);

		auto a  = root[4].as<ExpressionAddress>();
		auto b  = root[5].as<ExpressionAddress>();
		auto c  = root[6].as<ExpressionAddress>();
		auto d1 = root[7].as<ExpressionAddress>();
		auto e  = root[8].as<ExpressionAddress>();

		auto d2 = root[10].as<ExpressionAddress>();
		auto d3 = root[12].as<ExpressionAddress>();

		// check a
		EXPECT_FALSE( isAlias(a,b));
		EXPECT_FALSE(mayAlias(a,b));

		EXPECT_TRUE ( isAlias(a,c));
		EXPECT_TRUE (mayAlias(a,c));

		EXPECT_FALSE( isAlias(a,d1));
		EXPECT_FALSE(mayAlias(a,d1));

		EXPECT_TRUE ( isAlias(a,d2));
		EXPECT_TRUE (mayAlias(a,d2));

		EXPECT_FALSE( isAlias(a,d3));
		EXPECT_TRUE (mayAlias(a,d3));

		// check b
		EXPECT_FALSE( isAlias(b,c));
		EXPECT_FALSE(mayAlias(b,c));

		EXPECT_TRUE ( isAlias(b,d1));
		EXPECT_TRUE (mayAlias(b,d1));

		EXPECT_FALSE( isAlias(b,d2));
		EXPECT_FALSE(mayAlias(b,d2));

		EXPECT_FALSE( isAlias(b,d3));
		EXPECT_TRUE (mayAlias(b,d3));

		// and c
		EXPECT_FALSE( isAlias(c,d1));
		EXPECT_FALSE(mayAlias(c,d1));

		EXPECT_TRUE ( isAlias(c,d2));
		EXPECT_TRUE (mayAlias(c,d2));

		EXPECT_FALSE( isAlias(c,d3));
		EXPECT_TRUE (mayAlias(c,d3));

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
