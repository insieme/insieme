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
 */

#include <gtest/gtest.h>

#include <boost/algorithm/string/replace.hpp>

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/compare.h"
#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace core {
namespace analysis {

	bool notEqNameless(const NodePtr& a, const NodePtr& b) { return !equalNameless(a,b); }

	TEST(EqualNameless, Basic) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto expA = builder.parseExpr("7+5-3*8");
		auto expB = builder.parseExpr("7+5-2*8");

		EXPECT_PRED2(equalNameless, expA, expA);
		EXPECT_PRED2(notEqNameless, expA, expB);
	}

	TEST(EqualNameless, Lambdas) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto expA = builder.parseExpr("def a = ()->() { 5; }; a()");
		auto expB = builder.parseExpr("def b = ()->() { 5; }; b()");
		auto expC = builder.parseExpr("def b = ()->() { 7; }; b()");

		EXPECT_NE(expA, expB);
		EXPECT_PRED2(equalNameless, expA, expB);
		EXPECT_NE(expB, expC);
		EXPECT_PRED2(notEqNameless, expB, expC);
	}

	TEST(EqualNameless, TagTypes) {
		NodeManager nm;
		IRBuilder builder(nm);

		string structStringA = R"(
		def struct StructName {
			a : int<4>;
			lambda f = () -> int<4> { return *a; }
		}; StructName )";
		auto structStringC = structStringA;
		boost::algorithm::replace_all(structStringC, "int<4>", "int<8>");

		auto typeA = builder.parseType(structStringA);
		auto typeB =
			transform::replaceAll(nm, typeA, builder.tagTypeReference("StructName"), builder.tagTypeReference("AnotherName"), transform::globalReplacement);
		auto typeC = builder.parseType(structStringC);

		EXPECT_NE(typeA, typeB);
		EXPECT_PRED2(equalNameless, typeA, typeB);
		EXPECT_NE(typeB, typeC);
		EXPECT_PRED2(notEqNameless, typeB, typeC);
	}

	TEST(EqualNameless, FalsePositive) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto stmtA = builder.parseStmt("def a = ()->() { 5; }; def b = ()->() { 6; }; { a(); b(); }");
		auto stmtB = builder.parseStmt("def b = ()->() { 6; }; def a = ()->() { 5; }; { a(); b(); }");
		auto stmtC = builder.parseStmt("def a = ()->() { 6; }; def b = ()->() { 5; }; { a(); b(); }");
		auto stmtD = builder.parseStmt("def b = ()->() { 5; }; def a = ()->() { 6; }; { b(); a(); }");

		EXPECT_NE(stmtA, stmtD);
		EXPECT_PRED2(equalNameless, stmtA, stmtB);
		EXPECT_PRED2(notEqNameless, stmtA, stmtC);
		EXPECT_PRED2(equalNameless, stmtA, stmtD);

		EXPECT_PRED2(notEqNameless, stmtB, stmtC);
		EXPECT_PRED2(equalNameless, stmtB, stmtD);

		EXPECT_PRED2(notEqNameless, stmtC, stmtD);
	}

	TEST(EqualName, This_test_has_no_name) {
		// this test is only for coverage!!
		// testing a debug function

		NodeManager nm;
		IRBuilder b(nm);

		auto ir1 = b.parseExpr("def f = function () -> unit {}; def fun = function (v1 : ref<int<4>>) -> unit {f();v1;\"hello\";}; fun(2)");
		auto ir2 = b.parseExpr("def fun = function (v2 : ref<int<4>>) -> unit {1;2;3;}; fun(1)");
		irDiff(ir1, ir2, "ir1", "ir2", 1);

	}

} // namespace analysis
} // namespace core
} // namespace insieme
