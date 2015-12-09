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
		
		auto expA = builder.parseExpr("def a : ()->() { 5; }; a()");
		auto expB = builder.parseExpr("def b : ()->() { 5; }; b()");
		auto expC = builder.parseExpr("def b : ()->() { 7; }; b()");
		
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
			lambda f : () -> int<4> { return *a; }
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
		
		auto stmtA = builder.parseStmt("def a : ()->() { 5; }; def b : ()->() { 6; }; { a(); b(); }");
		auto stmtB = builder.parseStmt("def b : ()->() { 6; }; def a : ()->() { 5; }; { a(); b(); }");
		auto stmtC = builder.parseStmt("def a : ()->() { 6; }; def b : ()->() { 5; }; { a(); b(); }");
		auto stmtD = builder.parseStmt("def b : ()->() { 5; }; def a : ()->() { 6; }; { b(); a(); }");
		
		EXPECT_NE(stmtA, stmtD);
		EXPECT_PRED2(equalNameless, stmtA, stmtB);
		EXPECT_PRED2(notEqNameless, stmtA, stmtC);
		EXPECT_PRED2(equalNameless, stmtA, stmtD);	

		EXPECT_PRED2(notEqNameless, stmtB, stmtC);
		EXPECT_PRED2(equalNameless, stmtB, stmtD);

		EXPECT_PRED2(notEqNameless, stmtC, stmtD);	
	}
	
} // namespace analysis
} // namespace core
} // namespace insieme
