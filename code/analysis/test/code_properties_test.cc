/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/datalog/code_properties.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace datalog {

	using namespace core;

	TEST(CodeProperties, IsPolymorph) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_FALSE(isPolymorph(builder.parseType("bool")));
		EXPECT_FALSE(isPolymorph(builder.parseType("char")));
		EXPECT_FALSE(isPolymorph(builder.parseType("int")));
		EXPECT_FALSE(isPolymorph(builder.parseType("uint")));
		EXPECT_FALSE(isPolymorph(builder.parseType("string")));

		EXPECT_TRUE(isPolymorph(builder.parseType("'a")));

		EXPECT_FALSE(isPolymorph(builder.parseType("(bool)")));
		EXPECT_FALSE(isPolymorph(builder.parseType("(bool,int<4>)")));

		EXPECT_TRUE(isPolymorph(builder.parseType("('a)")));
		EXPECT_TRUE(isPolymorph(builder.parseType("('a,bool)")));

		EXPECT_FALSE(isPolymorph(builder.parseType("int<4>")));
		EXPECT_FALSE(isPolymorph(builder.parseType("ref<int<4>>")));
		EXPECT_TRUE(isPolymorph(builder.parseType("array<'a,'b>")));
		EXPECT_FALSE(isPolymorph(builder.parseType("(int<4>)->bool")));
		EXPECT_FALSE(isPolymorph(builder.parseType("(string, int<4>)->uint<4>")));

		// dumpText(builder.parseType("array<'a,'b>"));
	}

	TEST(CodeProperties, TopLevelTerm) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("('a)")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{var int<4> a = 1; if (a > 0) { a+1; }}")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("int<4>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("ref<int<4>>")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("struct { x : int<4>; }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct B; def struct B { x : int<4>; }; B")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct B; def struct B { x : ref<B>; }; B")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct data; def struct data { x : ptr<data>; }; data")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct A; decl struct B; def struct A { x : ref<B>; }; def struct B { x : ref<A>; }; B")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct A; decl struct B; def struct A { x : ptr<B>; }; def struct B { x : ptr<A>; }; A")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseType("decl struct A; decl struct B; def struct A { x : ref<A>; }; def struct B { y : ref<A>; }; B")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseType("def struct A { lambda retA = () -> A { return *this; } }; A")));

		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ while(true) { continue; } }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ while(true) { break; } }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseStmt("{ for(int<4> i = 1 .. 10 ) { continue; } }")));
		EXPECT_TRUE(getTopLevelNodes(builder.parseExpr("decl f : (int<4>)->int<4>;"
			                         "def f = (x : int<4>)->int<4> {"
			                         "	return (x==0)?1:f(x-1)*x;"
			                         "}; f"), true));

	}

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme
