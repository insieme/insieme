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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace core {
namespace analysis {

	TEST(IRppUtils, IsCTorTest) {
		NodeManager man;
		IRBuilder builder(man);

		auto call = builder.parseExpr(R"(
			def struct Whatever { };
			Whatever::(ref_temp(type_lit(Whatever)))
		)");

		EXPECT_TRUE(isConstructorCall(call));
	}

	TEST(IRppUtils, isIRpp) {
		// checking, if, starting from a nodePtr, there are some object orientated constructs
		NodeManager nm;
		IRBuilder b(nm);

		auto ir1 = b.parseType("def struct A {}; def struct B : [ public A ] {}; B");
		EXPECT_TRUE(isIRpp(ir1));

		auto ir2 = b.parseType("def struct A { ctor (v1: ref<int<4>>) {}}; A");
		EXPECT_TRUE(isIRpp(ir2));

		auto ir3 = b.parseStmt("{ throw 1; }");
		EXPECT_TRUE(isIRpp(ir3));

		auto ir4 = b.parseStmt("{ try {1;} catch (v1 : ref<int<4>>) {2;} }");
		EXPECT_TRUE(isIRpp(ir4));

		auto ir5 = b.parseType("int<4>");
		EXPECT_FALSE(isIRpp(ir5));

	}

	TEST(IsCopyConstructor,Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// some positive tests
		EXPECT_TRUE(isCopyConstructor(builder.parseType("A::(ref<A,t,f,cpp_ref>)")));
		EXPECT_TRUE(isCopyConstructor(builder.parseExpr("lit(\"X\" : A::(ref<A,t,f,cpp_ref>))")));

		// some negative tests
		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,f,f,cpp_ref>)")));
		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,t,t,cpp_ref>)")));
		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,f,t,cpp_ref>)")));

		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,f,f,cpp_rref>)")));
		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,t,t,cpp_rref>)")));
		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,f,t,cpp_rref>)")));
		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,t,f,cpp_rref>)")));

		EXPECT_FALSE(isCopyConstructor(builder.parseType("int<4>")));
		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,t,f,cpp_ref>)->A")));
		EXPECT_FALSE(isCopyConstructor(builder.parseType("A::(ref<A,t,f,cpp_ref>)->int<4>")));

	}

	TEST(IsMoveConstructor,Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// some positive tests
		EXPECT_TRUE(isMoveConstructor(builder.parseType("A::(ref<A,f,f,cpp_rref>)")));
		EXPECT_TRUE(isMoveConstructor(builder.parseExpr("lit(\"X\" : A::(ref<A,f,f,cpp_rref>))")));

		// some negative tests
		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,t,f,cpp_rref>)")));
		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,t,t,cpp_rref>)")));
		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,f,t,cpp_rref>)")));

		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,f,f,cpp_ref>)")));
		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,t,t,cpp_ref>)")));
		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,f,t,cpp_ref>)")));
		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,t,f,cpp_ref>)")));

		EXPECT_FALSE(isMoveConstructor(builder.parseType("int<4>")));
		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,f,f,cpp_rref>)->A")));
		EXPECT_FALSE(isMoveConstructor(builder.parseType("A::(ref<A,f,f,cpp_rref>)->int<4>")));

	}

	TEST(IsCopyAssignmentType,Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// some positive tests
		EXPECT_TRUE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_TRUE(isOfCopyAssignmentType(builder.parseExpr("lit(\"X\" : A::(ref<A,t,f,cpp_ref>)->ref<A,f,f,cpp_ref>)")));

		// some negative tests
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_ref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,t,cpp_ref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,f,t,cpp_ref>)->ref<A,f,f,cpp_ref>")));

		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,t,cpp_rref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,f,t,cpp_rref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_rref>)->ref<A,f,f,cpp_ref>")));

		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,t,f,cpp_ref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,f,t,cpp_ref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,t,t,cpp_ref>")));

		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,f,f,cpp_rref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,t,f,cpp_rref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,f,t,cpp_rref>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,t,t,cpp_rref>")));

		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("int<4>")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->A")));
		EXPECT_FALSE(isOfCopyAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->int<4>")));

	}

	TEST(IsMoveAssignmentType,Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// some positive tests
		EXPECT_TRUE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_TRUE(isOfMoveAssignmentType(builder.parseExpr("lit(\"X\" : A::(ref<A,f,f,cpp_rref>)->ref<A,f,f,cpp_ref>)")));

		// some negative tests
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_rref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,t,t,cpp_rref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,t,cpp_rref>)->ref<A,f,f,cpp_ref>")));

		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_ref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,t,t,cpp_ref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,t,cpp_ref>)->ref<A,f,f,cpp_ref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,t,f,cpp_ref>)->ref<A,f,f,cpp_ref>")));

		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,t,f,cpp_ref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,f,t,cpp_ref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,t,t,cpp_ref>")));

		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,f,f,cpp_rref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,t,f,cpp_rref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,f,t,cpp_rref>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->ref<A,t,t,cpp_rref>")));

		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("int<4>")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->A")));
		EXPECT_FALSE(isOfMoveAssignmentType(builder.parseType("A::(ref<A,f,f,cpp_rref>)->int<4>")));

	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
