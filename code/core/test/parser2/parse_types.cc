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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/parser2/types.h"

namespace insieme {
namespace core {
namespace parser2 {

	TEST(IR_Parser2, GenericTypes) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr test = builder.genericType("test");
		TypePtr A = builder.genericType("A");

		TypePtr testA = builder.genericType("test", toVector(A));
		TypePtr test2A = builder.genericType("test", toVector(testA));

		// just some simple generic types
		EXPECT_EQ(test, parseType(manager, "test"));

		// some type with parameters
		EXPECT_EQ(test, parseType(manager, "test<>"));
		EXPECT_EQ(testA, parseType(manager, "test<A>"));
		EXPECT_EQ(test2A, parseType(manager, "test<test<A>>"));

		// something that should fail
		EXPECT_FALSE(parseType(manager, "hello world"));
	}

	TEST(IR_Parser2, TupleType) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");
		TypePtr C = builder.genericType("C");

		// just some simple tuple types
		EXPECT_EQ(builder.tupleType(), parseType(manager, "()"));
		EXPECT_EQ(builder.tupleType(toVector(A)), parseType(manager, "(A)"));
		EXPECT_EQ(builder.tupleType(toVector(A,B)), parseType(manager, "(A,B)"));
		EXPECT_EQ(builder.tupleType(toVector(A,B,C)), parseType(manager, "(A,B,C)"));

		EXPECT_EQ(builder.tupleType(toVector(A,B,C,B,A,C)), parseType(manager, "(A,B,C,B,A,C)"));

	}

	TEST(IR_Parser2, FunctionType) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");
		TypePtr C = builder.genericType("C");

		// just some simple function types
		EXPECT_EQ(builder.functionType(TypeList(), A), parseType(manager, "()->A"));
		EXPECT_EQ(builder.functionType(toVector(A), A), parseType(manager, "(A)->A"));
		EXPECT_EQ(builder.functionType(toVector(A,B), B), parseType(manager, "(A,B)->B"));
		EXPECT_EQ(builder.functionType(toVector(A,B,A), B), parseType(manager, "(A,B,A)->B"));
		EXPECT_EQ(builder.functionType(toVector(A,B,C,A), C), parseType(manager, "(A,B,C,A)->C"));

		// also some non-plain types
		EXPECT_EQ(builder.functionType(toVector(B,C,A), A, false), parseType(manager, "(B, C, A) => A"));

		// a function taking a function as an argument + integration of tuples
		TypePtr tuple = builder.tupleType(toVector(A,C));
		TypePtr funA = builder.functionType(toVector(B,C,A), A, false);
		TypePtr funB = builder.functionType(toVector(C,tuple, A), B, false);

		EXPECT_EQ(builder.functionType(toVector(funA, tuple), funB), parseType(manager,"((B,C,A)=>A,(A,C))->(C,(A,C),A)=>B"));

	}

	TEST(IR_Parser2, TypeVariables) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.typeVariable("a");
		TypePtr B = builder.typeVariable("b");

		EXPECT_EQ(A, parseType(manager, "'a"));
		EXPECT_EQ(B, parseType(manager, "'b"));

		EXPECT_EQ(builder.genericType("pair", toVector(A,B)), parseType(manager, "pair<'a,'b>"));
	}

//	TEST(IR_Parser2, TypeDefinition) {
//
//		NodeManager manager;
//		IRBuilder builder(manager);
//
//		// test a simple type definition
//		TypePtr type = parseType(manager,
//				"type pair<'a,'b> = struct { first : 'a , second : 'b }"
//				"pair<int<4>,float<2>>"
//		);
//
//		EXPECT_TRUE(type);
//	}

} // end namespace parser2
} // end namespace core
} // end namespace insieme
