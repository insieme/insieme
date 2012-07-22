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

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/parser2/ir_parser.h"
#include "insieme/core/dump/text_dump.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/checks/ir_checks.h"

namespace insieme {
namespace core {
namespace parser {

	TEST(IR_Parser2, GenericTypes) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr test = builder.genericType("test");
		TypePtr A = builder.genericType("A");

		TypePtr testA = builder.genericType("test", toVector(A));
		TypePtr test2A = builder.genericType("test", toVector(testA));

		IntTypeParamPtr pA = builder.concreteIntTypeParam(12);
		IntTypeParamPtr pB = builder.variableIntTypeParam('c');
		IntTypeParamPtr pC = builder.infiniteIntTypeParam();

		TypePtr testB = builder.genericType("test", TypeList(), toVector(pA,pB,pC,pA));
		TypePtr testC = builder.genericType("test", toVector(A,testA), toVector(pA,pC));

		// just some simple generic types
		EXPECT_EQ(test, parse(manager, "test"));

		// some type with parameters
		EXPECT_EQ(test, parse(manager, "test<>"));
		EXPECT_EQ(testA, parse(manager, "test<A>"));
		EXPECT_EQ(test2A, parse(manager, "test<test<A>>"));

		// something that should fail
		EXPECT_FALSE(parse(manager, "hello world"));

		EXPECT_EQ(testB, parse(manager, "test<12,#c,#inf,12>"));
		EXPECT_EQ(testC, parse(manager, "test<A,test<A>,12,#inf>"));
	}

	TEST(IR_Parser2, TupleType) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");
		TypePtr C = builder.genericType("C");

		// just some simple tuple types
		EXPECT_EQ(builder.tupleType(), parse(manager, "()"));
		EXPECT_EQ(builder.tupleType(toVector(A)), parse(manager, "(A)"));
		EXPECT_EQ(builder.tupleType(toVector(A,B)), parse(manager, "(A,B)"));
		EXPECT_EQ(builder.tupleType(toVector(A,B,C)), parse(manager, "(A,B,C)"));

		EXPECT_EQ(builder.tupleType(toVector(A,B,C,B,A,C)), parse(manager, "(A,B,C,B,A,C)"));

	}

	TEST(IR_Parser2, FunctionType) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");
		TypePtr C = builder.genericType("C");

		// just some simple function types
		EXPECT_EQ(builder.functionType(TypeList(), A), parse(manager, "()->A"));
		EXPECT_EQ(builder.functionType(toVector(A), A), parse(manager, "(A)->A"));
		EXPECT_EQ(builder.functionType(toVector(A,B), B), parse(manager, "(A,B)->B"));
		EXPECT_EQ(builder.functionType(toVector(A,B,A), B), parse(manager, "(A,B,A)->B"));
		EXPECT_EQ(builder.functionType(toVector(A,B,C,A), C), parse(manager, "(A,B,C,A)->C"));

		// also some non-plain types
		EXPECT_EQ(builder.functionType(toVector(B,C,A), A, false), parse(manager, "(B, C, A) => A"));

		// a function taking a function as an argument + integration of tuples
		TypePtr tuple = builder.tupleType(toVector(A,C));
		TypePtr funA = builder.functionType(toVector(B,C,A), A, false);
		TypePtr funB = builder.functionType(toVector(C,tuple, A), B, false);

		EXPECT_EQ(builder.functionType(toVector(funA, tuple), funB), parse(manager,"((B,C,A)=>A,(A,C))->(C,(A,C),A)=>B"));

	}

	TEST(IR_Parser2, TypeVariables) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.typeVariable("a");
		TypePtr B = builder.typeVariable("b");

		EXPECT_EQ(A, parse(manager, "'a"));
		EXPECT_EQ(B, parse(manager, "'b"));

		EXPECT_EQ(builder.genericType("pair", toVector(A,B)), parse(manager, "pair<'a,'b>"));
	}

//	TEST(IR_Parser2, TypeDefinition) {
//
//		NodeManager manager;
//		IRBuilder builder(manager);
//
//		// test a simple type definition
//		TypePtr type = parse(manager,
//				"type pair<'a,'b> = struct { first : 'a , second : 'b }"
//				"pair<int<4>,float<2>>"
//		);
//
//		EXPECT_TRUE(type);
//	}


	TEST(IR_Parser2, IfStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		ExpressionPtr cond_true = builder.boolLit(true);
		ExpressionPtr cond_false = builder.boolLit(false);

		StatementPtr a = builder.intLit(1);
		StatementPtr b = builder.intLit(2);


		EXPECT_EQ(
				builder.ifStmt(cond_true, a),
				parse(manager, "if(true) 1;")
		);

		// tangling-else problem

		EXPECT_EQ(
				builder.ifStmt(cond_true, builder.ifStmt(cond_false, a, b)),
				parse(manager, "if(true) if(false) 1; else 2;")
		);

		EXPECT_EQ(
				builder.ifStmt(cond_true, builder.ifStmt(cond_false, builder.compoundStmt(a,b), b)),
				parse(manager, "if(true) if(false) { 1; 2; } else 2;")
		);

	}

	TEST(IR_Parser2, WhileStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		ExpressionPtr cond_true = builder.boolLit(true);
		ExpressionPtr cond_false = builder.boolLit(false);

		StatementPtr a = builder.intLit(1);
		StatementPtr b = builder.intLit(2);


		EXPECT_EQ(
				builder.whileStmt(cond_true, a),
				parse(manager, "while(true) 1;")
		);

		EXPECT_EQ(
				builder.whileStmt(cond_true, builder.compoundStmt(a,b,a)),
				parse(manager, "while(true) {1;2;1;}")
		);
	}

	TEST(IR_Parser2, DeclarationStmt) {
		NodeManager manager;
		IRBuilder builder(manager);

		EXPECT_TRUE(parse(manager, "{ decl int a = 10; }"));

	}

	TEST(IR_Parser2, VariableScopes) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodePtr res = parse(manager,
				"{"
				"	decl int<4> a = 15;"
				"	a;"
				"	{"
				"		decl int<4> b = 12;"
				"		a + b;"
				"		decl int<4> a = 14;"
				"		a + b;"
				"	}"
				"}"
		);

		ASSERT_TRUE(res);

		// use semantic checks to check IR structure
		auto msg = checks::check(res);
		EXPECT_TRUE(msg.empty()) << msg;

//		std::cout << core::printer::PrettyPrinter(res);

	}

	TEST(IR_Parser2, Arithmetic) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto one = builder.intLit(1);
		auto two = builder.intLit(2);
		auto tre = builder.intLit(3);
		auto half = builder.doubleLit(0.5);

		EXPECT_EQ(one, parse_expr(manager, "1"));

		EXPECT_EQ(
				builder.add(one,one),
				parse(manager, "1+1")
		);

		EXPECT_EQ(
				builder.mul(builder.add(one, two), tre),
				parse(manager, "1+2*3")
		);


		// TODO: fix this one
//		EXPECT_EQ(
//				builder.mul(one,one),
//				parse(manager, "2*0.5")
//		);
	}

	TEST(IR_Parser2, ForStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		ExpressionPtr cond_true = builder.boolLit(true);
		ExpressionPtr cond_false = builder.boolLit(false);

		StatementPtr a = builder.intLit(1);
		StatementPtr b = builder.intLit(2);


		auto res = parse(manager,
				"{"
				"	for(int<4> i = 0 .. 10) {"
				"		i;"
				"	}"
				"	for(int<4> j = 5 .. 10 : 2) {"
				"		j;"
				"	}"
				"}");

		ASSERT_TRUE(res);

		// use semantic checks to check IR structure
		auto msg = checks::check(res);
		EXPECT_TRUE(msg.empty()) << msg;
	}

} // end namespace parser2
} // end namespace core
} // end namespace insieme
