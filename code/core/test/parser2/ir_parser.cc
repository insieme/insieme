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

#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/test/test_utils.h"

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

		// test combinations of type parameters and int-type parameters
		EXPECT_EQ(testB, parse(manager, "test<12,#c,#inf,12>"));
		EXPECT_EQ(testC, parse(manager, "test<A,test<A>,12,#inf>"));

		// something with parents
		EXPECT_EQ("test:[A,virtual B]<A,B>", toString(*parse(manager, "test : A, virtual B <A,B>")));
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

	TEST(IR_Parser2, FunctionTypeConstructor) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");
		TypePtr C = builder.genericType("C");

		TypePtr O = builder.refType(C);

		EXPECT_EQ(builder.functionType(toVector(O), O, FK_CONSTRUCTOR), parse(manager, "C::()"));
		EXPECT_EQ(builder.functionType(toVector(O, A), O, FK_CONSTRUCTOR), parse(manager, "C::(A)"));
		EXPECT_EQ(builder.functionType(toVector(O, A, B), O, FK_CONSTRUCTOR), parse(manager, "C::(A, B)"));

	}

	TEST(IR_Parser2, FunctionTypeDestructor) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr C = builder.genericType("C");
		TypePtr O = builder.refType(C);

		EXPECT_EQ(builder.functionType(toVector(O), O, FK_DESTRUCTOR), parse(manager, "~C::()"));

	}

	TEST(IR_Parser2, MemberFunctionType) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr A = builder.genericType("A");
		TypePtr B = builder.genericType("B");
		TypePtr C = builder.genericType("C");
		TypePtr R = builder.genericType("R");

		TypePtr O = builder.refType(C);

		EXPECT_EQ(builder.functionType(toVector(O), R, FK_MEMBER_FUNCTION), parse(manager, "C::()->R"));
		EXPECT_EQ(builder.functionType(toVector(O, A), R, FK_MEMBER_FUNCTION), parse(manager, "C::(A)->R"));
		EXPECT_EQ(builder.functionType(toVector(O, A, B), R, FK_MEMBER_FUNCTION), parse(manager, "C::(A, B)->R"));

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

	TEST(IR_Parser2, StructAndUnionTypes) {

		NodeManager manager;
		IRBuilder builder(manager);

		// test some struct types
		EXPECT_EQ("struct<>", toString(*parse(manager, "struct { }")));
		EXPECT_EQ("struct<a:A,b:B,c:int<4>>", toString(*parse(manager, "struct { A a; B b; int<4> c; }")));

		EXPECT_EQ("union<>", toString(*parse(manager, "union { }")));
		EXPECT_EQ("union<a:A,b:B,c:int<4>>", toString(*parse(manager, "union { A a; B b; int<4> c; }")));

		EXPECT_EQ("tuple(10,\"string\")", toString(*parse(manager, "(10, \"string\")")));
		EXPECT_EQ(core::NT_TupleExpr, parse(manager, "(10, \"string\")")->getNodeType());
		EXPECT_EQ(core::NT_TupleType, parse(manager, "(10, \"string\")").as<ExpressionPtr>()->getType()->getNodeType());

	}

	TEST(IR_Parser2, StructInheritance) {

		NodeManager manager;
		IRBuilder builder(manager);

		// test some structs with inheritance
		EXPECT_EQ("struct<a:A,b:B,c:int<4>>", toString(*parse(manager, "struct { A a; B b; int<4> c; }")));
		EXPECT_EQ("struct : [A] <a:A,b:B,c:int<4>>", toString(*parse(manager, "struct : A { A a; B b; int<4> c; }")));
		EXPECT_EQ("struct : [A, B] <a:A,b:B,c:int<4>>", toString(*parse(manager, "struct : A, B { A a; B b; int<4> c; }")));
		EXPECT_EQ("struct : [A, virtual B] <a:A,b:B,c:int<4>>", toString(*parse(manager, "struct : A, virtual B { A a; B b; int<4> c; }")));
		EXPECT_EQ("struct : [virtual A, virtual B] <a:A,b:B,c:int<4>>", toString(*parse(manager, "struct : virtual A, virtual B { A a; B b; int<4> c; }")));

		// something that should not work
		EXPECT_FALSE(parse(manager, "struct : { A a; }"));
	}


	TEST(IR_Parser2, ArrayVectorRefAndChannelTypes) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr t = manager.getLangBasic().getInt4();
		IntTypeParamPtr two = builder.concreteIntTypeParam(2);
		IntTypeParamPtr zero = builder.concreteIntTypeParam(0);

		EXPECT_EQ(builder.arrayType(t, two), 		parse(manager, "array<int<4>,2>"));
		EXPECT_EQ(builder.vectorType(t, two), 		parse(manager, "vector<int<4>,2>"));
		EXPECT_EQ(builder.refType(t), 				parse(manager, "ref<int<4>>"));
		EXPECT_EQ(builder.channelType(t,zero), 		parse(manager, "channel<int<4>,0>"));

	}

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

	TEST(IR_Parser2, SwitchStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodePtr node;

		// without default case
		node = builder.parse(
				"switch(3) {"
				"	case 1: 3;"
				"	case 2: 2;"
				"	case 3: 1;"
				"}"
		);

		ASSERT_TRUE(node);
		EXPECT_EQ("switch(3) [ case 1: {3;} | case 2: {2;} | case 3: {1;} | default: {} ]", toString(*node));

		// with default case
		node = builder.parse(
				"switch(3) {"
				"	case 1: 3;"
				"	case 2: 2;"
				"	case 3: 1;"
				"	default: 0;"
				"}"
		);

		ASSERT_TRUE(node);
		EXPECT_EQ("switch(3) [ case 1: {3;} | case 2: {2;} | case 3: {1;} | default: {0;} ]", toString(*node));

		// default only
		node = builder.parse(
				"switch(3) {"
				"	default: 0;"
				"}"
		);

		ASSERT_TRUE(node);
		EXPECT_EQ("switch(3) [  default: {0;} ]", toString(*node));

		// with nothing
		node = builder.parse(
				"switch(3) {"
				"}"
		);

		ASSERT_TRUE(node);
		EXPECT_EQ("switch(3) [  default: {} ]", toString(*node));

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

		EXPECT_TRUE(parse(manager, "{ int a = 10; }"));

	}

	TEST(IR_Parser2, VariableScopes) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodePtr res = parse(manager,
				"{"
				"	int<4> a = 15;"
				"	a;"
				"	{"
				"		int<4> b = 12;"
				"		a + b;"
				"		int<4> a = 14;"
				"		a + b;"
				"       auto b = 15;"
				"		a + b;"
				"	}"
				"}"
		);

		ASSERT_TRUE(res);

		EXPECT_EQ(
			"{int<4> v1 = 15; v1; {int<4> v2 = 12; int.add(v1, v2); int<4> v3 = 14; int.add(v3, v2); int<4> v4 = 15; int.add(v3, v4);};}",
			toString(*res)
		);

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
				builder.add(one, builder.mul(two, tre)),
				parse(manager, "1+2*3")
		);

		// check associativity
		EXPECT_EQ(
				builder.add(builder.add(one, two), tre),
				parse(manager, "1+2+3")
		);

		// known bug: same precedence, different operator
//		EXPECT_EQ(
//				builder.sub(builder.add(one, two), tre),
//				parse(manager, "1+2-3")
//		);


		// TODO: fix this one
//		EXPECT_EQ(
//				builder.mul(one,one),
//				parse(manager, "2*0.5")
//		);
		
		// bitwise
		EXPECT_EQ(
				builder.bitwiseAnd(one, two),
				parse(manager, "1 & 2")
		);

		EXPECT_EQ(
				builder.bitwiseOr(one, two),
				parse(manager, "1 | 2")
		);

		EXPECT_EQ(
				builder.bitwiseXor(one, two),
				parse(manager, "1 ^ 2")
		);

		// bitwise precedence
		EXPECT_EQ(
				builder.bitwiseOr(builder.bitwiseAnd(one, tre), builder.bitwiseAnd(one, two)),
				parse(manager, "1 & 3 | 1 & 2")
		);
	}

	TEST(IR_Parser2, IfThenElse) {
		NodeManager manager;
		IRBuilder builder(manager);
		const auto& basic = manager.getLangBasic();

		auto res = analysis::normalize(builder.parse(
				"true?1:2"
		)).as<ExpressionPtr>();

		ASSERT_TRUE(res);

		EXPECT_EQ(
				analysis::normalize(builder.ite(basic.getTrue(), builder.wrapLazy(builder.intLit(1)), builder.wrapLazy(builder.intLit(2)))),
				res
		);
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

	TEST(IR_Parser2, TypeDecl) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto res = parse(manager,
				"{"
				"	int<4> x = 12;"
				"	let int = int<4>;"
				"	int y = 14;"
				"}");

		ASSERT_TRUE(res);

		// use semantic checks to check IR structure
		auto msg = checks::check(res);
		EXPECT_TRUE(msg.empty()) << msg;
	}

	TEST(IR_Parser2, Lambdas) {
		NodeManager manager;

		// parse two lambdas
//		EXPECT_EQ("rec v1.{v1=fun() {}}", toString(*parse(manager, "()->bool { }")));
//		EXPECT_EQ("rec v4.{v4=fun(int<4> v2, int<4> v3) {}}", toString(*parse(manager, "(int<4> a, int<4> b)->int<4> { }")));

		// add call
		EXPECT_EQ("AP({rec v3.{v3=fun(int<4> v1, int<4> v2) {return v1;}}(12, 14);})",
				toString(parse(manager, "{ (int<4> a, int<4> b)->int<4> { return a; }(12,14); }"))
		);

		// add call to empty function
		EXPECT_EQ("AP({rec v4.{v4=fun() {return 3;}}();})",
				toString(parse(manager, "{ ()->int<4> { return 3; } (); }", true))
		);

	}

	TEST(IR_Parser2, Constructors) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto ctor = builder.normalize(parse(manager, "C::(int<4> a, bool b) { this; }").as<LambdaExprPtr>());
		ASSERT_TRUE(ctor);

		auto ctorType = ctor->getLambda()->getType();
		EXPECT_TRUE(ctorType->isConstructor());
		EXPECT_EQ(builder.genericType("C"), ctorType->getObjectType());

		EXPECT_EQ("(C::(int<4>,bool))", toString(*ctorType));
		EXPECT_EQ("rec v0.{v0=fun(ref<C> v1, int<4> v2, bool v3) {v1;}}", toString(*ctor));
		EXPECT_EQ("ctor C v1 :: (int<4> v2, bool v3) {\n    v1;\n}", toString(core::printer::PrettyPrinter(ctor, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));

		// a default constructor
		ctor = builder.normalize(parse(manager, "C::() { this; }").as<LambdaExprPtr>());
		ASSERT_TRUE(ctor);

		ctorType = ctor->getLambda()->getType();
		EXPECT_TRUE(ctorType->isConstructor());
		EXPECT_EQ(1u, ctorType->getParameterTypes().size());
		EXPECT_EQ(builder.genericType("C"), ctorType->getObjectType());

		EXPECT_EQ("(C::())", toString(*ctorType));
		EXPECT_EQ("rec v0.{v0=fun(ref<C> v1) {v1;}}", toString(*ctor));
		EXPECT_EQ("ctor C v1 :: () {\n    v1;\n}", toString(core::printer::PrettyPrinter(ctor, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));

	}

	TEST(IR_Parser2, Destructor) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto dtor = builder.normalize(parse(manager, "~C::() { this; }").as<LambdaExprPtr>());
		ASSERT_TRUE(dtor);

		auto dtorType = dtor->getLambda()->getType();
		EXPECT_TRUE(dtorType->isDestructor());
		EXPECT_EQ(builder.genericType("C"), dtorType->getObjectType());

		EXPECT_EQ("(~C::())", toString(*dtorType));
		EXPECT_EQ("rec v0.{v0=fun(ref<C> v1) {v1;}}", toString(*dtor));
		EXPECT_EQ("dtor ~C v1 :: () {\n    v1;\n}", toString(core::printer::PrettyPrinter(dtor, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));

	}

	TEST(IR_Parser2, MemberFunction) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto fun = builder.normalize(parse(manager, "C::(int<4> a, bool b)->unit { this; }").as<LambdaExprPtr>());
		ASSERT_TRUE(fun);

		auto funType = fun->getLambda()->getType();
		EXPECT_TRUE(funType->isMemberFunction());
		EXPECT_EQ(builder.genericType("C"), funType->getObjectType());

		EXPECT_EQ("(C::(int<4>,bool)->unit)", toString(*funType));
		EXPECT_EQ("rec v0.{v0=fun(ref<C> v1, int<4> v2, bool v3) {v1;}}", toString(*fun));
		EXPECT_EQ("mfun C v1 :: (int<4> v2, bool v3) -> unit {\n    v1;\n}", toString(core::printer::PrettyPrinter(fun, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));

		// a default constructor
		fun = builder.normalize(parse(manager, "C::()->unit { this; }").as<LambdaExprPtr>());
		ASSERT_TRUE(fun);

		funType = fun->getLambda()->getType();
		EXPECT_TRUE(funType->isMemberFunction());
		EXPECT_EQ(1u, funType->getParameterTypes().size());
		EXPECT_EQ(builder.genericType("C"), funType->getObjectType());

		EXPECT_EQ("(C::()->unit)", toString(*funType));
		EXPECT_EQ("rec v0.{v0=fun(ref<C> v1) {v1;}}", toString(*fun));
		EXPECT_EQ("mfun C v1 :: () -> unit {\n    v1;\n}", toString(core::printer::PrettyPrinter(fun, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS)));

	}

	TEST(IR_Parser2, ConstructorLetBinding) {

		NodeManager manager;
		IRBuilder builder(manager);

		// create constructor solely
		auto fun = builder.normalize(builder.parseExpr(
				"let C = O::(int<4> a) { } in C"
		)).as<LambdaExprPtr>();

		ASSERT_TRUE(fun);

		FunctionTypePtr funType = fun->getLambda()->getType();
		EXPECT_EQ(FK_CONSTRUCTOR, funType->getKind());
		EXPECT_EQ("(O::(int<4>))", toString(*funType));

		// create constructor call
		auto call = builder.normalize(builder.parseExpr(
				"let Ctor = O::(int<4> a) {} in Ctor(var(undefined(lit(O))), 3)"
		)).as<CallExprPtr>();

		ASSERT_TRUE(call);

		EXPECT_EQ(fun, call->getFunctionExpr());
		EXPECT_EQ(builder.parseType("ref<O>"), call->getType());
	}


	TEST(IR_Parser2, DestructorLetBinding) {

		NodeManager manager;
		IRBuilder builder(manager);

		// create constructor solely
		auto fun = builder.normalize(builder.parseExpr(
				"let Dtor = ~O::() { } in Dtor"
		)).as<LambdaExprPtr>();

		ASSERT_TRUE(fun);

		FunctionTypePtr funType = fun->getLambda()->getType();
		EXPECT_EQ(FK_DESTRUCTOR, funType->getKind());
		EXPECT_EQ("(~O::())", toString(*funType));

		// create constructor call
		auto call = builder.normalize(builder.parseExpr(
				"let Dtor = ~O::() {} in Dtor(var(undefined(lit(O))))"
		)).as<CallExprPtr>();

		ASSERT_TRUE(call);

		EXPECT_EQ(fun, call->getFunctionExpr());
		EXPECT_EQ(builder.parseType("ref<O>"), call->getType());
	}

	TEST(IR_Parser2, MemberFunctionLetBinding) {

		NodeManager manager;
		IRBuilder builder(manager);

		// create constructor solely
		auto fun = builder.normalize(builder.parseExpr(
				"let f = O::(int<4> a)->int<4> { return a; } in f"
		)).as<LambdaExprPtr>();

		ASSERT_TRUE(fun);

		FunctionTypePtr funType = fun->getLambda()->getType();
		EXPECT_EQ(FK_MEMBER_FUNCTION, funType->getKind());
		EXPECT_EQ("(O::(int<4>)->int<4>)", toString(*funType));

		// create constructor call
		auto call = builder.normalize(builder.parseExpr(
				"let f = O::(int<4> a)->int<4> { return a; } in f(var(undefined(lit(O))), 3)"
		)).as<CallExprPtr>();

		ASSERT_TRUE(call);

		EXPECT_EQ(fun, call->getFunctionExpr());
		EXPECT_EQ(manager.getLangBasic().getInt4(), call->getType());
	}

	TEST(IR_Parser2, MemberFunctionCall) {

		NodeManager manager;
		IRBuilder builder(manager);

		// create constructor solely
		auto fun = builder.normalize(builder.parseExpr(
				"let f = O::(int<4> a)->int<4> { return a; } in f"
		)).as<LambdaExprPtr>();

		ASSERT_TRUE(fun);

		FunctionTypePtr funType = fun->getLambda()->getType();
		EXPECT_EQ(FK_MEMBER_FUNCTION, funType->getKind());
		EXPECT_EQ("(O::(int<4>)->int<4>)", toString(*funType));

		// create constructor call
		auto call = builder.normalize(builder.parseExpr(
				"let x = var(undefined(lit(O))) in "
				"let f = O::(int<4> a)->int<4> { return a; } in "
				"x.f(3)"
		)).as<CallExprPtr>();

		ASSERT_TRUE(call);

		EXPECT_EQ(fun, call->getFunctionExpr());
		EXPECT_EQ(manager.getLangBasic().getInt4(), call->getType());
		EXPECT_EQ("[AP(ref.var(undefined(O))),AP(3)]", toString(call->getArguments()));

		// create construct using -> instead of .
		auto call2 = builder.normalize(builder.parseExpr(
				"let x = var(undefined(lit(O))) in "
				"let f = O::(int<4> a)->int<4> { return a; } in "
				"x->f(3)"
		)).as<CallExprPtr>();
		EXPECT_EQ(call, call2);
	}

	TEST(IR_Parser2, LargeCode) {

		// this is mostly a speed test
		NodeManager manager;

		auto body = std::string(
				"	// some type definitions\n"
				"	let matrix = vector<vector<int<4>,4>,4>;\n"
				"	let int = int<4>;\n"
				"	\n"
				"	int a = 12;\n"
				"	int b = 14;\n"
				"	\n"
				"	// can also be used with auto\n"
				"	auto c = 115+b;\n"
				"	\n"
				"	// define a function\n"
				"	let min = (int a, int b)->int {\n"
				"		if (a < b) return a;\n"
				"		return b;\n"
				"	};\n"
				"	\n"
				"	// use it\n"
				"	min(a,b);\n"
				"	\n"
				"	for(int i=0..4) { }\n"
				"	\n"
				"	matrix m;\n"
				"	\n"
				"	m[1][2];\n"
				"	\n"
				"	let mmin = (matrix m)->int {\n"
				"		int res = 0;\n"
				"		for(int i=0..4) {\n"
				"			for(int j=0..4) {\n"
				"				if (res < m[i][j]) {\n"
				"					\n"
				"				}\n"
				"			}\n"
				"		}\n"
				"		return res;\n"
				"	};\n"
				"	\n"
		);

		// try a small example
		EXPECT_TRUE(
			parse(manager,"{\n" + body + "}")
		);

		EXPECT_TRUE(
			parse(manager,"{\n" + body + body + body + body + body + "}")
		);


		// also a corrupted case
		EXPECT_FALSE(
			parse(manager,"{\n" + body + body + body + "some shit" + body + body + "}")
		);

	}

	TEST(IR_Parser2, LetBinding) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto& basic = manager.getLangBasic();

		// --- types ---

		// test with types first
		EXPECT_EQ(basic.getBool(), parse_type(manager, "let a = bool in a"));
		EXPECT_EQ(basic.getBool(), parse_type(manager, "let a,b = bool,char in a"));
		EXPECT_EQ(basic.getChar(), parse_type(manager, "let a,b = bool,char in b"));

		// test simple recursive type
		EXPECT_EQ("AP(rec 'a.{'a=(int,ref<'a>)})", toString(parse_type(manager, "let a = (int,ref<a>) in a")));

		// test mutual recursive types
		EXPECT_EQ("AP(rec 'a.{'a=(int,ref<'b>),'b=(char,ref<'a>)})", toString(parse_type(manager, "let a,b = (int,ref<b>),(char,ref<a>) in a")));
		EXPECT_EQ("AP(rec 'b.{'a=(int,ref<'b>),'b=(char,ref<'a>)})", toString(parse_type(manager, "let a,b = (int,ref<b>),(char,ref<a>) in b")));

		// --- values ---

		// test simple value
		EXPECT_EQ(builder.intLit(12), parse_expr(manager, "let a = 12 in a"));

		// test multiple values
		EXPECT_EQ("AP(int.add(1, 2))",toString(parse_expr(manager, "let a,b = 1,2 in a + b")));


		// --- functions ---

		// test non-recursive function
		manager.setNextFreshID(0);
		EXPECT_EQ("AP(rec v2.{v2=fun(bool v1) {return bool.not(v1);}}(false))",
				toString(parse_expr(manager, "let f = (bool a)->bool { return !a; } in f(false)"))
		);

		// test recursive function
		manager.setNextFreshID(0);
		EXPECT_EQ("AP(rec v0.{v0=fun(int<4> v1) {if(int.eq(v1, 0)) {return 0;} else {}; return int.mul(v0(int.sub(v1, 1)), v1);}}(3))",
			toString(parse_expr(manager, "let f = (int<4> a)->int<4> { if (a == 0) return 0; return f(a-1)*a; } in f(3)"))
		);

		// test recursive function with multiple arguments
		manager.setNextFreshID(0);
		EXPECT_EQ("AP(rec v0.{v0=fun(int<4> v2, int<4> v3) {if(int.eq(v2, 0)) {return v3;} else {}; return v0(int.sub(v2, 1), int.add(v3, v2));}}(3, 0))",
			toString(parse_expr(manager, "let f = (int<4> a, int<4> sum)->int<4> { if (a == 0) return sum; return f(a-1, sum+a); } in f(3,0)"))
		);

		// test mutal recursive function
		manager.setNextFreshID(0);
		EXPECT_EQ("AP(rec v1.{v0=fun(int<4> v4) {if(int.eq(v4, 0)) {return true;} else {}; return v1(int.sub(v4, 1));}, "
				             "v1=fun(int<4> v6) {if(int.eq(v6, 0)) {return false;} else {}; return v0(int.sub(v6, 1));}})",
			toString(parse_expr(manager, ""
					"let even,odd = "
					"	(int<4> a)->bool { if (a == 0) return true; return odd(a-1); }, "
					"	(int<4> a)->bool { if (a == 0) return false; return even(a-1); }"
					"in odd"
				)
			)
		);

		// some encountered buggy cases:
		manager.setNextFreshID(0);
		EXPECT_EQ("AP(vector<vector<int<4>,4>,4>)", toString(parse_type(manager, "let t = vector<vector<int<4>,4>,4> in t")));

	}

	TEST(IR_Parser2, Literals) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr fun = parse_type(manager, "(ref<'a>)->'a");
		ASSERT_TRUE(fun);

		// test whether a literal can be parsed successfully
		EXPECT_EQ(builder.literal(fun, "test"), parse_expr(manager, "lit(\"test\":(ref<'a>)->'a)"));
		EXPECT_EQ(builder.literal(fun, "test.more"), parse_expr(manager, "lit(\"test.more\":(ref<'a>)->'a)"));

		// test type literal
		TypePtr type = builder.getLangBasic().getInt4();
		EXPECT_EQ(builder.getTypeLiteral(type), parse_expr(manager, "lit(int<4>)"));
	}

	TEST(IR_Parser2, PreDefinedSymbols) {
		NodeManager manager;
		IRBuilder builder(manager);

		ExpressionPtr one = builder.intLit(1);
		ExpressionPtr two = builder.intLit(2);
		ExpressionPtr x = builder.variable(builder.getLangBasic().getInt4());

		std::map<string, NodePtr> symbols;
		symbols["one"] = one;
		symbols["two"] = two;
		symbols["x"] = x;

		// test whether symbols are found
		EXPECT_EQ(builder.add(one, builder.mul(two, x)), parse(manager, "one + two * x", false, symbols));

		// test builder support
		EXPECT_EQ(builder.add(one, builder.mul(two, x)), builder.parse("one + two * x", symbols));
	}

	TEST(IR_Parser2, LangBasicSymbols) {
		NodeManager manager;
		auto& basic = manager.getLangBasic();

		// test some pre-defined expressions
		EXPECT_EQ(basic.getSignedIntAdd(), parse_expr(manager, "int.add"));
		EXPECT_EQ(basic.getRefNarrow(), parse_expr(manager, "ref.narrow"));
	}

	TEST(IR_Parser2, UnaryMinus) {
		NodeManager manager;
		IRBuilder builder(manager);

		EXPECT_EQ(builder.intLit(-5), parse_expr(manager, "-5"));
		EXPECT_EQ(builder.intLit(2), parse_expr(manager, "-(-2)"));

		EXPECT_EQ("AP(int.add(2, int.sub(0, int.add(1, 2))))", toString(parse_expr(manager, "2 + -(1 + 2)")));
	}

	TEST(IR_Parser2, Bind) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test a direct call
		EXPECT_EQ("AP({int<4> v0 = 7; bind(v1){rec v0.{v0=fun(int<4> v1, int<4> v2) {return int.add(v1, v2);}}(v1, v0)}(5);})",
			toString(builder.normalize(builder.parse(
				"{"
				"	let int = int<4>;"
				"	let sum = (int a, int b)->int { return a + b; };"
				"	auto x = 7;"
				" 	let pX = (int a)=>sum(a,x);"
				"	pX(5);"
				"}"
		))));

		// test returning a value
		EXPECT_EQ("AP({bind(v0){rec v0.{v0=fun('a v1) {return v1;}}(5)}(2);})",
			toString(builder.normalize(builder.parse(
				"{"
				" 	let pX = (int<4> a)=>5;"
				"	pX(2);"
				"}"
		))));

		// test a statement
		EXPECT_EQ("AP({ref<int<4>> v0 = ref.var(0); bind(v1){rec v0.{v0=fun(ref<int<4>> v1, int<4> v2) {ref.assign(v1, int.add(ref.deref(v1), v2));}}(v0, v1)}(5);})",
			toString(builder.normalize(builder.parse(
				"{"
				" 	ref<int<4>> x = var(0);"
				"	let p = (int<4> a)=>{"
				"		x = x + a;"
				"	};"
				"	p(5);"
				"}"
		))));

	}

	TEST(IR_Parser2, SizeOfBug) {
		NodeManager manager;
		IRBuilder builder(manager);

		NodePtr res = builder.parseExpr(
				"8u / sizeof(lit(uint<4>))"
		);

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::analysis::isCallOf(res, manager.getLangBasic().getUnsignedIntDiv()));

	}

	TEST(IR_Parser2, ParseAddresses) {
		NodeManager manager;
		IRBuilder builder(manager);

		// a simple example

		vector<NodeAddress> list = builder.parseAddresses(
				"{"
				"	1 + $2$ * 3;"
				"}"
		);

		ASSERT_EQ(1u, list.size());
		EXPECT_EQ("0-0-3-2", toString(list[0]));
		EXPECT_EQ("2", toString(*list[0].getAddressedNode()));
		EXPECT_EQ(core::NT_Literal, list[0]->getNodeType());


		// test multiple marks
		list = builder.parseAddresses(
				"{"
				"	1 + $$2$ * $3$$;"
				"}"
		);

		ASSERT_EQ(3u, list.size());

		EXPECT_EQ("0-0-3", toString(list[0]));
		EXPECT_EQ("int.mul(2, 3)", toString(*list[0].getAddressedNode()));
		EXPECT_EQ(core::NT_CallExpr, list[0]->getNodeType());

		EXPECT_EQ("0-0-3-2", toString(list[1]));
		EXPECT_EQ("2", toString(*list[1].getAddressedNode()));
		EXPECT_EQ(core::NT_Literal, list[1]->getNodeType());

		EXPECT_EQ("0-0-3-3", toString(list[2]));
		EXPECT_EQ("3", toString(*list[2].getAddressedNode()));
		EXPECT_EQ(core::NT_Literal, list[2]->getNodeType());


		// test statements
		list = builder.parseAddresses(
				"{"
				"	$1 + 2 * 3;$"
				"}"
		);

		ASSERT_EQ(1u, list.size());
		EXPECT_EQ("0-0", toString(list[0]));
		EXPECT_EQ("int.add(1, int.mul(2, 3))", toString(*list[0].getAddressedNode()));
		EXPECT_EQ(core::NT_CallExpr, list[0]->getNodeType());

	}

	TEST(IR_Parser2, ParseAddressSharing) {
		NodeManager manager;
		IRBuilder builder(manager);

		// a simple example

		vector<NodeAddress> list = builder.parseAddresses(
				"{"
				"	ref<int<4>> x = 2;"
				"	$x = 3$;"
				"	$x = $x$ + 2$;"
				"}"
		);

		// there should only be two addresses
		ASSERT_EQ(3u, list.size());

		EXPECT_EQ("0-1", toString(list[0]));
		EXPECT_EQ("ref.assign(v1, 3)", toString(*list[0].getAddressedNode()));
		EXPECT_EQ(core::NT_CallExpr, list[0]->getNodeType());

		EXPECT_EQ("0-2", toString(list[1]));
		EXPECT_EQ("ref.assign(v1, int.add(ref.deref(v1), 2))", toString(*list[1].getAddressedNode()));
		EXPECT_EQ(core::NT_CallExpr, list[1]->getNodeType());


		EXPECT_EQ("0-2-3-2-2", toString(list[2]));
		EXPECT_EQ("v1", toString(*list[2].getAddressedNode()));
		EXPECT_EQ(core::NT_Variable, list[2]->getNodeType());

	}

	TEST(IR_Parser2, ParseAddressRoot) {
		NodeManager manager;
		IRBuilder builder(manager);

		// a simple example

		vector<NodeAddress> list = builder.parseAddresses(
				"${"
				"	ref<int<4>> x = 2;"
				"	$x = 3$;"
				"	$x = $x$ + 2$;"
				"}$"
		);

		// there should only be two addresses
		ASSERT_EQ(4u, list.size());

		EXPECT_EQ("0", toString(list[0]));
		EXPECT_EQ(core::NT_CompoundStmt, list[0]->getNodeType());

		EXPECT_EQ("0-1", toString(list[1]));
		EXPECT_EQ("ref.assign(v1, 3)", toString(*list[1].getAddressedNode()));
		EXPECT_EQ(core::NT_CallExpr, list[1]->getNodeType());

		EXPECT_EQ("0-2", toString(list[2]));
		EXPECT_EQ("ref.assign(v1, int.add(ref.deref(v1), 2))", toString(*list[2].getAddressedNode()));
		EXPECT_EQ(core::NT_CallExpr, list[2]->getNodeType());

		EXPECT_EQ("0-2-3-2-2", toString(list[3]));
		EXPECT_EQ("v1", toString(*list[3].getAddressedNode()));
		EXPECT_EQ(core::NT_Variable, list[3]->getNodeType());
	}

//	TEST(IR_Parser2_ErrorReporting, If) {
//		NodeManager manager;
//		IRBuilder builder(manager);
//
//		try {
//			// parse something that is faulty
//			builder.parseStmt(
//					"{"
//					"	int<4> a = 4;"
//					"	if (true) {"
//					"		some shit"
//					"	}"
//					"}"
//			);
//		} catch(const IRParserException& ipe) {
//
//			// inspect error report
//			EXPECT_PRED2(notContainsSubString, ipe.getMessage(), "if ( true ) { some shit }");
//			EXPECT_PRED2(containsSubString, ipe.getMessage(), "if ( true ) { some shit }");
//
//			return;
//		}
//
//		FAIL() << "An exception should have been raised!";
//	}

} // end namespace parser2
} // end namespace core
} // end namespace insieme
