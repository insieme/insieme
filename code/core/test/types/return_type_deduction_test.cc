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

#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/checks/type_checks.h"

namespace insieme {
namespace core {
namespace types {

	TEST(ReturnTypeDeduction, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// some variables and types
		TypePtr varA = TypeVariable::get(manager, "a");
		TypePtr varB = TypeVariable::get(manager, "b");

		TypePtr typeA = GenericType::get(manager, "typeA");
		TypePtr typeB = GenericType::get(manager, "typeB");

		TypePtr genA = GenericType::get(manager, "type", toVector<TypePtr>(varA));
		TypePtr genB = GenericType::get(manager, "type", toVector<TypePtr>(varB));

		TypePtr genSpecA = GenericType::get(manager, "type", toVector<TypePtr>(typeA));
		TypePtr genSpecB = GenericType::get(manager, "type", toVector<TypePtr>(typeB));

		// test some functions
		FunctionTypePtr funType;

		// a simple case
		funType = FunctionType::get(manager, toVector<TypePtr>(varA), varA);
		EXPECT_EQ("(('a)->'a)", toString(*funType));
		EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(typeA))));


		funType = FunctionType::get(manager, toVector<TypePtr>(varA, varB), varA);
		EXPECT_EQ("(('a,'b)->'a)", toString(*funType));
		EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(typeA, typeB))));
		EXPECT_EQ("typeB", toString(*deduceReturnType(funType, toVector<TypePtr>(typeB, typeA))));


		funType = FunctionType::get(manager, toVector<TypePtr>(genA, varA), varA);
		EXPECT_EQ("((type<'a>,'a)->'a)", toString(*funType));
		EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(genSpecA, typeA))));
		EXPECT_EQ("typeB", toString(*deduceReturnType(funType, toVector<TypePtr>(genSpecB, typeB))));
		EXPECT_EQ("'a", toString(*deduceReturnType(funType, toVector<TypePtr>(genA, varA))));

		// test invalid call
		funType = FunctionType::get(manager, toVector<TypePtr>(typeA), typeA);
		EXPECT_EQ("unit", toString(*deduceReturnType(funType, toVector<TypePtr>(typeB))));
		EXPECT_THROW(tryDeduceReturnType(funType, toVector<TypePtr>(typeB)), ReturnTypeDeductionException);


		//	// make a call requiring sub-type deduction
		//	funType = FunctionType::get(manager, toVector<TypePtr>(varA, varA), varA);
		//	EXPECT_EQ("(('a,'a)->'a)", toString(*funType));
		//
		//	TypePtr vectorTypeA = VectorType::get(manager, typeA, builder.concreteIntTypeParam(12));
		//	TypePtr vectorTypeB = VectorType::get(manager, typeA, builder.concreteIntTypeParam(14));
		//	EXPECT_EQ("array<typeA,1>", toString(*deduceReturnType(funType, toVector(vectorTypeA, vectorTypeB))));
	}


	TEST(ReturnTypeDeduction, AutoTypeInference_ArrayInitCall) {
		// The Bug:
		// 		Unable to deduce return type for call to function of type
		//		(('elem,uint<8>)->array<'elem,1>) using arguments
		//		ref<struct<top:ref<array<ref<rec 'elem.{'elem=struct<value:ref<int<4>>,next:ref<array<ref<'elem>,1>>>}>,1>>>>, uint<8>

		// The reason:
		// 		within the element type the same variable 'elem is used as within the
		//		function type => leading to a mess

		// The fix:
		//		before trying to match the given arguments to the function parameters
		//		all type variables are replaced by fresh variables.


		NodeManager manager;
		IRBuilder builder(manager);

		// get element type
		TypePtr argType = builder.parseType("'elem");
		TypePtr funType = builder.parseType("('elem) -> foo<'elem>");

		// create the call
		ExpressionPtr element = builder.literal("a", argType);
		ExpressionPtr fun = builder.literal("f", funType);
		ExpressionPtr res = builder.callExpr(fun, element);

		// check inferred type
		EXPECT_EQ("foo<'elem>", toString(*res->getType()));
	}


	TEST(ReturnTypeDeduction, ReturnTypeBug) {
		// MSG: Invalid return type
		//		- expected: vector<'res,'l>, actual: vector<uint<4>,3>
		//		- function type: ((vector<'elem,'l>,vector<'elem,'l>)->vector<'res,'l>)
		//
		// => occurs in conjunction with the vector.pointwise operator

		// build a pointwise sum ...
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr uint4 = manager.getLangBasic().getUInt4();
		ExpressionPtr add = manager.getLangBasic().getOperator(uint4, lang::BasicGenerator::Add);
		ExpressionPtr pointwise = builder.callExpr(
				builder.literal("fun", builder.parseType("(('elem1, 'elem2) => 'res) -> (array<'elem1,'l>, array<'elem2,'l>) => array<'res, 'l>")),
				add
		);

		EXPECT_EQ("((uint<'a>,uint<'a>)->uint<'a>)", toString(*add->getType()));
		EXPECT_EQ("((array<uint<'a>,'l>,array<uint<'a>,'l>)=>array<uint<'a>,'l>)", toString(*pointwise->getType()));
	}


	TEST(ReturnTypeDeduction, VariableSubstitutionBug) {
		// The basis of this Test case is the following type checker error:
		//		MSG: Invalid return type -
		// 				expected: uint<a>
		// 				actual:   uint<4>
		// 				function type: ((array<'elem,l>,'res,(('elem,'res)->'res))->'res)
		//
		// This error occurs when the function is invoked using a literal as
		// its second argument and a generic integer operation is its last.
		// The expected return type should be consistent with the type of the
		// second argument.


		// reconstruct test case
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr intType = manager.getLangBasic().getUInt4();
		TypePtr arrayType = builder.parseType("array<uint<4>,8>");
		TypePtr funType = builder.parseType("(array<'elem,'l>,'res,('elem,'res)->'res)->'res");
		EXPECT_TRUE(funType);

		LiteralPtr fun = Literal::get(manager, funType, "fun");
		LiteralPtr array = Literal::get(manager, arrayType, "x");
		LiteralPtr zero = Literal::get(manager, intType, "0");
		LiteralPtr op = manager.getLangBasic().getUnsignedIntAdd();

		ExpressionPtr call = builder.callExpr(intType, fun, array, zero, op);

		// run check
		checks::CheckPtr callCheck = checks::make_check<checks::CallExprTypeCheck>();
		auto res = checks::check(call, callCheck);

		// there shouldn't be any errors
		EXPECT_TRUE(res.empty()) << "Errors: " << res;
	}

	TEST(ReturnTypeDeduction, TypeVariableCapture) {
		// Problem: A function of type
		//			(ref<'a>, type<'b>)->ref<'b>
		// called using arguments of type
		//			ref<any>, type<array<'a,1>>
		// results in a value
		//			ref<array<any,1>>
		// instead of a value of type
		//			ref<array<'a,1>>


		NodeManager manager;
		IRBuilder builder(manager);

		FunctionTypePtr funType = builder.parseType("(ref<'a>, type<'b>)->ref<'b>").as<FunctionTypePtr>();

		auto argTypes = toVector(builder.parseType("ref<any>"), builder.parseType("type<array<'a,1>>"));

		TypePtr resType = deduceReturnType(funType, argTypes);

		EXPECT_EQ("ref<array<'a,1>,f,f,plain>", toString(*resType));
	}


	TEST(ReturnTypeDeduction, NestedAlphaBug) {
		// Problem: the return type of a function of type
		//			(ref<list<'a>>, uint<4>)->ref<'a>
		// is called using arguments
		//			ref<list<X>>, uint<4>
		// and the result type is not properly deduced.

		NodeManager manager;
		IRBuilder builder(manager);

		FunctionTypePtr funType = builder.parseType("(ref<list<'a>>,uint<4>)->ref<'a>").as<FunctionTypePtr>();

		auto argTypes = toVector(builder.parseType("ref<list<X>>"), builder.parseType("uint<4>"));

		TypePtr resType = deduceReturnType(funType, argTypes);

		EXPECT_EQ("ref<X,f,f,plain>", toString(*resType));
	}


	TEST(ReturnTypeDeduction, MultipleIntTypeVariables) {
		// Problem: the return type of a function of type
		//			('a)->'a    passing p<'m,'n> returns p<'m,'m>
		// but should be
		//			p<'m,'n>

		NodeManager manager;
		IRBuilder builder(manager);

		auto f = builder.parseExpr("lit(\"f\":('a)->'a)");
		auto a = builder.parseExpr("lit(\"a\":p<'m,'n>)");

		auto c = builder.callExpr(f, a);

		EXPECT_EQ("p<'m,'n>", toString(*c->getType()));
	}

	TEST(ReturnTypeDeduction, VectorPointwise) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto op1 = builder.parseExpr(R"(
			(v1 : array<'elem1,'l>, v2 : array<'elem2,'l>) => (v1 : array<'elem1,'l>, v2 : array<'elem2,'l>, op : ('elem1, 'elem2) -> 'res) -> array<'res,'l> {
				var ref<array<'res,'l>> res;
				return *res;
			}(v1, v2, lit("x":('elem1,'elem2)->'elem2))
		)");

		EXPECT_EQ("((array<'elem1,'l>,array<'elem2,'l>)=>array<'elem2,'l>)", toString(*op1->getType()));

		auto op2 = builder.parseExpr(R"(
			(v1 : array<int<'a>,'l>, v2 : array<int<'a>,'l>) => (v1 : array<'elem1,'l>, v2 : array<'elem2,'l>, op : ('elem1, 'elem2) -> 'res) -> array<'res,'l> {
				var ref<array<'res,'l>> res;
				return *res;
			}(v1, v2, int_add)
		)");

		EXPECT_EQ("((array<int<'a>,'l>,array<int<'a>,'l>)=>array<int<'a>,'l>)", toString(*op2->getType()));
	}

	TEST(ReturnTypeDeduction, HigherOrderFunction) {

		NodeManager manager;
		IRBuilder builder(manager);

		auto argType = builder.parseType("int<4>");
		auto arfType = builder.parseType("('a,'a)->'a");
		auto funType = builder.parseType("('a,'a,('a,'a)->'b)->'b").as<FunctionTypePtr>();
		EXPECT_EQ("int<4>", toString(*deduceReturnType(funType, toVector(argType, argType, arfType))));

		arfType = builder.parseType("(int<'a>,int<'a>)->bool");
		EXPECT_EQ("bool", toString(*deduceReturnType(funType, toVector(argType, argType, arfType))));

		arfType = builder.parseType("(int<'a>,int<'a>)->int<'a>");
		EXPECT_EQ("int<4>", toString(*deduceReturnType(funType, toVector(argType, argType, arfType))));
	}

	TEST(ReturnTypeDeduction, PReduceBug) {

		NodeManager manager;
		IRBuilder builder(manager);

		auto argType = builder.parseType("int<4>");
		auto arfType = builder.parseType("(ref<array<'a>>, uint<8>, uint<8>)=>bool");
		auto funType = builder.parseType("('a, (ref<array<'a>>, uint<8>, uint<8>)=>'b )->'b").as<FunctionTypePtr>();
		EXPECT_EQ("bool", toString(*deduceReturnType(funType, toVector(argType, arfType))));

		arfType = builder.parseType("(ref<array<'a>>, uint<8>, uint<8>)=>'b");
		EXPECT_EQ("'b", toString(*deduceReturnType(funType, toVector(argType, arfType))));
	}

	TEST(ReturnTypeDeduction, GenericArgumentAndParameter) {

		NodeManager manager;
		IRBuilder builder(manager);

		// start with a function
		auto funType = builder.parseType("('a)->'a").as<FunctionTypePtr>();

		// passing a concrete type results in required type
		EXPECT_EQ("int<4>", toString(*deduceReturnType(funType, toVector(builder.parseType("int<4>")))));

		// passing a generic type should return the generic type
		EXPECT_EQ("'b", toString(*deduceReturnType(funType, toVector(builder.parseType("'b")))));

		// even if it is the same name
		EXPECT_EQ("'a", toString(*deduceReturnType(funType, toVector(builder.parseType("'a")))));

		// also if it is something more complex
		EXPECT_EQ("(('a)->'b)", toString(*deduceReturnType(funType, toVector(builder.parseType("('a)->'b")))));

		// --- more complex types ---

		// if the argument type is more complex
		funType = builder.parseType("(<'a>('b)->'c)->('b)->'c").as<FunctionTypePtr>();

		EXPECT_EQ("((B)->C)", toString(*deduceReturnType(funType, toVector(builder.parseType("<A>(B)->C")))));
		EXPECT_EQ("(('b)->'c)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'a>('b)->'c")))));
		EXPECT_EQ("(('y)->'z)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'x>('y)->'z")))));

		EXPECT_EQ("(('x)->'x)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'x>('x)->'x")))));

		// --- even more complex ---

		// something that caused problems
		funType = builder.parseType("(<'a,'b>('c)->'d)->('c)->'d").as<FunctionTypePtr>();
		EXPECT_EQ("(('b)->'a)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'a,'b>('b)->'a")))));
		EXPECT_EQ("(('y)->'x)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'x,'y>('y)->'x")))));

		// also with variadic parameters
		funType = builder.parseType("(<'a...>('b...)->'c)->('b...)->'c").as<FunctionTypePtr>();
		EXPECT_EQ("(('b)->'a)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'a,'b>('b)->'a")))));
		EXPECT_EQ("(('b)->'c)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'c,'b>('b)->'c")))));
		EXPECT_EQ("(('c)->'a)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'a,'c>('c)->'a")))));
		EXPECT_EQ("(('y)->'x)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'x,'y>('y)->'x")))));

	}

	TEST(ReturnTypeDeduction, GenericFunctionTypeObjectTypeBug) {

		NodeManager manager;
		IRBuilder builder(manager);

		auto funType = builder.parseType("(ref<'c,'x,'y,plain>::()->'r)-> 'r").as<FunctionTypePtr>();

		// it should accept simple member functions
		EXPECT_EQ("int<4>", toString(*deduceReturnType(funType, toVector(builder.parseType("A::()->int<4>")))));

		// it should also support the const version of it
		EXPECT_EQ("int<4>", toString(*deduceReturnType(funType, toVector(builder.parseType("const A::()->int<4>")))));

		// and a volatile
		EXPECT_EQ("int<4>", toString(*deduceReturnType(funType, toVector(builder.parseType("volatile A::()->int<4>")))));

	}

	TEST(ReturnTypeDeduction, GenericFunctionTypeParameterBug) {

		NodeManager manager;
		IRBuilder builder(manager);

		auto funType = builder.parseType("(<'p...>'c::('a...)->'r)-> 'c::('a...)->'r").as<FunctionTypePtr>();

		// it should accept simple member functions
		EXPECT_EQ("(A::()->int<4>)", toString(*deduceReturnType(funType, toVector(builder.parseType("A::()->int<4>")))));
		EXPECT_EQ("(A::(int<4>)->int<4>)", toString(*deduceReturnType(funType, toVector(builder.parseType("A::(int<4>)->int<4>")))));
		EXPECT_EQ("(A::(int<4>,bool)->int<4>)", toString(*deduceReturnType(funType, toVector(builder.parseType("A::(int<4>,bool)->int<4>")))));

		// it should also accept member functions with instantiation variables
		EXPECT_EQ("(A::()->int<4>)", toString(*deduceReturnType(funType, toVector(builder.parseType("<B>A::()->int<4>")))));
		EXPECT_EQ("(A::(int<4>)->int<4>)", toString(*deduceReturnType(funType, toVector(builder.parseType("<B>A::(int<4>)->int<4>")))));
		EXPECT_EQ("(A::(int<4>,bool)->int<4>)", toString(*deduceReturnType(funType, toVector(builder.parseType("<B>A::(int<4>,bool)->int<4>")))));

		// also generic functions
		EXPECT_EQ("('y::()->'z)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'x>'y::()->'z")))));
		EXPECT_EQ("('y::('w)->'z)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'x>'y::('w)->'z")))));
		EXPECT_EQ("('y::('w,'q)->'z)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'x>'y::('w,'q)->'z")))));

		// also generic functions with colliding parameters
		EXPECT_EQ("('b::()->'e)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'a>'b::()->'e")))));
		EXPECT_EQ("('b::('c)->'e)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'a>'b::('c)->'e")))));
		EXPECT_EQ("('b::('c,'d)->'e)", toString(*deduceReturnType(funType, toVector(builder.parseType("<'a>'b::('c,'d)->'e")))));


		auto funType2 = builder.parseType("(<'p...>'x::('a...)->'b,<'q...>'y::('c...)->'d)->('p...,'x,'a...,'b,'q...,'y,'c...,'d)").as<FunctionTypePtr>();

		EXPECT_EQ("(P,A,B,C,Q,X,Y,Z)", toString(*deduceReturnType(funType2, toVector(builder.parseType("<P>A::(B)->C"),builder.parseType("<Q>X::(Y)->Z")))));
		EXPECT_EQ("(P,A,B,C,P,A,B,C)", toString(*deduceReturnType(funType2, toVector(builder.parseType("<P>A::(B)->C"),builder.parseType("<P>A::(B)->C")))));

		EXPECT_EQ("(A,uint<8>,A,uint<8>)", toString(*deduceReturnType(funType2, toVector(builder.parseType("A::()->uint<8>"),builder.parseType("A::()->uint<8>")))));

		EXPECT_EQ("(A<R,S>,B,C,X<T,V>,Y,Z)", toString(*deduceReturnType(funType2, toVector(builder.parseType("A<R,S>::(B)->C"),builder.parseType("X<T,V>::(Y)->Z")))));

	}

	TEST(ReturnTypeDeduction, ImplicitMaterialization) {

		NodeManager manager;
		IRBuilder builder(manager);

		std::map<string,NodePtr> symbols;

		// a simple case for starters
		EXPECT_TRUE(analysis::isTrivial(builder.parseType("int<4>")));

		auto argType = builder.parseType("int<4>");
		auto funType = builder.parseType("(int<4>)->bool").as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		// now, a little more sophisticated
		argType = builder.parseType("int<4>");
		funType = builder.parseType("(ref<int<4>,t,f,cpp_ref>)->bool").as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		// also for r-value references
		argType = builder.parseType("int<4>");
		funType = builder.parseType("(ref<int<4>,t,f,cpp_rref>)->bool").as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		// but not for non-const references
		argType = builder.parseType("int<4>");
		funType = builder.parseType("(ref<int<4>,f,f,cpp_ref>)->bool").as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));

		// -- check a non-trivially copyable --
		symbols["A"] = builder.parseType("struct A { a : int<4>; ctor (other : ref<A,t,f,cpp_ref>) { } }");
		EXPECT_FALSE(analysis::isTrivial(symbols["A"].as<TypePtr>()));

		// pass by value is supported by the type deduction, but a semantic check will make it invalid
		argType = builder.parseType("A", symbols);
		funType = builder.parseType("(A)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		// pass by reference should be possible
		argType = builder.parseType("ref<A>", symbols);
		funType = builder.parseType("(ref<A>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,f,f,cpp_ref>", symbols);
		funType = builder.parseType("(ref<A,f,f,cpp_ref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,t,f,cpp_ref>", symbols);
		funType = builder.parseType("(ref<A,t,f,cpp_ref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,f,t,cpp_ref>", symbols);
		funType = builder.parseType("(ref<A,f,t,cpp_ref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,t,t,cpp_ref>", symbols);
		funType = builder.parseType("(ref<A,t,t,cpp_ref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		// pass by rvalue reference should also be supported
		argType = builder.parseType("ref<A,f,f,cpp_rref>", symbols);
		funType = builder.parseType("(ref<A,f,f,cpp_rref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));


		// passing a value to a reference should not be possible
		argType = builder.parseType("A", symbols);
		funType = builder.parseType("(ref<A,f,f,cpp_ref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("A", symbols);
		funType = builder.parseType("(ref<A,f,f,cpp_rref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));


		// also, non-trivial objects must be passed by reference to value parameters
		argType = builder.parseType("ref<A,t,f,cpp_ref>", symbols);
		funType = builder.parseType("(A)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,t,f,cpp_rref>", symbols);
		funType = builder.parseType("(A)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));


		// different ref kinds are incompatible
		argType = builder.parseType("ref<A>", symbols);
		funType = builder.parseType("(ref<A,f,f,cpp_ref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A>", symbols);
		funType = builder.parseType("(ref<A,f,f,cpp_rref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,f,f,cpp_ref>", symbols);
		funType = builder.parseType("(ref<A,f,f,cpp_rref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,f,f,cpp_rref>", symbols);
		funType = builder.parseType("(ref<A,f,f,cpp_ref>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));


		// addition of flags is ok, subtraction not
		argType = builder.parseType("ref<A,f,f>", symbols);
		funType = builder.parseType("(ref<A,f,t>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,f,f>", symbols);
		funType = builder.parseType("(ref<A,t,f>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,f,f>", symbols);
		funType = builder.parseType("(ref<A,t,t>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("bool",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,t,f>", symbols);
		funType = builder.parseType("(ref<A,f,t>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,f,t>", symbols);
		funType = builder.parseType("(ref<A,f,f>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,t,t>", symbols);
		funType = builder.parseType("(ref<A,t,f>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));


		// implict conversions to plain references are not ok
		argType = builder.parseType("ref<A,f,f,cpp_ref>", symbols);
		funType = builder.parseType("(ref<A>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));

		argType = builder.parseType("ref<A,f,f,cpp_rref>", symbols);
		funType = builder.parseType("(ref<A>)->bool", symbols).as<FunctionTypePtr>();
		EXPECT_EQ("unit",toString(*deduceReturnType(funType, {argType})));


	}

	TEST(ReturnTypeDeduction, ImplicitCopyConstruction) {

		NodeManager manager;
		IRBuilder builder(manager);

		std::map<string, NodePtr> symbols;

		// a simple case for starters
		EXPECT_TRUE(analysis::isTrivial(builder.parseType("int<4>")));

		// a trivial value can be passed by value
		auto argType = builder.parseType("int<4>");
		auto funType = builder.parseType("(int<4>)->bool").as<FunctionTypePtr>();
		EXPECT_EQ("bool", toString(*deduceReturnType(funType, { argType })));

		// a trivial value can also be provided as a reference to a value

		// -- cpp_refs are supported --
		argType = builder.parseType("ref<int<4>,f,f,cpp_ref>");
		funType = builder.parseType("(int<4>)->bool").as<FunctionTypePtr>();
		EXPECT_EQ("bool", toString(*deduceReturnType(funType, { argType })));

		// -- cpp_rrefs are also supported --
		argType = builder.parseType("ref<int<4>,f,f,cpp_rref>");
		funType = builder.parseType("(int<4>)->bool").as<FunctionTypePtr>();
		EXPECT_EQ("bool", toString(*deduceReturnType(funType, { argType })));

		// -- plain reference are not allowed --
		argType = builder.parseType("ref<int<4>>");
		funType = builder.parseType("(int<4>)->bool").as<FunctionTypePtr>();
		EXPECT_EQ("unit", toString(*deduceReturnType(funType, { argType })));

	}

	TEST(ReturnTypeDeduction, QualifierPromotion) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto fun = builder.parseExpr("lit(\"fun\" : (ref<'a,t,f,plain>) -> int<4>)");
		auto arg = builder.parseExpr("lit(\"arg\" : ref<int<4>,f,f,plain>)");
		auto call = builder.callExpr(fun, arg);

		EXPECT_EQ(builder.getLangBasic().getInt4(), call->getType());
	}

	TEST(ReturnTypeDeduction, VariadicTypeVariables) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto deduce = [&](const TypePtr& fun, const TypeList& args) {
			return deduceReturnType(fun.as<FunctionTypePtr>(), args);
		};

		auto type = [&](const std::string& type) {
			return builder.parseType(type);
		};

		// something simple to start with
		EXPECT_EQ("B", toString(*deduce(type("(A)->B"), { type("A") })));
		EXPECT_EQ("B", toString(*deduce(type("('a)->B"), { type("A") })));
		EXPECT_EQ("A", toString(*deduce(type("('a)->'a"), { type("A") })));

		EXPECT_EQ("A", toString(*deduce(type("('a,'a)->'a"), { type("A"), type("A") })));
		EXPECT_EQ("B", toString(*deduce(type("('a,'a)->'a"), { type("B"), type("B") })));
		EXPECT_EQ("unit", toString(*deduce(type("('a,'a)->'a"), { type("A"), type("B") })));

		// now, let's try variadic arguments
		EXPECT_EQ("A", toString(*deduce(type("('a...)->A"), {})));
		EXPECT_EQ("A", toString(*deduce(type("('a...)->A"), { type("A") })));
		EXPECT_EQ("A", toString(*deduce(type("('a...)->A"), { type("A"), type("B") })));

		EXPECT_EQ("()", toString(*deduce(type("('a...)->('a...)"), { })));
		EXPECT_EQ("(A)", toString(*deduce(type("('a...)->('a...)"), { type("A") })));
		EXPECT_EQ("(A,B)", toString(*deduce(type("('a...)->('a...)"), { type("A"), type("B") })));
		EXPECT_EQ("(A,B,C)", toString(*deduce(type("('a...)->('a...)"), { type("A"), type("B"), type("C") })));

		// test with nested tuple
		EXPECT_EQ("()", toString(*deduce(type("(('a...))->('a...)"), { type("()") })));
		EXPECT_EQ("(A)", toString(*deduce(type("(('a...))->('a...)"), { type("(A)") })));
		EXPECT_EQ("(A,B)", toString(*deduce(type("(('a...))->('a...)"), { type("(A,B)") })));
		EXPECT_EQ("(A,B,C)", toString(*deduce(type("(('a...))->('a...)"), { type("(A,B,C)") })));

		EXPECT_EQ("unit", toString(*deduce(type("(('a...))->('a...)"), {})));
		EXPECT_EQ("unit", toString(*deduce(type("(('a...))->('a...)"), { type("A"), type("B") })));

		// more complex case
		EXPECT_EQ("()", toString(*deduce(type("(('a...),('a...))->('a...)"), { type("()"), type("()") })));
		EXPECT_EQ("(A)", toString(*deduce(type("(('a...),('a...))->('a...)"), { type("(A)"), type("(A)") })));
		EXPECT_EQ("(A,B)", toString(*deduce(type("(('a...),('a...))->('a...)"), { type("(A,B)"), type("(A,B)") })));
		EXPECT_EQ("(A,B,C)", toString(*deduce(type("(('a...),('a...))->('a...)"), { type("(A,B,C)"), type("(A,B,C)") })));

		EXPECT_EQ("unit", toString(*deduce(type("(('a...),('a...))->('a...)"), { type("(A)"), type("(B)") })));

		// something more challenging
		EXPECT_EQ("(B)", toString(*deduce(type("(A,'a...)->('a...)"), { type("A"), type("B") })));
		EXPECT_EQ("(B,C)", toString(*deduce(type("(A,'a...)->('a...)"), { type("A"), type("B"), type("C") })));

		// even more complex
		EXPECT_EQ("('b...)", toString(*deduce(type("(('a...))->('a...)"), { type("('b...)") })));
		EXPECT_EQ("(A,'b...)", toString(*deduce(type("(('a...))->('a...)"), { type("(A,'b...)") })));


		EXPECT_EQ("((A,'c...),(B,'c...))", toString(*deduce(type("(('a...),('b...))->(('a...),('b...))"), { type("(A,'c...)"), type("(B,'c...)") })));
		EXPECT_EQ("unit", toString(*deduce(type("(('a...),('a...))->(('a...),('a...))"), { type("(A,B)"), type("('c...)") })));

		// and one more step
		EXPECT_EQ("unit", toString(*deduce(type("((A,'a...),(A,'a...))->('a...)"), { type("('c...)"), type("('c...)") })));
		EXPECT_EQ("unit", toString(*deduce(type("((A,'a...),(B,'a...))->('a...)"), { type("('c...)"), type("('c...)") })));
		EXPECT_EQ("unit", toString(*deduce(type("((A,'a...),(B,'a...))->('a...)"), { type("('c...)"), type("('d...)") })));


		// nested functions
		EXPECT_EQ("()", toString(*deduce(type("(('a...),('a...)->('b...))->('b...)"), { type("()"), type("()->()") })));
		EXPECT_EQ("(B)", toString(*deduce(type("(('a...),('a...)->('b...))->('b...)"), { type("(A)"), type("(A)->(B)") })));
		EXPECT_EQ("(B,C)", toString(*deduce(type("(('a...),('a...)->('b...))->('b...)"), { type("(A)"), type("(A)->(B,C)") })));

		EXPECT_EQ("unit", toString(*deduce(type("(('a...),('a...)->('b...))->('b...)"), { type("(A)"), type("(B)->(C)") })));

		// test structs
		EXPECT_EQ("struct {a:(A,B),", toString(*deduce(type("(('a...))->struct { a : ('a...); }"), { type("(A,B)") })).substr(0,16));
		EXPECT_EQ("(A,B)", toString(*deduce(type("(struct { a : ('a...); })->('a...)"), { type("struct { a : (A,B); }") })));

		EXPECT_EQ("struct {a:(A,B),b:(C)", toString(*deduce(type("(('a...),('b...))->struct { a : ('a...);  b: ('b...); }"), { type("(A,B)"), type("(C)") })).substr(0, 21));
		EXPECT_EQ("((A,B),(C))", toString(*deduce(type("(struct { a : ('a...); b:('b...); })->(('a...),('b...))"), { type("struct { a : (A,B); b:(C); }") })));


		// test concatenation

		auto funType = builder.parseType("(('a...),('b...))->('a...,'b...)").as<FunctionTypePtr>();

		auto test = [&](const std::string& p1, const std::string& p2) {
			return toString(*deduceReturnType(funType, toVector(builder.parseType(p1),builder.parseType(p2))));
		};

		EXPECT_EQ("()",        test("()","()"));
		EXPECT_EQ("(A)",       test("(A)","()"));
		EXPECT_EQ("(A)",       test("()","(A)"));
		EXPECT_EQ("(A,A)",     test("(A)","(A)"));
		EXPECT_EQ("(A,B)",     test("(A)","(B)"));
		EXPECT_EQ("(A,B,C)",   test("(A)","(B,C)"));
		EXPECT_EQ("(A,B,C)",   test("(A,B)","(C)"));
		EXPECT_EQ("(A,B,C,D)", test("(A,B)","(C,D)"));

	}

	TEST(ReturnTypeDeduction, GenericTypeVariables) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto deduce = [&](const TypePtr& fun, const TypeList& args) {
			return deduceReturnType(fun.as<FunctionTypePtr>(), args);
		};

		auto type = [&](const std::string& type) {
			return builder.parseType(type);
		};

		// something simple to start with
		EXPECT_EQ("B", toString(*deduce(type("(A)->B"), { type("A") })));
		EXPECT_EQ("B", toString(*deduce(type("('a<>)->B"), { type("A") })));
		EXPECT_EQ("A", toString(*deduce(type("('a<>)->'a<>"), { type("A") })));

		// check consistency constraints
		EXPECT_EQ("A", toString(*deduce(type("('a<>,'a<>)->'a<>"), { type("A"), type("A") })));
		EXPECT_EQ("A", toString(*deduce(type("('a<>,'b<>)->'a<>"), { type("A"), type("A") })));
		EXPECT_EQ("A", toString(*deduce(type("('a<>,'b<>)->'a<>"), { type("A"), type("B") })));
		EXPECT_EQ("B", toString(*deduce(type("('a<>,'b<>)->'b<>"), { type("A"), type("B") })));

		EXPECT_EQ("unit", toString(*deduce(type("('a<>,'a<>)->'a<>"), { type("A"), type("B") })));


		EXPECT_EQ("A", toString(*deduce(type("('a,'a)->'a"), { type("A"), type("A") })));
		EXPECT_EQ("B", toString(*deduce(type("('a,'a)->'a"), { type("B"), type("B") })));
		EXPECT_EQ("unit", toString(*deduce(type("('a,'a)->'a"), { type("A"), type("B") })));

		// now, let's try variadic arguments
		EXPECT_EQ("A", toString(*deduce(type("('a...)->A"), {})));
		EXPECT_EQ("A", toString(*deduce(type("('a...)->A"), { type("A") })));
		EXPECT_EQ("A", toString(*deduce(type("('a...)->A"), { type("A"), type("B") })));

		EXPECT_EQ("()", toString(*deduce(type("('a...)->('a...)"), {})));
		EXPECT_EQ("(A)", toString(*deduce(type("('a...)->('a...)"), { type("A") })));
		EXPECT_EQ("(A,B)", toString(*deduce(type("('a...)->('a...)"), { type("A"), type("B") })));
		EXPECT_EQ("(A,B,C)", toString(*deduce(type("('a...)->('a...)"), { type("A"), type("B"), type("C") })));

	}


	TEST(ReturnTypeDeduction, VariadicTypesAndMaterialization) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto deduce = [&](const TypePtr& fun, const TypeList& args) {
			return deduceReturnType(fun.as<FunctionTypePtr>(), args);
		};

		auto type = [&](const std::string& type) {
			return builder.parseType(type);
		};


		// parse the function type
		auto funType = type("(('Closure...),(ref<('Arg,'Closure...),t,f,cpp_ref>)->'Res) -> fun<'Arg,'Res>");
		ASSERT_TRUE(funType);

		EXPECT_EQ("fun<int<4>,bool>", toString(*deduce(funType, { type("(A)"),     type("(ref<(int<4>,A),t,f,cpp_ref>)->bool") })));
		EXPECT_EQ("fun<int<4>,bool>", toString(*deduce(funType, { type("(A,B)"),   type("(ref<(int<4>,A,B),t,f,cpp_ref>)->bool") })));
		EXPECT_EQ("fun<int<4>,bool>", toString(*deduce(funType, { type("(A,B,C)"), type("(ref<(int<4>,A,B,C),t,f,cpp_ref>)->bool") })));

		EXPECT_EQ("fun<int<4>,bool>", toString(*deduce(funType, { type("ref<(A,B),t,f,cpp_ref>"), type("(ref<(int<4>,A,B),t,f,cpp_ref>)->bool") })));

	}

	TEST(ReturnTypeDeduction, Bug_PassPlainReferenceToTypeVariable) {

		// This bug has been encountered by the AllScale compiler developers:
		//		Function Type:  ((dependencies,art_wi_desc<('Args...),'Res>,'InitArgs...)->treeture<'Res,f>)
		//		Argument Types:   dependencies,art_wi_desc<(int<4>),unit>,ref<int<4>,t,f,plain>
		// The return type could not be deduced.
		//
		// The cause of the bug was that when passing a reference to a type type variable, which is not a reference
		// type, an implicit materialization was impossed as if the type variable would have been a generic type.
		// This implicit materialization is not allowed starting from an ref<'a,t,f,plain>.
		//
		// Fix: implicit materialization is no longer attempted if the parameter type is a type variable.

		NodeManager manager;
		IRBuilder builder(manager);

		auto deduce = [&](const TypePtr& fun, const TypeList& args) {
			return deduceReturnType(fun.as<FunctionTypePtr>(), args);
		};

		auto type = [&](const std::string& type) {
			return builder.parseType(type);
		};


		// parse the function type
		auto funType = type("(deps,desc<('A...),'R>,'I...) -> t<'R,f>");
		ASSERT_TRUE(funType);

		EXPECT_EQ("t<unit,f>", toString(*deduce(funType, { type("deps"),     type("desc<(int<4>),unit>"),  type("ref<int<4>,t,f,plain>")})));
		EXPECT_EQ("t<uint<4>,f>", toString(*deduce(funType, { type("deps"),     type("desc<(int<4>),uint<4>>"),  type("ref<int<4>,t,f,plain>")})));


	}

} // end namespace types
} // end namespace core
} // end namespace insieme
