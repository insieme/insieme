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

#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/ir_builder.h"

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
		// 				function type: ((vector<'elem,l>,'res,(('elem,'res)->'res))->'res)
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

		EXPECT_EQ("ref<array<'a,1>,f,f>", toString(*resType));
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

		EXPECT_EQ("ref<X,f,f>", toString(*resType));
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
			lambda (array<'elem1,'l> v1, array<'elem2,'l> v2) => lambda (array<'elem1,'l> v1, array<'elem2,'l> v2, ('elem1, 'elem2) -> 'res op) -> array<'res,'l> {
				decl ref<array<'res,'l>> res = var(undefined(array<'res,'l>));
				return *res;
			}(v1, v2, lit("x":('elem1,'elem2)->'elem2))
		)");

		EXPECT_EQ("((array<'elem1,'l>,array<'elem2,'l>)=>array<'elem2,'l>)", toString(*op1->getType()));

		auto op2 = builder.parseExpr(R"(
			lambda (array<int<'a>,'l> v1, array<int<'a>,'l> v2) => lambda (array<'elem1,'l> v1, array<'elem2,'l> v2, ('elem1, 'elem2) -> 'res op) -> array<'res,'l> {
				decl ref<array<'res,'l>> res = var(undefined(array<'res,'l>));
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

} // end namespace types
} // end namespace core
} // end namespace insieme
