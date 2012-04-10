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

#include "insieme/simple_backend/transform/preprocessor.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/ir_checks.h"

#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace simple_backend {
namespace transform {

using namespace insieme::core;

bool isSubString(const string& substr, const string& target) {
	return target.find(substr) != string::npos;
}

TEST(Preprocessor, NodeManager) {

	NodeManager managerA;
	NodeManager managerB;

	IRBuilder builderA(managerA);

	TypePtr typeA = builderA.genericType("Test");
	EXPECT_TRUE(managerA.addressesLocal(typeA));
	EXPECT_FALSE(managerB.addressesLocal(typeA));

	NodePtr typeB = preprocess(managerB, typeA);
	EXPECT_TRUE(managerA.addressesLocal(typeA));
	EXPECT_FALSE(managerB.addressesLocal(typeA));
	EXPECT_FALSE(managerA.addressesLocal(typeB));
	EXPECT_TRUE(managerB.addressesLocal(typeB));

	NodePtr typeC = preprocess(managerA, typeA);
	EXPECT_TRUE(managerA.addressesLocal(typeA));
	EXPECT_FALSE(managerB.addressesLocal(typeA));
	EXPECT_FALSE(managerA.addressesLocal(typeB));
	EXPECT_TRUE(managerB.addressesLocal(typeB));
	EXPECT_TRUE(managerA.addressesLocal(typeC));
	EXPECT_FALSE(managerB.addressesLocal(typeC));
}


TEST(Preprocessor, Vector2Array_Parameter) {

	NodeManager manager;
	IRBuilder builder(manager);

	// construct a function accepting an array
	TypePtr element = builder.genericType("Test");
	TypePtr array = builder.arrayType(element);
	FunctionTypePtr funType = builder.functionType(toVector(array), array);
	VariablePtr var = builder.variable(array, 1);
	LambdaExprPtr lambda = builder.lambdaExpr(funType, toVector(var), builder.returnStmt(var));

	EXPECT_PRED2(isSubString, "=fun(array<Test,1> v1) {return v1;}}", toString(*lambda));

	// construct a literal (as an argument)
	VectorTypePtr vector = builder.vectorType(element, builder.concreteIntTypeParam(12));
	LiteralPtr literal = builder.literal(vector, "X");

	// construct call
	CallExprPtr call = builder.callExpr(lambda, literal);
	EXPECT_PRED2(isSubString, "=fun(array<Test,1> v1) {return v1;}}(X)", toString(*call));

	// check with semantic checker
	auto checker = checks::getFullCheck();
	EXPECT_EQ("[]", toString(check(call, checker)));


	// conduct preprocessing
	core::NodePtr res = preprocess(manager, call);
	EXPECT_PRED2(isSubString, "=fun(array<Test,1> v1) {return v1;}}(vector.to.array(X))", toString(*res));

}

TEST(Preprocessor, Vector2Array_GenericParameter) {

	NodeManager manager;
	IRBuilder builder(manager);

	// construct a function accepting an array
	TypePtr element = builder.genericType("Test");
	TypePtr alpha = builder.typeVariable("a");
	FunctionTypePtr funType = builder.functionType(toVector(alpha, alpha), alpha);
	VariablePtr var1 = builder.variable(alpha, 1);
	VariablePtr var2 = builder.variable(alpha, 2);
	LambdaExprPtr lambda = builder.lambdaExpr(funType, toVector(var1, var2), builder.returnStmt(var1));

	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}", toString(*lambda));

	TypePtr array = builder.arrayType(element);
	VectorTypePtr vector = builder.vectorType(element, builder.concreteIntTypeParam(12));

	// construct a literal (as an argument)
	LiteralPtr litX = builder.literal(array, "X");
	LiteralPtr litY = builder.literal(vector, "Y");

	// construct call
	CallExprPtr call = builder.callExpr(lambda, litX, litY);
	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}(X, Y)", toString(*call));

	// check with semantic checker
	auto checker = checks::getFullCheck();
	EXPECT_EQ("[]", toString(check(call, checker)));

	// conduct preprocessing
	core::NodePtr res = preprocess(manager, call, 0);
	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}(X, vector.to.array(Y))", toString(*res));
	EXPECT_EQ("[]", toString(check(res, checker)));

	// test swapped case
	call = builder.callExpr(lambda, litY, litX);
	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}(Y, X)", toString(*call));
	EXPECT_EQ("[]", toString(check(call, checker)));
	res = preprocess(manager, call, 0);
	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}(vector.to.array(Y), X)", toString(*res));
	EXPECT_EQ("[]", toString(check(res, checker)));


	// test same type case
	call = builder.callExpr(lambda, litX, litX);
	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}(X, X)", toString(*call));
	EXPECT_EQ("[]", toString(check(call, checker)));
	res = preprocess(manager, call, 0);
	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}(X, X)", toString(*res));
	EXPECT_EQ("[]", toString(check(res, checker)));

	call = builder.callExpr(lambda, litY, litY);
	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}(Y, Y)", toString(*call));
	EXPECT_EQ("[]", toString(check(call, checker)));
	res = preprocess(manager, call, 0);
	EXPECT_PRED2(isSubString, "=fun('a v1, 'a v2) {return v1;}}(Y, Y)", toString(*res));
	EXPECT_EQ("[]", toString(check(res, checker)));

}

//
// TODO: re-enable when sink has been introduced
//
//TEST(Preprocessor, RefVector2RefArray_Parameter) {
//
//	NodeManager manager;
//	IRBuilder builder(manager);
//
//	// construct a function accepting an array
//	TypePtr element = builder.genericType("Test");
//	TypePtr array = builder.refType(builder.arrayType(element));
//	FunctionTypePtr funType = builder.functionType(toVector(array), array);
//	VariablePtr var = builder.variable(array, 1);
//	LambdaExprPtr lambda = builder.lambdaExpr(funType, toVector(var), builder.returnStmt(var));
//
//	EXPECT_EQ("rec v3.{v3=fun(ref<array<Test,1>> v1) return v1}", toString(*lambda));
//
//	// construct a literal (as an argument)
//	TypePtr vector = builder.refType(builder.vectorType(element, builder.concreteIntTypeParam(12)));
//	LiteralPtr literal = builder.literal(vector, "X");
//
//	// construct call
//	CallExprPtr call = builder.callExpr(lambda, literal);
//	EXPECT_EQ("rec v3.{v3=fun(ref<array<Test,1>> v1) return v1}(X)", toString(*call));
//
//	// check with semantic checker
//	auto checker = checks::getFullCheck();
//	EXPECT_FALSE(check(call,checker).empty());
////	EXPECT_EQ("[]", toString(check(call, checker)));
//
//	// conduct preprocessing
//	core::NodePtr res = preprocess(manager, call);
//	EXPECT_PRED2(isSubString, "=fun(ref<array<Test,1>> v1) return v1}(ref.vector.to.ref.array(X))", toString(*res));
//}

TEST(Preprocessor, LazyITE) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	// create a lazy call
	const TypePtr& boolean = basic.getBool();
	const FunctionTypePtr lazyType = builder.functionType(toVector<TypePtr>(), boolean);
	ExpressionPtr ifOption = builder.lambdaExpr(lazyType, toVector<VariablePtr>(), builder.returnStmt(builder.literal(boolean, "true")));
	ExpressionPtr elseOption = builder.lambdaExpr(lazyType, toVector<VariablePtr>(), builder.returnStmt(builder.literal(boolean, "false")));
	ExpressionPtr lazyCall = builder.callExpr(basic.getIfThenElse(), builder.literal(boolean, "true"), ifOption, elseOption);

	auto checker = checks::getFullCheck();
	EXPECT_EQ("[]", toString(check(lazyCall, checker)));


	EXPECT_EQ("((true)?true:false)", toString(printer::PrettyPrinter(lazyCall, printer::PrettyPrinter::OPTIONS_SINGLE_LINE)));
	NodePtr res = preprocess(manager, lazyCall);
	EXPECT_EQ("lazyITE(true, true, false)", toString(printer::PrettyPrinter(res)));

	// try a nested ITE
	lazyCall = builder.markerExpr(lazyCall, 123);
	EXPECT_EQ("[]", toString(check(lazyCall, checker)));
	EXPECT_EQ("<m id=123>((true)?true:false)</m>",
		toString(printer::PrettyPrinter(lazyCall, printer::PrettyPrinter::OPTIONS_SINGLE_LINE)));
	res = preprocess(manager, lazyCall);
	EXPECT_EQ("lazyITE(true, true, false)", toString(printer::PrettyPrinter(res)));
}


} // end namespace transform
} // end namespace simple_backend
} // end namespace insieme

