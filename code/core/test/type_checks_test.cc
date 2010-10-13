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

#include "ast_builder.h"
#include "checks/typechecks.h"

namespace insieme {
namespace core {
namespace checks {

TEST(NumArgumentCheck, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr type = builder.genericType("int");

	// ... define some types
	TupleTypePtr empty = builder.tupleType(toVector<TypePtr>());
	EXPECT_EQ("()", toString(*empty));
	FunctionTypePtr nullary = builder.functionType(empty, type);
	EXPECT_EQ("(()->int)", toString(*nullary));
	FunctionTypePtr unary = builder.functionType(type, type);
	EXPECT_EQ("(int->int)", toString(*unary));
	TupleTypePtr single = builder.tupleType(toVector(type));
	EXPECT_EQ("(int)", toString(*single));
	FunctionTypePtr unary2 = builder.functionType(single, type);
	EXPECT_EQ("((int)->int)", toString(*unary2));
	TupleTypePtr pair = builder.tupleType(toVector(type, type));
	EXPECT_EQ("(int,int)", toString(*pair));
	FunctionTypePtr binary = builder.functionType(pair, type);
	EXPECT_EQ("((int,int)->int)", toString(*binary));

	// define literals
	LiteralPtr x = builder.literal("1",type);
	EXPECT_EQ("1", toString(*x));

	LiteralPtr constFun = builder.literal("zero", nullary);
	LiteralPtr unaryFun = builder.literal("succ", unary);
	LiteralPtr unary2Fun = builder.literal("succ", unary2);
	LiteralPtr binaryFun = builder.literal("sum", binary);


	ExpressionPtr expr = constFun;

	// correct examples ...
	expr = builder.callExpr(constFun, toVector<ExpressionPtr>());
	EXPECT_TRUE(check(expr, NumArgumentCheck()).empty());

	expr = builder.callExpr(unaryFun, toVector<ExpressionPtr>(x));
	EXPECT_TRUE(check(expr, NumArgumentCheck()).empty());

	expr = builder.callExpr(unary2Fun, toVector<ExpressionPtr>(x));
	EXPECT_TRUE(check(expr, NumArgumentCheck()).empty());

	expr = builder.callExpr(binaryFun, toVector<ExpressionPtr>(x,x));
	EXPECT_TRUE(check(expr, NumArgumentCheck()).empty());

	// incorrect example ...
	expr = builder.callExpr(constFun, toVector<ExpressionPtr>(x));
	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);

	expr = builder.callExpr(constFun, toVector<ExpressionPtr>(x, x));
	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);

	expr = builder.callExpr(unaryFun, toVector<ExpressionPtr>());
	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);

	expr = builder.callExpr(unaryFun, toVector<ExpressionPtr>(x,x));
	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);

	expr = builder.callExpr(unary2Fun, toVector<ExpressionPtr>());
	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);

	expr = builder.callExpr(unary2Fun, toVector<ExpressionPtr>(x,x));
	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);

	expr = builder.callExpr(binaryFun, toVector<ExpressionPtr>());
	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);

	expr = builder.callExpr(binaryFun, toVector<ExpressionPtr>(x));
	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
}

TEST(ReturnTypeCheck, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr var = builder.typeVariable("a");
	TypePtr concrete = builder.genericType("int");
	TypePtr type = builder.genericType("gen", toVector(var), toVector(IntTypeParam::getVariableIntParam('p')));
	TypePtr typeVV = type;
	TypePtr typeCV = builder.genericType("gen", toVector(concrete), toVector(IntTypeParam::getVariableIntParam('p')));
	TypePtr typeVC = builder.genericType("gen", toVector(var), toVector(IntTypeParam::getConcreteIntParam(12)));
	TypePtr typeCC = builder.genericType("gen", toVector(concrete), toVector(IntTypeParam::getConcreteIntParam(12)));

	EXPECT_EQ("gen<'a,p>", toString(*type));
	EXPECT_EQ("gen<'a,p>", toString(*typeVV));
	EXPECT_EQ("gen<int,p>", toString(*typeCV));
	EXPECT_EQ("gen<'a,12>", toString(*typeVC));
	EXPECT_EQ("gen<int,12>", toString(*typeCC));


	// TODO: finish test case considering unification
//	LiteralPtr literal
//
//	// ... define some types
//	TupleTypePtr empty = builder.tupleType(toVector<TypePtr>());
//	EXPECT_EQ("()", toString(*empty));
//	FunctionTypePtr nullary = builder.functionType(empty, type);
//	EXPECT_EQ("(()->int)", toString(*nullary));
//	FunctionTypePtr unary = builder.functionType(type, type);
//	EXPECT_EQ("(int->int)", toString(*unary));
//	TupleTypePtr single = builder.tupleType(toVector(type));
//	EXPECT_EQ("(int)", toString(*single));
//	FunctionTypePtr unary2 = builder.functionType(single, type);
//	EXPECT_EQ("((int)->int)", toString(*unary2));
//	TupleTypePtr pair = builder.tupleType(toVector(type, type));
//	EXPECT_EQ("(int,int)", toString(*pair));
//	FunctionTypePtr binary = builder.functionType(pair, type);
//	EXPECT_EQ("((int,int)->int)", toString(*binary));
//
//	// define literals
//	LiteralPtr x = builder.literal("1",type);
//	EXPECT_EQ("1", toString(*x));
//
//	LiteralPtr constFun = builder.literal("zero", nullary);
//	LiteralPtr unaryFun = builder.literal("succ", unary);
//	LiteralPtr unary2Fun = builder.literal("succ", unary2);
//	LiteralPtr binaryFun = builder.literal("sum", binary);
//
//
//	ExpressionPtr expr = constFun;
//
//	// correct examples ...
//	expr = builder.callExpr(constFun, toVector<ExpressionPtr>());
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).empty());
//
//	expr = builder.callExpr(unaryFun, toVector<ExpressionPtr>(x));
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).empty());
//
//	expr = builder.callExpr(unary2Fun, toVector<ExpressionPtr>(x));
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).empty());
//
//	expr = builder.callExpr(binaryFun, toVector<ExpressionPtr>(x,x));
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).empty());
//
//	// incorrect example ...
//	expr = builder.callExpr(constFun, toVector<ExpressionPtr>(x));
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
//
//	expr = builder.callExpr(constFun, toVector<ExpressionPtr>(x, x));
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
//
//	expr = builder.callExpr(unaryFun, toVector<ExpressionPtr>());
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
//
//	expr = builder.callExpr(unaryFun, toVector<ExpressionPtr>(x,x));
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
//
//	expr = builder.callExpr(unary2Fun, toVector<ExpressionPtr>());
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
//
//	expr = builder.callExpr(unary2Fun, toVector<ExpressionPtr>(x,x));
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
//
//	expr = builder.callExpr(binaryFun, toVector<ExpressionPtr>());
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
//
//	expr = builder.callExpr(binaryFun, toVector<ExpressionPtr>(x));
//	EXPECT_TRUE(check(expr, NumArgumentCheck()).size() == 1);
}


} // end namespace checks
} // end namespace core
} // end namespace insieme

