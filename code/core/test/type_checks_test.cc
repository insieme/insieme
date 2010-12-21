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

#include "insieme/core/ast_builder.h"
#include "insieme/core/checks/typechecks.h"

namespace insieme {
namespace core {
namespace checks {

bool containsMSG(const MessageList& list, const Message& msg) {
	return contains(list, msg);
}


TEST(CallExprTypeCheck, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr type = builder.genericType("int", toVector<TypePtr>(), toVector<IntTypeParam>(IntTypeParam::getVariableIntParam('a')));

	// ... define some types
	TupleTypePtr empty = builder.tupleType(toVector<TypePtr>());
	EXPECT_EQ("()", toString(*empty));
	FunctionTypePtr nullary = builder.functionType(empty->getElementTypes(), type);
	EXPECT_EQ("(()->int<#a>)", toString(*nullary));
	TupleTypePtr single = builder.tupleType(toVector(type));
	EXPECT_EQ("(int<#a>)", toString(*single));
	FunctionTypePtr unary = builder.functionType(single->getElementTypes(), type);
	EXPECT_EQ("((int<#a>)->int<#a>)", toString(*unary));
	TupleTypePtr pair = builder.tupleType(toVector(type, type));
	EXPECT_EQ("(int<#a>,int<#a>)", toString(*pair));
	FunctionTypePtr binary = builder.functionType(pair->getElementTypes(), type);
	EXPECT_EQ("((int<#a>,int<#a>)->int<#a>)", toString(*binary));

	// define literals
	LiteralPtr x = builder.literal(type, "1");
	EXPECT_EQ("1", toString(*x));

	TypePtr concreteType = builder.genericType("int", toVector<TypePtr>(), toVector<IntTypeParam>(IntTypeParam::getConcreteIntParam(4)));
	LiteralPtr y = builder.literal(concreteType, "2");
	EXPECT_EQ("2", toString(*y));

	LiteralPtr constFun = builder.literal(nullary, "zero");
	LiteralPtr unaryFun = builder.literal(unary, "succ");
	LiteralPtr binaryFun = builder.literal(binary, "sum");


	ExpressionPtr expr = constFun;

	CheckPtr typeCheck = make_check<CallExprTypeCheck>();

	// correct examples ...
	expr = builder.callExpr(type, constFun, toVector<ExpressionPtr>());
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(type, unaryFun, toVector<ExpressionPtr>(x));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(concreteType, unaryFun, toVector<ExpressionPtr>(y));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(x,x));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(concreteType, binaryFun, toVector<ExpressionPtr>(x,y));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(concreteType, binaryFun, toVector<ExpressionPtr>(y,y));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	// invalid return type
	MessageList issues;
	expr = builder.callExpr(type, unaryFun, toVector<ExpressionPtr>(y));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(x,y));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(y,y));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

	// invalid argument types
	TypePtr concreteType2 = builder.genericType("int", toVector<TypePtr>(), toVector<IntTypeParam>(IntTypeParam::getConcreteIntParam(2)));
	LiteralPtr z = builder.literal(concreteType2, "3");
	EXPECT_EQ("3", toString(*z));

	TypePtr boolType = builder.genericType("bool");
	LiteralPtr w = builder.literal(boolType, "true");
	EXPECT_EQ("true", toString(*w));

	// => not unifyable arguments ...
	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(y,z));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

	// => not unifyable arguments
	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(y,w));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

	// => wrong number of arguments
	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(x));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS, "", Message::ERROR));

	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(x,y,z));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS, "", Message::ERROR));
}


TEST(DeclarationStmtTypeCheck, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr type = builder.genericType("int");
	TypePtr type2 = builder.genericType("uint");
	ExpressionPtr init = builder.literal(type, "4");
	DeclarationStmtPtr ok = builder.declarationStmt(builder.variable(type), init);
	DeclarationStmtPtr err = builder.declarationStmt(builder.variable(type2), init);

	CheckPtr typeCheck = make_check<DeclarationStmtTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_INITIALIZATION_EXPR, "", Message::ERROR));
}

TEST(IfCondition, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr intType = builder.genericType("int");
	TypePtr boolType = builder.genericType("bool");
	ExpressionPtr intLit = builder.literal(intType, "4");
	ExpressionPtr boolLit = builder.literal(boolType, "true");
	NodePtr ok = builder.ifStmt(boolLit, builder.getBasicGenerator().getNoOp());
	NodePtr err = builder.ifStmt(intLit, builder.getBasicGenerator().getNoOp());

	CheckPtr typeCheck = make_check<IfConditionTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_CONDITION_EXPR, "", Message::ERROR));
}

TEST(WhileCondition, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr intType = builder.genericType("int");
	TypePtr boolType = builder.genericType("bool");
	ExpressionPtr intLit = builder.literal(intType, "4");
	ExpressionPtr boolLit = builder.literal(boolType, "true");
	NodePtr ok = builder.whileStmt(boolLit, builder.getBasicGenerator().getNoOp());
	NodePtr err = builder.whileStmt(intLit, builder.getBasicGenerator().getNoOp());

	CheckPtr typeCheck = make_check<WhileConditionTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_CONDITION_EXPR, "", Message::ERROR));
}

TEST(Switch, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr intType = builder.getBasicGenerator().getInt1();
	TypePtr boolType = builder.genericType("bool");
	ExpressionPtr intLit = builder.literal(intType, "4");
	ExpressionPtr boolLit = builder.literal(boolType, "true");
	SwitchStmtPtr ok = builder.switchStmt(intLit, vector<SwitchStmt::Case>());
	SwitchStmtPtr err = builder.switchStmt(boolLit, vector<SwitchStmt::Case>());

	CheckPtr typeCheck = make_check<SwitchExpressionTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_SWITCH_EXPR, "", Message::ERROR));
}

TEST(BuildInLiterals, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal

	LiteralPtr ok = builder.getBasicGenerator().getFalse();
	LiteralPtr err = builder.literal(builder.genericType("strangeType"), ok->getValue());

	CheckPtr typeCheck = make_check<BuiltInLiteralCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_TYPE_OF_LITERAL, "", Message::WARNING));
}

TEST(RefCastExpr, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal

	TypePtr type = builder.genericType("int");
	TypePtr ref = builder.refType(type);

	CastExprPtr ok = builder.castExpr(type, builder.literal(type, "1"));
	CastExprPtr err1 = builder.castExpr(ref, builder.literal(type, "1"));
	CastExprPtr err2 = builder.castExpr(type, builder.literal(ref, "1"));
	CastExprPtr err3 = builder.castExpr(builder.refType(type), builder.literal(builder.refType(ref), "1"));

	CheckPtr typeCheck = make_check<RefCastCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err1, typeCheck).empty());
	ASSERT_FALSE(check(err2, typeCheck).empty());
	ASSERT_FALSE(check(err3, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err1,typeCheck), Message(NodeAddress(err1), EC_TYPE_NON_REF_TO_REF_CAST, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err2,typeCheck), Message(NodeAddress(err2), EC_TYPE_REF_TO_NON_REF_CAST, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_REF_TO_NON_REF_CAST, "", Message::ERROR));
}

TEST(CastExpr, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal

	TypePtr type = builder.genericType("int");
	TypePtr ref = builder.refType(type);
	TypePtr fun = builder.functionType(toVector(type), ref);

	CastExprPtr ok = builder.castExpr(type, builder.literal(type, "1"));
	CastExprPtr err1 = builder.castExpr(fun, builder.literal(type, "1"));
	CastExprPtr err2 = builder.castExpr(type, builder.literal(fun, "1"));
	CastExprPtr err3 = builder.castExpr(builder.refType(fun), builder.literal(builder.refType(ref), "1"));

	CheckPtr typeCheck = make_check<CastCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err1, typeCheck).empty());
	ASSERT_FALSE(check(err2, typeCheck).empty());
	ASSERT_FALSE(check(err3, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err1,typeCheck), Message(NodeAddress(err1), EC_TYPE_ILLEGAL_CAST, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err2,typeCheck), Message(NodeAddress(err2), EC_TYPE_ILLEGAL_CAST, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_ILLEGAL_CAST, "", Message::ERROR));
}

TEST(KeywordCheck, Basic) {
	ASTBuilder builder;

	// OK ... create correct and wrong instances

	TypePtr element = builder.genericType("A");
	IntTypeParam param = IntTypeParam::getConcreteIntParam(8);

	CheckPtr typeCheck = make_check<KeywordCheck>();

	// test vector
	{
		TypePtr ok = builder.vectorType(element, param);
		TypePtr err = builder.genericType("vector", toVector<TypePtr>(element), toVector<IntTypeParam>(param));

		EXPECT_FALSE(*ok == *err);

		EXPECT_TRUE(check(ok, typeCheck).empty());
		EXPECT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
	}

	// test array
	{
		TypePtr ok = builder.arrayType(element, param);
		TypePtr err = builder.genericType("array", toVector<TypePtr>(element), toVector<IntTypeParam>(param));

		EXPECT_FALSE(*ok == *err);

		EXPECT_TRUE(check(ok, typeCheck).empty());
		EXPECT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
	}

	// test references
	{
		TypePtr ok = builder.refType(element);
		TypePtr err = builder.genericType("ref", toVector<TypePtr>(element));

		EXPECT_TRUE(check(ok, typeCheck).empty());
		EXPECT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
	}

	// test channel
	{
		TypePtr ok = builder.channelType(element, param);
		TypePtr err = builder.genericType("channel", toVector<TypePtr>(element), toVector<IntTypeParam>(param));

		EXPECT_FALSE(*ok == *err);

		EXPECT_TRUE(check(ok, typeCheck).empty());
		EXPECT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
	}

}


} // end namespace checks
} // end namespace core
} // end namespace insieme
