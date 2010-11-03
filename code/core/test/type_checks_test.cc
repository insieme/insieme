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
	FunctionTypePtr nullary = builder.functionType(empty, type);
	EXPECT_EQ("(()->int<a>)", toString(*nullary));
	TupleTypePtr single = builder.tupleType(toVector(type));
	EXPECT_EQ("(int<a>)", toString(*single));
	FunctionTypePtr unary = builder.functionType(single, type);
	EXPECT_EQ("((int<a>)->int<a>)", toString(*unary));
	TupleTypePtr pair = builder.tupleType(toVector(type, type));
	EXPECT_EQ("(int<a>,int<a>)", toString(*pair));
	FunctionTypePtr binary = builder.functionType(pair, type);
	EXPECT_EQ("((int<a>,int<a>)->int<a>)", toString(*binary));

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
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(unaryFun), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(x,y));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(unaryFun), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(y,y));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(unaryFun), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

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
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(unaryFun), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

	// => not unifyable arguments
	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(y,w));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(unaryFun), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

	// => wrong number of arguments
	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(x));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(unaryFun), EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS, "", Message::ERROR));

	expr = builder.callExpr(type, binaryFun, toVector<ExpressionPtr>(x,y,z));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(unaryFun), EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS, "", Message::ERROR));
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
	NodePtr ok = builder.ifStmt(boolLit, lang::STMT_NO_OP_PTR);
	NodePtr err = builder.ifStmt(intLit, lang::STMT_NO_OP_PTR);

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
	NodePtr ok = builder.whileStmt(boolLit, lang::STMT_NO_OP_PTR);
	NodePtr err = builder.whileStmt(intLit, lang::STMT_NO_OP_PTR);

	CheckPtr typeCheck = make_check<WhileConditionTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_CONDITION_EXPR, "", Message::ERROR));
}

TEST(Switch, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr intType = lang::TYPE_INT_1_PTR;
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

	LiteralPtr ok = lang::CONST_BOOL_FALSE_PTR;
	LiteralPtr err = builder.literal(builder.genericType("strangeType"), ok->getValue());

	CheckPtr typeCheck = make_check<BuildInLiteralCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_TYPE_OF_LITERAL, "", Message::WARNING));
}

} // end namespace checks
} // end namespace core
} // end namespace insieme

