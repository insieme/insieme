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

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/checks/type_checks.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/parser2/ir_parser.h"
#include "insieme/core/parser2/grammar.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/normalize.h"

namespace insieme {
namespace core {
namespace checks {

bool containsMSG(const MessageList& list, const Message& msg) {
	return contains(list.getAll(), msg);
}


TEST(CallExprTypeCheck, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	// OK ... create some types
	TypePtr int2 = basic.getInt2();
	TypePtr int4 = basic.getInt4();
	TypePtr intA = basic.getIntGen();

	// ... define some types
	TupleTypePtr empty = builder.tupleType(toVector<TypePtr>());
	EXPECT_EQ("()", toString(*empty));
	FunctionTypePtr nullary = builder.functionType(empty->getElementTypes(), intA);
	EXPECT_EQ("(()->int<#a>)", toString(*nullary));
	TupleTypePtr single = builder.tupleType(toVector(intA));
	EXPECT_EQ("(int<#a>)", toString(*single));
	FunctionTypePtr unary = builder.functionType(single->getElementTypes(), intA);
	EXPECT_EQ("((int<#a>)->int<#a>)", toString(*unary));
	TupleTypePtr pair = builder.tupleType(toVector(intA, intA));
	EXPECT_EQ("(int<#a>,int<#a>)", toString(*pair));
	FunctionTypePtr binary = builder.functionType(pair->getElementTypes(), intA);
	EXPECT_EQ("((int<#a>,int<#a>)->int<#a>)", toString(*binary));

	// define literals
	LiteralPtr x = builder.literal(int2, "1");
	EXPECT_EQ("1", toString(*x));

	LiteralPtr y = builder.literal(int4, "2");
	EXPECT_EQ("2", toString(*y));

	LiteralPtr constFun = builder.literal(nullary, "zero");
	LiteralPtr unaryFun = builder.literal(unary, "succ");
	LiteralPtr binaryFun = builder.literal(binary, "sum");


	ExpressionPtr expr = constFun;

	CheckPtr typeCheck = make_check<CallExprTypeCheck>();

	// correct examples ...
	expr = builder.callExpr(intA, constFun, toVector<ExpressionPtr>());
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(int2, unaryFun, toVector<ExpressionPtr>(x));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(int4, unaryFun, toVector<ExpressionPtr>(y));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(int2, binaryFun, toVector<ExpressionPtr>(x,x));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(int4, binaryFun, toVector<ExpressionPtr>(x,y));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	expr = builder.callExpr(int4, binaryFun, toVector<ExpressionPtr>(y,y));
	EXPECT_EQ("[]", toString(check(expr, typeCheck)));

	// invalid return type
	MessageList issues;
	expr = builder.callExpr(int2, unaryFun, toVector<ExpressionPtr>(y));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

	expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(x,y));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

	expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(y,y));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

	// invalid argument types
	TypePtr concreteType2 = builder.genericType("int", toVector<TypePtr>(), toVector<IntTypeParamPtr>(ConcreteIntTypeParam::get(manager, 2)));
	LiteralPtr z = builder.literal(concreteType2, "3");
	EXPECT_EQ("3", toString(*z));

	TypePtr boolType = builder.genericType("bool");
	LiteralPtr w = builder.literal(boolType, "true");
	EXPECT_EQ("true", toString(*w));

	// => not unifyable arguments (but forming a sub-type) ...
	expr = builder.callExpr(int4, binaryFun, toVector<ExpressionPtr>(y,z));
	issues = check(expr, typeCheck);
	EXPECT_TRUE(issues.empty()) << issues;

	// => not unifyable arguments
	expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(y,w));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

	// => wrong number of arguments
	expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(x));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS, "", Message::ERROR));

	expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(x,y,z));
	issues = check(expr, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS, "", Message::ERROR));
}

TEST(FunctionKindCheck, Basic) {

	NodeManager manager;
	IRBuilder builder(manager);

	// build some ingredients
	TypePtr A = builder.genericType("A");
	TypePtr refA = builder.refType(A);

	// ------- function kind -----------

	// use an invalid function kind
	FunctionTypePtr funType = builder.functionType(toVector(A), A, ((FunctionKind)777));
	EXPECT_EQ((unsigned)777, funType->getFunctionKind()->getValue());

	// check function type
	auto issues = check(funType);	// use full checks
	EXPECT_EQ((std::size_t)1, issues.size()) << issues;
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_FUNCTION_TYPE_KIND, "", Message::ERROR));

	// use a valid function type
	funType = builder.functionType(toVector(A), A, FK_PLAIN);
	EXPECT_EQ((unsigned)FK_PLAIN, funType->getFunctionKind()->getValue());
	issues = check(funType);	// use full checks
	EXPECT_TRUE(issues.empty()) << issues;


	// -------- object type -----------

	// an example that is correct
	funType = builder.functionType(toVector(refA), refA, FK_CONSTRUCTOR);
	issues = check(funType);
	EXPECT_TRUE(issues.empty()) << issues;

	funType = builder.functionType(toVector(refA), refA, FK_DESTRUCTOR);
	issues = check(funType);
	EXPECT_TRUE(issues.empty()) << issues;

	funType = builder.functionType(toVector(refA), A, FK_MEMBER_FUNCTION);
	issues = check(funType);
	EXPECT_TRUE(issues.empty()) << issues;

	// an few invalid example
	funType = builder.functionType(toVector(A), A, FK_CONSTRUCTOR);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));

	funType = builder.functionType(toVector(A), A, FK_DESTRUCTOR);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));

	funType = builder.functionType(toVector(A), A, FK_MEMBER_FUNCTION);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));

	funType = builder.functionType(TypeList(), A, FK_CONSTRUCTOR);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));

	funType = builder.functionType(TypeList(), A, FK_DESTRUCTOR);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));

	funType = builder.functionType(TypeList(), A, FK_MEMBER_FUNCTION);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));


	// -------- destructor parameters -----------

	// cases that are fine
	funType = builder.functionType(toVector(refA), refA, FK_DESTRUCTOR);
	issues = check(funType);
	EXPECT_TRUE(issues.empty()) << issues;

	// invalid case
	funType = builder.functionType(toVector(refA, A), refA, FK_DESTRUCTOR);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_DESTRUCTOR_PARAMETERS, "", Message::ERROR));


	// -------- constructor return type -----------

	// cases that are fine
	funType = builder.functionType(toVector(refA, A), refA, FK_CONSTRUCTOR);
	issues = check(funType);
	EXPECT_TRUE(issues.empty()) << issues;

	// invalid case
	funType = builder.functionType(toVector(refA, A), A, FK_CONSTRUCTOR);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_CONSTRUCTOR_RETURN_TYPE, "", Message::ERROR));


	// -------- destructor return type -----------

	// cases that are fine
	funType = builder.functionType(toVector(refA), refA, FK_DESTRUCTOR);
	issues = check(funType);
	EXPECT_TRUE(issues.empty()) << issues;

	// invalid case
	funType = builder.functionType(toVector(refA), A, FK_DESTRUCTOR);
	issues = check(funType);
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_DESTRUCTOR_RETURN_TYPE, "", Message::ERROR));

}

TEST(FunctionType, Basic) {

	NodeManager manager;
	IRBuilder builder(manager);

	// check whether variations of function types are tolleratred
	//  options: ctors, dtors, member function types and plain functions

	// create some types
	TypePtr A = builder.genericType("A");
	TypePtr refA = builder.refType(A);
	TypePtr B = builder.genericType("B");

	// create variations of function types
	FunctionTypePtr funTypeA = builder.functionType(toVector(refA, B), refA, FK_PLAIN);
	FunctionTypePtr funTypeB = builder.functionType(toVector(refA, B), refA, FK_CONSTRUCTOR);
	FunctionTypePtr funTypeC = builder.functionType(toVector(refA, B), refA, FK_MEMBER_FUNCTION);
	FunctionTypePtr funTypeD = builder.functionType(toVector(refA), refA, FK_DESTRUCTOR);


	// create lambda body
	StatementPtr body = builder.returnStmt(builder.refNew(builder.undefined(A)));

	// create lambdas
	auto params = toVector(builder.variable(refA), builder.variable(B));
	auto funA = builder.lambdaExpr(funTypeA, params, body);
	auto funB = builder.lambdaExpr(funTypeB, params, body);
	auto funC = builder.lambdaExpr(funTypeC, params, body);
	auto funD = builder.lambdaExpr(funTypeD, toVector(params[0]), body);

	// all those should be typed correctly
	CheckPtr typeCheck = make_check<FunctionTypeCheck>();
	EXPECT_EQ("[]", toString(check(funA, typeCheck)));
	EXPECT_EQ("[]", toString(check(funB, typeCheck)));
	EXPECT_EQ("[]", toString(check(funC, typeCheck)));
	EXPECT_EQ("[]", toString(check(funD, typeCheck)));


	// also check some faulty configurations
	params = toVector(builder.variable(B), builder.variable(refA));

	ExpressionPtr fun = builder.lambdaExpr(funTypeA, params, body);
	auto issues = check(fun, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(fun), EC_TYPE_INVALID_FUNCTION_TYPE, "", Message::ERROR));

	fun = builder.lambdaExpr(funTypeB, params, body);
	issues = check(fun, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(fun), EC_TYPE_INVALID_FUNCTION_TYPE, "", Message::ERROR));

	fun = builder.lambdaExpr(funTypeC, params, body);
	issues = check(fun, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(fun), EC_TYPE_INVALID_FUNCTION_TYPE, "", Message::ERROR));

	fun = builder.lambdaExpr(funTypeD, toVector(params[0]), body);
	issues = check(fun, typeCheck);
	EXPECT_EQ((std::size_t)1, issues.size());
	EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(fun), EC_TYPE_INVALID_FUNCTION_TYPE, "", Message::ERROR));

}

TEST(ParentCheck, Basic) {

	NodeManager manager;
	IRBuilder builder(manager);

	// create a few parent nodes
	auto ok1 = builder.parent(builder.genericType("A"));
	auto ok2 = builder.parent(builder.structType());
	auto ok3 = builder.parent(builder.typeVariable("a"));
	auto ok4 = builder.parent(builder.parseType("let a,b = struct { ref<b> x; }, struct { ref<a> x; } in a"));

	auto err1 = builder.parent(builder.parseType("union { int<4> x; }"));
	auto err2 = builder.parent(builder.parseType("(A,B)->R"));
	auto err3 = builder.parent(builder.parseType("ref<A>"));
	auto err4 = builder.parent(builder.parseType("array<A,1>"));
	auto err5 = builder.parent(builder.parseType("vector<A,2>"));
	auto err6 = builder.parent(builder.parseType("channel<A,2>"));

	// check the correct types
	EXPECT_TRUE(check(ok1).empty()) << check(ok1);
	EXPECT_TRUE(check(ok2).empty()) << check(ok2);
	EXPECT_TRUE(check(ok3).empty()) << check(ok3);
	EXPECT_TRUE(check(ok4).empty()) << check(ok4);

	// check the invalid types
	EXPECT_PRED2(containsMSG, check(err1), Message(NodeAddress(err1), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err2), Message(NodeAddress(err2), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err3), Message(NodeAddress(err3), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err4), Message(NodeAddress(err4), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err5), Message(NodeAddress(err5), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err6), Message(NodeAddress(err6), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));

}

TEST(StructExprTypeCheck, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// some preparations
	core::StringValuePtr name = builder.stringValue("x");
	core::TypePtr typeA = builder.genericType("A");
	core::TypePtr typeB = builder.genericType("B");

	core::ExpressionPtr valueA = builder.literal(typeA, "a");
	core::ExpressionPtr valueB = builder.literal(typeB, "b");

	NamedTypeList entries;
	entries.push_back(builder.namedType(name, typeA));
	core::StructTypePtr structType = builder.structType(entries);


	// create struct expression
	NamedValueList members;
	members.push_back(builder.namedValue(name, valueA));
	core::StructExprPtr ok = builder.structExpr(structType, members);

	members.clear();
	members.push_back(builder.namedValue(name, valueB));
	core::StructExprPtr err = builder.structExpr(structType, members);

	// conduct checks
	CheckPtr typeCheck = make_check<StructExprTypeCheck>();

	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_INITIALIZATION_EXPR, "", Message::ERROR));
}

TEST(MemberAccessElementTypeCheck, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = builder.getLangBasic();

	// get function to be tested
	LiteralPtr fun = basic.getCompositeMemberAccess();

	// Create a example expressions
	TypePtr typeA = builder.genericType("typeA");
	TypePtr typeB = builder.genericType("typeB");
	TypePtr typeC = builder.genericType("typeC");

	StringValuePtr identA = builder.stringValue("a");
	StringValuePtr identB = builder.stringValue("b");
	StringValuePtr identC = builder.stringValue("c");

	NamedTypeList entries;
	entries.push_back(builder.namedType(identA, typeA));
	entries.push_back(builder.namedType(identB, typeB));

	TypePtr structType = builder.structType(entries);
	VariablePtr var = builder.variable(structType);
	VariablePtr var2 = builder.variable(typeA);

	ExpressionPtr ok = builder.callExpr(fun, var, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeA));
	ExpressionPtr err1 = builder.callExpr(fun, var, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeB));
	ExpressionPtr err2 = builder.callExpr(fun, var, builder.getIdentifierLiteral(identC), builder.getTypeLiteral(typeB));
	ExpressionPtr err3 = builder.callExpr(fun, var2, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeA));
	ExpressionPtr err4 = builder.callExpr(fun, var2, var, builder.getTypeLiteral(typeA));


	CheckPtr typeCheck = make_check<MemberAccessElementTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err1, typeCheck).empty());
	ASSERT_FALSE(check(err2, typeCheck).empty());
	ASSERT_TRUE(check(err3, typeCheck).empty()); // having a member access in a generic type is no longer an error
	ASSERT_FALSE(check(err4, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err1,typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err2,typeCheck), Message(NodeAddress(err2), EC_TYPE_NO_SUCH_MEMBER, "", Message::ERROR));
	//EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_ACCESSING_MEMBER_OF_NON_NAMED_COMPOSITE_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err4,typeCheck), Message(NodeAddress(err4), EC_TYPE_INVALID_IDENTIFIER, "", Message::ERROR));
}

TEST(MemberAccessElementTypeCheck, References) {
	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = builder.getLangBasic();

	// get function to be tested
	ExpressionPtr fun = basic.getCompositeRefElem();

	// Create a example expressions
	TypePtr typeA = builder.genericType("typeA");
	TypePtr typeB = builder.genericType("typeB");
	TypePtr typeC = builder.genericType("typeC");

	TypePtr typeRefA = builder.refType(typeA);
	TypePtr typeRefB = builder.refType(typeB);
	TypePtr typeRefC = builder.refType(typeC);

	StringValuePtr identA = builder.stringValue("a");
	StringValuePtr identB = builder.stringValue("b");
	StringValuePtr identC = builder.stringValue("c");

	NamedCompositeType::Entries entries;
	entries.push_back(builder.namedType(identA, typeA));
	entries.push_back(builder.namedType(identB, typeB));

	TypePtr structType = builder.structType(entries);
	TypePtr structRefType = builder.refType(structType);

	VariablePtr var = builder.variable(structRefType);
	VariablePtr var2 = builder.variable(typeRefA);

	ExpressionPtr ok = builder.callExpr(fun, var, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeA));
	ExpressionPtr err1 = builder.callExpr(fun, var, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeB));
	ExpressionPtr err2 = builder.callExpr(fun, var, builder.getIdentifierLiteral(identC), builder.getTypeLiteral(typeB));
	ExpressionPtr err3 = builder.callExpr(fun, var2, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeA));
	ExpressionPtr err4 = builder.callExpr(fun, var2, var, builder.getTypeLiteral(typeA));


	CheckPtr typeCheck = make_check<MemberAccessElementTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err1, typeCheck).empty());
	ASSERT_FALSE(check(err2, typeCheck).empty());
	ASSERT_TRUE(check(err3, typeCheck).empty());  //having a member access on a generic typed var is no longer an error
	ASSERT_FALSE(check(err4, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err1,typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err2,typeCheck), Message(NodeAddress(err2), EC_TYPE_NO_SUCH_MEMBER, "", Message::ERROR));
	//EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_ACCESSING_MEMBER_OF_NON_NAMED_COMPOSITE_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err4,typeCheck), Message(NodeAddress(err4), EC_TYPE_INVALID_IDENTIFIER, "", Message::ERROR));
}

TEST(ComponentAccessTypeCheck, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = builder.getLangBasic();

	// get function to be tested
	LiteralPtr fun = basic.getTupleMemberAccess();

	// Create a example expressions
	TypePtr typeA = builder.genericType("typeA");
	TypePtr typeB = builder.genericType("typeB");
	TypePtr typeC = builder.genericType("typeC");

	TypePtr tupleType = builder.tupleType({ typeA, typeB, typeC });
	VariablePtr var = builder.variable(tupleType);
	VariablePtr var2 = builder.variable(typeA);

	ExpressionPtr ok = builder.callExpr(fun, var, builder.uintLit(0), builder.getTypeLiteral(typeA));
	ExpressionPtr err1 = builder.callExpr(fun, var, builder.uintLit(0), builder.getTypeLiteral(typeB));
	ExpressionPtr err2 = builder.callExpr(fun, var, builder.uintLit(5), builder.getTypeLiteral(typeB));
	ExpressionPtr err3 = builder.callExpr(fun, var2, builder.uintLit(0), builder.getTypeLiteral(typeA));
	ExpressionPtr err4 = builder.callExpr(fun, var2, var, builder.getTypeLiteral(typeA));


	CheckPtr typeCheck = make_check<ComponentAccessTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err1, typeCheck).empty());
	ASSERT_FALSE(check(err2, typeCheck).empty());
	ASSERT_FALSE(check(err3, typeCheck).empty());
	ASSERT_FALSE(check(err4, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err1,typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err2,typeCheck), Message(NodeAddress(err2), EC_TYPE_NO_SUCH_MEMBER, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_ACCESSING_MEMBER_OF_NON_TUPLE_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err4,typeCheck), Message(NodeAddress(err4), EC_TYPE_INVALID_TUPLE_INDEX, "", Message::ERROR));
}

TEST(ComponentAccessTypeCheck, References) {
	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = builder.getLangBasic();

	// get function to be tested
	LiteralPtr fun = basic.getTupleRefElem();

	// Create a example expressions
	TypePtr typeA = builder.genericType("typeA");
	TypePtr typeB = builder.genericType("typeB");
	TypePtr typeC = builder.genericType("typeC");

	TypePtr typeRefA = builder.refType(typeA);
	TypePtr typeRefB = builder.refType(typeB);
	TypePtr typeRefC = builder.refType(typeC);

	TypePtr tupleType = builder.tupleType({ typeA, typeB, typeC });
	VariablePtr var = builder.variable(builder.refType(tupleType));
	VariablePtr var2 = builder.variable(typeRefC);

	ExpressionPtr ok = builder.callExpr(fun, var, builder.uintLit(0), builder.getTypeLiteral(typeA));
	ExpressionPtr err1 = builder.callExpr(fun, var, builder.uintLit(0), builder.getTypeLiteral(typeB));
	ExpressionPtr err2 = builder.callExpr(fun, var, builder.uintLit(5), builder.getTypeLiteral(typeB));
	ExpressionPtr err3 = builder.callExpr(fun, var2, builder.uintLit(0), builder.getTypeLiteral(typeA));
	ExpressionPtr err4 = builder.callExpr(fun, var2, var, builder.getTypeLiteral(typeA));


	CheckPtr typeCheck = make_check<ComponentAccessTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err1, typeCheck).empty());
	ASSERT_FALSE(check(err2, typeCheck).empty());
	ASSERT_FALSE(check(err3, typeCheck).empty());
	ASSERT_FALSE(check(err4, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err1,typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err2,typeCheck), Message(NodeAddress(err2), EC_TYPE_NO_SUCH_MEMBER, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_ACCESSING_MEMBER_OF_NON_TUPLE_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err4,typeCheck), Message(NodeAddress(err4), EC_TYPE_INVALID_TUPLE_INDEX, "", Message::ERROR));
}

TEST(ReturnTypeCheck, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = manager.getLangBasic();

	// create a function type (for all those functions)
	TypePtr resultType = basic.getInt4();
	FunctionTypePtr funType = builder.functionType(TypeList(), resultType);

	// create a function where everything is correct
	StatementPtr body = builder.returnStmt(builder.literal(resultType, "1"));
	LambdaPtr ok1 = builder.lambda(funType, VariableList(), body);

	// create a function where return type is wrong - and nested
	body = builder.returnStmt(builder.literal(basic.getInt2(), "1"));
	body = builder.compoundStmt(body);
	LambdaPtr ok2 = builder.lambda(funType, VariableList(), body);

	body = builder.returnStmt(builder.literal(basic.getInt8(), "1"));
	body = builder.compoundStmt(body);
	LambdaPtr err1 = builder.lambda(funType, VariableList(), body);

	CheckPtr returnTypeCheck = make_check<ReturnTypeCheck>();
	EXPECT_TRUE(check(ok1, returnTypeCheck).empty());
	EXPECT_TRUE(check(ok2, returnTypeCheck).empty());

	EXPECT_FALSE(check(err1, returnTypeCheck).empty()) ;

	EXPECT_PRED2(containsMSG, check(err1,returnTypeCheck), Message(NodeAddress(err1).getAddressOfChild(2,0), EC_TYPE_INVALID_RETURN_VALUE_TYPE, "", Message::ERROR));
}

TEST(DeclarationStmtTypeCheck, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

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

TEST(DeclarationStmtTypeCheck, SubTypes) {
	NodeManager manager;
	IRBuilder builder(manager);
	auto& basic = manager.getLangBasic();

	// OK ... create a function literal
	TypePtr typeA = basic.getInt4();
	TypePtr typeB = basic.getInt8();
	ExpressionPtr init = builder.literal(typeA, "4");
	DeclarationStmtPtr ok = builder.declarationStmt(builder.variable(typeB), builder.literal(typeA, "4"));
	DeclarationStmtPtr err = builder.declarationStmt(builder.variable(typeA), builder.literal(typeB, "4"));

	CheckPtr typeCheck = make_check<DeclarationStmtTypeCheck>();
 	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_INITIALIZATION_EXPR, "", Message::ERROR));
}

TEST(DeclarationStmtTypeCheck, RecursiveTypes) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a function literal
	RecTypePtr typeA = builder.parseType("let t = struct { A a; ref<t> next; } in t").as<RecTypePtr>();
	TypePtr typeB = typeA->unroll();

	// all of the following should be supported
	DeclarationStmtPtr ok0 = builder.declarationStmt(builder.variable(typeA), builder.literal(typeA, "X"));
	DeclarationStmtPtr ok1 = builder.declarationStmt(builder.variable(typeA), builder.literal(typeB, "X"));
	DeclarationStmtPtr ok2 = builder.declarationStmt(builder.variable(typeB), builder.literal(typeA, "X"));
	DeclarationStmtPtr ok3 = builder.declarationStmt(builder.variable(typeB), builder.literal(typeB, "X"));

	CheckPtr typeCheck = make_check<DeclarationStmtTypeCheck>();
 	EXPECT_TRUE(check(ok0, typeCheck).empty()) << check(ok0, typeCheck);
 	EXPECT_TRUE(check(ok1, typeCheck).empty()) << check(ok1, typeCheck);
 	EXPECT_TRUE(check(ok2, typeCheck).empty()) << check(ok2, typeCheck);
 	EXPECT_TRUE(check(ok3, typeCheck).empty()) << check(ok3, typeCheck);
}

TEST(IfCondition, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a function literal
	TypePtr intType = builder.genericType("int");
	TypePtr boolType = builder.genericType("bool");
	ExpressionPtr intLit = builder.literal(intType, "4");
	ExpressionPtr boolLit = builder.literal(boolType, "true");
	NodePtr ok = builder.ifStmt(boolLit, builder.getNoOp());
	NodePtr err = builder.ifStmt(intLit, builder.getNoOp());

	CheckPtr typeCheck = make_check<IfConditionTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_CONDITION_EXPR, "", Message::ERROR));
}

TEST(ForStmt, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);
	auto& basic = manager.getLangBasic();

	// OK ... create a for stmt
	TypePtr intType = basic.getInt4();
	TypePtr boolType = basic.getBool();

	ExpressionPtr intLit = builder.literal(intType, "4");
	ExpressionPtr boolLit = builder.literal(boolType, "true");

	VariablePtr intVar = builder.variable(intType,1);
	VariablePtr boolVar = builder.variable(boolType,1);


	NodePtr ok = builder.forStmt(intVar, intLit, intLit, intLit, intLit);
	NodePtr err1 = builder.forStmt(boolVar, boolLit, boolLit, boolLit, boolLit);
	NodePtr err2 = builder.forStmt(intVar, intLit, boolLit, intLit, boolLit);
	NodePtr err3 = builder.forStmt(intVar, intLit, intLit, boolLit, boolLit);

	CheckPtr typeCheck = make_check<ForStmtTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err1,typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_ITERATOR_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err2,typeCheck), Message(NodeAddress(err2), EC_TYPE_INVALID_BOUNDARY_TYPE, "", Message::ERROR));
	EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_INVALID_BOUNDARY_TYPE, "", Message::ERROR));

}

TEST(WhileCondition, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a function literal
	TypePtr intType = builder.genericType("int");
	TypePtr boolType = builder.genericType("bool");
	ExpressionPtr intLit = builder.literal(intType, "4");
	ExpressionPtr boolLit = builder.literal(boolType, "true");
	NodePtr ok = builder.whileStmt(boolLit, builder.getNoOp());
	NodePtr err = builder.whileStmt(intLit, builder.getNoOp());

	CheckPtr typeCheck = make_check<WhileConditionTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_CONDITION_EXPR, "", Message::ERROR));
}

TEST(Switch, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a function literal
	TypePtr intType = builder.getLangBasic().getInt1();
	TypePtr boolType = builder.genericType("bool");
	ExpressionPtr intLit = builder.literal(intType, "4");
	ExpressionPtr boolLit = builder.literal(boolType, "true");
	SwitchStmtPtr ok = builder.switchStmt(intLit, vector<SwitchCasePtr>());
	SwitchStmtPtr err = builder.switchStmt(boolLit, vector<SwitchCasePtr>());

	CheckPtr typeCheck = make_check<SwitchExpressionTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_SWITCH_EXPR, "", Message::ERROR));
}

TEST(BuildInLiterals, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a function literal

	LiteralPtr ok = builder.getLangBasic().getFalse();
	LiteralPtr err = builder.literal(builder.genericType("strangeType"), ok->getValue());

	CheckPtr typeCheck = make_check<BuiltInLiteralCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_TYPE_OF_LITERAL, "", Message::WARNING));
}

TEST(RefCastExpr, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

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
	NodeManager manager;
	IRBuilder builder(manager);

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
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create correct and wrong instances

	TypePtr element = builder.genericType("A");
	IntTypeParamPtr param = builder.concreteIntTypeParam(8);

	CheckPtr typeCheck = make_check<KeywordCheck>();

	// test vector
	{
		TypePtr ok = builder.vectorType(element, param);
		TypePtr err = builder.genericType("vector", toVector<TypePtr>(element), toVector<IntTypeParamPtr>(param));

		EXPECT_FALSE(*ok == *err);

		EXPECT_TRUE(check(ok, typeCheck).empty());
		EXPECT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
	}

	// test array
	{
		TypePtr ok = builder.arrayType(element, param);
		TypePtr err = builder.genericType("array", toVector<TypePtr>(element), toVector<IntTypeParamPtr>(param));

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
		TypePtr err = builder.genericType("channel", toVector<TypePtr>(element), toVector<IntTypeParamPtr>(param));

		EXPECT_FALSE(*ok == *err);

		EXPECT_TRUE(check(ok, typeCheck).empty());
		EXPECT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
	}
}

TEST(ExternalFunctionType, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a function literal
	TypePtr intType = builder.genericType("int");
	TypePtr boolType = builder.genericType("bool");
	FunctionTypePtr funTypeOK  = builder.functionType(toVector(intType), boolType, FK_PLAIN);
	FunctionTypePtr funTypeERR = builder.functionType(toVector(intType), boolType, FK_CLOSURE);

	NodePtr ok = builder.literal(funTypeOK, "fun");
	NodePtr err = builder.literal(funTypeERR, "fun");

	CheckPtr typeCheck = make_check<ExternalFunctionTypeCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_FUNCTION_TYPE, "", Message::ERROR));
}

TEST(LambdaExprType, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);
	auto& basic = manager.getLangBasic();

	// build a lambda expression which is fine
	LambdaExprPtr lambda = builder.parse(
			"let int = int<4> in (int a)->int { return a; }"
	).as<LambdaExprPtr>();

	ASSERT_TRUE(lambda);

	// get addresses to all kind of variables
	VariableAddress outer = LambdaExprAddress(lambda)->getVariable();
	VariableAddress inner = LambdaExprAddress(lambda)->getDefinition()[0]->getVariable();

	// check a correct version
	CheckPtr typeCheck = make_check<LambdaTypeCheck>();
	EXPECT_TRUE(check(lambda, typeCheck).empty()) << check(lambda, typeCheck);


	// build an invalid variable as a replacement
	VariablePtr invalid = builder.variable(outer->getType());

	// case 1 - lambda expression selects non-existing body
	auto err = transform::replaceNode(manager, outer, invalid).as<LambdaExprPtr>();

	auto errors = check(err,typeCheck);
	EXPECT_EQ(1u, errors.size()) << errors;
	EXPECT_PRED2(containsMSG, errors, Message(NodeAddress(err), EC_TYPE_INVALID_LAMBDA_EXPR_NO_SUCH_DEFINITION, "", Message::ERROR));

	// case 2 - wrong type of lambda expression
	FunctionTypePtr invalidType = builder.functionType(TypeList(), basic.getUnit());
	err = transform::replaceNode(manager, LambdaExprAddress(lambda)->getType(), invalidType).as<LambdaExprPtr>();

	errors = check(err,typeCheck);
	EXPECT_EQ(1u, errors.size()) << errors;
	EXPECT_PRED2(containsMSG, errors, Message(NodeAddress(err), EC_TYPE_INVALID_LAMBDA_EXPR_TYPE, "", Message::ERROR));


	// case 3 - use invalid variable type in lambda
	invalid = builder.variable(invalidType);
	err = transform::replaceNode(manager,
				inner.switchRoot(transform::replaceNode(manager, outer.switchRoot(err), invalid).as<LambdaExprPtr>())
			, invalid).as<LambdaExprPtr>();
	errors = check(err,typeCheck);
	EXPECT_EQ(1u, errors.size()) << errors;
	EXPECT_PRED2(containsMSG, errors, Message(NodeAddress(err), EC_TYPE_INVALID_LAMBDA_REC_VAR_TYPE, "", Message::ERROR));


	// case 4 - wrong lambda type
	VariablePtr param = lambda->getLambda()->getParameterList()[0];
	VariablePtr invalidParam = builder.variable(basic.getFloat());

	err = transform::replaceAll(manager,lambda, param, invalidParam, false).as<LambdaExprPtr>();
	errors = check(err,typeCheck);
	EXPECT_EQ(1u, errors.size()) << errors;
	EXPECT_PRED2(containsMSG, errors, Message(NodeAddress(err), EC_TYPE_INVALID_LAMBDA_TYPE, "", Message::ERROR));


}

TEST(ArrayTypeChecks, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	CheckPtr typeCheck = getFullCheck();

	// create simple, context less array type
	TypePtr element = manager.getLangBasic().getInt4();
	TypePtr arrayType = builder.arrayType(element);

	TypePtr cur;

	// test something without context
	EXPECT_TRUE(check(arrayType, typeCheck).empty()) << check(arrayType, typeCheck);

	auto errors = check(arrayType, typeCheck);

	// ----- struct ------

	// test it within a struct
	cur = builder.structType(toVector(builder.namedType("a", arrayType)));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;

	// .. a bigger struct
	cur = builder.structType(toVector(builder.namedType("c", element), builder.namedType("a", arrayType)));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;

	// it has to be the last element
	cur = builder.structType(toVector(builder.namedType("a", arrayType), builder.namedType("c", element)));
	errors = check(cur, typeCheck);
	EXPECT_FALSE(errors.empty()) << errors;
	EXPECT_EQ(1u, errors.size());
	EXPECT_PRED2(containsMSG, check(cur, typeCheck), Message(NodeAddress(cur), EC_TYPE_INVALID_ARRAY_CONTEXT, "", Message::ERROR));

	// and must not be present multiple times
	cur = builder.structType(toVector(builder.namedType("a", arrayType), builder.namedType("b", element), builder.namedType("c", arrayType)));
	errors = check(cur, typeCheck);
	EXPECT_FALSE(errors.empty()) << errors;
	EXPECT_EQ(1u, errors.size());
	EXPECT_PRED2(containsMSG, check(cur, typeCheck), Message(NodeAddress(cur), EC_TYPE_INVALID_ARRAY_CONTEXT, "", Message::ERROR));

	// may be nested inside another struct
	cur = builder.structType(toVector(builder.namedType("a", builder.structType(toVector(builder.namedType("a", arrayType))))));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;


	// ----- union ------

	// also union
	cur = builder.unionType(toVector(builder.namedType("c", element), builder.namedType("a", arrayType)));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;

	// here the order is not important
	cur = builder.unionType(toVector(builder.namedType("a", arrayType), builder.namedType("c", element)));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;


	// ----- tuples ------

	// test it within a tuple
	cur = builder.tupleType(toVector(arrayType));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;

	// .. a bigger tuple
	cur = builder.tupleType(toVector(element, arrayType));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;

	// it has to be the last element
	cur = builder.tupleType(toVector(arrayType, element));
	errors = check(cur, typeCheck);
	EXPECT_FALSE(errors.empty()) << errors;
	EXPECT_EQ(1u, errors.size());
	EXPECT_PRED2(containsMSG, check(cur, typeCheck), Message(NodeAddress(cur), EC_TYPE_INVALID_ARRAY_CONTEXT, "", Message::ERROR));

	// and must not be present multiple times
	cur = builder.tupleType(toVector(arrayType, element, arrayType));
	errors = check(cur, typeCheck);
	EXPECT_FALSE(errors.empty()) << errors;
	EXPECT_EQ(1u, errors.size());
	EXPECT_PRED2(containsMSG, check(cur, typeCheck), Message(NodeAddress(cur), EC_TYPE_INVALID_ARRAY_CONTEXT, "", Message::ERROR));

	// variable size tuple must not be nested inside a non-reference
	cur = builder.structType(toVector(builder.namedType("a", builder.tupleType(toVector(arrayType)))));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;

	// also, there must not be a value of type array
	ExpressionPtr exp = builder.literal("val", arrayType);
	errors = check(exp, typeCheck);
	EXPECT_FALSE(errors.empty()) << errors;
	EXPECT_EQ(1u, errors.size());
	EXPECT_PRED2(containsMSG, check(exp, typeCheck), Message(NodeAddress(exp), EC_TYPE_INVALID_ARRAY_VALUE, "", Message::ERROR));

	exp = builder.literal("val", builder.refType(arrayType));
	errors = check(exp, typeCheck);
	EXPECT_TRUE(errors.empty()) << errors;
}


TEST(NarrowExpression, Basic) {

	NodeManager manager;
	IRBuilder builder(manager);
	CheckPtr typeCheck = getFullCheck();

	NodePtr res = analysis::normalize(builder.parse(
		"{"
		" let inner = struct{ int<4> a;};"
		" let two   = struct{ inner a; int<4> b;};"
		" ref<two> obj; "
		" ref<int<4>> inside = ref.narrow( obj, dp.member(dp.root, lit(\"b\")), lit(int<4>));"
		" ref<int<4>> morein = ref.narrow( obj, dp.member(dp.member(dp.root, lit(\"a\")),lit(\"a\")), lit(int<4>));"
		"}"
	));
	ASSERT_TRUE (res);
	auto errors = check(res, typeCheck);
	EXPECT_TRUE(errors.empty()) << "Correct Narrow Test\n" << errors;
	EXPECT_EQ("{decl ref<struct<a:struct<a:int<4>>,b:int<4>>> v0 =  var(undefined(type<struct<a:struct<a:int<4>>,b:int<4>>>));decl ref<int<4>> v1 = ref.narrow(v0, dp.root.b, type<int<4>>);decl ref<int<4>> v2 = ref.narrow(v0, dp.root.a.a, type<int<4>>);}",
			  toString(printer::PrettyPrinter(res, printer::PrettyPrinter::PRINT_SINGLE_LINE)));

	res = builder.parse(
		"{"
		" let inner = struct{ int<4> a;};"
		" let two   = struct{ inner a; int<4> b;};"
		" ref<two> obj; "
		" ref<int<4>> x = ref.narrow( obj, dp.member(dp.member(dp.root, lit(\"b\")),lit(\"a\")), lit(int<4>));"
		" ref<int<4>> y = ref.narrow( obj, dp.member(dp.member(dp.root, lit(\"a\")),lit(\"b\")), lit(int<4>));"
		" ref<int<8>> z = ref.narrow( obj, dp.member(dp.member(dp.root, lit(\"a\")),lit(\"a\")), lit(int<8>));"  // this test is no longer wrong since generic types can be narrowed
		"}"
	);
	ASSERT_TRUE (res);
	errors = check(res, typeCheck);
	EXPECT_EQ(2u, errors.size());
}

TEST(NarrowExpression, Parents) {

	NodeManager manager;
	IRBuilder builder(manager);

	NodePtr ok = builder.parse(
		"{"
		"	let int = int<4>;"
		"	let A = struct { int a; };"
		"	let B = struct : A { int b; };"
		"	ref<B> b;"
		"	auto ref2A = ref.narrow( b, dp.parent( dp.root, lit(A) ), lit(A) );"
		"	auto ref2a = ref.narrow( b, dp.member( dp.parent( dp.root, lit(A) ), lit(\"a\")), lit(int) );"
		"}"
	);

	ASSERT_TRUE(ok);

	EXPECT_TRUE(checks::check(ok).empty()) << checks::check(ok);

	NodePtr err = builder.parse(
		"{"
		"	let int = int<4>;"
		"	let A = struct { int a; };"
		"	let B = struct : A { int b; };"
		"	ref<B> b; "
		"	auto x = ref.narrow( b, dp.parent( dp.root, lit(B) ), lit(B) );"
		"}"
	);
	ASSERT_TRUE(err);

	auto errors = checks::check(err);
	EXPECT_FALSE(errors.empty());
	EXPECT_EQ(1u, errors.size());
}


TEST(ExpandExpression, Basic) {

	NodeManager manager;
	IRBuilder builder(manager);
	CheckPtr typeCheck = getFullCheck();

	NodePtr res = core::parser::parse(manager,
		"{"
		" let int   = int<4>;"
		" let inner = struct{ int a;};"
		" let outer = struct{ inner a; int b;};"
		""
		" ref<inner>  obj;"
		" obj.a = -15;"
		" ref<int> x = obj.a;"
		" ref<inner> exp = ref.expand(x, dp.member(dp.root, lit(\"a\")), lit(inner));"
		""
		" ref<outer> obj2; "
		" ref<inner> y = obj2.a;"
		" ref<int>   z = y.a;"
		" ref<outer> exp2 = ref.expand (z, dp.member (dp.member (dp.root, lit(\"a\")), lit(\"a\")), lit(outer));"
		"}"
		);
	ASSERT_TRUE (res);
	auto errors = check(res, typeCheck);
	EXPECT_TRUE(errors.empty()) << "Correct Narrow Test\n" << errors;

	res = core::parser::parse(manager,
		"{"
		" let int   = int<4>;"
		" let inner = struct{ int a;};"
		" let outer = struct{ inner a; int b;};"
		""
		" ref<inner>  obj;"
		" obj.a = -15;"
		" ref<int> x = obj.a;"
		" ref<inner> exp = ref.expand(x, dp.member(dp.root, lit(\"r\")), lit(inner));"
		""
		" ref<outer> obj2; "
		" ref<inner> y = obj2.a;"
		" ref<int>   z = y.a;"
		" ref<outer> exp2 = ref.expand (z, dp.member (dp.member (dp.root, lit(\"b\")), lit(\"a\")), lit(outer));"
		" ref<int>   exp3 = ref.expand (z, dp.member (dp.member (dp.root, lit(\"a\")), lit(\"a\")), lit(int));"
		"}"
		);
	ASSERT_TRUE (res);
	errors = check(res, typeCheck);
	EXPECT_EQ(2u, errors.size());
}

TEST(ExpandExpression, Parents) {

	NodeManager manager;
	IRBuilder builder(manager);

	NodePtr ok = builder.parse(
		"{"
		"	let int = int<4>;"
		"	let A = struct { int a; };"
		"	let B = struct : A { int b; };"
		"	ref<A> a;"
		"	auto ref2B = ref.expand( a, dp.parent( dp.root, lit(A) ), lit(B) );"
		"}"
	);

	ASSERT_TRUE(ok);
	EXPECT_TRUE(checks::check(ok).empty()) << checks::check(ok);

	NodePtr err = builder.parse(
		"{"
		"	let int = int<4>;"
		"	let A = struct { int a; };"
		"	let B = struct { int b; };"
		"	ref<A> a;"
		"	auto ref2B = ref.expand( a, dp.parent( dp.root, lit(A) ), lit(B) );"
		"}"
	);
	ASSERT_TRUE(err);

	auto errors = checks::check(err);
	EXPECT_FALSE(errors.empty());
	EXPECT_EQ(1u, errors.size());
}


TEST(ArrayTypeChecks, Exceptions) {
	NodeManager manager;
	IRBuilder builder(manager);
	auto& basic = manager.getLangBasic();

	CheckPtr typeCheck = getFullCheck();

	// create simple, context less array type
	TypePtr element = manager.getLangBasic().getInt4();
	TypePtr arrayType = builder.arrayType(element);

	NodePtr cur;
	auto errors = check(arrayType, typeCheck);

	// allow arrays to be used within type literals
	cur = builder.getTypeLiteral(arrayType);
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

	ExpressionPtr arrayPtr = builder.callExpr(basic.getArrayCreate1D(), builder.getTypeLiteral(element), builder.uintLit(12u));

	// also, allow array values to be used within ref.new, ref.var, struct, tuple and union expressions

	// ref.var
	cur = builder.refVar(arrayPtr);
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

	// ref.new
	cur = builder.refNew(arrayPtr);
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

	// struct expression
	cur = builder.structExpr(toVector(builder.namedValue("a", arrayPtr)));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

	// union expression
	UnionTypePtr unionType = builder.unionType(toVector(builder.namedType("a", arrayType)));
	cur = builder.unionExpr(unionType, builder.stringValue("a"), arrayPtr);
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

	// tuple expression
	cur = builder.tupleExpr(toVector(arrayPtr));
	errors = check(cur, typeCheck);
	EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

}

TEST(ClassMetaInfo, TargetType) {
	NodeManager manager;
	IRBuilder builder(manager);

	// create types to be tested
	TypePtr ok = builder.genericType("A");
	TypePtr err = builder.refType(ok);

	// create some class info object
	ClassMetaInfo info;
	ok->attachValue(info);
	err->attachValue(info);

	EXPECT_TRUE(check(ok).empty());
	EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err), EC_TYPE_ILLEGAL_OBJECT_TYPE, "", Message::ERROR));
}

TEST(ClassMetaInfo, InfoObjectType) {
	NodeManager manager;
	IRBuilder builder(manager);

	// create types to be tested
	auto ok = builder.genericType("A");
	auto err = ok;

	ClassMetaInfo infoA;
	infoA.addConstructor(builder.parse("A::() {}").as<LambdaExprPtr>());

	ClassMetaInfo infoB;
	infoB.addConstructor(builder.parse("B::() {}").as<LambdaExprPtr>());


	// check the correct version (no information)
	EXPECT_TRUE(check(ok).empty());

	// use correct extra information
	ok->attachValue(infoA);
	EXPECT_TRUE(check(ok).empty());

	// use invalid extra information
	err->attachValue(infoB);
	EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err), EC_TYPE_MISMATCHING_OBJECT_TYPE, "", Message::ERROR));

}


} // end namespace checks
} // end namespace core
} // end namespace insieme
