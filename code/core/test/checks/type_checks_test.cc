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

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/checks/type_checks.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/enum.h"

namespace insieme {
namespace core {
namespace checks {

	bool containsMSG(const MessageList& list, const Message& msg) {
		return contains(list.getAll(), msg);
	}

	bool getDetails = true;

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
		EXPECT_EQ("(()->int<'a>)", toString(*nullary));
		TupleTypePtr single = builder.tupleType(toVector(intA));
		EXPECT_EQ("(int<'a>)", toString(*single));
		FunctionTypePtr unary = builder.functionType(single->getElementTypes(), intA);
		EXPECT_EQ("((int<'a>)->int<'a>)", toString(*unary));
		TupleTypePtr pair = builder.tupleType(toVector(intA, intA));
		EXPECT_EQ("(int<'a>,int<'a>)", toString(*pair));
		FunctionTypePtr binary = builder.functionType(pair->getElementTypes(), intA);
		EXPECT_EQ("((int<'a>,int<'a>)->int<'a>)", toString(*binary));

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

		expr = builder.callExpr(int2, binaryFun, toVector<ExpressionPtr>(x, x));
		EXPECT_EQ("[]", toString(check(expr, typeCheck)));

		expr = builder.callExpr(int4, binaryFun, toVector<ExpressionPtr>(x, y));
		EXPECT_EQ("[]", toString(check(expr, typeCheck)));

		expr = builder.callExpr(int4, binaryFun, toVector<ExpressionPtr>(y, y));
		EXPECT_EQ("[]", toString(check(expr, typeCheck)));

		// invalid return type
		MessageList issues;
		expr = builder.callExpr(int2, unaryFun, toVector<ExpressionPtr>(y));
		issues = check(expr, typeCheck);
		EXPECT_EQ((std::size_t)1, issues.size());
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

		expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(x, y));
		issues = check(expr, typeCheck);
		EXPECT_EQ((std::size_t)1, issues.size());
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

		expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(y, y));
		issues = check(expr, typeCheck);
		EXPECT_EQ((std::size_t)1, issues.size());
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_RETURN_TYPE, "", Message::ERROR));

		// invalid argument types
		TypePtr concreteType2 = builder.parseType("int<2>");
		LiteralPtr z = builder.literal(concreteType2, "3");
		EXPECT_EQ("3", toString(*z));

		TypePtr boolType = builder.genericType("bool");
		LiteralPtr w = builder.literal(boolType, "true");
		EXPECT_EQ("true", toString(*w));

		// => not unifyable arguments (but forming a sub-type) ...
		expr = builder.callExpr(int4, binaryFun, toVector<ExpressionPtr>(y, z));
		issues = check(expr, typeCheck);
		EXPECT_TRUE(issues.empty()) << issues;

		// => not unifyable arguments
		expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(y, w));
		issues = check(expr, typeCheck);
		EXPECT_EQ((std::size_t)1, issues.size());
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// => wrong number of arguments
		expr = builder.callExpr(intA, binaryFun, toVector<ExpressionPtr>(x));
		issues = check(expr, typeCheck);
		EXPECT_EQ((std::size_t)1, issues.size());
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS, "", Message::ERROR));

		expr = builder.callExpr(intA, binaryFun,
			                    toVector<DeclarationPtr>(builder.declaration(int4, x), builder.declaration(int4, y), builder.declaration(int4, z)));
		issues = check(expr, typeCheck);
		EXPECT_EQ((std::size_t)1, issues.size());
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(expr), EC_TYPE_INVALID_NUMBER_OF_ARGUMENTS, "", Message::ERROR));
	}

	TEST(CallExprTypeCheck, ReferencePassing) {
		NodeManager manager;
		IRBuilder builder(manager);

		// Test: pass ref<A,f,f,plain> to A parameter must not be allowed

		// instantiate the type check
		CheckPtr typeCheck = make_check<CallExprTypeCheck>();

		CallExprPtr call;
		MessageList issues;

		auto test = [&](const std::string& paramType, const std::string& argType) {
			auto fun = builder.parseExpr("lit(\"fun\":(" + paramType + ")->unit)");
			auto arg = builder.parseExpr("lit(\"a\":" + argType + ")");
			call = builder.callExpr(manager.getLangBasic().getUnit(),fun,arg);
			issues = check(call,typeCheck);
		};

		// --- positive cases ---

		// passing a value by value should be fine
		test("A","A");
		EXPECT_TRUE(issues.empty()) << "Issues: " << issues;

		// passing by cpp ref should also be allowed
		test("ref<A,f,f,cpp_ref>","ref<A,f,f,cpp_ref>");
		EXPECT_TRUE(issues.empty()) << "Issues: " << issues;

		// also non-const to const
		test("ref<A,t,f,cpp_ref>","ref<A,f,f,cpp_ref>");
		EXPECT_TRUE(issues.empty()) << "Issues: " << issues;

		// or non-volatile to volatile
		test("ref<A,f,t,cpp_ref>","ref<A,f,f,cpp_ref>");
		EXPECT_TRUE(issues.empty()) << "Issues: " << issues;

		// or rvalue references
		test("ref<A,f,f,cpp_rref>","ref<A,f,f,cpp_rref>");
		EXPECT_TRUE(issues.empty()) << "Issues: " << issues;

		// or plain references
		test("ref<A,f,f,plain>","ref<A,f,f,plain>");
		EXPECT_TRUE(issues.empty()) << "Issues: " << issues;


		// pass a cpp_ref to a value should be fine
		test("A","ref<A,t,f,cpp_ref>");
		EXPECT_TRUE(issues.empty()) << "Issues: " << issues;

		// as well as an r-value reference
		test("A","ref<A,f,f,cpp_rref>");
		EXPECT_TRUE(issues.empty()) << "Issues: " << issues;

		// --- negative cases ---

		// can't pass unrelated values
		test("A","B");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a plain reference to a value
		test("A","ref<A,f,f,plain>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a const plain reference to a value
		test("A","ref<A,t,f,plain>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a volatile plain reference to a value
		test("A","ref<A,f,t,plain>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a const volatile plain reference to a value
		test("A","ref<A,t,t,plain>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a cpp_ref to a plain reference
		test("ref<A,f,f,plain>","ref<A,f,f,cpp_ref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a const cpp_ref to a plain reference
		test("ref<A,f,f,plain>","ref<A,t,f,cpp_ref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a volatile cpp_ref to a plain reference
		test("ref<A,f,f,plain>","ref<A,f,t,cpp_ref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a cpp_rref to a plain reference
		test("ref<A,f,f,plain>","ref<A,f,f,cpp_rref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a const cpp_rref to a plain reference
		test("ref<A,f,f,plain>","ref<A,t,f,cpp_rref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a volatile cpp_rref to a plain reference
		test("ref<A,f,f,plain>","ref<A,f,t,cpp_rref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// -- same for generic parameter types --

		// can't pass a cpp_ref to a plain reference
		test("ref<A,'c,'v,plain>","ref<A,f,f,cpp_ref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a const cpp_ref to a plain reference
		test("ref<A,'c,'v,plain>","ref<A,t,f,cpp_ref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a volatile cpp_ref to a plain reference
		test("ref<A,'c,'v,plain>","ref<A,f,t,cpp_ref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a cpp_rref to a plain reference
		test("ref<A,'c,'v,plain>","ref<A,f,f,cpp_rref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a const cpp_rref to a plain reference
		test("ref<A,'c,'v,plain>","ref<A,t,f,cpp_rref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

		// can't pass a volatile cpp_rref to a plain reference
		test("ref<A,'c,'v,plain>","ref<A,f,t,cpp_rref>");
		EXPECT_EQ(1,issues.size()) << "Issues: " << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(call), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));

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
		auto issues = check(funType); // use full checks
		EXPECT_EQ((std::size_t)1, issues.size()) << issues;
		EXPECT_PRED2(containsMSG, issues, Message(NodeAddress(funType), EC_TYPE_ILLEGAL_FUNCTION_TYPE_KIND, "", Message::ERROR));

		// use a valid function type
		funType = builder.functionType(toVector(A), A, FK_PLAIN);
		EXPECT_EQ((unsigned)FK_PLAIN, funType->getFunctionKind()->getValue());
		issues = check(funType); // use full checks
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

		funType = builder.functionType(toVector(refA), A, FK_VIRTUAL_MEMBER_FUNCTION);
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

		funType = builder.functionType(toVector(A), A, FK_VIRTUAL_MEMBER_FUNCTION);
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

		funType = builder.functionType(TypeList(), A, FK_VIRTUAL_MEMBER_FUNCTION);
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

	TEST(InvalidTypeInstantiationCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// Correct
		{
			auto test = builder.parseExpr(R"(
				let fun = lit("fun" : <'a>()->'a) in
				instantiate(lit("target_type" : <int>()->int), fun)()
			)");

			auto res = checks::check(test);
			EXPECT_TRUE(res.empty()) << res;
		}
		{
			auto test = builder.parseExpr(R"(
				let fun = lit("fun" : <'a, 'b>('b)->'a) in
				instantiate(lit("target_type" : <int<4>,uint<4>>(uint<4>)->int<4>), fun)(4u)
			)");

			auto res = checks::check(test);
			EXPECT_TRUE(res.empty()) << res;
		}

		// Instantiation type list arity mismatch
		{
			auto test = builder.parseExpr(R"(
				let fun = lit("fun" : <'a,'b>()->'a) in
				instantiate(lit("target_type" : <int>()->int), fun)
			)");

			auto res = checks::check(test);
			EXPECT_PRED2(containsMSG, res, Message(NodeAddress(test), EC_TYPE_ILLEGAL_FUNCTION_INSTANTIATION, "", Message::ERROR));
		}

		// Not a function type
		{
			auto test = builder.parseExpr(R"(
				instantiate(lit("target_type" : int<4>), lit("not_fun" : 'a))
			)");

			auto res = checks::check(test);
			EXPECT_PRED2(containsMSG, res, Message(NodeAddress(test), EC_TYPE_ILLEGAL_FUNCTION_INSTANTIATION, "", Message::ERROR));
		}

	}

	TEST(ParentCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// create a few parent nodes
		auto ok1 = builder.parent(builder.genericType("A"));
		auto ok2 = builder.parent(builder.structType());
		auto ok3 = builder.parent(builder.typeVariable("a"));
		auto ok4 = builder.parent(builder.parseType("def struct a { x : ref<b>; }; def struct b { x : ref<a>; }; a"));

		auto err1 = builder.parent(builder.parseType("union { x : int<4>; }"));
		auto err2 = builder.parent(builder.parseType("(A,B)->R"));
		auto err3 = builder.parent(builder.parseType("ref<A>"));
		auto err4 = builder.parent(builder.parseType("array<A,1u>"));
		auto err5 = builder.parent(builder.parseType("channel<A,2>"));

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
	}

	TEST(FreeTagTypeReferences, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// create a few parent nodes
		auto ok1 = builder.genericType("A");
		auto ok2 = builder.parseType("struct { x : int<4>; }");
		auto ok3 = builder.parseType("let list = struct { next : ref<list>; } in list");
		auto ok4 = builder.parseType("let list = struct { next : ptr<list>; } in list");

		std::map<string,NodePtr> symbols;
		symbols["a"] = builder.tagTypeReference("a");

		auto err0 = builder.parseType("a", symbols);
		auto err1 = builder.parseType("ref<a>", symbols);
		auto err2 = builder.parseType("struct { x : ref<a>; }", symbols);
		auto err3 = builder.parseType("let list = struct { next : ref<list>; value : ref<a>; } in list", symbols);
		auto err4 = builder.parseExpr("decl struct list; def struct list { next : ref<list>; value : ref<a>; }; lit(\"A\" : list)", symbols);

		// check the correct types
		EXPECT_TRUE(check(ok1).empty()) << check(ok1);
		EXPECT_TRUE(check(ok2).empty()) << check(ok2);
		EXPECT_TRUE(check(ok3).empty()) << check(ok3);
		EXPECT_TRUE(check(ok4).empty()) << check(ok4);

		// check the invalid types
		EXPECT_PRED2(containsMSG, check(err0), Message(NodeAddress(err0), EC_TYPE_FREE_TAG_TYPE_REFERENCE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err1), Message(NodeAddress(err1).getAddressOfChild(2,0), EC_TYPE_FREE_TAG_TYPE_REFERENCE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2), Message(NodeAddress(err2).getAddressOfChild(1,0,1,1,0,1,2,0), EC_TYPE_FREE_TAG_TYPE_REFERENCE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err3), Message(NodeAddress(err3).getAddressOfChild(1,0,1,1,1,1,2,0), EC_TYPE_FREE_TAG_TYPE_REFERENCE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err4), Message(NodeAddress(err4).getAddressOfChild(0,1,0,1,1,1,1,2,0), EC_TYPE_FREE_TAG_TYPE_REFERENCE, "", Message::ERROR));
	}

	TEST(ConstructorTypeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		//create structs with correct constructors
		auto ok1 = builder.parseType("struct A { ctor () {} }");
		auto ok2 = builder.parseType("struct A { ctor (a : int<4>) {} }");
		auto ok3 = builder.parseType("struct A { ctor (a : int<4>, b : int<4>) {} }");

		EXPECT_TRUE(check(ok1).empty()) << check(ok1);
		EXPECT_TRUE(check(ok2).empty()) << check(ok2);
		EXPECT_TRUE(check(ok3).empty()) << check(ok3);

		{
			//create a custom constructor of the wrong function type
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), thisType, FK_PLAIN);
			auto ctor = builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp());
			auto err = builder.structType("A", ParentList(), FieldList(), toVector(ctor.as<ExpressionPtr>()), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, MemberFunctionList(), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_CONSTRUCTOR_TYPE, "", Message::ERROR));
		}

		{
			//create a custom constructor of the wrong type (wrong this parameter type)
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(builder.refType(thisType).as<TypePtr>()), thisType, FK_CONSTRUCTOR);
			auto ctor = builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp());
			auto err = builder.structType("A", ParentList(), FieldList(), toVector(ctor.as<ExpressionPtr>()), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, MemberFunctionList(), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_CONSTRUCTOR_TYPE, "", Message::ERROR));
		}

		{
			//create a custom constructor of the wrong type (wrong return type)
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), builder.genericType("A"), FK_CONSTRUCTOR);
			auto ctor = builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp());
			auto err = builder.structType("A", ParentList(), FieldList(), toVector(ctor.as<ExpressionPtr>()), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, MemberFunctionList(), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_CONSTRUCTOR_TYPE, "", Message::ERROR));
		}
	}

	TEST(DuplicateConstructorTypeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		//correct constructors
		auto ok1 = builder.parseType("struct A { ctor () {} }");
		auto ok2 = builder.parseType("struct A { ctor (a : int<4>) {} }");
		auto ok3 = builder.parseType("struct A { ctor (a : int<4>, b : int<4>) {} }");

		EXPECT_TRUE(check(ok1).empty()) << check(ok1);
		EXPECT_TRUE(check(ok2).empty()) << check(ok2);
		EXPECT_TRUE(check(ok3).empty()) << check(ok3);

		//duplicate constructor type
		{
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), thisType, FK_CONSTRUCTOR);
			auto ctor = builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp());
			auto err = builder.structType("A", ParentList(), FieldList(), toVector<ExpressionPtr>(ctor, ctor), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, MemberFunctionList(), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_DUPLICATE_CONSTRUCTOR_TYPE, "", Message::ERROR));
		}

		//duplicate constructor type
		{
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), thisType, FK_CONSTRUCTOR);
			auto ctor = builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType)), builder.variable(builder.refType(builder.getLangBasic().getInt4()))), builder.getNoOp());
			auto err = builder.structType("A", ParentList(), FieldList(), toVector<ExpressionPtr>(ctor, ctor), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, MemberFunctionList(), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_DUPLICATE_CONSTRUCTOR_TYPE, "", Message::ERROR));
		}
	}

	TEST(DestructorTypeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		//create structs using the default destructor
		auto ok1 = builder.parseType("struct { x : int<4>; }");
		auto ok2 = builder.parseType("struct A { x : int<4>; }");
		//as well as a custom destructor
		auto ok3 = builder.parseType("struct A { dtor () {} }");

		EXPECT_TRUE(check(ok1).empty()) << check(ok1);
		EXPECT_TRUE(check(ok2).empty()) << check(ok2);
		EXPECT_TRUE(check(ok3).empty()) << check(ok3);

		{
			//create a custom destructor of the wrong type
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), thisType, FK_PLAIN);
			auto dtor = builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp());
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), dtor, false, MemberFunctionList(), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_DESTRUCTOR_TYPE, "", Message::ERROR));
		}

		{
			//create a custom destructor of the wrong type (wrong this parameter type)
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(builder.refType(thisType).as<TypePtr>()), thisType, FK_DESTRUCTOR);
			auto dtor = builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp());
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), dtor, false, MemberFunctionList(), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_DESTRUCTOR_TYPE, "", Message::ERROR));
		}

		{
			//create a custom destructor of the wrong type (wrong return type)
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), builder.genericType("A"), FK_DESTRUCTOR);
			auto dtor = builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp());
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), dtor, false, MemberFunctionList(), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_DESTRUCTOR_TYPE, "", Message::ERROR));
		}
	}

	TEST(MemberFunctionTypeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		//create structs with correct member functions
		auto ok1 = builder.parseType("struct A { lambda f = () -> unit {} }");
		auto ok2 = builder.parseType("struct A { lambda f = () -> unit {} lambda g = () -> unit {} }");
		auto ok3 = builder.parseType("struct A { lambda f = (a : int<4>) -> unit {} lambda g = (a : int<4>) -> unit {} }");
		auto ok4 = builder.parseType("struct A { lambda f = () -> unit {} pure virtual g : () -> unit }");
		auto ok5 = builder.parseType("struct A { lambda f = (a : int<4>) -> unit {} pure virtual g : (int<4>) -> unit }");

		EXPECT_TRUE(check(ok1).empty()) << check(ok1);
		EXPECT_TRUE(check(ok2).empty()) << check(ok2);
		EXPECT_TRUE(check(ok3).empty()) << check(ok3);
		EXPECT_TRUE(check(ok4).empty()) << check(ok4);
		EXPECT_TRUE(check(ok5).empty()) << check(ok5);

		{
			//create a custom member function of the wrong function type
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), builder.getLangBasic().getUnit(), FK_PLAIN);
			auto memberFunction = builder.memberFunction(false, "f", builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp()));
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, toVector(memberFunction), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_MEMBER_FUNCTION_TYPE, "", Message::ERROR));
		}

		{
			//create a custom member function of the wrong type (wrong this parameter type)
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(builder.refType(thisType).as<TypePtr>()), builder.getLangBasic().getUnit(), FK_MEMBER_FUNCTION);
			auto memberFunction = builder.memberFunction(false, "f", builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp()));
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, toVector(memberFunction), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_MEMBER_FUNCTION_TYPE, "", Message::ERROR));
		}

		{
			//create a custom pure virtual member function of the wrong function type
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), builder.getLangBasic().getUnit(), FK_PLAIN);
			auto memberFunction = builder.pureVirtualMemberFunction("f", functionType);
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, MemberFunctionList(), toVector(memberFunction), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_MEMBER_FUNCTION_TYPE, "", Message::ERROR));
		}

		{
			//create a custom pure virtual member function of the wrong function type (wrong this parameter type)
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(builder.refType(thisType).as<TypePtr>()), builder.getLangBasic().getUnit(), FK_MEMBER_FUNCTION);
			auto memberFunction = builder.pureVirtualMemberFunction("f", functionType);
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, MemberFunctionList(), toVector(memberFunction), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_INVALID_MEMBER_FUNCTION_TYPE, "", Message::ERROR));
		}
	}

	TEST(DuplicateMemberFunctionCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		//create structs with correct member functions
		auto ok1 = builder.parseType("struct A { lambda f = () -> unit {} }");
		auto ok2 = builder.parseType("struct A { lambda f = () -> unit {} lambda g = () -> unit {} }");
		auto ok3 = builder.parseType("struct A { lambda f = (a : int<4>) -> unit {} lambda g = (a : int<4>) -> unit {} }");
		auto ok4 = builder.parseType("struct A { lambda f = () -> unit {} pure virtual g : () -> unit }");
		auto ok5 = builder.parseType("struct A { lambda f = (a : int<4>) -> unit {} pure virtual g : (int<4>) -> unit }");

		{
			//create a struct with two identical member functions
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), builder.getLangBasic().getUnit(), FK_MEMBER_FUNCTION);
			auto memberFunction = builder.memberFunction(false, "f", builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp()));
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, toVector(memberFunction, memberFunction), PureVirtualMemberFunctionList(), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_DUPLICATE_MEMBER_FUNCTION, "", Message::ERROR));
		}

		{
			//create a struct with two identical pure virtual member functions
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), builder.getLangBasic().getUnit(), FK_MEMBER_FUNCTION);
			auto memberFunction = builder.pureVirtualMemberFunction("f", functionType);
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, MemberFunctionList(), toVector(memberFunction, memberFunction), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_DUPLICATE_MEMBER_FUNCTION, "", Message::ERROR));
		}

		{
			//create a struct with two identically typed and named member and pure member functions
			TypePtr thisType = builder.refType(builder.tagTypeReference("A"));
			auto functionType = builder.functionType(toVector(thisType), builder.getLangBasic().getUnit(), FK_MEMBER_FUNCTION);
			auto memberFunction = builder.memberFunction(false, "f", builder.lambdaExpr(functionType, toVector(builder.variable(builder.refType(thisType))), builder.getNoOp()));
			auto pureVirtualMemberFunction = builder.pureVirtualMemberFunction("f", functionType);
			auto err = builder.structType("A", ParentList(), FieldList(), ExpressionList(), analysis::buildDefaultDestructor(thisType, builder.parents(), builder.fields()), false, toVector(memberFunction), toVector(pureVirtualMemberFunction), StaticMemberFunctionList());

			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0), EC_TYPE_DUPLICATE_MEMBER_FUNCTION, "", Message::ERROR));
		}
	}

	TEST(DuplicateMemberFieldCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		{
			vector<FieldPtr> fields;
			fields.push_back(builder.field("", GenericType::get(manager, "a")));
			auto err = builder.structType(fields);
			EXPECT_PRED2(containsMSG, check(err), Message(NodeAddress(err).getAddressOfChild(1,0,1,1), EC_TYPE_INVALID_IDENTIFIER, "", Message::ERROR));
		}
	}

	TEST(InitExprTypeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// some preparations
		core::StringValuePtr name = builder.stringValue("x");
		core::TypePtr typeA = builder.genericType("A");
		core::TypePtr typeB = builder.genericType("B");

		core::ExpressionPtr valueA = builder.literal(typeA, "a");
		core::ExpressionPtr valueB = builder.literal(typeB, "b");

		FieldList fields;
		fields.push_back(builder.field(name, typeA));
		TagTypePtr structType = builder.structType(fields);

		// create struct expression
		ExpressionList members;
		members.push_back(valueA);
		core::InitExprPtr ok = builder.initExprTemp(builder.refType(structType), members);

		members.clear();
		members.push_back(valueB);
		core::InitExprPtr err = builder.initExprTemp(builder.refType(structType), members);

		// conduct checks
		CheckPtr typeCheck = make_check<InitExprTypeCheck>();

		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(InitExprAddress(err)->getInitDecls()[0], EC_TYPE_INVALID_INITIALIZATION_ARGUMENT_TYPE, "", Message::ERROR));
	}


	TEST(InitExprTypeCheck, MaterializingDeclarations) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto decl = [&](const std::string& declType, const std::string& initType) {
			auto dT = builder.parseType(declType);
			auto iT = builder.parseType(initType);
			return builder.declaration(dT,builder.literal("X",iT));
		};

		auto trg = builder.parseExpr(
				"def struct S { "
				"	x : int<4>;"
				"	y : A;"
				"	z : B;"
				"	w : ref<C,t,f,cpp_ref>;"
				"};"
				"ref_temp(type_lit(S))"
		);

		CheckPtr initCheck = make_check<InitExprTypeCheck>();

		// check a positive case
		{
			DeclarationList decls;
			decls.push_back(decl("ref<int<4>,f,f,plain>","int<4>"));
			decls.push_back(decl("ref<A,f,f,plain>","A"));
			decls.push_back(decl("ref<B,f,f,plain>","ref<B,t,f,cpp_ref>"));
			decls.push_back(decl("ref<C,t,f,cpp_ref>","ref<C,t,f,cpp_ref>"));
			auto init = builder.initExpr(trg,decls);

			auto issues = check(init,initCheck);
			EXPECT_TRUE(issues.empty()) << "Issues: " << issues;
		}

		// check some negative case
		{
			DeclarationList decls;
			decls.push_back(decl("int<4>","int<4>"));			// this one is not materialized
			decls.push_back(decl("ref<A,f,f,plain>","A"));
			decls.push_back(decl("ref<B,f,f,plain>","ref<B,t,f,cpp_ref>"));
			decls.push_back(decl("ref<C,t,f,cpp_ref>","ref<C,t,f,cpp_ref>"));
			auto init = builder.initExpr(trg,decls);

			auto issues = check(init,initCheck);
			EXPECT_EQ(2,issues.size()) << "Issues: " << issues;
			EXPECT_PRED2(containsMSG, issues, Message(InitExprAddress(init)->getInitDecls()[0], EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));
			EXPECT_PRED2(containsMSG, issues, Message(InitExprAddress(init)->getInitDecls()[0], EC_TYPE_INVALID_INITIALIZATION_ARGUMENT_TYPE, "", Message::ERROR));
		}

		{
			DeclarationList decls;
			decls.push_back(decl("ref<int<4>,f,f,plain>","int<4>"));
			decls.push_back(decl("ref<A,f,f,plain>","A"));
			decls.push_back(decl("ref<B,f,f,plain>","ref<B,t,f,cpp_ref>"));
			decls.push_back(decl("ref<C,f,f,plain>","ref<C,t,f,cpp_ref>"));		// this one should not be materialized
			auto init = builder.initExpr(trg,decls);

			auto issues = check(init,initCheck);
			EXPECT_EQ(2,issues.size()) << "Issues: " << issues;
			EXPECT_PRED2(containsMSG, issues, Message(InitExprAddress(init)->getInitDecls()[3], EC_TYPE_INVALID_INITIALIZATION_ARGUMENT_TYPE, "", Message::ERROR));
			EXPECT_PRED2(containsMSG, issues, Message(InitExprAddress(init)->getInitDecls()[3], EC_TYPE_INVALID_INITIALIZATION_ARGUMENT_MATERIALIZATION, "", Message::ERROR));
		}
	}

	TEST(InitExprTypeCheck, Tuple) {
		NodeManager manager;
		IRBuilder builder(manager);

		// some preparations
		core::TypePtr typeA = builder.genericType("A");
		core::TypePtr typeB = builder.genericType("B");
		core::TypePtr typeC = builder.genericType("C");

		core::ExpressionPtr valueA = builder.literal(typeA, "a");
		core::ExpressionPtr valueB = builder.literal(typeB, "b");
		core::ExpressionPtr valueC = builder.literal(typeC, "c");

		core::ExpressionPtr cppRefValueA = builder.literal(builder.parseType("cpp_ref<A,t,f>"), "a");
		core::ExpressionPtr cppRefValueB = builder.literal(builder.parseType("cpp_ref<B,t,f>"), "b");
		core::ExpressionPtr cppRefValueC = builder.literal(builder.parseType("cpp_ref<C,t,f>"), "c");

		// create the resulting tuple type
		auto tupleType = builder.tupleType({ typeA, typeB });

		// create some init expressions
		core::ExpressionList ok = {
			builder.initExprTemp(builder.refType(tupleType), { valueA, valueB }),
			builder.initExprTemp(builder.refType(tupleType), { valueA, cppRefValueB }),
			builder.initExprTemp(builder.refType(tupleType), { cppRefValueA, valueB }),
			builder.initExprTemp(builder.refType(tupleType), { cppRefValueA, cppRefValueB })
		};

		core::ExpressionList err = {
			builder.initExprTemp(builder.refType(tupleType), { valueA }),
			builder.initExprTemp(builder.refType(tupleType), { valueA, valueA }),
			builder.initExprTemp(builder.refType(tupleType), { valueB, valueA }),
			builder.initExprTemp(builder.refType(tupleType), { valueB, valueB }),
			builder.initExprTemp(builder.refType(tupleType), { valueA, valueB, valueC }),
			builder.initExprTemp(builder.refType(tupleType), { cppRefValueA, cppRefValueA }),
			builder.initExprTemp(builder.refType(tupleType), { cppRefValueB, cppRefValueA })
		};

		// conduct checks
		CheckPtr typeCheck = make_check<InitExprTypeCheck>();

		for(const auto& cur : ok) {
			EXPECT_TRUE(check(cur, typeCheck).empty()) << "Failed on " << dumpReadable(cur);
		}

		for(const auto& cur : err) {
			auto errors = check(cur, typeCheck);
			EXPECT_FALSE(errors.empty()) << "Should have failed on " << dumpReadable(cur);
			if (!errors.empty()) {
				EXPECT_PRED2(containsMSG, errors, Message(NodeAddress(cur), EC_TYPE_INVALID_INITIALIZATION_EXPR, "", Message::ERROR));
			}
		}
	}

	TEST(UnhandledMemberStructExprTypeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// conduct checks
		CheckPtr typeCheck = make_check<InitExprTypeCheck>();
		core::TypePtr structType = builder.parseType("struct { a : A; b : B; }");

		// Create a example expressions
		core::TypePtr typeA = builder.genericType("A");
		core::ExpressionPtr valueA = builder.literal(typeA, "a");
		ExpressionList members;
		members.push_back(valueA);
		core::InitExprPtr err = builder.initExprTemp(builder.refType(structType.as<TagTypePtr>()), members);
		EXPECT_EQ(check(err, typeCheck).size(), 1);
	}

	TEST(MemberAccessCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto stmt = builder.parseStmt(R"({
			var ref<int<4>> a;
			composite_member_access(a, lit("bla"), type_lit(int<4>));
		})");

		// conduct checks
		CheckPtr typeCheck = makeRecursive(make_check<MemberAccessElementTypeCheck>());
		EXPECT_EQ(check(stmt, typeCheck).size(), 1);
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

		FieldList fields;
		fields.push_back(builder.field(identA, typeA));
		fields.push_back(builder.field(identB, typeB));

		TypePtr structType = builder.structType(fields);
		VariablePtr var = builder.variable(structType);
		VariablePtr var2 = builder.variable(typeA);

		ExpressionPtr ok = builder.callExpr(fun, var, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeA));
		ExpressionPtr err1 = builder.callExpr(fun, var, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeB));
		ExpressionPtr err2 = builder.callExpr(fun, var, builder.getIdentifierLiteral(identC), builder.getTypeLiteral(typeB));
		ExpressionPtr err3 = builder.callExpr(fun, var2, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeA));
		ExpressionPtr err4 = builder.callExpr(basic.getUnit(), fun, var2, var, builder.getTypeLiteral(typeA));


		CheckPtr typeCheck = make_check<MemberAccessElementTypeCheck>();
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err1, typeCheck).empty());
		ASSERT_FALSE(check(err2, typeCheck).empty());
		ASSERT_TRUE(check(err3, typeCheck).empty()); // having a member access in a generic type is no longer an error
		ASSERT_FALSE(check(err4, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err1, typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2, typeCheck), Message(NodeAddress(err2), EC_TYPE_NO_SUCH_MEMBER, "", Message::ERROR));
		// EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_ACCESSING_MEMBER_OF_NON_NAMED_COMPOSITE_TYPE, "",
		// Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err4, typeCheck), Message(NodeAddress(err4), EC_TYPE_INVALID_IDENTIFIER, "", Message::ERROR));
	}

	TEST(MemberAccessElementTypeCheck, References) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = builder.getLangBasic();
		const lang::ReferenceExtension& refExt = manager.getLangExtension<lang::ReferenceExtension>();

		// get function to be tested
		ExpressionPtr fun = refExt.getRefMemberAccess();

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

		FieldList fields;
		fields.push_back(builder.field(identA, typeA));
		fields.push_back(builder.field(identB, typeB));

		TypePtr structType = builder.structType(fields);
		TypePtr structRefType = builder.refType(structType);

		VariablePtr var = builder.variable(structRefType);
		VariablePtr var2 = builder.variable(typeRefA);

		ExpressionPtr ok = builder.callExpr(fun, var, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeA));
		ExpressionPtr err1 = builder.callExpr(fun, var, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeB));
		ExpressionPtr err2 = builder.callExpr(fun, var, builder.getIdentifierLiteral(identC), builder.getTypeLiteral(typeB));
		ExpressionPtr err3 = builder.callExpr(fun, var2, builder.getIdentifierLiteral(identA), builder.getTypeLiteral(typeA));
		ExpressionPtr err4 = builder.callExpr(basic.getUnit(), fun, var2, var, builder.getTypeLiteral(typeA));


		CheckPtr typeCheck = make_check<MemberAccessElementTypeCheck>();
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err1, typeCheck).empty());
		ASSERT_FALSE(check(err2, typeCheck).empty());
		ASSERT_TRUE(check(err3, typeCheck).empty()); // having a member access on a generic typed var is no longer an error
		ASSERT_FALSE(check(err4, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err1, typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2, typeCheck), Message(NodeAddress(err2), EC_TYPE_NO_SUCH_MEMBER, "", Message::ERROR));
		// EXPECT_PRED2(containsMSG, check(err3,typeCheck), Message(NodeAddress(err3), EC_TYPE_ACCESSING_MEMBER_OF_NON_NAMED_COMPOSITE_TYPE, "",
		// Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err4, typeCheck), Message(NodeAddress(err4), EC_TYPE_INVALID_IDENTIFIER, "", Message::ERROR));
	}

	TEST(MemberAccessElementTypeInTagTypeCheck, RecursiveNested) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto pos = builder.parseAddressesType(
			R"(
				struct A {
					x : int<4>;
					lambda f = () -> unit { $x$; }
				}
			)"
			);

		ASSERT_EQ(1, pos.size());
		auto x = pos[0].as<CallExprAddress>();

		auto ok = x.getRootNode();
		auto err1 = transform::replaceNode(manager, x->getArgument(2), builder.getTypeLiteral(builder.parseType("ref<int<4>>")));
		auto err2 = transform::replaceNode(manager, x->getArgument(2), builder.getTypeLiteral(builder.parseType("float")));

		CheckPtr typeCheck = makeVisitOnce(make_check<MemberAccessElementTypeInTagTypeCheck>());
		EXPECT_TRUE(check(ok, typeCheck).empty()) << check(ok, typeCheck);
		EXPECT_FALSE(check(err1, typeCheck).empty());
		EXPECT_FALSE(check(err2, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err1, typeCheck), Message(NodeAddress(x.switchRoot(err1)), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2, typeCheck), Message(NodeAddress(x.switchRoot(err2)), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));


		// in the full-check context
		EXPECT_TRUE(check(ok).empty()) << check(ok, typeCheck);
		EXPECT_FALSE(check(err1).empty());
		EXPECT_FALSE(check(err2).empty());

		EXPECT_PRED2(containsMSG, check(err1), Message(NodeAddress(x.switchRoot(err1)), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2), Message(NodeAddress(x.switchRoot(err2)), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
	}

	TEST(MemberAccessElementTypeInTagTypeCheck, RecursiveNestedSelfReference) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto pos = builder.parseAddressesType(
			R"(
				struct A {
					x : ptr<A>;
					lambda f = () -> unit { $x$; }
				}
			)"
			);

		ASSERT_EQ(1, pos.size());
		auto x = pos[0].as<CallExprAddress>();

		auto ok = x.getRootNode();

		CheckPtr typeCheck = makeVisitOnce(make_check<MemberAccessElementTypeInTagTypeCheck>());
		EXPECT_TRUE(check(ok, typeCheck).empty()) << check(ok, typeCheck);

		// in the full-check context
		EXPECT_TRUE(check(ok).empty()) << check(ok, typeCheck);
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

		TypePtr tupleType = builder.tupleType({typeA, typeB, typeC});
		VariablePtr var = builder.variable(tupleType);
		VariablePtr var2 = builder.variable(typeA);

		ExpressionPtr ok = builder.callExpr(fun, var, builder.uintLit(0), builder.getTypeLiteral(typeA));
		ExpressionPtr err1 = builder.callExpr(fun, var, builder.uintLit(0), builder.getTypeLiteral(typeB));
		ExpressionPtr err2 = builder.callExpr(fun, var, builder.uintLit(5), builder.getTypeLiteral(typeB));
		ExpressionPtr err3 = builder.callExpr(fun, var2, builder.uintLit(0), builder.getTypeLiteral(typeA));
		ExpressionPtr err4 = builder.callExpr(basic.getUnit(), fun, var2, var, builder.getTypeLiteral(typeA));


		CheckPtr typeCheck = make_check<ComponentAccessTypeCheck>();
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err1, typeCheck).empty());
		ASSERT_FALSE(check(err2, typeCheck).empty());
		ASSERT_FALSE(check(err3, typeCheck).empty());
		ASSERT_FALSE(check(err4, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err1, typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2, typeCheck), Message(NodeAddress(err2), EC_TYPE_NO_SUCH_MEMBER, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err3, typeCheck), Message(NodeAddress(err3), EC_TYPE_ACCESSING_MEMBER_OF_NON_TUPLE_TYPE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err4, typeCheck), Message(NodeAddress(err4), EC_TYPE_INVALID_TUPLE_INDEX, "", Message::ERROR));
	}

	TEST(ComponentAccessTypeCheck, References) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = builder.getLangBasic();
		const lang::ReferenceExtension& refExt = manager.getLangExtension<lang::ReferenceExtension>();

		// get function to be tested
		ExpressionPtr fun = refExt.getRefComponentAccess();

		// Create a example expressions
		TypePtr typeA = builder.genericType("typeA");
		TypePtr typeB = builder.genericType("typeB");
		TypePtr typeC = builder.genericType("typeC");

		TypePtr typeRefA = builder.refType(typeA);
		TypePtr typeRefB = builder.refType(typeB);
		TypePtr typeRefC = builder.refType(typeC);

		TypePtr tupleType = builder.tupleType({typeA, typeB, typeC});
		VariablePtr var = builder.variable(builder.refType(tupleType));
		VariablePtr var2 = builder.variable(typeRefC);

		ExpressionPtr ok = builder.callExpr(fun, var, builder.uintLit(0), builder.getTypeLiteral(typeA));
		ExpressionPtr err1 = builder.callExpr(fun, var, builder.uintLit(0), builder.getTypeLiteral(typeB));
		ExpressionPtr err2 = builder.callExpr(fun, var, builder.uintLit(5), builder.getTypeLiteral(typeB));
		ExpressionPtr err3 = builder.callExpr(fun, var2, builder.uintLit(0), builder.getTypeLiteral(typeA));
		ExpressionPtr err4 = builder.callExpr(basic.getUnit(), fun, var2, var, builder.getTypeLiteral(typeA));


		CheckPtr typeCheck = make_check<ComponentAccessTypeCheck>();
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err1, typeCheck).empty());
		ASSERT_FALSE(check(err2, typeCheck).empty());
		ASSERT_FALSE(check(err3, typeCheck).empty());
		ASSERT_FALSE(check(err4, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err1, typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_TYPE_OF_MEMBER, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2, typeCheck), Message(NodeAddress(err2), EC_TYPE_NO_SUCH_MEMBER, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err3, typeCheck), Message(NodeAddress(err3), EC_TYPE_ACCESSING_MEMBER_OF_NON_TUPLE_TYPE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err4, typeCheck), Message(NodeAddress(err4), EC_TYPE_INVALID_TUPLE_INDEX, "", Message::ERROR));
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

		EXPECT_FALSE(check(err1, returnTypeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err1, returnTypeCheck),
		             Message(NodeAddress(err1).getAddressOfChild(2, 0), EC_TYPE_INVALID_RETURN_VALUE_TYPE, "", Message::ERROR));
	}

	TEST(ReturnTypeCheck, ObjectTypes) {
		NodeManager manager;
		IRBuilder builder(manager);

		// return type rules for value types:
		//  - trivial types can be returned by value or reference
		//  - non-trivially copyable types must be returned by reference

		TypePtr trivialClass = builder.parseType("struct A { x : int<4>; }");
		TypePtr nonTriviallyCopyableClass = builder.parseType("struct B { x : int<4>; ctor (other : ref<B,t,f,cpp_ref>) { } }");

		EXPECT_TRUE(trivialClass);
		EXPECT_TRUE(nonTriviallyCopyableClass);

		EXPECT_TRUE(analysis::isTrivial(trivialClass));
		EXPECT_FALSE(analysis::isTrivial(nonTriviallyCopyableClass));


		{
			// -- trivial case --
			FunctionTypePtr trivialFunType = builder.functionType(TypeList(), trivialClass);

			// -- return by value --
			LambdaPtr a = builder.lambda(trivialFunType, VariableList(), builder.returnStmt(
				builder.literal(trivialClass, "X")
			));

			// -- return a reference
			LambdaPtr b = builder.lambda(trivialFunType, VariableList(), builder.returnStmt(
				builder.literal(builder.refType(trivialClass), "X")
			));

			// -- return as C++ reference
			LambdaPtr c = builder.lambda(trivialFunType, VariableList(), builder.returnStmt(
				builder.literal(builder.refType(trivialClass, true, false, lang::ReferenceType::Kind::CppReference), "X")
			));

			// -- not something different, e.g. a pointer
			LambdaPtr d = builder.lambda(trivialFunType, VariableList(), builder.returnStmt(
				builder.literal(builder.ptrType(trivialClass), "X")
			));

			CheckPtr returnTypeCheck = make_check<ReturnTypeCheck>();
			EXPECT_TRUE(check(a, returnTypeCheck).empty()) << check(a, returnTypeCheck);
			EXPECT_TRUE(check(b, returnTypeCheck).empty()) << check(b, returnTypeCheck);
			EXPECT_TRUE(check(c, returnTypeCheck).empty()) << check(c, returnTypeCheck);
			EXPECT_EQ(1, check(d, returnTypeCheck).size()) << check(d, returnTypeCheck);
		}

		{
			// -- non-trivially copyable case --
			FunctionTypePtr nonTrivialFunType = builder.functionType(TypeList(), nonTriviallyCopyableClass);

			// -- return by value --
			LambdaPtr a = builder.lambda(nonTrivialFunType, VariableList(), builder.returnStmt(
				builder.literal(nonTriviallyCopyableClass, "X")
			));

			// -- return a reference
			LambdaPtr b = builder.lambda(nonTrivialFunType, VariableList(), builder.returnStmt(
				builder.literal(builder.refType(nonTriviallyCopyableClass), "X")
			));

			// -- return as C++ reference
			LambdaPtr c = builder.lambda(nonTrivialFunType, VariableList(), builder.returnStmt(
				builder.literal(builder.refType(nonTriviallyCopyableClass, true, false, lang::ReferenceType::Kind::CppReference), "X")
			));

			// -- not something different, e.g. a pointer
			LambdaPtr d = builder.lambda(nonTrivialFunType, VariableList(), builder.returnStmt(
				builder.literal(builder.ptrType(nonTriviallyCopyableClass), "X")
			));

			CheckPtr returnTypeCheck = make_check<ReturnTypeCheck>();
			EXPECT_EQ(1, check(a, returnTypeCheck).size()) << check(a, returnTypeCheck);
			EXPECT_TRUE(check(b, returnTypeCheck).empty()) << check(b, returnTypeCheck);
			EXPECT_TRUE(check(c, returnTypeCheck).empty()) << check(c, returnTypeCheck);
			EXPECT_EQ(1, check(d, returnTypeCheck).size()) << check(d, returnTypeCheck);
		}

		/*
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

		EXPECT_FALSE(check(err1, returnTypeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err1, returnTypeCheck),
			Message(NodeAddress(err1).getAddressOfChild(2, 0), EC_TYPE_INVALID_RETURN_VALUE_TYPE, "", Message::ERROR));
		*/
	}

	TEST(DeclarationTypeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.genericType("int");
		TypePtr type2 = builder.genericType("uint");
		ExpressionPtr init = builder.literal(type, "4");
		DeclarationStmtPtr ok = builder.declarationStmt(builder.variable(type), init);
		DeclarationStmtPtr err = builder.declarationStmt(builder.variable(type2), init);

		CheckPtr typeCheck = makeRecursive(make_check<DeclarationTypeCheck>());
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err, typeCheck),
			         Message(DeclarationStmtAddress(err)->getDeclaration(), EC_TYPE_INVALID_INITIALIZATION_EXPR, "", Message::ERROR));
	}

	TEST(DeclarationTypeCheck, SubTypes) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto& basic = manager.getLangBasic();

		// OK ... create a function literal
		TypePtr typeA = basic.getInt4();
		TypePtr typeB = basic.getInt8();
		ExpressionPtr init = builder.literal(typeA, "4");
		DeclarationStmtPtr ok = builder.declarationStmt(builder.variable(typeB), builder.literal(typeA, "4"));
		DeclarationStmtPtr err = builder.declarationStmt(builder.variable(typeA), builder.literal(typeB, "4"));

		CheckPtr typeCheck = makeRecursive(make_check<DeclarationTypeCheck>());
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err, typeCheck),
			         Message(DeclarationStmtAddress(err)->getDeclaration(), EC_TYPE_INVALID_INITIALIZATION_EXPR, "", Message::ERROR));
	}

	TEST(DeclarationTypeCheck, RecursiveTypes) {
		NodeManager manager;
		IRBuilder builder(manager);

		// OK ... create a function literal
		TagTypePtr typeA = builder.parseType("decl struct StructX; def struct StructX { a : A; next : ref<StructX>; }; StructX").as<TagTypePtr>();
		TypePtr typeB = typeA->peel();

		// all of the following should be supported
		DeclarationStmtPtr ok0 = builder.declarationStmt(builder.variable(typeA), builder.literal(typeA, "X"));
		DeclarationStmtPtr ok1 = builder.declarationStmt(builder.variable(typeA), builder.literal(typeB, "X"));
		DeclarationStmtPtr ok2 = builder.declarationStmt(builder.variable(typeB), builder.literal(typeA, "X"));
		DeclarationStmtPtr ok3 = builder.declarationStmt(builder.variable(typeB), builder.literal(typeB, "X"));

		CheckPtr typeCheck = makeRecursive(make_check<DeclarationTypeCheck>());
		EXPECT_TRUE(check(ok0, typeCheck).empty()) << check(ok0, typeCheck);
		EXPECT_TRUE(check(ok1, typeCheck).empty()) << check(ok1, typeCheck);
		EXPECT_TRUE(check(ok2, typeCheck).empty()) << check(ok2, typeCheck);
		EXPECT_TRUE(check(ok3, typeCheck).empty()) << check(ok3, typeCheck);
	}

	TEST(DeclarationTypeCheck, ReferenceSemantics) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto& basic = manager.getLangBasic();
		const auto& int4 = basic.getInt4();
		const auto& int8 = basic.getInt8();

		CheckPtr typeCheck = makeRecursive(make_check<DeclarationTypeCheck>());

		{ //plain integer
			DeclarationStmtPtr ok = builder.declarationStmt(builder.variable(builder.refType(int4)), builder.literal(int4, "42"));
			EXPECT_TRUE(check(ok, typeCheck).empty()) << check(ok, typeCheck);
		}

		{ //const integer
			DeclarationStmtPtr ok = builder.declarationStmt(builder.variable(builder.refType(int4, true)), builder.literal(int4, "42"));
			EXPECT_TRUE(check(ok, typeCheck).empty()) << check(ok, typeCheck);
		}

		{ //volatile integer
			DeclarationStmtPtr ok = builder.declarationStmt(builder.variable(builder.refType(int4, false, true)), builder.literal(int4, "42"));
			EXPECT_TRUE(check(ok, typeCheck).empty()) << check(ok, typeCheck);
		}

		{ //cpp ref
			auto var = builder.variable(builder.refType(int4, false, false, lang::ReferenceType::Kind::CppReference));
			DeclarationStmtPtr ok = builder.declarationStmt(var, var);
			EXPECT_TRUE(check(ok, typeCheck).empty()) << check(ok, typeCheck);
		}

		{ //cpp rref
			auto var = builder.variable(builder.refType(int4, false, false, lang::ReferenceType::Kind::CppRValueReference));
			DeclarationStmtPtr ok = builder.declarationStmt(var, var);
			EXPECT_TRUE(check(ok, typeCheck).empty()) << check(ok, typeCheck);
		}

		{ // different types do not work
			DeclarationStmtPtr err = builder.declarationStmt(builder.variable(builder.refType(int4)), builder.literal(int8, "42"));
			ASSERT_FALSE(check(err, typeCheck).empty());
			EXPECT_PRED2(containsMSG, check(err, typeCheck),
				         Message(DeclarationStmtAddress(err)->getDeclaration(), EC_TYPE_INVALID_INITIALIZATION_EXPR, "", Message::ERROR));
		}
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

		EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_CONDITION_EXPR, "", Message::ERROR));
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

		VariablePtr intVar = builder.variable(intType, 1);
		VariablePtr boolVar = builder.variable(boolType, 1);


		NodePtr ok = builder.forStmt(intVar, intLit, intLit, intLit, intLit);
		NodePtr err1 = builder.forStmt(boolVar, boolLit, boolLit, boolLit, boolLit);
		NodePtr err2 = builder.forStmt(intVar, intLit, boolLit, intLit, boolLit);
		NodePtr err3 = builder.forStmt(intVar, intLit, intLit, boolLit, boolLit);

		CheckPtr typeCheck = make_check<ForStmtTypeCheck>();
		EXPECT_TRUE(check(ok, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err1, typeCheck), Message(NodeAddress(err1), EC_TYPE_INVALID_ITERATOR_TYPE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2, typeCheck), Message(NodeAddress(err2), EC_TYPE_INVALID_BOUNDARY_TYPE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err3, typeCheck), Message(NodeAddress(err3), EC_TYPE_INVALID_BOUNDARY_TYPE, "", Message::ERROR));
	}

	TEST(WhileCondition, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr intType = builder.genericType("int");
		TypePtr boolType = builder.genericType("bool");
		ExpressionPtr intLit = builder.literal(intType, "4");
		ExpressionPtr boolLit = builder.literal(boolType, "true");
		NodePtr ok = builder.whileStmt(boolLit, builder.getNoOp());
		NodePtr err = builder.whileStmt(intLit, builder.getNoOp());

		CheckPtr typeCheck = make_check<WhileConditionTypeCheck>();
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_CONDITION_EXPR, "", Message::ERROR));
	}

	TEST(Switch, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr intType = builder.getLangBasic().getInt1();
		TypePtr enumType = core::lang::buildEnumType(core::lang::EnumDefinition::create(builder.genericType("MyEnum"),intType,{}));
		TypePtr boolType = builder.genericType("bool");
		ExpressionPtr intLit = builder.literal(intType, "4");
		ExpressionPtr enumLit = builder.literal(enumType, "X");
		ExpressionPtr boolLit = builder.literal(boolType, "true");
		SwitchStmtPtr ok1 = builder.switchStmt(intLit, vector<SwitchCasePtr>());
		SwitchStmtPtr ok2 = builder.switchStmt(enumLit, vector<SwitchCasePtr>());
		SwitchStmtPtr err = builder.switchStmt(boolLit, vector<SwitchCasePtr>());

		EXPECT_TRUE(core::lang::isEnum(enumType));

		CheckPtr typeCheck = make_check<SwitchExpressionTypeCheck>();
		EXPECT_TRUE(check(ok1, typeCheck).empty()) << check(ok1, typeCheck);
		EXPECT_TRUE(check(ok2, typeCheck).empty()) << check(ok2, typeCheck);
		ASSERT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_SWITCH_EXPR, "", Message::ERROR));
	}

	TEST(RefDeclTypeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto ok = builder.parseStmt("var ref<array<int<4>,2>> v = <ref<array<int<4>,2>>>(ref_decl(type_lit(ref<array<int<4>,2>>))) { 1, 2 };");
		auto errAddr =
			builder.parseAddressesStatement("var ref<array<int<4>,2>> v = <ref<array<int<4>,2>>>($ref_decl(type_lit(ref<array<int<4>,3>>))$) { 1, 2 };");
		ASSERT_EQ(errAddr.size(), 1);
		auto err = errAddr[0].getRootNode();

		CheckPtr typeCheck = makeRecursive(make_check<RefDeclTypeCheck>());
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(errAddr[0], EC_TYPE_REF_DECL_TYPE_MISMATCH, "", Message::ERROR));
	}

	TEST(BuildInLiterals, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		LiteralPtr ok = builder.getLangBasic().getFalse();
		LiteralPtr err = builder.literal(builder.genericType("strangeType"), ok->getValue());

		CheckPtr typeCheck = make_check<BuiltInLiteralCheck>();
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_TYPE_OF_LITERAL, "", Message::WARNING));
	}

	TEST(RefCastExpr, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

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

		EXPECT_PRED2(containsMSG, check(err1, typeCheck), Message(NodeAddress(err1), EC_TYPE_NON_REF_TO_REF_CAST, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2, typeCheck), Message(NodeAddress(err2), EC_TYPE_REF_TO_NON_REF_CAST, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err3, typeCheck), Message(NodeAddress(err3), EC_TYPE_REF_TO_NON_REF_CAST, "", Message::ERROR));
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

		EXPECT_PRED2(containsMSG, check(err1, typeCheck), Message(NodeAddress(err1), EC_TYPE_ILLEGAL_CAST, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2, typeCheck), Message(NodeAddress(err2), EC_TYPE_ILLEGAL_CAST, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err3, typeCheck), Message(NodeAddress(err3), EC_TYPE_ILLEGAL_CAST, "", Message::ERROR));
	}

	TEST(GenericZero, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto& base = manager.getLangBasic();

		// OK ... create a function literal

		TypePtr type = builder.genericType("int");
		TypePtr ref = builder.refType(type);
		TypePtr fun = builder.functionType(toVector(type), ref);

		auto zero = [&](const TypePtr& t) { return builder.callExpr(t, base.getZero(), builder.getTypeLiteral(t)); };

		NodePtr ok = zero(builder.genericType("A"));
		NodePtr err1 = zero(base.getInt4());
		NodePtr err2 = zero(base.getUnit());
		NodePtr err3 = zero(builder.parseType("struct A {}"));

		EXPECT_TRUE(check(ok).empty());
		ASSERT_FALSE(check(err1).empty());
		ASSERT_FALSE(check(err2).empty());
		ASSERT_FALSE(check(err3).empty());

		EXPECT_PRED2(containsMSG, check(err1), Message(NodeAddress(err1), EC_TYPE_ILLEGAL_GENERIC_ZERO_TYPE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2), Message(NodeAddress(err2), EC_TYPE_ILLEGAL_GENERIC_ZERO_TYPE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err3), Message(NodeAddress(err3), EC_TYPE_ILLEGAL_GENERIC_ZERO_TYPE, "", Message::ERROR));
	}

	TEST(KeywordCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// OK ... create correct and wrong instances

		TypePtr element = builder.genericType("A");
		NumericTypePtr size = builder.numericType(builder.intLit(8));

		CheckPtr typeCheck = make_check<KeywordCheck>();

		// test array
		{
			TypePtr ok = builder.parseType("array<int<4>,12u>");
			TypePtr err = builder.parseType("array<int<4>,12u,14>");

			EXPECT_FALSE(*ok == *err);

			EXPECT_TRUE(check(ok, typeCheck).empty());
			EXPECT_FALSE(check(err, typeCheck).empty());

			EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
		}

		// test array
		{
			TypePtr ok = builder.parseType("array<int<4>,12u>");
			TypePtr err = builder.parseType("array<12,int<4>>");

			EXPECT_FALSE(*ok == *err);

			EXPECT_TRUE(check(ok, typeCheck).empty());
			EXPECT_FALSE(check(err, typeCheck).empty());

			EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
		}

		// test references
		{
			TypePtr ok = lang::ReferenceType::create(builder.parseType("int<4>"));
			TypePtr err = builder.parseType("ref<int<4>,f>");

			EXPECT_TRUE(check(ok, typeCheck).empty());
			EXPECT_FALSE(check(err, typeCheck).empty());

			EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
		}

		// test channel
		{
			TypePtr ok = builder.parseType("channel<int<4>,12>");
			TypePtr err = builder.parseType("channel<12,int<4>>");

			EXPECT_FALSE(*ok == *err);

			EXPECT_TRUE(check(ok, typeCheck).empty());
			EXPECT_FALSE(check(err, typeCheck).empty());

			EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_ILLEGAL_USE_OF_TYPE_KEYWORD, "", Message::WARNING));
		}
	}

	TEST(ExternalFunctionType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// OK ... create a function literal
		TypePtr intType = builder.genericType("int");
		TypePtr boolType = builder.genericType("bool");
		FunctionTypePtr funTypeOK = builder.functionType(toVector(intType), boolType, FK_PLAIN);
		FunctionTypePtr funTypeERR = builder.functionType(toVector(intType), boolType, FK_CLOSURE);

		NodePtr ok = builder.literal(funTypeOK, "fun");
		NodePtr err = builder.literal(funTypeERR, "fun");

		CheckPtr typeCheck = make_check<ExternalFunctionTypeCheck>();
		EXPECT_TRUE(check(ok, typeCheck).empty());
		ASSERT_FALSE(check(err, typeCheck).empty());

		EXPECT_PRED2(containsMSG, check(err, typeCheck), Message(NodeAddress(err), EC_TYPE_INVALID_FUNCTION_TYPE, "", Message::ERROR));
	}

	TEST(LambdaExprType, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto& basic = manager.getLangBasic();

		// build a lambda expression which is fine
		LambdaExprPtr lambda = builder.parseExpr("alias int = int<4> ; (a : int)->int { return a; }").as<LambdaExprPtr>();

		ASSERT_TRUE(lambda);

		// get addresses to all kind of variables
		LambdaReferenceAddress outer = LambdaExprAddress(lambda)->getReference();
		LambdaReferenceAddress inner = LambdaExprAddress(lambda)->getDefinition()[0]->getReference();

		// check a correct version
		CheckPtr typeCheck = make_check<LambdaTypeCheck>();
		EXPECT_TRUE(check(lambda, typeCheck).empty()) << check(lambda, typeCheck);


		// build an invalid variable as a replacement
		LambdaReferencePtr invalid = builder.lambdaReference(outer->getType(),"-non-existing-");

		// case 1 - lambda expression selects non-existing body
		auto err = transform::replaceNode(manager, outer, invalid).as<LambdaExprPtr>();

		auto errors = check(err, typeCheck);
		EXPECT_EQ(1u, errors.size()) << errors;
		EXPECT_PRED2(containsMSG, errors, Message(NodeAddress(err), EC_TYPE_INVALID_LAMBDA_EXPR_NO_SUCH_DEFINITION, "", Message::ERROR));

		// case 2 - wrong type of lambda expression
		FunctionTypePtr invalidType = builder.functionType(TypeList(), basic.getUnit());
		err = transform::replaceNode(manager, LambdaExprAddress(lambda)->getType(), invalidType).as<LambdaExprPtr>();

		errors = check(err, typeCheck);
		EXPECT_EQ(1u, errors.size()) << errors;
		EXPECT_PRED2(containsMSG, errors, Message(NodeAddress(err), EC_TYPE_INVALID_LAMBDA_EXPR_TYPE, "", Message::ERROR));

		// case 3 - use invalid variable type in lambda
		invalid = builder.lambdaReference(invalidType,"_");
		err = transform::replaceNode(manager, inner.switchRoot(transform::replaceNode(manager, outer.switchRoot(err), invalid).as<LambdaExprPtr>()), invalid)
		          .as<LambdaExprPtr>();
		errors = check(err, typeCheck);
		EXPECT_EQ(1u, errors.size()) << errors;
		EXPECT_PRED2(containsMSG, errors, Message(NodeAddress(err), EC_TYPE_INVALID_LAMBDA_REC_VAR_TYPE, "", Message::ERROR));

		// case 4 - wrong lambda type
		VariablePtr param = lambda->getLambda()->getParameterList()[0];
		VariablePtr invalidParam = builder.variable(builder.refType(basic.getFloat()));

		err = transform::replaceAll(manager, lambda, param, invalidParam, transform::globalReplacement).as<LambdaExprPtr>();
		errors = check(err, typeCheck);
		EXPECT_EQ(1u, errors.size()) << errors;
		EXPECT_PRED2(containsMSG, errors, Message(LambdaExprAddress(err)->getParameterList()[0], EC_TYPE_INVALID_LAMBDA_TYPE, "", Message::ERROR));

		// case 5 - non-ref parameter type
		param = lambda->getLambda()->getParameterList()[0];
		invalidParam = builder.variable(basic.getInt4());

		err = transform::replaceAll(manager, lambda, param, invalidParam, transform::globalReplacement).as<LambdaExprPtr>();
		errors = check(err, typeCheck);
		EXPECT_EQ(1u, errors.size()) << errors;
		EXPECT_PRED2(containsMSG, errors, Message(LambdaExprAddress(err)->getParameterList()[0], EC_TYPE_INVALID_LAMBDA_PARAMETER_TYPE, "", Message::ERROR));


		// case 6 - parametes have to be the materialized version of the function type parts
		// here we have a cpp_ref as parameter and the function type has a plain reference
		auto int4 = builder.getLangBasic().getInt4();
		param = builder.variable(builder.refType(int4, true, false, lang::ReferenceType::Kind::CppReference));
		auto funType = builder.functionType(builder.refType(int4), basic.getUnit());
		err = builder.lambdaExpr(funType, { param }, builder.getNoOp());
		errors = check(err, typeCheck);
		EXPECT_EQ(1u, errors.size()) << errors;
		EXPECT_PRED2(containsMSG, errors, Message(LambdaExprAddress(err)->getParameterList()[0], EC_TYPE_INVALID_LAMBDA_TYPE, "", Message::ERROR));


		// case 7 - non-ref parameter type is ok if the materialized version of the parameter only has different const and volatile qualifiers
		// here we have a const int plain ref as parameter and the function type has a plain value
		param = builder.variable(builder.refType(int4, true));
		funType = builder.functionType(int4, basic.getUnit());
		lambda = builder.lambdaExpr(funType, { param }, builder.getNoOp());
		errors = check(lambda, typeCheck);
		EXPECT_EQ(0, errors.size()) << errors;
	}

	TEST(ArrayTypeChecks, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		CheckPtr typeCheck = getFullCheck();

		// create simple, context less array type
		TypePtr element = manager.getLangBasic().getInt4();
		TypePtr arrayType = builder.arrayType(element);
		TypePtr fixedArrayType = builder.arrayType(element, 12);
		TypePtr cur;

		EXPECT_TRUE(lang::isUnknownSizedArray(arrayType));
		EXPECT_TRUE(lang::isFixedSizedArray(fixedArrayType));

		// test something without context
		EXPECT_TRUE(check(arrayType, typeCheck).empty()) << check(arrayType, typeCheck);

		auto errors = check(arrayType, typeCheck);

		// ----- struct ------

//		// test it within a struct
//		cur = builder.structType(toVector(builder.field("a", arrayType)));
//		errors = check(cur, typeCheck);
//		EXPECT_TRUE(errors.empty()) << errors;
//
//		// .. a bigger struct
//		cur = builder.structType(toVector(builder.field("c", element), builder.field("a", arrayType)));
//		errors = check(cur, typeCheck);
//		EXPECT_TRUE(errors.empty()) << errors;
//
//		// it has to be the last element
//		cur = builder.structType(toVector(builder.field("a", arrayType), builder.field("c", element)));
//		errors = check(cur, typeCheck);
//		EXPECT_FALSE(errors.empty()) << errors;
//		EXPECT_EQ(1u, errors.size());
//		EXPECT_PRED2(containsMSG, check(cur, typeCheck), Message(NodeAddress(cur), EC_TYPE_INVALID_ARRAY_CONTEXT, "", Message::ERROR));
//
//		// and must not be present multiple times
//		cur = builder.structType(toVector(builder.field("a", arrayType), builder.field("b", element), builder.field("c", arrayType)));
//		errors = check(cur, typeCheck);
//		EXPECT_FALSE(errors.empty()) << errors;
//		EXPECT_EQ(1u, errors.size());
//		EXPECT_PRED2(containsMSG, check(cur, typeCheck), Message(NodeAddress(cur), EC_TYPE_INVALID_ARRAY_CONTEXT, "", Message::ERROR));
//
//		// may be nested inside another struct
//		cur = builder.structType(toVector(builder.field("a", builder.structType(toVector(builder.field("a", arrayType))))));
//		errors = check(cur, typeCheck);
//		EXPECT_TRUE(errors.empty()) << errors;
//
//
//		// ----- union ------
//
//		// also union
//		cur = builder.unionType(toVector(builder.field("c", element), builder.field("a", arrayType)));
//		errors = check(cur, typeCheck);
//		EXPECT_TRUE(errors.empty()) << errors;
//
//		// here the order is not important
//		cur = builder.unionType(toVector(builder.field("a", arrayType), builder.field("c", element)));
//		errors = check(cur, typeCheck);
//		EXPECT_TRUE(errors.empty()) << errors;


		// ----- tuples ------

		// test it within a tuple
		cur = builder.tupleType(toVector(fixedArrayType));
		errors = check(cur, typeCheck);
		EXPECT_TRUE(errors.empty()) << errors;

		// .. a bigger tuple
		cur = builder.tupleType(toVector(element, fixedArrayType));
		errors = check(cur, typeCheck);
		EXPECT_TRUE(errors.empty()) << errors;

		// variable size arrays are not allowed
		cur = builder.tupleType(toVector(arrayType, element));
		errors = check(cur, typeCheck);
		EXPECT_FALSE(errors.empty()) << errors;
		EXPECT_EQ(1u, errors.size());
		EXPECT_PRED2(containsMSG, check(cur, typeCheck), Message(NodeAddress(cur), EC_TYPE_INVALID_ARRAY_CONTEXT, "", Message::ERROR));

		// also not multiple times
		cur = builder.tupleType(toVector(arrayType, element, arrayType));
		errors = check(cur, typeCheck);
		EXPECT_FALSE(errors.empty()) << errors;
		EXPECT_EQ(2u, errors.size());
		EXPECT_PRED2(containsMSG, check(cur, typeCheck), Message(NodeAddress(cur), EC_TYPE_INVALID_ARRAY_CONTEXT, "", Message::ERROR));

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


	TEST(GenOperatorsChecks, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto& base = manager.getLangBasic();

		CheckPtr typeCheck = getFullCheck();

		auto i12 = builder.intLit(12);
		auto i14 = builder.intLit(14);
		auto r16 = builder.floatLit(16);

		auto s1 = builder.tupleExpr(i12);
		auto s2 = builder.tupleExpr(i14);

		auto v1 = builder.literal(builder.typeVariable("b"), "c");

		auto it = i12.getType();
		auto rt = r16.getType();

		auto ok1 = builder.callExpr(it, base.getGenAdd(), i12, i14);
		auto ok2 = builder.callExpr(rt, base.getGenSub(), r16, r16);
		auto ok3 = builder.callExpr(base.getGenSub(), v1, v1);

		auto err1 = builder.callExpr(it, base.getGenAdd(), i12, r16);
		auto err2 = builder.callExpr(base.getGenAdd(), s1, s2);

		EXPECT_TRUE(check(ok1).empty()) << check(ok1);
		EXPECT_TRUE(check(ok2).empty()) << check(ok2);
		EXPECT_TRUE(check(ok3).empty()) << check(ok3);

		EXPECT_FALSE(check(err1).empty());
		EXPECT_FALSE(check(err2).empty());

		EXPECT_PRED2(containsMSG, check(err1), Message(NodeAddress(err1), EC_TYPE_INVALID_ARGUMENT_TYPE, "", Message::ERROR));
		EXPECT_PRED2(containsMSG, check(err2), Message(NodeAddress(err2), EC_TYPE_INVALID_GENERIC_OPERATOR_APPLICATION, "", Message::ERROR));
	}



	TEST(ArrayTypeChecks, Exceptions) {
		NodeManager manager;
		IRBuilder builder(manager);

		CheckPtr typeCheck = getFullCheck();

		// create simple, context less array type
		TypePtr element = manager.getLangBasic().getInt4();
		TypePtr arrayType = builder.arrayType(element, 12);

		NodePtr cur;
		auto errors = check(arrayType, typeCheck);

		// allow arrays to be used within type literals
		cur = builder.getTypeLiteral(arrayType);
		errors = check(cur, typeCheck);
		EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

		ExpressionPtr arrayPtr = builder.parseExpr("*<ref<array<int<4>,12u>,f,f,plain>> {0}");

		// also, allow array values to be used within ref.new, ref.temp, struct, tuple and union expressions

		// ref.temp
		cur = builder.refTemp(arrayPtr);
		errors = check(cur, typeCheck);
		EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

		// ref.new
		cur = builder.refNew(arrayPtr);
		errors = check(cur, typeCheck);
		EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

		// struct expression
		TagTypePtr structType = builder.structType(toVector(builder.field("a", arrayType)));
		cur = builder.initExprTemp(builder.refType(structType), arrayPtr);
		errors = check(cur, typeCheck);
		EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

		// union expression
		TagTypePtr unionType = builder.unionType(toVector(builder.field("a", arrayType)));
		cur = builder.initExprTemp(builder.refType(unionType), arrayPtr);
		errors = check(cur, typeCheck);
		EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;

		// tuple expression
		cur = builder.tupleExpr(toVector(arrayPtr));
		errors = check(cur, typeCheck);
		EXPECT_TRUE(errors.empty()) << cur << "\n" << errors;
	}

	TEST(IllegalNumCastCheck, Simple2) {
		NodeManager manager;
		IRBuilder builder(manager);
		CheckPtr illegalNumCastCheckCheck = makeRecursive(make_check<IllegalNumCastCheck>());

		//legal numeric cast of constant to integral type
		{
			auto expr = builder.parseExpr("num_cast(3, type_lit(int<4>))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumCastCheckCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		//legal numeric cast of expression to integral type
		{
			auto expr = builder.parseExpr("num_cast(5 + 4, type_lit(int<4>))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumCastCheckCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		//legal numeric cast of constant to real type
		{
			auto expr = builder.parseExpr("num_cast(3, type_lit(real<4>))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumCastCheckCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		//legal numeric cast of constant to generic type
		{
			auto expr = builder.parseExpr("num_cast(3, type_lit('a))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumCastCheckCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		//legal numeric cast of constant to generic type
		{
			auto expr = builder.parseExpr("num_cast(3, type_lit(int<'a>))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumCastCheckCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		//illegal numeric cast from string type
		{
			auto expr = builder.parseExpr("num_cast(\"test\", type_lit(real<4>))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumCastCheckCheck);
			EXPECT_FALSE(checkResult.empty());
			auto errorString = toString(checkResult[0]);
			EXPECT_TRUE(errorString.find("SEMANTIC / ILLEGAL_NUM_CAST") != string::npos);
			EXPECT_TRUE(errorString.find("MSG: given source value is not of a numeric type") != string::npos) << errorString;
		}

		//illegal numeric cast of constant to non-type element
		{
			auto addresses = builder.parseAddressesExpression("num_cast(4, $type_lit(real<4>)$)");
			EXPECT_EQ(1u, addresses.size()) << "parsing error";
			auto expr = transform::replaceNode(manager, addresses[0], builder.parseExpr("(12)")).as<ExpressionPtr>();

			auto checkResult = check(expr);
			EXPECT_FALSE(checkResult.empty());
			auto errorString = toString(checkResult[0]);
			EXPECT_TRUE(errorString.find("TYPE / INVALID_ARGUMENT_TYPE") != string::npos) << errorString;
		}

		//illegal numeric cast to arbitrary type
		{
			auto expr = builder.parseExpr("num_cast(3, type_lit(foo))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumCastCheckCheck);
			EXPECT_FALSE(checkResult.empty());
			auto errorString = toString(checkResult[0]);
			EXPECT_TRUE(errorString.find("SEMANTIC / ILLEGAL_NUM_CAST") != string::npos);
			EXPECT_TRUE(errorString.find("MSG: given target type is not a numeric type") != string::npos);
		}
	}


	TEST(IllegalNumTypeToInt, Simple) {
		NodeManager manager;
		IRBuilder builder(manager);
		CheckPtr illegalNumTypeToIntCheck = makeRecursive(make_check<IllegalNumTypeToIntCheck>());

		//legal conversion from integral type to integral type
		{
			auto expr = builder.parseExpr("type_to_int(type_lit(int<4>))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumTypeToIntCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		//legal conversion from real type to integral type
		{
			auto expr = builder.parseExpr("type_to_int(type_lit(real<4>))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumTypeToIntCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		//legal conversion from integral type to integral type
		{
			auto expr = builder.parseStmt("{ var int<4> x = 0; type_to_int(type_of(x)); }");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumTypeToIntCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		//illegal conversion from arbitrary type to integral type
		{
			auto expr = builder.parseExpr("type_to_int(type_lit(foo))");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumTypeToIntCheck);
			EXPECT_FALSE(checkResult.empty());
			auto errorString = toString(checkResult[0]);
			EXPECT_TRUE(errorString.find("SEMANTIC / ILLEGAL_NUM_TYPE_TO_INT") != string::npos);
			EXPECT_TRUE(errorString.find("MSG: given source type is not a numeric type") != string::npos);
		}

		//illegal conversion from arbitrary type to integral type
		{
			auto expr = builder.parseStmt("{ var foo x = 0; type_to_int(type_of(x)); }");
			EXPECT_TRUE(expr) << "parsing error";

			auto checkResult = check(expr, illegalNumTypeToIntCheck);
			EXPECT_FALSE(checkResult.empty());
			auto errorString = toString(checkResult[0]);
			EXPECT_TRUE(errorString.find("SEMANTIC / ILLEGAL_NUM_TYPE_TO_INT") != string::npos);
			EXPECT_TRUE(errorString.find("MSG: given source type is not a numeric type") != string::npos);
		}
	}


TEST(RefToFunCastCheck, Simple) {
	NodeManager manager;
	IRBuilder builder(manager);
	CheckPtr RefToFunCastCheckCheck = makeRecursive(make_check<RefOfFunCastCheck>());

	//legal checks
	{
		auto expr = builder.parseExpr("ref_of_function(() -> unit { })");
		EXPECT_TRUE(expr) << "parsing error";
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	{
		auto expr = builder.parseExpr("ref_of_function((a : bool) -> bool { return a; })");
		EXPECT_TRUE(expr) << "parsing error";
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	{
		auto expr = builder.parseExpr("ref_of_function((_ : 'a) -> bool { return true; })");
		EXPECT_TRUE(expr) << "parsing error";
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	{
		auto expr = builder.parseExpr("ref_of_function((x : 'a) -> 'a { return x+CAST('a) 3; })");
		EXPECT_TRUE(expr) << "parsing error";
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	{
		auto expr = builder.parseExpr("ref_of_function(lit(\"x\":'a))");
		EXPECT_TRUE(expr) << "parsing error: " << expr;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	// illegal checks
	{
		auto expr = builder.parseExpr("ref_of_function((x : 'a) => x+CAST('a) 3)");
		EXPECT_TRUE(expr) << "parsing error: " << expr;
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_FALSE(checkResult.empty());
		auto errorString = toString(checkResult[0]);
		EXPECT_TRUE(errorString.find("MSG: this is a illegal ref_to_fun() cast!") != string::npos);
	}
	{
		auto expr = builder.parseExpr("ref_of_function(4)");
		EXPECT_TRUE(expr) << "parsing error: " << expr;
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_FALSE(checkResult.empty());
		auto errorString = toString(checkResult[0]);
		EXPECT_TRUE(errorString.find("MSG: this is a illegal ref_to_fun() cast!") != string::npos);
	}
}
TEST(PtrToFunCastCheck, Simple) {
	NodeManager manager;
	IRBuilder builder(manager);
	CheckPtr RefToFunCastCheckCheck = makeRecursive(make_check<RefOfFunCastCheck>());

	//legal checks
	{
		auto expr = builder.parseExpr("ptr_of_function(() -> unit { })");
		EXPECT_TRUE(expr) << "parsing error";
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	{
		auto expr = builder.parseExpr("ptr_of_function((a : bool) -> bool { return a; })");
		EXPECT_TRUE(expr) << "parsing error";
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	{
		auto expr = builder.parseExpr("ptr_of_function((_ : 'a) -> bool { return true; })");
		EXPECT_TRUE(expr) << "parsing error";
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	{
		auto expr = builder.parseExpr("ptr_of_function((x : 'a) -> 'a { return x+CAST('a) 3; })");
		EXPECT_TRUE(expr) << "parsing error";
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	{
		auto expr = builder.parseExpr("ptr_of_function(lit(\"x\":'a))");
		EXPECT_TRUE(expr) << "parsing error: " << expr;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	// illegal checks
	{
		auto expr = builder.parseExpr("ptr_of_function((x : 'a) => x+CAST('a) 3)");
		EXPECT_TRUE(expr) << "parsing error: " << expr;
		//if(getDetails)	std::cout << "detail: parsed expression: " << expr << std::endl;
		auto checkResult = check(expr, RefToFunCastCheckCheck);
		EXPECT_FALSE(checkResult.empty());
		auto errorString = toString(checkResult[0]);
		EXPECT_TRUE(errorString.find("MSG: this is a illegal ref_to_fun() cast!") != string::npos);
	}
}

} // end namespace checks
} // end namespace core
} // end namespace insieme
