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

#include <iostream>
#include <gtest/gtest.h>

#include "insieme/core/parser/detail/driver.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/checks/full_check.h"


namespace insieme {
namespace core {
namespace parser {

	using namespace detail;

	bool test_type(NodeManager& nm, const std::string& x) {
		InspireDriver driver(x, nm);
		driver.parseType();
		if(driver.result) {
//			std::cout << "Result of parsing " << x << "\n";
//			dumpColor(driver.result);
//			std::cout << "    ------------\n\n";
//			std::cout << " ============== TEST ============ " << std::endl;
			auto msg = checks::check(driver.result);
			EXPECT_TRUE(msg.empty()) << msg;
		} else {
			driver.printErrors();
		}
		return driver.result;
	}

	TEST(IR_Parser, Types) {
		NodeManager nm;
		EXPECT_TRUE(test_type(nm, "int<4>"));
		EXPECT_TRUE(test_type(nm, "someweirdname<>"));
		EXPECT_TRUE(test_type(nm, "vector<int<4>, 4>"));
		EXPECT_TRUE(test_type(nm, "vector<'a, 4>"));
		EXPECT_TRUE(test_type(nm, "struct { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "struct name { a : int<4> ; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let papa = t<11> in struct name : [papa] { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let papa = t<11> in struct name : [private papa] { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let papa = t<11> in struct name : [protected papa] { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let papa = t<11> in struct name : [public papa] { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "struct { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let int = int<4> in int"));

		EXPECT_TRUE(test_type(nm, "rf<int<4>>"));
		EXPECT_TRUE(test_type(nm, "() -> unit"));
		EXPECT_TRUE(test_type(nm, "(int<4>) -> int<4>"));
		EXPECT_TRUE(test_type(nm, "( int<4> , rf<int<4>>) -> int<4>"));
		EXPECT_TRUE(test_type(nm, "( int<4> , rf<int<4>>) => int<4>"));
		EXPECT_TRUE(test_type(nm, "(ary<'elem,'n>, vector<uint<8>,'n>) -> 'elem"));

		EXPECT_TRUE(test_type(nm, "let class = struct name { a : int<4>; b : int<5>; } in "
		                          "class::()->int<4> "));
		EXPECT_TRUE(test_type(nm, "let class = struct name { a : int<4>; b : int<5>; } in "
		                          "class::()~>int<4> "));
		EXPECT_TRUE(test_type(nm, "let class = struct name { a : int<4>; b : int<5>; } in "
		                          "~class::()"));
		EXPECT_TRUE(test_type(nm, "let class = struct name { a : int<4>; b : int<5>; } in "
		                          "class::()"));

		EXPECT_TRUE(test_type(nm, "struct C { field : int<4>; }"));
		EXPECT_TRUE(test_type(nm, "(rf<ary<rf<ary<struct{int : int<4>; float : real<4>; },1>>,1>>,"
		                          " rf<ary<rf<ary<real<4>,1>>,1>>,"
		                          " rf<ary<uint<8>,1>>)"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  b : int<4>;"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  ctor () { }"
		                          "}"));
		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  ctor () { }"
		                          "  ctor (a : int<4>) { }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  dtor () { }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  dtor virtual () { }"
		                          "}"));

		EXPECT_FALSE(test_type(nm, "struct class {" //multiple destructors
		                           "  a : int<4>;"
		                           "  dtor () { }"
		                           "  dtor () { }"
		                           "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  lambda f : () -> int<4> { return 1; }"
		                          "}"));

		EXPECT_FALSE(test_type(nm, "struct class {" //multiple functions with the same name
		                           "  a : int<4>;"
		                           "  lambda f : () -> int<4> { return 1; }"
		                           "  lambda f : (a : int<4>) -> int<4> { return 1; }"
		                           "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  lambda f : () -> int<4> { return 1; }"
		                          "  lambda g : (b : int<4>) -> int<4> { return b; }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  const lambda b : () -> int<4> { return 1; }"
		                          "  volatile lambda c : () -> int<4> { return 1; }"
		                          "  volatile const lambda d : (a : int<4>) -> int<4> { return 1; }"
		                          "  const volatile lambda e : (a : int<4>) -> int<4> { return 1; }"
		                          "  virtual const lambda f : () -> int<4> { return 1; }"
		                          "  virtual volatile lambda g : () -> int<4> { return 1; }"
		                          "  virtual volatile const lambda h : (a : int<4>) -> int<4> { return 1; }"
		                          "  virtual const volatile lambda i : (a : int<4>) -> int<4> { return 1; }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  pure virtual b : () -> int<4>"
		                          "  pure virtual const c : () -> int<4>"
		                          "  pure virtual volatile d : () -> int<4>"
		                          "  pure virtual volatile const e : (int<4>) -> int<4>"
		                          "  pure virtual const volatile f : (int<4>) -> int<4>"
		                          "}"));

		EXPECT_FALSE(test_type(nm, "struct class {" //wrong member order
		                           "  a : int<4>;"
		                           "  lambda f : () -> int<4> { return 1; }"
		                           "  lambda g : (a : int<4>) -> int<4> { return a; }"
		                           "  dtor () { }"
		                           "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  ctor () { }"
		                          "  ctor (a : int<4>) { }"
		                          "  dtor () { }"
		                          "  lambda f : () -> int<4> { return 1; }"
		                          "  virtual const volatile lambda g : () -> int<4> { return 1; }"
		                          "  pure virtual h : () -> int<4>"
		                          "}"));

//		EXPECT_TRUE(test_type(nm, "struct class {"
//		                          "  lambda f : () -> int<4> { return f(); }"
//		                          "  lambda g : (a : int<4>) -> int<4> { return f(); }"
//		                          "}"));
	}

	bool test_expression(NodeManager& nm, const std::string& x) {
		InspireDriver driver(x, nm);
		driver.parseExpression();
		if(driver.result) {
//			std::cout << driver.result << std::endl;
//			dumpColor(driver.result);
			auto msg = checks::check(driver.result);
			EXPECT_TRUE(msg.empty()) << msg;
		} else {
			driver.printErrors();
		}
		//   std::cout << " ============== TEST ============ " << std::endl;
		return driver.result;
	}

	TEST(IR_Parser, RecordTypes) {
		NodeManager nm;

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own field
		                          "  a : int<4>;"
		                          "  lambda f : () -> int<4> { return *a; }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own field using the this pointer
		                          "  a : int<4>;"
		                          "  lambda f : () -> int<4> { return *this.a; }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference the field of another struct - that struct is created locally. this also tests the init expression
		                          "  a : int<4>;"
		                          "};"
		                          "def struct B {"
		                          "  lambda f : () -> int<4> {"
		                          "    var ref<A,f,f,plain> a;"
		                          "    return *(a.a);"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference the field of another struct - that struct is held as a field
		                          "  a : int<4>;"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda f : () -> int<4> {"
		                          "    return *(a.a);"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference the field of another struct - that struct is held as a field and used in an expression
		                          "  a : int<4>;"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda f : () -> int<4> {"
		                          "    return a.a + 5;"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference the field of another struct - that struct is held as a field and it is accessed using the this pointer
		                          "  a : int<4>;"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda f : () -> int<4> {"
		                          "    return *(this.a.a);"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own member function
		                          "  lambda f : () -> int<4> { return 1; }"
		                          "  lambda g : () -> int<4> { return f(); }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own member function using the this pointer
		                          "  lambda f : () -> int<4> { return 1; }"
		                          "  lambda g : () -> int<4> { return this.f(); }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct
		                          "  lambda f : () -> int<4> {"
		                          "    return 1;"
		                          "  }"
		                          "};"
		                          "def struct B {"
		                          "  lambda g : (a : ref<A,f,f,plain>) -> int<4> {"
		                          "    return a.f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct - that struct is held as a field
		                          "  lambda f : () -> int<4> { return 1; }"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda g : () -> int<4> {"
		                          "    return a.f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct - that struct is held as a field and it is accessed using the this pointer
		                          "  lambda f : () -> int<4> { return 1; }"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda g : () -> int<4> {"
		                          "    return this.a.f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "decl struct B;" //reference another struct which has been declared using a forward declaration
		                          "def struct A {"
		                          "  lambda f : (b : ref<B,f,f,plain>) -> int<4> {"
		                          "    return 1;"
		                          "  }"
		                          "};"
		                          "def struct B {"
		                          "}; A"));

//		EXPECT_TRUE(test_type(nm, "decl struct B;" //reference another struct's member function which has been declared using a forward declaration
//		                          "decl B :: g : () -> int<4>;"
//		                          "def struct A {"
//		                          "  lambda f : (b : B) -> int<4> {"
//		                          "    return b.g();"
//		                          "  }"
//		                          "};"
//		                          "def struct B {"
//		                          "  lambda h : () -> int<4> {"
//		                          "    return 1;"
//		                          "  }"
//		                          "}; A"));
	}

	TEST(IR_Parser, Strings) {
		NodeManager nm;
		EXPECT_TRUE(test_expression(nm, "\"foo\""));
		EXPECT_TRUE(test_expression(nm, "\"\"foo\"\""));
		EXPECT_TRUE(test_expression(nm, "\"5\""));
		EXPECT_TRUE(test_expression(nm, "\"\"foo\\nbar\"\""));
	}

	TEST(IR_Parser, Expressions) {
		NodeManager nm;
		EXPECT_TRUE(test_expression(nm, "true?1:0"));
		EXPECT_TRUE(test_expression(nm, "type_lit(1)"));

		EXPECT_TRUE(test_expression(nm, "1"));
		EXPECT_TRUE(test_expression(nm, "1u"));
		EXPECT_TRUE(test_expression(nm, "1l"));
		EXPECT_TRUE(test_expression(nm, "1ul"));
		EXPECT_TRUE(test_expression(nm, "1ll"));
		EXPECT_TRUE(test_expression(nm, "1ull"));

		EXPECT_TRUE(test_expression(nm, "1.0f"));
		EXPECT_TRUE(test_expression(nm, "1.0"));

		EXPECT_TRUE(test_expression(nm, "1 + 3"));
		EXPECT_TRUE(test_expression(nm, "1 - 0"));
		EXPECT_TRUE(test_expression(nm, "1 * 0"));
		EXPECT_TRUE(test_expression(nm, "1 / 0"));
		EXPECT_TRUE(test_expression(nm, "1 | 0"));
		EXPECT_TRUE(test_expression(nm, "1 & 0"));
		EXPECT_TRUE(test_expression(nm, "1 ^ 0"));

		// precedence
		EXPECT_TRUE(test_expression(nm, "1 + 0 * 5"));
		EXPECT_TRUE(test_expression(nm, "1 * 0 + 5"));

		EXPECT_TRUE(test_expression(nm, "(1.0)"));
		EXPECT_TRUE(test_expression(nm, "((1.0))"));
		EXPECT_TRUE(test_expression(nm, "((1.0) + 4.0)"));

		EXPECT_TRUE(test_expression(nm, "1 + 2 * 3"));
		EXPECT_TRUE(test_expression(nm, "(1 + 2) * 3"));

		EXPECT_TRUE(test_expression(nm, "alias uni = union{ a : int<4>; }; <uni> { 4 }"));
		EXPECT_FALSE(test_expression(nm, "alias uni = union{ a : int<4>; }; <uni> { \"4\" }"));
		EXPECT_FALSE(test_expression(nm, "alias uni = union{ a : int<4>; }; <uni> { 4, 5 }"));
		EXPECT_TRUE(test_expression(nm, "alias x = struct{ a : int<4>; }; <x> { 4 }"));
		EXPECT_TRUE(test_expression(nm, "alias x = struct{ }; <x> { }"));

		EXPECT_FALSE(test_expression(nm, "x"));

		EXPECT_TRUE(test_expression(nm, "(_ : 'a) -> bool { return true; }"));
		EXPECT_TRUE(test_expression(nm, "(x : 'a) -> 'a { return x+CAST('a) 3; }"));
		EXPECT_TRUE(test_expression(nm, "(x : 'a) => x+CAST('a) 3"));

		// return type deduction
		EXPECT_TRUE(test_expression(nm, "() => { return true; }"));
		EXPECT_TRUE(test_expression(nm, "(x : bool) => { if(x) { return true; } else { return false; } }"));
		EXPECT_TRUE(test_expression(nm, "(x : bool) => { if(x) { return 1; } else { return -5; } }"));

		EXPECT_TRUE(test_expression(nm, "type_lit(int<4>)"));

		EXPECT_TRUE(test_expression(nm, "decl f : ()->unit;"
		                                "let f = ()->unit { 5; ()->unit { f(); } (); } in f"));

		EXPECT_TRUE(test_expression(nm, "(v : int<4>, exp : int<4>) -> int<4> { "
		                                "	let one = (_ : int<4>)=>4; "
		                                "	let two = (x : int<4>)=>x+exp; "
		                                "    return one(two(exp));"
		                                "}"));
		EXPECT_TRUE(test_expression(nm, "(v : int<4>, exp : int<4>) -> int<4> { "
		                                "	let one = (_ : int<4>)-> int<4> { return 4;  };"
		                                "	let two = (x : int<4>)-> int<4> { return x+5; };"
		                                "    return one(two(exp));"
		                                "}"));
	}

	TEST(IR_Parser, Precedence) {
		NodeManager mgr;
		EXPECT_EQ("int_add(1, int_mul(2, 3))", toString(*parseExpr(mgr, "1+2*3")));
		EXPECT_EQ("int_mul(int_add(1, 2), 3)", toString(*parseExpr(mgr, "(1+2)*3")));
		EXPECT_EQ("int_add(1, int_mul(2, 3))", toString(*parseExpr(mgr, "1+(2*3)")));
	}

	TEST(IR_Parser, LambdasAndFunctions) {
		NodeManager nm;
		IRBuilder builder(nm);

//		auto funA = builder.normalize(parseExpr(nm, "(x : int<4>) -> int<4> { return x; }"));
//		auto funB = builder.normalize(parseExpr(nm, "(x : ref<int<4>,f,f,plain>) -> int<4> { return *x; }"));
//
//		auto funC = builder.normalize(parseExpr(nm, "let f = (x : int<4>) -> int<4> { return x; } in f"));
//		auto funD = builder.normalize(parseExpr(nm, "let f = (y : ref<int<4>,f,f,plain>) -> int<4> { return *y; } in f"));
//
//		EXPECT_EQ(funA, funB);
//		EXPECT_EQ(funB, funC);
//		EXPECT_EQ(funC, funD);

		EXPECT_TRUE(test_expression(nm, "decl foo : (int<4>) -> int<4>;" //self-recursion
		                                "def foo = (a : int<4>) -> int<4> { return foo(a); }; foo"));

		EXPECT_TRUE(test_expression(nm, "decl foo : (int<4>) -> int<4>;" //mutual recursion
		                                "decl bar : (int<4>) -> int<4>;"
		                                "def bar = (a : int<4>) -> int<4> { return foo(a); };"
		                                "def foo = (a : int<4>) -> int<4> { return bar(a); };"
		                                "foo"));

		EXPECT_TRUE(test_expression(nm, "decl foo : (int<4>) -> int<4>;" //mutual recursion without declarations for both functions
		                                "def bar = (a : int<4>) -> int<4> { return foo(a); };"
		                                "def foo = (a : int<4>) -> int<4> { return bar(a); };"
		                                "foo"));

		EXPECT_TRUE(test_expression(nm, "decl foo : (int<4>) -> int<4>;" //mutual recursion without declarations for each function
		                                "def bar = (a : int<4>) -> int<4> { return foo(a); };"
		                                "def baz = (a : int<4>) -> int<4> { return bar(a); };"
		                                "def foo = (a : int<4>) -> int<4> { return baz(a); };"
		                                "foo"));

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def a = ()->unit { }; a");
			auto type2 = builder.parseType("def a = function ()->unit { }; a");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def a = (b : int<4>)->int<4> { return b; }; a");
			auto type2 = builder.parseType("def a = function (b : ref<int<4>,f,f,plain>)->int<4> { return *b; }; a");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { lambda a : ()->unit { } }; s");
			auto type2 = builder.parseType("def struct s { function a : ()->unit { } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { lambda a : (b : int<4>)->int<4> { return b; } }; s");
			auto type2 = builder.parseType("def struct s { function a : (b : ref<int<4>,f,f,plain>)->int<4> { return *b; } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("decl struct s; def struct s { lambda a : ()->ref<s,f,f,plain> { return this; } }; s");
			auto type2 = builder.parseType("decl struct s; def struct s { function a : ()->ref<s,f,f,plain> { return *this; } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { ctor () { } }; s");
			auto type2 = builder.parseType("def struct s { ctor function () { } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { ctor (b : int<4>) { } }; s");
			auto type2 = builder.parseType("def struct s { ctor function (b : ref<int<4>,f,f,plain>) { } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}
	}

	bool test_statement(NodeManager& nm, const std::string& x) {
		InspireDriver driver(x, nm);
		driver.parseStmt();
		if(driver.result) {
//			dumpColor(driver.result);
			auto msg = checks::check(driver.result);
			EXPECT_TRUE(msg.empty()) << msg;
		} else {
			driver.printErrors();
		}
//		std::cout << " ============== TEST ============ " << std::endl;
		return driver.result;
	}

	TEST(IR_Parser, Statements) {
		NodeManager nm;
		EXPECT_TRUE(test_statement(nm, "1;"));
		EXPECT_TRUE(test_statement(nm, "1u;"));

		EXPECT_TRUE(test_statement(nm, "1.0f;"));
		EXPECT_TRUE(test_statement(nm, "1.0;"));

		EXPECT_TRUE(test_statement(nm, "1 + 3;"));
		EXPECT_TRUE(test_statement(nm, "1 - 0;"));
		EXPECT_TRUE(test_statement(nm, "1 * 0;"));
		EXPECT_TRUE(test_statement(nm, "1 / 0;"));
		EXPECT_TRUE(test_statement(nm, "1 | 0;"));
		EXPECT_TRUE(test_statement(nm, "1 & 0;"));
		EXPECT_TRUE(test_statement(nm, "1 ^ 0;"));

		EXPECT_TRUE(test_statement(nm, "(1.0);"));
		EXPECT_TRUE(test_statement(nm, "((1.0));"));

		EXPECT_FALSE(test_statement(nm, "x"));
		EXPECT_FALSE(test_statement(nm, "x;"));

		EXPECT_TRUE(test_statement(nm, "{ var int<4> x = 0; x+1; }"));
		EXPECT_TRUE(test_statement(nm, "{ var ref<int<4>,f,f,plain> x; }"));
		EXPECT_TRUE(test_statement(nm, "{ auto x = 0; x+1; }"));

		EXPECT_TRUE(test_statement(nm, "if ( true ) {}"));
		EXPECT_TRUE(test_statement(nm, "if ( true ) {} else {}"));
		EXPECT_TRUE(test_statement(nm, "if ( true ) { if ( false ) { } else { } }"));
		EXPECT_TRUE(test_statement(nm, "if ( true ) { if ( false ) { } else { } } else { } "));
		EXPECT_TRUE(test_statement(nm, "if( false ) { return 0; } else { return 1+2; }"));
		EXPECT_TRUE(test_statement(nm, "if( false ) { return 0; }"));
		EXPECT_TRUE(test_statement(nm, "if(1 != 0) { return 0; }"));
		EXPECT_TRUE(test_statement(nm, "while ( true ) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "while ( false ) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "while ( false || true ) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3: 2) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3: 2) { 1+1; }"));

		EXPECT_TRUE(test_statement(nm, "switch (2) { case 1: { 1; } case 2: { 2; } }"));
		EXPECT_FALSE(test_statement(nm, "switch (2) { case 1: { 1; } case 2: { 2; } default { 2; }}"));
		EXPECT_FALSE(test_statement(nm, "switch (2) { case 1+1: { 1; } case 2: { 2; } default: { 2; }}"));

		EXPECT_TRUE(test_statement(nm, "{ }"));


		EXPECT_TRUE(test_statement(nm, "alias type = struct a { a : int<4>; b : int<8>; };"
		                               "{"
		                               "    var ref<type,f,f,plain> variable;"
		                               "    var ref<rf<type>,f,f,plain> var2;"
		                               "    auto var3 = var2;"
		                               "}"));

		EXPECT_TRUE(test_statement(nm, "alias class = struct name { a : int<2>; };"
		                               "alias collection = array<class, 10>;"
		                               "{"
		                               "    var ref<collection,f,f,plain> x;"
		                               "    var int<2> y = CAST(int<2>) 5;"
		                               "    x[5].a = y;"
		                               "}"));
	}


	TEST(IR_Parser, Tuples) {
		NodeManager nm;

		EXPECT_TRUE(test_statement(nm, "alias int = int<4>;"
		                               "decl f : (int,(int),(int,int),(int,int,int)) -> unit;"
		                               "{ "
		                               "  f(1,(1),(1,2),(1,2,3));"
		                               "}"));

		EXPECT_TRUE(test_statement(nm, "alias int = int<4>;"
		                               "decl f : (int,(int),(int,int),(int,int,int)) -> unit;"
		                               "{ "
		                               "  f(1+2,(1+2),(1,2),(1,2,3+3));"
		                               "}"));
	}

	bool test_program(NodeManager& nm, const std::string& x) {

		InspireDriver driver(x, nm);

		driver.parseProgram();

		if(driver.result) {
//			dumpColor(driver.result);
			auto msg = checks::check(driver.result);
			EXPECT_TRUE(msg.empty()) << msg;
		} else {
			driver.printErrors();
		}
//		   std::cout << " ============== TEST ============ " << std::endl;
		return driver.result;
	}

	TEST(IR_Parser, Program) {
		NodeManager nm;
		EXPECT_TRUE(test_program(nm, "int<4> main (a : ref<int<4>,f,f,plain>, b : ref<int<4>,f,f,plain>)  { return 1+1; }"));
		EXPECT_TRUE(test_program(nm, "alias int = int<4>; int main (a : ref<int,f,f,plain>, b : ref<int,f,f,plain>) { return 1+1; }"));
		EXPECT_TRUE(test_program(nm, "alias int = int<4>; def f = (a : int) -> int { return a; }; int main (a : ref<int,f,f,plain>, b : ref<int,f,f,plain>) { return f(1); }"));
		EXPECT_TRUE(test_program(nm, "alias int = int<4>;"
		                             "def h = (f : (int)->int) -> int { return f(5); };"
		                             "decl f : (int) -> int;"
		                             "decl g : (int) -> int;"
		                             "def f = (a : int) -> int {"
		                             "    h(f);"
		                             "    f(4);"
		                             "    g(f(4));"
		                             "    return h(g);"
		                             "};"
		                             "def g = (a : int) -> int {"
		                             "    h(f);"
		                             "    f(g(4));"
		                             "    g(4);"
		                             "    return h(g);"
		                             "};"
		                             "unit main() { f(1); }"));
	}


	TEST(IR_Parser, Scopes) {
		NodeManager nm;
		IRBuilder builder(nm);

		EXPECT_FALSE(test_statement(nm, "{" //re-declaration of variable
		                                "  var int<4> a = 0;"
		                                "  (a : int<4>, a : int<4>) -> unit { a; };"
		                                "  a;"
		                                "}"));

		EXPECT_FALSE(test_statement(nm, "{" //re-declaration of variable
		                                "  var int<4> a = 0;"
		                                "  (a : int<4>) -> unit { a; var int<4> a = 0; };"
		                                "  a;"
		                                "}"));


		{
		auto addresses = builder.parseAddressesStatement("{" //correct opening of scope before the parameters of the lambda
		                                                 "  var int<4> a = 0;"
		                                                 "  (a : int<8>) -> unit { $a$; };"
		                                                 "  $a$;"
		                                                 "}");
		ASSERT_EQ(2, addresses.size());
		EXPECT_EQ(nm.getLangBasic().getInt8(), addresses[0].getAddressedNode().as<CallExprPtr>()->getType());
		EXPECT_EQ(nm.getLangBasic().getInt4(), addresses[1].getAddressedNode().as<VariablePtr>()->getType());
		}

		{
		auto addresses = builder.parseAddressesStatement("{" //correct opening of scope before the parameters of the lambda as well as in compound statements
		                                                  "  var int<4> a = 0;"
		                                                  "  (a : int<8>) -> unit {"
		                                                  "    $a$;"
		                                                  "    if (a == 0) {"
		                                                  "      var int<16> a = 0;"
		                                                  "      $a$;"
		                                                  "    }"
		                                                  "  };"
		                                                  "  $a$;"
		                                                  "}");
		ASSERT_EQ(3, addresses.size());
		EXPECT_EQ(nm.getLangBasic().getInt8(),  addresses[0].getAddressedNode().as<CallExprPtr>()->getType());
		EXPECT_EQ(nm.getLangBasic().getInt16(), addresses[1].getAddressedNode().as<VariablePtr>()->getType());
		EXPECT_EQ(nm.getLangBasic().getInt4(),  addresses[2].getAddressedNode().as<VariablePtr>()->getType());
		}

		{
		auto type = builder.parseType("struct A {" //correct access to field and param with the same name
		                              "  a : int<4>;"
		                              "  lambda f : (a : int<8>) -> unit {"
		                              "    a;"
		                              "    this.a;"
		                              "  }"
		                              "}");
		auto memberFunction = type.as<TagTypePtr>()->getRecord()->getMemberFunctions()[0];
		auto body = memberFunction->getImplementation().as<LambdaExprPtr>()->getBody();

		auto stmt1 = body[0];
		auto stmt2 = body[1];

		EXPECT_EQ(nm.getLangBasic().getInt8(), stmt1.as<CallExprPtr>()->getType());
		EXPECT_EQ(nm.getLangBasic().getInt4(), analysis::getReferencedType(stmt2.as<CallExprPtr>()->getType()));
		}


		EXPECT_FALSE(test_expression(nm, "decl a : int<4>;" //declaring the same variable twice
		                                 "decl a : int<4>;"
		                                 "a"));

		EXPECT_FALSE(test_expression(nm, "decl a : () -> unit;" //declaring the same function twice
		                                 "decl a : () -> unit;"
		                                 "a"));

		EXPECT_FALSE(test_type(nm, "decl struct A;" //declaring the same class name twice
		                           "decl struct A;"
		                           "A"));

		EXPECT_FALSE(test_expression(nm, "def a = () -> unit {};" //defining the same function name twice
		                                 "def a = () -> unit {};"
		                                 "a"));

		EXPECT_FALSE(test_type(nm, "def struct A { a : int<8>; };" //defining the same class name twice
		                           "def struct A { a : int<8>; };"
		                           "A"));
	}

	TEST(IR_Parser, References) {
		NodeManager nm;
		IRBuilder builder(nm);

		{
			auto addresses = builder.parseAddressesStatement("def struct A {" //check that member access returns the correct ref/non-ref variant
			                                                 "  a : int<4>;"
			                                                 "};"
			                                                 "{"
			                                                 "  var A a;"
			                                                 "  var ref<A,f,f,plain> ra;"
			                                                 "  $a.a$;"
			                                                 "  $ra.a$;"
			                                                 "}");
			ASSERT_EQ(2, addresses.size());
			EXPECT_FALSE(analysis::isRefType(addresses[0].getAddressedNode().as<ExpressionPtr>()->getType()));
			EXPECT_TRUE( analysis::isRefType(addresses[1].getAddressedNode().as<ExpressionPtr>()->getType()));
		}

		{
			auto addresses = builder.parseAddressesStatement("def struct A {" //check that member access returns the correct ref variant
			                                                 "  a : int<4>;"
			                                                 "};"
			                                                 "{"
			                                                 "  var ref<A,f,f,plain> ra;"
			                                                 "  var ref<A,t,f,plain> ca;"
			                                                 "  var ref<A,f,t,plain> va;"
			                                                 "  var ref<A,t,t,plain> cva;"
			                                                 "  $ra.a$;"
			                                                 "  $ca.a$;"
			                                                 "  $va.a$;"
			                                                 "  $cva.a$;"
			                                                 "}");
			ASSERT_EQ(4, addresses.size());
			auto reference0 = lang::ReferenceType(addresses[0].getAddressedNode().as<ExpressionPtr>()->getType());
			EXPECT_FALSE(reference0.isConst());
			EXPECT_FALSE(reference0.isVolatile());
			EXPECT_TRUE (reference0.isPlain());

			auto reference1 = lang::ReferenceType(addresses[1].getAddressedNode().as<ExpressionPtr>()->getType());
			EXPECT_TRUE (reference1.isConst());
			EXPECT_FALSE(reference1.isVolatile());
			EXPECT_TRUE (reference1.isPlain());

			auto reference2 = lang::ReferenceType(addresses[2].getAddressedNode().as<ExpressionPtr>()->getType());
			EXPECT_FALSE(reference2.isConst());
			EXPECT_TRUE (reference2.isVolatile());
			EXPECT_TRUE (reference2.isPlain());

			auto reference3 = lang::ReferenceType(addresses[3].getAddressedNode().as<ExpressionPtr>()->getType());
			EXPECT_TRUE (reference3.isConst());
			EXPECT_TRUE (reference3.isVolatile());
			EXPECT_TRUE (reference3.isPlain());
		}
	}

} // parser
} // core
} // insieme
