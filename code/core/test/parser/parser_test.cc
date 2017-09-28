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

#include <iostream>
#include <gtest/gtest.h>

#include "insieme/core/parser/detail/driver.h"

#include "insieme/core/analysis/compare.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/parallel.h"

#include "insieme/utils/name_mangling.h"


namespace insieme {
namespace core {
namespace parser {

	using namespace detail;

	bool test_type(NodeManager& nm, const std::string& x) {
		IRBuilder builder(nm);
		try {
			auto result = builder.parseType(x);
//			std::cout << "Result of parsing " << x << "\n";
//			dumpColor(result);
//			std::cout << "    ------------\n\n";
//			std::cout << " ============== TEST ============ " << std::endl;
			auto msg = checks::check(result);
			if (!msg.empty()) {
				std::cout << msg;
			}
			return msg.empty();

		} catch (const IRParserException& ex) {
			std::cout << ex.what() << std::endl;
		}
		return false;
	}

	bool test_statement(NodeManager& nm, const std::string& x);
	bool test_expression(NodeManager& nm, const std::string& x);
	bool test_program(NodeManager& nm, const std::string& x);

	TEST(IR_Parser, Types) {
		NodeManager nm;
		EXPECT_TRUE(test_type(nm, "int<4>"));
		EXPECT_TRUE(test_type(nm, "someweirdname<>"));
		EXPECT_TRUE(test_type(nm, "'a"));
		EXPECT_TRUE(test_type(nm, "'a..."));
		EXPECT_TRUE(test_type(nm, "'a<>"));
		EXPECT_TRUE(test_type(nm, "'a...<>"));
		EXPECT_TRUE(test_type(nm, "vector<int<4>, 4>"));
		EXPECT_TRUE(test_type(nm, "vector<'a, 4>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,f,f,plain>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,f,t,plain>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,t,f,plain>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,t,t,plain>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,f,f,cpp_ref>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,f,f,cpp_rref>"));
		EXPECT_TRUE(test_type(nm, "ptr<'a>"));
		EXPECT_TRUE(test_type(nm, "ptr<'a,f,f>"));
		EXPECT_TRUE(test_type(nm, "ptr<'a,t,f>"));
		EXPECT_TRUE(test_type(nm, "ptr<'a,f,t>"));
		EXPECT_TRUE(test_type(nm, "ptr<'a,t,t>"));
		EXPECT_TRUE(test_type(nm, "struct { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "struct name { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let papa = t<11> in struct name : [papa] { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let papa = t<11> in struct name : [private papa] { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let papa = t<11> in struct name : [protected papa] { a : int<4>; b : int<5>; }"));
		EXPECT_TRUE(test_type(nm, "let papa = t<11> in struct name : [public papa] { a : int<4>; b : int<5>; }"));
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
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {" //multiple functions with the same name
		                          "  a : int<4>;"
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "  lambda f = (a : int<4>) -> int<4> { return 1; }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "  lambda g = (b : int<4>) -> int<4> { return b; }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  const lambda b = () -> int<4> { return 1; }"
		                          "  volatile lambda c = () -> int<4> { return 1; }"
		                          "  volatile const lambda d = (a : int<4>) -> int<4> { return 1; }"
		                          "  const volatile lambda e = (a : int<4>) -> int<4> { return 1; }"
		                          "  virtual const lambda f = () -> int<4> { return 1; }"
		                          "  virtual volatile lambda g = () -> int<4> { return 1; }"
		                          "  virtual volatile const lambda h = (a : int<4>) -> int<4> { return 1; }"
		                          "  virtual const volatile lambda i = (a : int<4>) -> int<4> { return 1; }"
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
		                           "  lambda f = () -> int<4> { return 1; }"
		                           "  lambda g = (a : int<4>) -> int<4> { return a; }"
		                           "  dtor () { }"
		                           "}"));

		EXPECT_TRUE(test_type(nm, "struct class {"
		                          "  a : int<4>;"
		                          "  ctor () { }"
		                          "  ctor (a : int<4>) { }"
		                          "  dtor () { }"
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "  virtual const volatile lambda g = () -> int<4> { return 1; }"
		                          "  pure virtual h : () -> int<4>"
		                          "}"));
	}

	TEST(IR_Parser, TypeVariables) {

		NodeManager nm;
		IRBuilder builder(nm);

		EXPECT_TRUE(builder.parseType("'a").isa<TypeVariablePtr>());
		EXPECT_TRUE(builder.parseType("'a...").isa<VariadicTypeVariablePtr>());
		EXPECT_TRUE(builder.parseType("'a<>").isa<GenericTypeVariablePtr>());
		EXPECT_TRUE(builder.parseType("'a...<>").isa<VariadicGenericTypeVariablePtr>());

		EXPECT_TRUE(builder.parseType("'a<'b>").isa<GenericTypeVariablePtr>());
		EXPECT_TRUE(builder.parseType("'a...<'b>").isa<VariadicGenericTypeVariablePtr>());


		EXPECT_EQ("'a",      toString(*builder.parseType("'a")));
		EXPECT_EQ("'a...",   toString(*builder.parseType("'a...")));
		EXPECT_EQ("'a<>",    toString(*builder.parseType("'a<>")));
		EXPECT_EQ("'a...<>", toString(*builder.parseType("'a...<>")));

		EXPECT_EQ("'a<'b>",    toString(*builder.parseType("'a<'b>")));
		EXPECT_EQ("'a...<'b>", toString(*builder.parseType("'a...<'b>")));

	}


	bool test_expression(NodeManager& nm, const std::string& x) {
		IRBuilder builder(nm);
		try {
			auto result = builder.parseExpr(x);
//			std::cout << "Result of parsing " << x << "\n";
//			dumpColor(result);
//			std::cout << "    ------------\n\n";
//			std::cout << " ============== TEST ============ " << std::endl;
			auto msg = checks::check(result);
			if (!msg.empty()) {
				std::cout << msg;
			}
			return msg.empty();

		} catch (const IRParserException& ex) {
			std::cout << ex.what() << std::endl;
		}
		return false;
	}

	TEST(IR_Parser, RecordTypes) {
		NodeManager nm;
		IRBuilder builder(nm);

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own field
		                          "  a : int<4>;"
		                          "  lambda f = () -> int<4> { return a; }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //write our own field
		                          "  a : int<4>;"
		                          "  lambda f = () -> unit { a = 5; }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own field using the this pointer
		                          "  a : int<4>;"
		                          "  lambda f = () -> int<4> { return this.a; }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference the field of another struct - that struct is created locally. this also tests the init expression
		                          "  a : int<4>;"
		                          "};"
		                          "def struct B {"
		                          "  lambda f = () -> int<4> {"
		                          "    var ref<A,f,f,plain> a;"
		                          "    return a.a;"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference the field of another struct - that struct is held as a field
		                          "  a : int<4>;"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda f = () -> int<4> {"
		                          "    return a.a;"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference the field of another struct - that struct is held as a field and used in an expression
		                          "  a : int<4>;"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda f = () -> int<4> {"
		                          "    return a.a + 5;"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference the field of another struct - that struct is held as a field and it is accessed using the this pointer
		                          "  a : int<4>;"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda f = () -> int<4> {"
		                          "    return this.a.a;"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "decl struct B;" //reference the field of another struct which has been declared previously
		                          "decl B::b : int<4>;"
		                          "def struct A {"
		                          "  lambda f = () -> int<4> {"
		                          "    var ref<B,f,f,plain> b;"
		                          "    return b.b;"
		                          "  }"
		                          "};"
		                          "def struct B {"
		                          "  b : int<4>;"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own member function
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "  lambda g = () -> int<4> { return f(); }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "struct class {" //reference out own member function (with parameters)
		                          "  lambda f = (a : int<4>) -> int<4> { return 42; }"
		                          "  lambda g = (a : int<4>) -> int<4> { return f(5); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own member function (function variant)
		                          "  function f = () -> int<4> { return 1; }"
		                          "  function g = () -> int<4> { return f(); }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "struct class {" //reference out own member function (with parameters) (function variant)
		                          "  function f = (a : ref<int<4>,f,f,plain>) -> int<4> { return 42; }"
		                          "  function g = (a : ref<int<4>,f,f,plain>) -> int<4> { return f(*a); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "decl struct class;" //reference our own member function which has been declared previously
		                          "decl g : class::() -> int<4>;"
		                          "struct class {"
		                          "  lambda f = () -> int<4> { return 42; }"
		                          "  lambda g = () -> int<4> { return f(); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "decl struct class;" //reference our own member function which has been declared previously (with parameters)
		                          "decl g : class::(int<4>) -> int<4>;"
		                          "struct class {"
		                          "  lambda f = (a : int<4>) -> int<4> { return 42; }"
		                          "  lambda g = (a : int<4>) -> int<4> { return f(5); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "decl struct class;" //reference our own member function which has been declared previously (function variant)
		                          "decl g : class::() -> int<4>;"
		                          "struct class {"
		                          "  function f = () -> int<4> { return 42; }"
		                          "  function g = () -> int<4> { return f(); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "decl struct class;" //reference our own member function which has been declared previously (with parameters) (function variant)
		                          "decl g : class::(int<4>) -> int<4>;"
		                          "struct class {"
		                          "  function f = (a : ref<int<4>,f,f,plain>) -> int<4> { return 42; }"
		                          "  function g = (a : ref<int<4>,f,f,plain>) -> int<4> { return f(*a); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "decl struct class;" //reference the current member function which has been declared previously
		                          "decl f : class::() -> int<4>;"
		                          "struct class {"
		                          "  lambda f = () -> int<4> { return f(); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "decl struct class;" //reference the current member function which has been declared previously (function variant)
		                          "decl f : class::() -> int<4>;"
		                          "struct class {"
		                          "  function f = () -> int<4> { return f(); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "decl struct class;" //reference the current member function which has been declared previously
		                          "decl f : class::(int<4>) -> int<4>;"
		                          "struct class {"
		                          "  lambda f = (a : int<4>) -> int<4> { return f(a); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "decl struct class;" //reference the current member function which has been declared previously (function variant)
		                          "decl f : class::(int<4>) -> int<4>;"
		                          "struct class {"
		                          "  function f = (a : ref<int<4>,f,f,plain>) -> int<4> { return f(*a); }"
		                          "}"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own member function using the this pointer
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "  lambda g = () -> int<4> { return this.f(); }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference our own member function using the this pointer (function variant)
		                          "  function f = () -> int<4> { return 1; }"
		                          "  function g = () -> int<4> { return this.f(); }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct
		                          "  lambda f = () -> int<4> {"
		                          "    return 1;"
		                          "  }"
		                          "};"
		                          "def struct B {"
		                          "  lambda g = (a : ref<A,f,f,plain>) -> int<4> {"
		                          "    return a.f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct (function variant)
		                          "  function f = () -> int<4> {"
		                          "    return 1;"
		                          "  }"
		                          "};"
		                          "def struct B {"
		                          "  function g = (a : ref<ref<A,f,f,plain>,f,f,plain>) -> int<4> {"
		                          "    return (*a).f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct - that struct is held as a field
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda g = () -> int<4> {"
		                          "    return a.f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct - that struct is held as a field (function variant)
		                          "  function f = () -> int<4> { return 1; }"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  function g = () -> int<4> {"
		                          "    return a.f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct - that struct is held as a field and it is accessed using the this pointer
		                          "  lambda f = () -> int<4> { return 1; }"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  lambda g = () -> int<4> {"
		                          "    return this.a.f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //reference a member function of another struct - that struct is held as a field and it is accessed using the this pointer (function variant)
		                          "  function f = () -> int<4> { return 1; }"
		                          "};"
		                          "def struct B {"
		                          "  a : A;"
		                          "  function g = () -> int<4> {"
		                          "    return this.a.f();"
		                          "  }"
		                          "}; B"));

		EXPECT_TRUE(test_type(nm, "decl struct B;" //reference another struct which has been declared using a forward declaration
		                          "def struct A {"
		                          "  lambda f = (b : ref<B,f,f,plain>) -> int<4> {"
		                          "    return 1;"
		                          "  }"
		                          "};"
		                          "def struct B {"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "decl struct B;" //reference another struct's member function which has been declared using a forward declaration
		                          "decl g : B::() -> int<4>;"
		                          "def struct A {"
		                          "  lambda f = (b : ref<B,f,f,plain>) -> int<4> {"
		                          "    return b.g();"
		                          "  }"
		                          "};"
		                          "def struct B {"
		                          "  lambda g = () -> int<4> {"
		                          "    return 1;"
		                          "  }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct a { };"
		                          "def struct b : [ public a ] { };"
		                          "def struct c : [ public b ] { };"
		                          "c"));

		EXPECT_TRUE(test_type(nm, "def struct a { };"
				                  "def struct b { };"
				                  "def struct c : [ public a, public b ] { };"
				                  "c"));

		EXPECT_TRUE(test_type(nm, "def struct a { };"
				                  "def struct b { };"
				                  "def struct c : [ virtual public a, public b ] { };"
				                  "c"));

		EXPECT_TRUE(test_type(nm, "def struct a { };"
					              "def struct b { };"
					              "def struct c : [ virtual public a, virtual public b ] { };"
					              "c"));

		{
			auto addresses = builder.parseAddressesStatement("def struct A {" //check that member calls get translated to calls of the actual lambda
			                                                 "  ctor() {}"
			                                                 "  dtor() {}"
			                                                 "  lambda f = ()->unit {}"
			                                                 "};"
			                                                 "{"
			                                                 "  var ref<A,f,f,plain> ra;"
			                                                 "  $A::(ra)$;"
			                                                 "  $ra.f()$;"
			                                                 "  $A::~(ra)$;"
			                                                 "}");

			ASSERT_EQ(3, addresses.size());
			EXPECT_EQ(NT_LambdaExpr, addresses[0].getAddressedNode().as<CallExprPtr>()->getFunctionExpr()->getNodeType());
			EXPECT_EQ(NT_LambdaExpr, addresses[1].getAddressedNode().as<CallExprPtr>()->getFunctionExpr()->getNodeType());
			EXPECT_EQ(NT_LambdaExpr, addresses[2].getAddressedNode().as<CallExprPtr>()->getFunctionExpr()->getNodeType());
		}
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
		EXPECT_TRUE(test_expression(nm, "2.3E-5f"));
		EXPECT_TRUE(test_expression(nm, "2.0E+0"));

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

		EXPECT_FALSE(test_expression(nm, "def union uni { a : int<4>; }; <ref<uni>> { \"4\" }"));
		EXPECT_FALSE(test_expression(nm, "def union uni { a : int<4>; }; <ref<uni>> { 4, 5 }"));
		EXPECT_TRUE(test_expression(nm, "def struct x { a : int<4>; b : int<4>; }; <ref<x>> { 4, 5 }"));
		EXPECT_TRUE(test_expression(nm, "def struct x { a : int<4>; }; <ref<x>> { 4 }"));
		EXPECT_TRUE(test_expression(nm, "def struct x { }; <ref<x>> { }"));

		EXPECT_TRUE(test_type(nm, "def union uni { a : int<4>; lambda f = ()->unit {} }; uni"));
		EXPECT_TRUE(test_statement(nm, "def union uni { a : int<4>; lambda f = ()->unit {} }; { var ref<uni,f,f,plain> a; }"));
		EXPECT_TRUE(test_statement(nm, "def union uni { a : int<4>; lambda f = ()->unit {} }; { <ref<uni>> { 4 }; }"));

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

		// calls on variadic lambda
		EXPECT_TRUE(test_expression(nm, "decl variadic : ('a...) -> unit;"
		                                "variadic()"));
		EXPECT_TRUE(test_expression(nm, "decl variadic : ('a...) -> unit;"
		                                "variadic(1)"));
		EXPECT_TRUE(test_expression(nm, "decl variadic : ('a...) -> unit;"
		                                "variadic(1, 2)"));
	}

	TEST(IR_Parser, FreeVariable) {
			NodeManager nm;
			EXPECT_TRUE(test_expression(nm, "free_var(bool)"));
			EXPECT_TRUE(test_expression(nm, "let x = free_var(bool) in true || x"));
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

		auto funA = builder.normalize(parseExpr(nm, "(x : int<4>) -> int<4> { return x; }"));
		auto funB = builder.normalize(parseExpr(nm, "(x : ref<int<4>,f,f,plain>) -> int<4> { return *x; }"));

		auto funC = builder.normalize(parseExpr(nm, "function (x : ref<int<4>,f,f,plain>) -> int<4> { return *x; }"));
		auto funD = builder.normalize(parseExpr(nm, "function (x : ref<ref<int<4>,f,f,plain>,f,f,plain>) -> int<4> { return **x; }"));

		EXPECT_TRUE(analysis::equalNameless(funA, funC)) << "funA: " << funA << "\nfunC: " << funC << "\n";
		EXPECT_TRUE(analysis::equalNameless(funB, funD)) << "funB: " << funB << "\nfunD: " << funD << "\n";;

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
			auto type1 = builder.parseType("def struct s { lambda a = ()->unit { } }; s");
			auto type2 = builder.parseType("def struct s { function a = ()->unit { } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { lambda a = (b : int<4>)->int<4> { return b; } }; s");
			auto type2 = builder.parseType("def struct s { function a = (b : ref<int<4>,f,f,plain>)->int<4> { return *b; } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("decl struct s; def struct s { lambda a = ()->ref<s,f,f,plain> { return this; } }; s");
			auto type2 = builder.parseType("decl struct s; def struct s { function a = ()->ref<s,f,f,plain> { return this; } }; s");

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

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { a : int<4>; ctor () { a = 5; } }; s");
			auto type2 = builder.parseType("def struct s { a : int<4>; ctor function () { a = 5; } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { a : int<4>; ctor () { a = 5; } }; s");
			auto type2 = builder.parseType("def struct s { a : int<4>; ctor function () { a = 5; } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { a : int<4>; ctor (b : int<4>) { a = b; } }; s");
			auto type2 = builder.parseType("def struct s { a : int<4>; ctor function (b : ref<int<4>,f,f,plain>) { a = *b; } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { dtor () { } }; s");
			auto type2 = builder.parseType("def struct s { dtor function () { } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ //ensure that functions and lambdas end up the same when written correctly
			auto type1 = builder.parseType("def struct s { a : int<4>; dtor () { a = 5; } }; s");
			auto type2 = builder.parseType("def struct s { a : int<4>; dtor function () { a = 5; } }; s");

			ASSERT_TRUE(checks::check(type1).empty()) << checks::check(type1);
			ASSERT_TRUE(checks::check(type2).empty()) << checks::check(type2);

			EXPECT_EQ(builder.normalize(type1), builder.normalize(type2));
		}

		{ // check lookup of member lambdas
			auto expr = builder.parseExpr("def struct S { lambda foo = () -> unit { 42; } }; S::foo");

			ASSERT_TRUE(expr);
			ASSERT_TRUE(checks::check(expr).empty()) << checks::check(expr);
		}
	}

	TEST(IR_Parser, LambdaNames) {
		NodeManager nm;
		IRBuilder builder(nm);

		//normal functions
		{
			auto lambda = builder.parseExpr("def foo = () -> unit { }; foo").as<LambdaExprPtr>();
			EXPECT_EQ("foo", lambda->getReference()->getNameAsString());
		}

		//member functions
		{
			auto addresses = builder.parseAddressesType("def struct A { lambda foo = () -> unit { $1$; } }; A");
			ASSERT_EQ(1, addresses.size());
			EXPECT_EQ("A::foo", addresses[0].getParentAddress(3).getAddressedNode().as<LambdaBindingPtr>()->getReference()->getNameAsString());
		}

		//constructors
		{
			auto addresses = builder.parseAddressesType("def struct A { ctor() { $1$; } }; A");
			ASSERT_EQ(1, addresses.size());
			EXPECT_EQ("A::ctor", addresses[0].getParentAddress(3).getAddressedNode().as<LambdaBindingPtr>()->getReference()->getNameAsString());
		}

		//free member functions
		{
			auto addresses = builder.parseAddressesStatement("def struct A { }; def A::lambda foo = () -> unit { 1; }; { var ref<A> a; $a.foo()$;}");
			ASSERT_EQ(1, addresses.size());
			EXPECT_EQ("A::foo", addresses[0].getAddressedNode().as<CallExprPtr>()->getFunctionExpr().as<LambdaExprPtr>()->getReference()->getNameAsString());
		}

		//free constructor
		{
			auto addresses = builder.parseAddressesStatement("def struct A { }; def A:: foo = ctor () { 1; }; { var ref<A> a = $foo(a)$;}");
			ASSERT_EQ(1, addresses.size());
			EXPECT_EQ("foo", addresses[0].getAddressedNode().as<CallExprPtr>()->getFunctionExpr().as<LambdaExprPtr>()->getReference()->getNameAsString());
		}
	}

	bool test_statement(NodeManager& nm, const std::string& x) {
		IRBuilder builder(nm);
		try {
			auto result = builder.parseStmt(x);
//			std::cout << "Result of parsing " << x << "\n";
//			dumpColor(result);
//			std::cout << "    ------------\n\n";
//			std::cout << " ============== TEST ============ " << std::endl;
			auto msg = checks::check(result);
			if (!msg.empty()) {
				std::cout << msg;
			}
			return msg.empty();

		} catch (const IRParserException& ex) {
			std::cout << ex.what() << std::endl;
		}
		return false;
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
		                               "alias collection = array<class, 10u>;"
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
		IRBuilder builder(nm);
		try {
			auto result = builder.parseProgram(x);
//			std::cout << "Result of parsing " << x << "\n";
//			dumpColor(result);
//			std::cout << "    ------------\n\n";
//			std::cout << " ============== TEST ============ " << std::endl;
			auto msg = checks::check(result);
			if (!msg.empty()) {
				std::cout << msg;
			}
			return msg.empty();

		} catch (const IRParserException& ex) {
			std::cout << ex.what() << std::endl;
		}
		return false;
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
		                              "  lambda f = (a : int<8>) -> unit {"
		                              "    a;"
		                              "    this.a;"
		                              "  }"
		                              "}");
		core::MemberFunctionPtr memberFunction;
		for (auto mfun : type.as<TagTypePtr>()->getRecord()->getMemberFunctions()) {
			if (mfun->getNameAsString() == "f")
				memberFunction = mfun;
		}
		assert_true(memberFunction) << "could not find member function in struct: \n" << dumpColor(type);
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
			                                                 "  var A a = *ref_temp(type_lit(ref<A>));"
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
				                                             "  ra.a = 7;"
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

			EXPECT_EQ(checks::check(addresses[0].getRootNode()).size(), 0);
		}
	}

	TEST(IR_Parser, ThisMemberAccess) {
		NodeManager nm;
		IRBuilder builder(nm);

		{
			auto addresses = builder.parseAddressesType(R"(
				def struct A {
					a : int<4>;
					const lambda foo = () -> unit { $this.a$; $a$; }
				};
				A
			)");
			ASSERT_EQ(2, addresses.size());
			ASSERT_TRUE(core::lang::isReference(addresses[0]));
			EXPECT_TRUE(core::lang::ReferenceType(addresses[0]).isConst());
			ASSERT_TRUE(core::lang::isReference(addresses[1]));
			EXPECT_TRUE(core::lang::ReferenceType(addresses[1]).isConst());
		}

	}

	TEST(IR_Parser, MemberFunctionLookup) {
		NodeManager nm;
		IRBuilder builder(nm);

		EXPECT_TRUE(test_type(nm, "def struct A {" //referencing a member function which has been declared
		                          "  lambda f = () -> unit { }"
		                          "  lambda g = () -> unit { f(); }"
		                          "}; A"));

		EXPECT_TRUE(test_type(nm, "def struct A {" //referencing a member function which has been declared using the this pointer
		                          "  lambda f = () -> unit { }"
		                          "  lambda g = () -> unit { this.f(); }"
		                          "}; A"));

		EXPECT_FALSE(test_type(nm, "def struct A {" //referencing a member function which has not been declared
		                           "  lambda f = () -> unit { f(); }"
		                           "}; A"));

		EXPECT_FALSE(test_type(nm, "def struct A {" //referencing a member function which has not been declared using the this pointer
		                           "  lambda f = () -> unit { this.f(); }"
		                           "}; A"));

		EXPECT_TRUE(test_statement(nm, "def struct A {" //externally referencing a member function which has been declared
		                               "  lambda f = () -> unit { }"
		                               "};"
		                               "{"
		                               "  var ref<A,f,f,plain> a;"
		                               "  a.f();"
		                               "}"));

		{
			auto addresses = builder.parseAddressesStatement("def struct A {" //externally referencing a member function which has been declared multiple times
			                                                 "  lambda f = () -> bool { return false; }"
			                                                 "  lambda f = (a : int<4>) -> int<4> { return a; }"
			                                                 "};"
			                                                 "{"
			                                                 "  var ref<A,f,f,plain> a;"
			                                                 "  $a.f()$;"
			                                                 "  $a.f(5)$;"
			                                                 "}");

			ASSERT_EQ(2, addresses.size());
			EXPECT_EQ(nm.getLangBasic().getBool(), addresses[0].getAddressedNode().as<CallExprPtr>()->getType());
			EXPECT_EQ(nm.getLangBasic().getInt4(), addresses[1].getAddressedNode().as<CallExprPtr>()->getType());
		}

		{
			auto addresses = builder.parseAddressesStatement("def struct A {" //externally referencing a member function which has been declared multiple times
			                                                 "  lambda f = (a : bool) -> bool { return a; }"
			                                                 "  lambda f = (a : int<4>) -> int<4> { return a; }"
			                                                 "};"
			                                                 "{"
			                                                 "  var ref<A,f,f,plain> a;"
			                                                 "  $a.f(false)$;"
			                                                 "  $a.f(5)$;"
			                                                 "}");

			ASSERT_EQ(2, addresses.size());
			EXPECT_EQ(nm.getLangBasic().getBool(), addresses[0].getAddressedNode().as<CallExprPtr>()->getType());
			EXPECT_EQ(nm.getLangBasic().getInt4(), addresses[1].getAddressedNode().as<CallExprPtr>()->getType());
		}
	}

	TEST(IR_Parser, ManuallyBuilt) {

		NodeManager nm;
		IRBuilder builder(nm);
		auto& basic = nm.getLangBasic();
		auto& refExt = nm.getLangExtension<core::lang::ReferenceExtension>();

		auto parsed = parseType(nm, R"(
			def struct A {
				i : int<4>;
				ctor() { i = 0; }
				ctor(other: ref<A,t,f,cpp_ref>) = delete;
				ctor(other: ref<A,f,f,cpp_rref>) = delete;
				lambda )" + utils::getMangledOperatorAssignName() + R"( = (other: ref<A,t,f,cpp_ref>) -> ref<A,f,f,cpp_ref> = delete;
				lambda )" + utils::getMangledOperatorAssignName() + R"( = (other: ref<A,t,f,cpp_rref>) -> ref<A,f,f,cpp_ref> = delete;
			};
			A
		)");

		parsed = builder.normalize(parsed);

		auto parsedChecked = checks::check(parsed);
		ASSERT_TRUE(parsedChecked.empty()) << parsedChecked;

		auto field = builder.field("i", basic.getInt4());
		auto fields = toVector(field);
		auto thisType = builder.refType(builder.tagTypeReference("A"));
		auto thisVariable = builder.variable(builder.refType(thisType));
		auto ctorType = builder.functionType(builder.types(toVector<TypePtr>(thisType)), thisType, FK_CONSTRUCTOR);
		auto fieldAccess = builder.callExpr(refExt.getRefMemberAccess(), builder.deref(thisVariable), builder.getIdentifierLiteral("i"),
			                                builder.getTypeLiteral(basic.getInt4()));
		auto ctorBody = builder.compoundStmt(builder.assign(fieldAccess, builder.intLit(0)));
		auto ctor = builder.lambdaExpr(ctorType, builder.parameters(thisVariable), ctorBody, "A::ctor");

		auto constructed = builder.structType("A", ParentList(), fields, toVector<ExpressionPtr>(ctor),
			analysis::getDefaultDestructor(thisType, builder.parents(), builder.fields(fields)), false, MemberFunctionList(), PureVirtualMemberFunctionList());

		constructed = builder.normalize(constructed);

		auto constructedChecked = checks::check(constructed);
		ASSERT_TRUE(constructedChecked.empty()) << constructedChecked;

		EXPECT_EQ(parsed, constructed);
	}

	TEST(IR_Parser, DefaultConstructedCalls) {
		NodeManager nm;
		IRBuilder builder(nm);

		const std::string testString = "var ref<A,t,f,cpp_ref> a_ref;"
		                               "var ref<A,f,f,cpp_rref> a_rref;"
		                               "var ref<A,f,f,plain> a = A::(ref_decl(type_lit(ref<A>)));"                     //call the default constructor
		                               "var ref<A,f,f,plain> a_copy = A::(ref_decl(type_lit(ref<A>)), a_ref);"         //call the copy constructor
		                               "var ref<A,f,f,plain> a_move = A::(ref_decl(type_lit(ref<A>)), a_rref);"        //call the move constructor
		                               "A::~(a);"                                                  //call the default destructor
		                               "a." + utils::getMangledOperatorAssignName() + "(a_ref);"   //call the default copy assignment operator
		                               "a." + utils::getMangledOperatorAssignName() + "(a_rref);"; //call the default move assignment operator

		{
			auto res = builder.parseStmt("def struct A { };" //call the default generated members outside the struct
			                              "{"
			                              + testString +
			                              "}");
			EXPECT_TRUE(res);
			EXPECT_TRUE(checks::check(res).empty()) << checks::check(res);
		}

		{
			auto res = builder.parseType("def struct A {" //call the default generated members within the struct itself
			                             "  lambda f = () -> unit {"
			                             + testString +
			                             "  }"
			                             "}; A");
			EXPECT_TRUE(res);
			EXPECT_TRUE(checks::check(res).empty()) << checks::check(res);
		}

		{
			auto res = builder.parseType("decl struct A;" //call the default generated members in another struct after only a forward declaration has been encountered
			                             "def struct B {"
			                             "  lambda f = () -> unit {"
			                             + testString +
			                             "  }"
			                             "};"
			                             "def struct A { }; B");
			EXPECT_TRUE(res);
			EXPECT_TRUE(checks::check(res).empty()) << checks::check(res);
		}
	}

	TEST(IR_Parser, DefaultedAndDeletedMembers) {
		NodeManager nm;
		IRBuilder builder(nm);

		{
			auto typ = builder.parseType(R"(
				def struct S0 {
					ctor() = delete;
					ctor(other: ref<S0,f,f,cpp_rref>) = delete;
				};
				S0
			)");

			EXPECT_TRUE(typ);
			//std::cout << printer::PrettyPrinter(typ, printer::PrettyPrinter::PRINT_DEFAULT_MEMBERS) << std::endl;
			EXPECT_TRUE(checks::check(typ).empty()) << checks::check(typ);
			EXPECT_EQ(0, typ.as<TagTypePtr>()->getRecord()->getConstructors().size());
		}
		{
			auto typ = builder.parseType(R"(
				def struct S1 {
					ctor() = default;
					ctor(a : type) {}
				};
				S1
			)");

			EXPECT_TRUE(typ);
			//std::cout << printer::PrettyPrinter(typ, printer::PrettyPrinter::PRINT_DEFAULT_MEMBERS) << std::endl;
			EXPECT_TRUE(checks::check(typ).empty()) << checks::check(typ);
			EXPECT_EQ(4, typ.as<TagTypePtr>()->getRecord()->getConstructors().size());
		}

		{
			auto typ = builder.parseType(R"(
				def struct S2 {
					lambda )" + utils::getMangledOperatorAssignName() + R"( = (other: ref<S2,t,f,cpp_ref>) -> ref<S2,f,f,cpp_ref> = delete;
				};
				S2
			)");

			EXPECT_TRUE(typ);
			//std::cout << printer::PrettyPrinter(typ, printer::PrettyPrinter::PRINT_DEFAULT_MEMBERS) << std::endl;
			EXPECT_TRUE(checks::check(typ).empty()) << checks::check(typ);
			EXPECT_EQ(0, typ.as<TagTypePtr>()->getRecord()->getMemberFunctions().size());
		}
		{
			auto typ = builder.parseType(R"(
				def struct S3 {
					lambda )" + utils::getMangledOperatorAssignName() + R"( = (other: ref<S3,t,f,cpp_ref>) -> ref<S3,f,f,cpp_ref> = default;
				};
				S3
			)");

			EXPECT_TRUE(typ);
			//std::cout << printer::PrettyPrinter(typ, printer::PrettyPrinter::PRINT_DEFAULT_MEMBERS) << std::endl;
			EXPECT_TRUE(checks::check(typ).empty()) << checks::check(typ);
			EXPECT_EQ(1, typ.as<TagTypePtr>()->getRecord()->getMemberFunctions().size());
		}

		{
			auto typ = builder.parseType(R"(
				def struct S4 {
					dtor() = delete;
				};
				S4
			)");

			EXPECT_TRUE(typ);
			//std::cout << printer::PrettyPrinter(typ, printer::PrettyPrinter::PRINT_DEFAULT_MEMBERS) << std::endl;
			EXPECT_TRUE(checks::check(typ).empty()) << checks::check(typ);
			EXPECT_FALSE(typ.as<TagTypePtr>()->getRecord()->hasDestructor());
		}
		{
			auto typ = builder.parseType(R"(
				def struct S5 {
					dtor() = default;
				};
				S5
			)");

			EXPECT_TRUE(typ);
			//std::cout << printer::PrettyPrinter(typ, printer::PrettyPrinter::PRINT_DEFAULT_MEMBERS) << std::endl;
			EXPECT_TRUE(checks::check(typ).empty()) << checks::check(typ);
			EXPECT_TRUE(typ.as<TagTypePtr>()->getRecord()->hasDestructor());
		}
	}

	TEST(IR_Parser, TypedExpression) {
		NodeManager nm;

		const std::string commonCode = "def g = (a : int<4>) -> unit {};";

		//calling with the correct type
		EXPECT_TRUE(test_statement(nm, commonCode + "{ g(42); }"));

		//manually specifying a type which is not correct
		EXPECT_FALSE(test_statement(nm, commonCode + "{ var int<8> param = 42; g(param : int<4>); }"));

		//manually specifying a type which is not correct
		EXPECT_FALSE(test_statement(nm, commonCode + "{ g(42 : int<1>); }"));

		//as with wrong size conversions, mixing types also doesn't work
		EXPECT_FALSE(test_statement(nm, commonCode + "{ var real<4> realParam = 5.0; g(realParam : int<4>); }"));
	}

	TEST(IR_Parser, ManualOverloadSelection) {
		NodeManager nm;

		const std::string commonClass = "def struct A {"
		                                "  ctor(a : int<2>) {}"
		                                "  ctor(a : int<4>) {}"
		                                "  ctor(a : int<8>) {}"
		                                "  ctor(a : int<16>) {}"
		                                "  ctor(a : int<4>, b : int<4>) {}"
		                                "  ctor(a : int<8>, b : int<4>) {}"
		                                "  lambda f = (a : int<2>) -> unit {}"
		                                "  lambda f = (a : int<4>) -> unit {}"
		                                "  lambda f = (a : int<8>) -> unit {}"
		                                "  lambda f = (a : int<16>) -> unit {}"
		                                "  lambda f = (a : int<4>, b : int<4>) -> unit {}"
		                                "  lambda f = (a : int<8>, b : int<4>) -> unit {}"
		                                "  lambda g = (a : ref<int<4>>) -> unit {}"
		                                "  lambda g = (a : ref<int<4>,t,f>) -> unit {}"
		                                "  lambda g = (a : ref<int<4>,f,t>) -> unit {}"
		                                "  lambda g = (a : ref<int<4>,t,t>) -> unit {}"
		                                "};";

		//multiple possible overloads. Simple call fails for constructor with a single param
		EXPECT_FALSE(test_statement(nm, commonClass + "{ var int<1> param = 5; var ref<A> a = A::(a), param); }"));

		//multiple possible overloads. Simple call fails for constructor with multiple params
		EXPECT_FALSE(test_statement(nm, commonClass + "{ var int<1> param = 5; var ref<A> a = A::(a), param, param); }"));

		//multiple possible overloads. Simple call fails for function with a single param
		EXPECT_FALSE(test_statement(nm, commonClass + "{ var int<1> param = 5; var ref<A> a; a.f(param); }"));

		//multiple possible overloads. Simple call fails for function with multiple params
		EXPECT_FALSE(test_statement(nm, commonClass + "{ var int<1> param = 5; var ref<A> a; a.f(param, param); }"));


		//multiple possible overloads. Specifying the desired overload will work for constructor with a single param
		EXPECT_TRUE(test_statement(nm, commonClass + "{ var int<1> param = lit(\"5\" : int<1>); var ref<A> a = A::(a, param : int<4>); }"));

		//multiple possible overloads. Specifying the desired overload will work for constructor with multiple params
		EXPECT_TRUE(test_statement(nm, commonClass + "{ var int<1> param = lit(\"5\" : int<1>); var ref<A> a = A::(a, param : int<4>, param : int<4>); }"));

		//multiple possible overloads. Specifying the desired overload will work for function with a single param
		EXPECT_TRUE(test_statement(nm, commonClass + "{ var int<1> param = lit(\"5\" : int<1>); var ref<A> a; a.f(param : int<4>); }"));

		//multiple possible overloads. Specifying the desired overload will work for function with a single param
		EXPECT_TRUE(test_statement(nm, commonClass + "{ var int<1> param = lit(\"5\" : int<1>); var ref<A> a; a.f(param : int<4>, param : int<4>); }"));


		//calling the default generated constructs without specifying the desired overload should fail as we have two candidates
		EXPECT_FALSE(test_statement(nm, "def struct A { };"
		                     "{"
		                     "  var ref<A> a;"
		                     "  var ref<A> a_copy = A::(a_copy, a);"
		                     "}"));

		//calling the default generated assignment operator without specifying the desired overload should fail as we have two candidates
		EXPECT_FALSE(test_statement(nm, "def struct A { };"
		                     "{"
		                     "  var ref<A> a;"
		                     "  a." + utils::getMangledOperatorAssignName() + "(a);"
		                     "}"));

		// call g and manually specify which const/volatile overload to take
		EXPECT_TRUE(test_statement(nm, commonClass +
			                           "{"
			                           "  var ref<A> a;"
			                           "  var ref<int<4>> b;"
			                           "  a.g(b : ref<int<4>,f,f>);"
			                           "  a.g(b : ref<int<4>,t,f>);"
			                           "  a.g(b : ref<int<4>,f,t>);"
			                           "  a.g(b : ref<int<4>,t,t>);"
			                           "}"));
	}

	TEST(IR_Parser, FreeMembers) {
		NodeManager nm;

		//free ctors are used differently than the ones defined within record types
		//member functions can be used just like the ones defined within the record type itself
		EXPECT_TRUE(test_statement(nm, "def struct A {"
		                               "  a : int<4>;"
		                               "  ctor(x : int<4>) { a = x; }"
		                               "  lambda mfun = (x : int<4>) -> int<4> { return a; }"
		                               "};"
		                               "def A:: free_ctor = ctor (x : int<4>) { a = x; };"
		                               "def A::lambda free_mfun = (x : int<4>) -> int<4> { return a; };"
		                               "{"
		                               "  var ref<A> a1 = A::(a1, 10);"
		                               "  a1.mfun(12);"
		                               "  a1.free_mfun(12);"
		                               "  var ref<A> a2 = free_ctor(a2, 10);"
		                               "  a2.mfun(12);"
		                               "  a2.free_mfun(12);"
		                               "}"));

		// free non-ctor member
		{
			auto expected = R"(
				def IMP_SimplestDI::function IMP_get = () -> unit {
				};
				{
					var ref<IMP_SimplestDI, f, f, plain> v0 = lit("IMP_SimplestDI::ctor" : IMP_SimplestDI::())(ref_decl(type_lit(ref<IMP_SimplestDI, f, f, plain>)));
					v0.IMP_get();
				}
			)";
			EXPECT_TRUE(test_statement(nm, expected));
		}

		// free lambda on templated generic type
		{
			auto expected = R"(
				def A<int<4>>::function bla = () -> unit {
				};
				{
					lambda_name A<int<4>>::bla;

					var ref<A<int<4>>> a;
					lambda_name A<int<4>>::bla(a);
				}
			)";
			EXPECT_TRUE(test_statement(nm, expected));
		}
	}

	TEST(IR_Parser, DuplicateMemberFunctions) {
		NodeManager nm;
		IRBuilder builder(nm);

		//Note: We do not use the helper function test_statement here, as that one will also run the semantic checks.
		//Those will fail for certain duplicate members and thus mask shortcomings of the parser error detections we actually want to test here

		//re-declaration of member functions with the same type but a different name is ok
		EXPECT_TRUE(builder.parseStmt("def struct A {"
		                              "  lambda foo = () -> unit { }"
		                              "  lambda bar = () -> unit { }"
		                              "};"
		                              "{"
		                              "  var ref<A> a;"
		                              "}"));

		//re-declaration of member functions with the same type and the same name is not allowed
		EXPECT_ANY_THROW(builder.parseStmt("def struct A {"
		                                   "  lambda foo = () -> unit { }"
		                                   "  lambda foo = () -> unit { }"
		                                   "};"
		                                   "{"
		                                   "  var ref<A> a;"
		                                   "}"));

		//re-declaration of a constructor with the same type isn't allowed
		EXPECT_ANY_THROW(builder.parseStmt("def struct A {"
		                                   "  ctor() { }"
		                                   "  ctor() { }"
		                                   "};"
		                                   "{"
		                                   "  var ref<A> a;"
		                                   "}"));

		//re-declaration of (free) member functions with the same type but a different name is ok
		EXPECT_TRUE(builder.parseStmt("def struct A {"
		                              "  lambda foo = () -> unit { }"
		                              "};"
		                              "def A::lambda bar = () -> unit { };"
		                              "{"
		                              "  var ref<A> a;"
		                              "}"));

		//re-declaration of (free) member functions with the same type and the same name is not allowed
		EXPECT_ANY_THROW(builder.parseStmt("def struct A {"
		                                   "  lambda foo = () -> unit { }"
		                                   "};"
		                                   "def A::lambda foo = () -> unit { };"
		                                   "{"
		                                   "  var ref<A> a;"
		                                   "}"));

		//re-declaration of a (free) constructor with the same type is allowed, as it has a different name
		EXPECT_TRUE(builder.parseStmt("def struct A {"
		                              "  ctor() { }"
		                              "};"
		                              "def A:: foo = ctor () { };"
		                              "{"
		                              "  var ref<A> a;"
		                              "}"));
	}

	TEST(IR_Parser, ParentCalls) {
		NodeManager nm;

		const std::string classA = "def struct A {"
		                           "  x : int<4>;"
		                           "  lambda a = ()->unit { }"
		                           "};";
		const std::string classB = "def struct B : [A] {"
		                           "  y : int<4>;"
		                           "  ctor() {"
		                           "    A::(this);"
		                           "  }"
		                           "  lambda b = ()->unit {"
		                           "    this.as(A).a();"
		                           "    y = *this.as(A).x;"
		                           "  }"
		                           "};";
		const std::string body = "{"
		                         "  var ref<A> a = A::(a);"
		                         "  var ref<B> b = B::(b);"
		                         "  a.a();"
		                         "  a.x;"
		                         "  b.b();"
		                         "  b.y;"
		                         "  b.as(A).a();"
		                         "  b.as(A).x;"
		                         "}";

		//calling of superclass ctor and of member functions of super classes
		EXPECT_TRUE(test_statement(nm, classA
		                               + classB
		                               + body));

		//the same as above but with the two structs switched and using a forward decl
		EXPECT_TRUE(test_statement(nm, "decl struct A;"
		                               "decl A::x : int<4>;"
		                               "decl a : A::()->unit;"
		                               + classB
		                               + classA
		                               + body));
	}

	TEST(IR_Parser, ExplicitDeclarations) {
		NodeManager nm;
		IRBuilder builder(nm);

		const std::string classA = "decl a : A::()->unit;"
		                           "def struct A {"
		                           "  lambda b = ()->unit { }"
		                           "};"
		                           "A";

		auto type = builder.parseType(classA);
		EXPECT_TRUE(checks::check(type).empty()) << checks::check(type);
		EXPECT_TRUE(toString(type).find(",a()->unit,") != std::string::npos);
	}

	TEST(IR_Parser, Job) {
		NodeManager nm;
		IRBuilder builder(nm);
		auto& parExt = nm.getLangExtension<lang::ParallelExtension>();

		{
			auto exp = builder.parseExpr(R"(def foo = () -> unit { 5; }; job[] => foo())");
			ASSERT_TRUE(exp);
			EXPECT_TRUE(checks::check(exp).empty()) << checks::check(exp);
			ASSERT_TRUE(exp.isa<JobExprPtr>());
			auto range = exp.as<JobExprPtr>()->getThreadNumRange();
			EXPECT_TRUE(parExt.isCallOfCreateMinRange(range)) << range;
		}
		{
			auto exp = builder.parseExpr(R"(def foo = () -> unit { 7; }; job[2...] => foo())");
			ASSERT_TRUE(exp);
			EXPECT_TRUE(checks::check(exp).empty()) << checks::check(exp);
			ASSERT_TRUE(exp.isa<JobExprPtr>());
			auto range = exp.as<JobExprPtr>()->getThreadNumRange();
			EXPECT_TRUE(parExt.isCallOfCreateMinRange(range)) << range;
		}
		{
			auto exp = builder.parseExpr(R"(def foo = () -> unit { 8; }; job[40..42] => foo())");
			ASSERT_TRUE(exp);
			EXPECT_TRUE(checks::check(exp).empty()) << checks::check(exp);
			ASSERT_TRUE(exp.isa<JobExprPtr>());
			auto range = exp.as<JobExprPtr>()->getThreadNumRange();
			EXPECT_TRUE(parExt.isCallOfCreateBoundRange(range)) << range;
		}
		{
			auto exp = builder.parseExpr(R"(def foo = () -> unit { 8; }; job[4..16:4] => foo())");
			ASSERT_TRUE(exp);
			EXPECT_TRUE(checks::check(exp).empty()) << checks::check(exp);
			ASSERT_TRUE(exp.isa<JobExprPtr>());
			auto range = exp.as<JobExprPtr>()->getThreadNumRange();
			EXPECT_TRUE(parExt.isCallOfCreateBoundRangeMod(range)) << range;
		}
	}

	TEST(IR_Parser, MaterializeCall) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto int4 = builder.getLangBasic().getInt4();
		auto plainRef = builder.refType(builder.getLangBasic().getInt4());
		auto cppRef = builder.refType(builder.getLangBasic().getInt4(), false, false, lang::ReferenceType::Kind::CppReference);

		const std::string code = "def f1 = ()->int<4> { return 42; };"
		                         "def f2 = ()->ref<int<4>,f,f,plain> { return 42; };"
		                         "def f3 = ()->ref<int<4>,f,f,cpp_ref> { return 42; };"
		                         "{"
		                         "  $f1()$;"
		                         "  $f2()$;"
		                         "  $f3()$;"
		                         "  $f1() materialize$;"
		                         "  $f2() materialize$;"
		                         "  $f3() materialize$;"
		                         "}";

		auto addresses = builder.parseAddressesStatement(code);

		ASSERT_EQ(6, addresses.size());
		EXPECT_EQ(addresses[0].getAddressedNode().as<CallExprPtr>()->getType(), int4);
		EXPECT_EQ(addresses[1].getAddressedNode().as<CallExprPtr>()->getType(), plainRef);
		EXPECT_EQ(addresses[2].getAddressedNode().as<CallExprPtr>()->getType(), cppRef);
		EXPECT_EQ(addresses[3].getAddressedNode().as<CallExprPtr>()->getType(), plainRef);
		EXPECT_EQ(addresses[4].getAddressedNode().as<CallExprPtr>()->getType(), plainRef);
		EXPECT_EQ(addresses[5].getAddressedNode().as<CallExprPtr>()->getType(), plainRef);
	}

	TEST(IR_Parser, Comments) {
		NodeManager mgr;

		EXPECT_TRUE(parseExpr(mgr, "12"));
		EXPECT_TRUE(parseExpr(mgr, "12 // this is the number 12"));

		EXPECT_FALSE(parseStmt(mgr, "{ 12; // this is the number 12 }"));
		EXPECT_TRUE(parseStmt(mgr, "{ 12; // this is the number \n 12; }"));

		EXPECT_TRUE(parseExpr(mgr, "/* before */ 12 /* after */"));
		EXPECT_TRUE(parseStmt(mgr, " { /* before */ 12 /* mid */ ; /* after */ }"));
	}

	TEST(IR_Parser, Unresolved) {
		NodeManager mgr;
		// the PARSER_UNRESOLVED_ prefix is used when the TU shouldn't resolve a specific literal
		// it needs to be replaced at the very end of the process
		EXPECT_EQ(parseExpr(mgr, R"(lit("PARSER_UNRESOLVED_bla": int<4>))"), parseExpr(mgr, R"(lit("bla": int<4>))"));
	}

	TEST(IR_Parser, GenericTypeFreeMemberCall) {
		NodeManager mgr;

		auto code = R"(
			def GenericTypeWithFreeMember<TypeParam1> :: const function free = () -> unit {
				this;
			};
			{
				var ref<GenericTypeWithFreeMember<TypeParam1>,f,f,plain> v0;
				v0.free();
			}
		)";

		EXPECT_TRUE(parseStmt(mgr, code));
	}

} // parser
} // core
} // insieme
