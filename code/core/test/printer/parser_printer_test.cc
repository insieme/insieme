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

#include <iostream>
#include <sstream>
#include <fstream>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/parser/ir_parser.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/compare.h"

namespace insieme {
namespace core {
namespace parser {

	using namespace detail;
	using namespace insieme::core::printer;

	bool test_statement(NodeManager& nm, const std::string& x);

	bool test_type(NodeManager &nm, const std::string &x) {
		IRBuilder builder(nm);

		std::cout << " ============== TEST ============== " << std::endl;
		auto type1 = builder.parseType(x);

		if (type1) {
			PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
			                              | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
			                              | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_DERIVED_IMPL
			                              | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY);
			std::ostringstream ss;
			ss << printerA;
			auto type2 = builder.parseType(ss.str());
			if (type2) {
				PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
				                              | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
				                              | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_DERIVED_IMPL
				                              | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY);

				if (builder.normalize(type1) == builder.normalize(type2)) {
					return true;
				} else {
					std::cout << "[equality check turned out false!]" << std::endl;
					std::cout << "printerA[ " << printerA << " ]" << std::endl;
					std::cout << "printerB[ " << printerB << " ]" << std::endl;
					return false;
				}
			} else {
				std::cout << "[parsing type2 went wrong!]" << std::endl;
				return false;
			}
		} else {
			std::cout << "[parsing type1 went wrong!]" << std::endl;
			return false;
		}
	}

	TEST(After_Before_Test, Types) {
		NodeManager nm;

		EXPECT_TRUE(test_type(nm, "int<8>"));
		EXPECT_TRUE(test_type(nm, "alias int = int<4>; int"));
		EXPECT_TRUE(test_type(nm, "someweirdname<>"));
		EXPECT_TRUE(test_type(nm, "vector<int<4>, 4>"));
		EXPECT_TRUE(test_type(nm, "vector<'a, 4>"));
		// references
		EXPECT_TRUE(test_type(nm, "ref<'a,f,f,plain>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,f,t,plain>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,t,f,plain>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,t,t,plain>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,f,f,cpp_ref>"));
		EXPECT_TRUE(test_type(nm, "ref<'a,f,f,cpp_rref>"));
		// pointer
		EXPECT_TRUE(test_type(nm, "ref<'a,f,f,cpp_rref>"));

		EXPECT_TRUE(test_type(nm, "struct { a : int<4>; b : int<8>;}"));
		EXPECT_TRUE(test_type(nm, "struct name { a: int<4>; b : int<8>;}"));
		EXPECT_TRUE(test_type(nm, "struct { a : int<4>; b: int<8>;}"));

		// function
		EXPECT_TRUE(test_type(nm, "(int<4> , ref<int<4>>) -> int<4>"));
		EXPECT_TRUE(test_type(nm, "(int<4> , ref<int<4>>) => int<4>"));
		EXPECT_TRUE(test_type(nm, "(array<'elem,'n>, vector<uint<8>,'n>) -> 'elem"));
		EXPECT_TRUE(test_type(nm, "<'a,'b>('a , int<4>) -> 'b"));
		EXPECT_TRUE(test_type(nm, "<'a,uint<4>>('a , int<4>) -> uint<4>"));

		// function as parameter to generic type which has a member function
		EXPECT_TRUE(test_type(nm, "Foo<()->int<4>>::() -> unit"));

		EXPECT_TRUE(test_type(nm, "struct C { field : int<4>; }"));
		EXPECT_TRUE(test_type(nm, "alias papa = struct P { field : int<4>; }; alias mama = struct M { field : int<4>; }; struct name : [ papa, mama ] { a: int<4>; b : int<8>;}"));
		EXPECT_TRUE(test_type(nm, "alias papa = struct P { field : int<4>; }; struct name :[ papa ]{ a : int<4>; b : int<8>;}"));

		// tuple types
		EXPECT_TRUE(test_type(nm, "(int<4>, int<8>)"));
		EXPECT_TRUE(test_type(nm, "(int<4>, struct {a:int<4>;})"));
		EXPECT_TRUE(test_type(nm, "(int<4>, struct {a:int<4>; b:int<8>;})"));
		EXPECT_TRUE(test_type(nm, "alias int = int<4>; (int, struct {a:int; b:int<8>;}, int<7>)"));

		// Extended test for structs with parents
		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ private A ]         { b : int<8>;}"));
		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ virtual private A ] { b : int<8>;}"));
		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ public A ]          { b : int<8>;}"));
		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ protected A ]       { b : int<8>;}"));
		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ A ]                 { b : int<8>;}"));
		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ virtual A ]         { b : int<8>;}"));
	}


	bool test_expression(NodeManager& nm, const std::string& x) {
		IRBuilder builder(nm);

		std::cout << " ============== TEST ============ " << std::endl;
		auto type1 = builder.normalize(builder.parseExpr(x));

		if(type1) {
			PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
										  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_ATTRIBUTES
										  | PrettyPrinter::PRINT_DERIVED_IMPL);
			std::ostringstream ss;
			ss << printerA;
			//std::cout << printerA << std::endl;
			auto type2 = builder.normalize(builder.parseExpr(ss.str()));
			if(type2) {
				PrettyPrinter printerB(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
											  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_ATTRIBUTES
											  | PrettyPrinter::PRINT_DERIVED_IMPL);

				std::ostringstream ss1;
				ss1 << printerA;

				auto type3 = builder.normalize(builder.parseExpr(ss1.str()));

				if(analysis::equalNameless(builder.normalize(type1), builder.normalize(type2))) {
					dumpColor(type2);
					return true;
				} else {
					std::cout << "[equality check turned out false!]" << std::endl;
					std::cout << "printerA[[ " << std::endl << printerA << std::endl << "]]" << std::endl;
					std::cout << "printerB[[ " << std::endl << printerB << std::endl << "]]" << std::endl;
					if(builder.normalize(type2) == builder.normalize(type3)) {
						std::cout << "type 2-3 equivalent!\n";
					}
					return false;
				}
			} else {
				std::cout << "[parsing type2 went wrong!]" << std::endl;
				return false;
			}
		} else {
			std::cout << "[parsing type1 went wrong!]" << std::endl;
			return false;
		}
		return false;
	}

	TEST(After_Before_Test, Expressions) {
	NodeManager nm;

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

		// precedence
		EXPECT_TRUE(test_expression(nm, "1 + 0 * 5"));
		EXPECT_TRUE(test_expression(nm, "1 * 0 + 5"));

		EXPECT_TRUE(test_statement(nm, "def union uni { a : int<4>; }; { var ref<uni,f,f,plain> a; }"));
		EXPECT_TRUE(test_statement(nm, "def union uni { a : int<4>; }; { <ref<uni>> { 4 }; }"));
		EXPECT_TRUE(test_statement(nm, ""
				"def struct BU { b : int<8>; };"
				"def union uni { a : int<4>; b : BU; };"
				"{"
				"  auto b = <ref<BU>> {6};"
				"  auto u1 = <ref<uni>> { b };"
				"  auto u2 = <ref<uni>> { 3 };"
				"  auto u3 = <ref<uni>> { <ref<BU>> { 5 } };"
				"}"));

		EXPECT_TRUE(test_expression(nm, "def struct x { a : int<4>; b : int<4>; }; <ref<x>> { 4, 5 }"));
		EXPECT_TRUE(test_expression(nm, "def struct x { a : uint<4>; b : int<4>; }; <ref<x>> { 4, -5 }"));
		EXPECT_TRUE(test_expression(nm, "def struct x { a : int<4>; b : uint<4>; }; <ref<x>> { -4, 5 }"));
		EXPECT_TRUE(test_expression(nm, "def struct x { a : int<4>; }; <ref<x>> { 4 }"));
		EXPECT_TRUE(test_expression(nm, "def struct x { }; <ref<x>> { }"));

		EXPECT_TRUE(test_expression(nm, "() -> unit { }"));
		EXPECT_TRUE(test_expression(nm, "( a : int<4>) -> unit { }"));
		EXPECT_TRUE(test_expression(nm, "() -> unit { var int<4> a = 1+1; }"));
		EXPECT_TRUE(test_expression(nm, "( a : bool) -> bool { return a; }"));

		EXPECT_TRUE(test_expression(nm, "( _ : 'a ) -> bool { return true; }"));
		EXPECT_TRUE(test_expression(nm, "( x : 'a ) -> bool { return true; }"));
		EXPECT_TRUE(test_expression(nm, "( x : 'a ) -> 'a { return x+CAST('a) 3; }"));
		EXPECT_TRUE(test_expression(nm, "( x : 'a ) -> 'a { return(x+CAST('a) 3); }"));
		EXPECT_TRUE(test_expression(nm, "( x : 'a ) -> 'a { return x+CAST('a) 3; }"));
		EXPECT_TRUE(test_expression(nm, "( x : 'a ) => x+CAST('a) 3"));

		EXPECT_TRUE(test_expression(nm, "type_lit(int<4>)"));

		EXPECT_TRUE(test_expression(nm, "100+200*300"));
		EXPECT_TRUE(test_expression(nm, "100-200*300"));
		EXPECT_TRUE(test_expression(nm, "100-200+300"));
		EXPECT_TRUE(test_expression(nm, "100-(200+300)"));
		EXPECT_TRUE(test_expression(nm, "100-200-300"));

		EXPECT_TRUE(test_expression(nm, ""
		                                "decl foo : () -> unit; "
		                                "def bar = () -> unit { foo(); }; "
		                                "def foo = () -> unit { bar(); }; "
		                                "foo"));

		EXPECT_TRUE(test_expression(nm, ""
		                                "decl foo : () -> unit; "
		                                "def bar = () -> unit { foo(); }; "
		                                "def foo = () -> unit { bar(); }; "
		                                "bar"));

		EXPECT_TRUE(test_expression(nm, ""
		                                "decl foo : (int<4>) -> int<4>; "
		                                "def bar = (a : int<4>) -> int<4> { return foo(a); }; "
		                                "def foo = (b : int<4>) -> int<4> { return bar(b); }; "
		                                "foo"));

		EXPECT_TRUE(test_expression(nm, ""
		                                "decl foo : (int<4>) -> int<4>; "
		                                "def bar = function (a : ref<int<4>,f,f,plain>) -> int<4> { return foo(*a); }; "
		                                "def foo = (b : int<4>) -> int<4> { return bar(b); }; "
		                                "foo"));

		// job expressions
		EXPECT_TRUE(test_expression(nm, "def f = () -> int { return 0; };"
		                                "job [] => f()"));

		EXPECT_TRUE(test_expression(nm, "def f = () -> unit {};"
		                                "job [0ul .. 5ul] => f()"));

		EXPECT_TRUE(test_expression(nm, "def f = () -> unit {};"
		                                "job [0ul .. 8ul : 2ul ] => f()"));

		EXPECT_TRUE(test_expression(nm, "def f = () -> unit {};"
		                                "job [42ul ...] => f()"));


	}

	bool test_statement(NodeManager& nm, const std::string& x) {
		IRBuilder builder(nm);

		std::cout << " ============== TEST ============ " << std::endl;
		auto type1 = builder.normalize(builder.parseStmt(x));
		if(type1) {
			PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS
				                              | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
				                              | PrettyPrinter::NO_EVAL_LAZY | PrettyPrinter::PRINT_DERIVED_IMPL);
			std::ostringstream ss;
			ss << printerA;
			dumpColor(type1);
			auto type2 = builder.normalize(builder.parseStmt(ss.str()));
			if(type2) {
				PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS
					                              | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
					                              | PrettyPrinter::NO_EVAL_LAZY | PrettyPrinter::PRINT_DERIVED_IMPL);

				if(analysis::equalNameless(type1, type2)) {
					return true;
				} else {
					std::cout << "[equality check turned out false!]" << std::endl;
					std::cout << "printerA[ " << printerA << " ]" << std::endl;
					std::cout << "printerB[ " << printerB << " ]" << std::endl;
					return false;
				}
			} else {
				std::cout << "[parsing type2 went wrong!]" << std::endl;
				return false;
			}
		} else {
			std::cout << "[parsing type1 went wrong!]" << std::endl;
			return false;
		}
	}


	TEST(After_Before_Test, Statements) {
		NodeManager nm;

		EXPECT_TRUE(test_statement(nm, "{ var int<4> x = 0; x+1; }"));
		EXPECT_TRUE(test_statement(nm, "{ auto x = 0; x+1; }"));

		EXPECT_TRUE(test_statement(nm, "if ( true ) {}"));
		EXPECT_TRUE(test_statement(nm, "if ( true ) {} else {}"));
		EXPECT_TRUE(test_statement(nm, "if ( true ) { if ( false ) { } else { 1; } }"));
		EXPECT_TRUE(test_statement(nm, "if ( true ) { if ( false ) { } else { 1; } } else { 2; }"));
		EXPECT_TRUE(test_statement(nm, "if( false ) { return 0; } else { return 1+2; }"));
		EXPECT_TRUE(test_statement(nm, "if( false ) { return 0; }"));
		EXPECT_TRUE(test_statement(nm, "while ( true ) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "while ( false ) { 1+1; }"));

		EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3: 2) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3: 2) { 1+1; }"));

		EXPECT_TRUE(test_statement(nm, "switch (2) { case 1: {1;} case 2: {2;} }"));

		EXPECT_TRUE(test_statement(nm, "try  {2;} catch( e : int<4>) { 1+1; }"));
		EXPECT_TRUE(test_statement(nm, "try  {2;} catch( e : int<4>) { 1+1; } catch (r : ref<int<4>>) { 3+4; }"));

		EXPECT_TRUE(test_statement(nm, "{ }"));

		EXPECT_TRUE(test_statement(nm, "def struct name { a : int<2>; };"
				                       "{"
			                           "    var ref<name> a;"
			                           "}"));

		EXPECT_TRUE(test_statement(nm, "def struct name { a : int<2>; };"
				                       "alias collection = vector<class, 10>;"
			                           "{"
				                       "    var ref<collection> col;"
			                           "}"));

		EXPECT_TRUE(test_statement(nm, "def struct somenewname { a : int<2>; };"
			                           "alias collection = vector<int<2>,5>;"
									   "{"
			                           "    var ref<collection> x;"
			                           "    var ref<somenewname> y;"
			                           "}"));

		EXPECT_TRUE(test_statement(nm, "def struct someoldname { a : int<2>; };"
									   "alias collection = vector<int<2>,5>;"
									   "{"
			                           "    var ref<collection> x;"
			                           "    var ref<someoldname> y;"
			                           "}"));

		// test return statements
		EXPECT_TRUE(test_statement(nm, "def struct A { a : int<4>; };"
		                               "def foo1 = () -> A {"
		                               "  return A::(ref_decl(type_lit(ref<A>)));" // this is the correct way to do it
		                               "};"
		                               "def foo2 = () -> int<4> {"
		                               "  return 5;"
		                               "};"
		                               "def foo3 = () -> int<4> {"
		                               "  var ref<int<4>> v = 5;"
		                               "  return v;"
		                               "};"
		                               "{"
		                               "  foo1();"
		                               "  foo2();"
		                               "  foo3();"
		                               "}"));
	}


	bool test_program(NodeManager& nm, const std::string& x) {
		IRBuilder builder1(nm);

		std::cout << " ============== TEST ============== " << std::endl;
		auto type1 = builder1.normalize(builder1.parseProgram(x));
		if(type1) {
			PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS
				                              | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
				                              | PrettyPrinter::PRINT_DERIVED_IMPL);
			std::ostringstream ss;
			ss << printerA;
			dumpColor(type1);
			auto type2 = builder1.normalize(builder1.parseProgram(ss.str()));
			if(type2) {
				PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS
					                              | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
					                              | PrettyPrinter::PRINT_DERIVED_IMPL);

				if(analysis::equalNameless(type1, type2)) {
					return true;
				} else {
					std::cout << "[equality check turned out false!]" << std::endl;
					std::cout << "printerA\n[[\n" << printerA << " \n]]" << std::endl;
					std::cout << "printerB\n[[\n" << printerB << " \n]]" << std::endl;
					return false;
				}
			} else {
				std::cout << "[parsing type2 went wrong!]" << std::endl;
				return false;
			}
		} else {
			std::cout << "[parsing type1 went wrong!]" << std::endl;
			return false;
		}
	}

	TEST(After_Before_Test, Let) {
		NodeManager mgr;

    	EXPECT_TRUE(test_program(mgr, "alias int = int<4>; int main () { return 1; }"));
		EXPECT_TRUE(test_program(mgr, "alias int = int<4>; alias long = int<8>; long main ( a : int) { return 1; }"));
		EXPECT_TRUE(test_program(mgr, "alias int = int<4>; alias long = int<8>; int<4> main () { return 1; }"));
		EXPECT_TRUE(test_program(mgr, "def f = () -> unit { }; int<4> main () { f(); return 1; }"));

		EXPECT_TRUE(test_program(mgr, "alias int = int<4>; def f = (a : int) -> int { return a; }; int main () { f(1); return 1; }"));
		EXPECT_TRUE(test_program(mgr, "decl f : ()->unit; decl g : ()->unit; def f = ()->unit{g();}; def g = ()->unit{f();}; unit main() { f(); }"));
		EXPECT_TRUE(test_program(mgr, "decl f : ()->unit; decl g : ()->unit; def f = ()->unit{g();}; def g = ()->unit{f();}; unit main() { f(); 5;}"));
		EXPECT_TRUE(test_program(mgr, "decl f : ()->unit; decl g : ()->unit; def f = ()->unit{g();}; def g = ()->unit{f();}; unit main() { f(); g(); }"));
    	EXPECT_TRUE(test_program(mgr, ""
			                          "alias int = int<4>;"
			                          "def f = (a : int) -> int {"
			                          "   return 0;"
			                          "};"
			                          "def g = (a : int, b : int) -> unit {};"
			                          "unit main() {"
			                          "   f(3);"
			                          "   var int x = 4;"
			                          "   var int<4> y = 2;"
			                          "   g(1,2);"
			                          "   g(x,y);"
			                          "}"));

		EXPECT_TRUE(test_statement(mgr, ""
		"def struct name { "
		"    lambda fun = () -> unit {}"
		"};"
		"{"
		"   var ref<name> x;"
		"   x.fun();"
		"}"));

		EXPECT_TRUE(test_statement(mgr, ""
		"decl struct B;"
		"def struct A { "
		"    b : B;"
		"    lambda fun = () -> unit {}"
		"};"
		"def struct B {"
		"    a : A;"
		"    lambda xxx = () -> unit {}"
		"};"
		"{"
		"   var ref<A> x;"
		"   x.fun();"
		"}"));

		EXPECT_TRUE(test_statement(mgr, ""
		"decl struct B;"
		"def struct A { "
		"    b : B;"
		"    lambda fun = () -> unit {}"
		"};"
		"def struct B {"
		"    a : A;"
		"    lambda xxx = () -> unit {}"
		"};"
		"{"
		"   var ref<B> x;"
		"   x.xxx();"
		"}"));

		EXPECT_TRUE(test_statement(mgr, ""
		"decl struct B;"
		"def struct A { "
		"    b : B;"
		"    lambda fun = () -> unit {}"
		"};"
		"def struct B {"
		"    a : A;"
		"    lambda xxx = () -> unit {}"
		"};"
		"{"
		"   var ref<A> x;"
		"   x.b.xxx();"
		"}"));

		EXPECT_TRUE(test_statement(mgr, ""
		"def struct name { "
		"    a : int<4>;"
		"    lambda f = (b : int<4>) -> unit {"
		"        this.a = 5;"
		"    }"
		"};"
		"{"
		"   var ref<name> x;"
		"   x.f(6);"
		"}"));

		EXPECT_TRUE(test_program(mgr, ""
		"def struct name { "
		"    a : int<4>;"
		"    lambda f = () -> unit {"
		"        this.a = 5;"
		"    }"
		"};"
		"unit main(y:int<4>) {"
		"   var ref<name> x;"
		"   x.f();"
		"}"));

    	EXPECT_TRUE(test_program(mgr,
		"alias int = int<4>;"
		"def struct name { "
		"    a : int;"
		"    b : int;"
		"    lambda f = () -> int {"
		"        return 0;"
		"    }"
		"    lambda f = (a : int) -> int {"
		"        return 1;"
		"    }"
		"};"
		"unit main() {"
		"   var ref<name> x;"
		"   x.f();"
		"   x.f(1);"
		"}"));

		EXPECT_TRUE(test_program(mgr,
		"def struct name { a: int<4>; b : int<8>;};"
		"alias class = name;"
		"unit main() {"
		"   var ref<class> x;"
		"   var ref<int<4>> y;"
		"}"));

		EXPECT_TRUE(test_program(mgr, ""
		"decl f:name::()->unit;"
		"def struct name {"
		"    lambda f = () -> unit {"
		"        f();"
		"    }"
		"};"
		"alias fancy = name;"
		"unit main () {"
		"   var ref<fancy> x;"
		"   x.f();"
		"}"));

    	EXPECT_TRUE(test_program(mgr,
		"decl struct name;"
		"decl f:name::()->unit;"
		"decl g:name::()->unit;"
		"def struct shoe { a : int<8>; c : int<8>; d : int<4>; g : int<1>;};"
		"alias fancy = shoe;"
		"def struct hair { f : int<4>; a : int<2>; z : int<16>;};"
		"alias fency = hair;"
		"def struct name : [ fancy,fency ] { "
		"    a : int<4>;"
		"    b : int<8>;"
		"    lambda f = () -> unit {"
		"        g();"
		"    }"
		"    lambda g = () -> unit {"
		"        f();"
		"    }"
		"};"
		"alias class = name;"
		"unit main() {  "
		"    var ref<class> x;"
		"    x.f();"
		"    x.g();"
		"}" ));

    	EXPECT_TRUE(test_program(mgr,
		"def struct name { "
		"    a : int<4>;"
		"    ctor() {"
		"        a = 5;"
		"    }"
		"    ctor(b : int<4>) {"
		"        a = b;"
		"    }"
		"};"
		"unit main() {"
		"    var ref<name> y;"
		"}"
		));
	}

	TEST(After_Before_Test, Struct) {
		NodeManager nm;

		// reference check
		EXPECT_TRUE(test_statement(nm, "def struct A {"
				                       "  x : int<4>;"
				                       "};"
				                       "{"
				                       "  var ref<A> b = A::(b);"
				                       "}"));

		EXPECT_TRUE(test_statement(nm, "def struct A {"
				                       "  x : int<4>;"
				                       "  ctor () {}"
				                       "  ctor (a : int<4>) {"
				                       "    x = a;"
				                       "  }"
				                       "};"
				                       "{"
				                       "  var ref<A> a = A::(a, 10);"
				                       "  var ref<A> b = A::(b);"
				                       "}"));

		EXPECT_TRUE(test_statement(nm, "def struct A {"
				                       "  x : int<4>;"
				                       "};"
				                       "def A :: foo1 = ctor () {};"
				                       "def A :: foo2 = ctor function (a : ref<int<4>>) {x = *a;};"
				                       "{"
				                       "  var ref<A> a = foo1(a);"
				                       "  var ref<A> b = foo2(b, 10);"
				                       "}"));

		// This snippet can't be parsed, printed and parsed again becuase:
		// - Upon the first parsing, the default members will be added (default ctor, and copy operations - no move operations)
		// - Since the move ctor will be printed as deleted, during the next parsing the default ctor will not be generated
		// - thus the parser will still contain a call to the fake dummy lambda it generated during record declaration
//		EXPECT_TRUE(test_statement(nm, "def struct A {"
//				                       "  x : int<4>;"
//				                       "  dtor() { 5; }"
//				                       "};"
//				                       "{"
//				                       "  var ref<A> b = A::(b);"
//				                       "}"));

	}

    TEST(After_Before_Test, Program) {
		NodeManager nm;

		EXPECT_TRUE(test_program(nm, "unit main() {}"));

		EXPECT_TRUE(test_program(nm, "alias int = int<4>;"
			                         "int main() {"
			                         "   return 4;"
			                         "}"));

		EXPECT_TRUE(test_program(nm, ""
			                         "int<4> main(a : ref<int<4>,f,f,plain>) {"
			                         "   var int<4> c = 5;"
			                         "   return 42;"
			                         "}"));
		EXPECT_TRUE(test_program(nm, ""
			                         "alias int = int<4>;"
			                         "def f = ()->unit{};"
			                         "int main() {"
			                         "   f();"
			                         "   return 4;"
			                         "}"));
		EXPECT_TRUE(test_program(nm, ""
			                         "alias int = int<4>;"
			                         "def f = (a : int)->int{"
			                         "   return a;"
			                         "};"
			                         "unit main(a : ref<int<4>,f,f,plain>) {"
			                         "   f(6);"
			                         "}"));

		EXPECT_TRUE(test_program(nm, ""
			                         "alias int = int<4>;"
			                         "def f = function (a : ref<int,f,f,plain>)->unit{"
			                         "   a = 5;"
			                         "};"
			                         "unit main(a : ref<int<4>,f,f,plain>) {"
			                         "   var int b = 6;"
			                         "   f(b);"
			                         "}"));
		EXPECT_TRUE(test_program(nm, ""
			                         "alias int = int<4>;"
			                         "def f = function (a : ref<int<4>,f,f,plain>)->int{"
			                         "   return *a;"
			                         "};"
			                         "unit main() {"
			                         "   var int z = f(5);"
			                         "}"));

		EXPECT_TRUE(test_program(nm, ""
			                         "def f = function (a : ref<int<4>,f,f,plain>) -> int<4> { "
			                         "   return *a; "
			                         "}; "
			                         "unit main (argc : int<8>, argc2 : int<4>) {1;2;3;f(2);}"));
		EXPECT_TRUE(test_program(nm, "unit main (a : int<4>, b : int<4>)  { "
			                         "   var int<4> c = a;"
			                         "}"));
		EXPECT_TRUE(test_program(nm, ""
			                         "alias int = int<4>; "
			                         "unit main (a : int, b : int) { 1+1; }"));
		EXPECT_TRUE(test_program(nm, "alias int = int<4>; "
			                         "def f = function (a : ref<int,f,f,plain>) ->int { "
			                         "  return *a; "
			                         "}; "
			                         "unit main (a : int, b : int) { "
			                         "  f(1);"
				                     "  f(a);"
			                         "}"));

		EXPECT_TRUE(test_program(nm, "alias int = int<4>;"
			                         "decl g : () ->unit;"
				                     "def f = () -> unit{"
			                         "    g();"
			                         "};"
			                         "def g = () -> unit {"
			                         "    f();"
			                         "};"
			                         "int main() { "
			                         "      var ref<int> x = 10;"
			                         "      f();"
			                         "      return 0; "
			                         "}"));

		EXPECT_TRUE(test_program(nm, "decl ffunc : (int<4>)->int<4>;"
				                     "def ffunc = function (a : ref<int<4>,f,f,plain>)->int<4> {"
			                         "         ffunc(*a);"
			                         "         return 5;"
			                         "};"
			                         "def gfunc = (a : int<4>)->int<4> {"
			                         "  return a;"
			                         "};"
			                         "unit main() { "
			                         "  var ref<int<4>,f,f,plain> s = 5;"
			                         "  ffunc(*s);"
			                         "  gfunc(4);"
			                         " }"));

		EXPECT_TRUE(test_program(nm, "alias int = int<4>;"
			                         "def f = (a : int) -> int {"
			                         "   return a;"
			                         "};"
			                         "int main() {"
			                         "   var ref<int> a = 5;"
			                         "   f(4);"
			                         "   f(*a);"
			                         "   return 0;"
			                         "}"));

		EXPECT_TRUE(test_program(nm, "decl ffunc : (int<4>)->unit;"
				                     "def ffunc = function (a : ref<int<4>,f,f,plain>)->unit {"
			                         "         ffunc(*a);"
			                         "};"
			                         "unit main() { "
			                         "  var ref<int<4>,f,f,plain> s = 5;"
			                         "  ffunc(*s); "
			                         "}"));

		EXPECT_TRUE(test_program(nm, "def ffunc = (a : int<4>)->int<4> {"
			                         "      return a;"
			                         "    };"
			                         "unit main() { ffunc(12); }"));

		EXPECT_TRUE(test_program(nm, "alias int = int<4>;"
			                         "def f = (a : int) -> unit {};"
			                         "def g = (b : int) -> unit {"
			                         "   f(5);"
			                         "   f(b);"
			                         "};"
			                         "unit main() {"
			                         "   g(5);"
			                         "}"));

		EXPECT_TRUE(test_program(nm, "decl ffunc : (int<4>)->int<4>;"
				                     "decl gfunc : (int<4>)->int<4>;"
			                         "def ffunc = function (a : ref<int<4>,f,f,plain>)->int<4> {"
			                         "         ffunc(*a);"
			                         "         gfunc(6);"
			                         "         return 5;"
			                         "};"
			                         "def gfunc = function (b : ref<int<4>,f,f,plain>)->int<4> {"
			                         "         ffunc(7);"
			                         "         gfunc(*b);"
			                         "         return 3;"
			                         "};"
			                         "unit main() { ffunc(12); }"));

		EXPECT_TRUE(test_program(nm, "decl ffunc : (int<4>)->int<4>;"
									 "decl gfunc : (int<4>)->int<4>;"
			                         "def ffunc = (a : int<4>)->unit {"
			                         "         ffunc(a);"
			                         "         gfunc(6);"
			                         "};"
			                         "def gfunc = (b : int<4>)->unit {"
			                         "         ffunc(7);"
			                         "         gfunc(b);"
			                         "};"
			                         "unit main() { ffunc(12); }"));

		EXPECT_TRUE(test_program(nm, "decl struct A;"
				                     "decl struct B;"
				                     "def struct A {"
				                     "    a : B;"
				                     "};"
				                     "def struct B {"
				                     "    b : A;"
				                     "};"
				                     "unit main() { "
				                     "    var ref<A> a;"
				                     "    return unit;"
				                     "}"));

		EXPECT_TRUE(test_program(nm, "unit main()  {"
			                         "while ( false || true ) { 1+1; }"
			                         "}"));


		EXPECT_TRUE(test_program(nm, "unit main()  {"
			                         "while ( true || false ) { 1+1; }"
			                         "}"));


/*
		EXPECT_TRUE(test_program(nm, "unit main()  {"
			                         "while ( (false && true) || (true && false) ) { var int<4> a = 5; }"
			                         "}"));

		EXPECT_TRUE(test_program(nm, "unit main()  {"
			                         "while ( false || true && true ) { var int<4> a = 5; }"
			                         "}"));

		EXPECT_TRUE(test_program(nm, "unit main()  {"
			                         "while ( false && true || true ) { var int<4> a = 5; }"
			                         "}"));
*/
	}

} // parser
} // core
} // insieme
