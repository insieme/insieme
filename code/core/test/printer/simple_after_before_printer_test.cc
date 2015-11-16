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

#include <iostream>
#include <sstream>
#include <fstream>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/core/parser/ir_parser.h"
#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {
namespace parser {

	using namespace detail;
	using namespace insieme::core::printer;

	bool test_type(NodeManager &nm, const std::string &x) {
		IRBuilder builder(nm);

		std::cout << " ============== TEST ============== " << std::endl;
		auto type1 = builder.parseType(x);

		if (type1) {
			dumpColor(type1);
			PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
			                              | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
			                              | PrettyPrinter::NO_LIST_SUGAR
			                              | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
			                              | /*PrettyPrinter::PRINT_LITERAL_TYPES |*/ PrettyPrinter::PRINT_DERIVED_IMPL);
			std::ostringstream ss;
			ss << printerA;
			auto type2 = builder.parseType(ss.str());
			if (type2) {
				PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
				                              | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
				                              | PrettyPrinter::NO_LIST_SUGAR
				                              | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
				                              | /*PrettyPrinter::PRINT_LITERAL_TYPES |*/ PrettyPrinter::PRINT_DERIVED_IMPL);

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
		EXPECT_TRUE(test_type(nm, "alias int = int<5>; int"));
		EXPECT_TRUE(test_type(nm, "someweirdname<>"));
		EXPECT_TRUE(test_type(nm, "vector<int<4>, 4>"));
		EXPECT_TRUE(test_type(nm, "vector<'a, 4>"));
//		EXPECT_TRUE(test_type(nm, "struct { a : int<4>; b : int<5>;}"));
//		EXPECT_TRUE(test_type(nm, "struct name { a: int<4>; b : int<5>;}"));
//		EXPECT_TRUE(test_type(nm, "struct { a : int<4>; b: int<5>;}"));

		EXPECT_TRUE(test_type(nm, "( int<4> , ref<int<4>>) -> int<4>"));
		EXPECT_TRUE(test_type(nm, "( int<4> , ref<int<4>>) => int<4>"));
		EXPECT_TRUE(test_type(nm, "(array<'elem,'n>, vector<uint<8>,'n>) -> 'elem"));

//		EXPECT_TRUE(test_type(nm, "struct C { field : int<4>; }"));
//		EXPECT_TRUE(test_type(nm, "alias papa = t<11>; alias mama = t<4>; struct name : [ papa, mama ] { a: int<4>; b : int<5>;}"));
//		EXPECT_TRUE(test_type(nm, "alias papa = t<11>; struct name :[ papa ]{ a : int<4>; b : int<5>;}"));

		// Extended test for structs with parents
//		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ private A ]         { b : int<8>;}"));
//		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ virtual private A ] { b : int<8>;}"));
//		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ public A ]          { b : int<8>;}"));
//		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ protected A ]       { b : int<8>;}"));
//		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ A ]                 { b : int<8>;}"));
//		EXPECT_TRUE(test_type(nm, "alias A = struct name { a : int<4>;}; struct B : [ virtual A ]         { b : int<8>;}"));
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
			std::cout << printerA << std::endl;
			auto type2 = builder.normalize(builder.parseExpr(ss.str()));
			if(type2) {
				PrettyPrinter printerB(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
											  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_ATTRIBUTES
											  | PrettyPrinter::PRINT_DERIVED_IMPL);

				std::ostringstream ss1;
				ss1 << printerA;

				auto type3 = builder.normalize(builder.parseExpr(ss1.str()));


				if(builder.normalize(type1) == builder.normalize(type2)) {
					dumpColor(type2);
					return true;
				} else {
					std::cout << "[equality check turned out false!]" << std::endl;
					std::cout << "printerA[[ " << std::endl << printerA << std::endl << "]]" << std::endl;
					std::cout << "printerB[[ " << std::endl << printerB << std::endl << "]]" << std::endl;
					std::cout << "dumpText(A):" << std::endl;
					dumpText(type1);
					std::cout << "dumpText(B):" << std::endl;
					dumpText(type2);
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
//		EXPECT_TRUE(test_expression(nm, "param(123456789)"));

/*
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
*/
/*
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
*/
		EXPECT_TRUE(test_expression(nm, ""
				"decl foo : (int<4>) -> int<4>; "
				"def bar = (a : int<4>) -> int<4> { return foo(a); }; "
				"def foo = (b : int<4>) -> int<4> { return bar(b); }; "
				"foo"));


	}

	bool test_statement(NodeManager& nm, const std::string& x) {
		IRBuilder builder(nm);

		std::cout << " ============== TEST ============ " << std::endl;
		auto type1 = builder.parseStmt(x);

		if(type1) {
			dumpColor(type1);
			PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS
				                              | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
				                              | PrettyPrinter::NO_EVAL_LAZY | PrettyPrinter::PRINT_DERIVED_IMPL);

			std::ostringstream ss;
			std::cout << printerA << std::endl;
			ss << printerA;
			auto type2 = builder.parseStmt(ss.str());
			if(type2) {
				PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS
					                              | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
					                              | PrettyPrinter::NO_EVAL_LAZY | PrettyPrinter::PRINT_DERIVED_IMPL);

				if(builder.normalize(type1) == builder.normalize(type2)) {
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
/*
		EXPECT_TRUE(test_statement(nm, "def struct name { a : int<2>; };"
				                       "{"
			                           "    var name a;"
			                           "}"));

		EXPECT_TRUE(test_statement(nm, "{"
			                           "    let class = struct name { int<2> a};"
			                           "    let collection = vector<class, 10>;"
			                           "}"));

		EXPECT_TRUE(test_statement(nm, "{"
			                           "    let class = struct somenewname { int<2> a};"
			                           "    let collection = vector<int<2>,5>;"
			                           "    decl ref<collection> x;"
			                           "    decl ref<somenewname> y;"
			                           "}"));

		EXPECT_TRUE(test_statement(nm, "{"
			                           "    let class = struct somenewname { int<2> a};"
			                           "    let collection = vector<int<2>,5>;"
			                           "    decl ref<collection> x;"
			                           "    decl ref<someoldname> y;"
			                           "}"));
*/
	}


	bool test_program(NodeManager& nm, const std::string& x) {
		IRBuilder builder1(nm);

		std::ofstream ostreamA, ostreamB;


		std::cout << " ============== TEST ============== " << std::endl;
		auto type1 = builder1.parseProgram(x);
		EXPECT_TRUE(checks::check(type1).empty()) << checks::check(type1);

		if(type1) {
			dumpColor(type1);
			PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS
				                              | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
				                              | /*PrettyPrinter::NO_EVAL_LAZY    |*/ PrettyPrinter::PRINT_DERIVED_IMPL);
			std::ostringstream ss;
			ss << printerA;
			auto type2 = builder1.parseProgram(ss.str());
			if(type2) {
				PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS
					                              | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
					                              | /*PrettyPrinter::NO_EVAL_LAZY    |*/ PrettyPrinter::PRINT_DERIVED_IMPL);

				if(builder1.normalize(type1) == builder1.normalize(type2)) {
					return true;
				} else {
					std::cout << "[equality check turned out false!]" << std::endl;
					std::ofstream out1("printerA.txt");
					std::ofstream out2("printerB.txt");
					out1 << dumpText(type1);
					out2 << dumpText(type2);
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
		/*
		EXPECT_TRUE(test_program(mgr, ""
		"let class = struct name {int<4> a};"
		"let f = lambda class::()->unit {"
		"   this.a = 5;"
		"};"
		"unit main() {"
		"   decl ref<class> x;"
		"   f(x);"
		"}"));
    	EXPECT_TRUE(test_program(mgr, ""
		"let class = struct name {int<4> a};"
		"let f = lambda class::()->unit {"
		"   f(this);"
		"};"
		"unit main() {"
		"   decl ref<class> x;"
		"   f(x);"
		"}"));
    	EXPECT_TRUE(test_program(mgr,
		"let int = int<4>;"
		"let class = struct name { int a; int b};"
		"let f = lambda class::()->int {"
		"   return 0;"
		"};"
		"let g = lambda class::(int a)->int {"
		"   return 1;"
		"};"
		"unit main() {"
		"   decl ref<class> x;"
		"   f(x);"
		"   g(x, 1);"
		"}"));
		*/
//		EXPECT_TRUE(test_program(mgr, ""
//			                          "def struct name { a: int<4>; b : int<5>;};"
//									  "alias class = name;"
//			                          "unit main() {"
//			                          "   var ref<class> x;"
//			                          "   var ref<int<4>> y;"
//			                          "}"));
		/*
		EXPECT_TRUE(test_program(mgr, ""
		"let fancy = struct name { int<4> a; };"
		"let f = lambda fancy::()->unit {"
		"   f(this);"
		"};"
		"unit main () {"
		"   decl ref<fancy> x;"
		"   f(x);"
		"}"));
    	EXPECT_TRUE(test_program(mgr,
		"let fancy = struct shoe { int<8> a; int<9> c; int<4> d; int<1> g;};"
		"let fency = struct hair { int<3> f; int<2> a; int<16> z;};"
		"let class = struct name : fancy,fency { int<4> a; int<5> b};"
		"let f,g = lambda class :: ()->unit{"
		"        g(this);"
		"    },"
		"    lambda class ::()->unit{"
		"        f(this);"
		"    }; "
		"unit main() {  "
		"    decl ref<class> x;"
		"    decl fancy y;"
		"    f(x);"
		"    g(x);"
		"}" ));
    	EXPECT_TRUE(test_program(mgr,
		"let fancy = struct name { int<8> a; };"
		"let f = method fancy::(int<8>)->unit;"
		"let g = ctor fancy::();"
		"unit main() {"
		"decl fancy y;"
		""
		"}"
		));
		*/
	}
    //TEST(After_Before_Test, Program) {
	//	NodeManager nm;

	//	EXPECT_TRUE(test_program(nm, "unit main() {}"));

	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "let int = int<4>;"
	//		                         "int main() {"
	//		                         "   return 4;"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "int<4> main(ref<int<4>,f,f,plain> a) {"
	//		                         "   decl int <4> c = 5;"
	//		                         "   return 42;"
	//		                         "}"));
	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "let int = int<4>;"
	//		                         "let f = lambda()->unit{};"
	//		                         "int<4> main() {"
	//		                         "   f();"
	//		                         "   return 4;"
	//		                         "}"));
	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "let int = int<4>;"
	//		                         "let f = lambda(int a)->int{"
	//		                         "   return a;"
	//		                         "};"
	//		                         "unit main(ref<int<4>,f,f,plain> a) {"
	//		                         "   f(6);"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "let int = int<4>;"
	//		                         "let f = function(ref<int,f,f,plain> a)->unit{"
	//		                         "   a = 5;"
	//		                         "};"
	//		                         "unit main(ref<int<4>,f,f,plain> a) {"
	//		                         "   decl int b = 6;"
	//		                         "   f(b);"
	//		                         "}"));
	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "let int = int<4>;"
	//		                         "let f = function(ref<int<4>,f,f,plain> a)->int{"
	//		                         "   return *a;"
	//		                         "};"
	//		                         "unit main() {"
	//		                         "   decl int z = f(5);"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "let f = function (ref<int<4>,f,f,plain> a) -> int<4> { "
	//		                         "   return *a; "
	//		                         "}; "
	//		                         "unit main (ref<int<8>,f,f,plain> argc, ref<int<4>,f,f,plain> argc2) {1;2;3;f(2);}"));
	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "unit main (ref<int<4>,f,f,plain> a, ref<int<4>,f,f,plain> b)  { "
	//		                         "   decl int <4> c = *a;"
	//		                         "   a = 5; "
	//		                         "}"));
	//	EXPECT_TRUE(test_program(nm, ""
	//		                         "let int = int<4>; "
	//		                         "unit main (ref<int,f,f,plain> a, ref<int,f,f,plain> b) { 1+1; }"));
	//	EXPECT_TRUE(test_program(nm, "let int = int<4>; "
	//		                         "let f = function (ref<int,f,f,plain> a) ->int { "
	//		                         "  return *a; "
	//		                         "}; "
	//		                         "unit main (ref<int,f,f,plain> a, ref<int,f,f,plain> b) { "
	//		                         "  f(1); "
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "let int = int<4>;"
	//		                         "let fun000 = recFunc function_name {"
	//		                         "      function_name = lambda() -> unit{"
	//		                         "          v001();"
	//		                         "      };"
	//		                         "      v001 = lambda() -> unit {"
	//		                         "          function_name();"
	//		                         "      };"
	//		                         "};"
	//		                         "int main() { "
	//		                         "      decl int x = 10;"
	//		                         "      fun000();"
	//		                         "      return 0; "
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "let ffunc = function (ref<int<4>,f,f,plain> a)->int<4> {"
	//		                         "         ffunc(*a);"
	//		                         "         return 5;"
	//		                         "};"
	//		                         "let gfunc = lambda(int<4> a)->int<4> {"
	//		                         "  return a;"
	//		                         "};"
	//		                         "unit main() { "
	//		                         "  decl ref<int<4>,f,f,plain> s = var(5);"
	//		                         "  ffunc(*s);"
	//		                         "  gfunc(4);"
	//		                         " }"));

	//	EXPECT_TRUE(test_program(nm, "let int = int<4>;"
	//		                         "let f = lambda (int a) -> int {"
	//		                         "   return a;"
	//		                         "};"
	//		                         "int main() {"
	//		                         "   decl int a = 5;"
	//		                         "   f(4);"
	//		                         "   f(a);"
	//		                         "   return 0;"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "let ffunc = function (ref<int<4>,f,f,plain> a)->unit {"
	//		                         "         ffunc(*a);"
	//		                         "};"
	//		                         "unit main() { "
	//		                         "  decl ref<int<4>,f,f,plain> s = var(5);"
	//		                         "  ffunc(*s); "
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "let ffunc = lambda (int<4> a)->int<4> {"
	//		                         "      return a;"
	//		                         "    };"
	//		                         "unit main() { ffunc(12); }"));

	//	EXPECT_TRUE(test_program(nm, "let int = int<4>;"
	//		                         "let f = lambda(int a) -> unit {};"
	//		                         "let g = lambda(int b) -> unit {"
	//		                         "   f(5);"
	//		                         "   f(b);"
	//		                         "};"
	//		                         "unit main() {"
	//		                         "   g(5);"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "let ffunc,gfunc = "
	//		                         "    function (ref<int<4>,f,f,plain> a)->int<4> {"
	//		                         "         ffunc(*a);"
	//		                         "         gfunc(6);"
	//		                         "         return 5;"
	//		                         "    },"
	//		                         "    function (ref<int<4>,f,f,plain> b)->int<4> {"
	//		                         "         ffunc(7);"
	//		                         "         gfunc(*b);"
	//		                         "         return 3;"
	//		                         "    };"
	//		                         "unit main() { ffunc(12); }"));

	//	EXPECT_TRUE(test_program(nm, "let ffunc,gfunc = "
	//		                         "    lambda (int<4> a)->unit {"
	//		                         "         ffunc(a);"
	//		                         "         gfunc(6);"
	//		                         "    },"
	//		                         "    lambda (int<4> b)->unit {"
	//		                         "         ffunc(7);"
	//		                         "         gfunc(b);"
	//		                         "    };"
	//		                         "unit main() { ffunc(12); }"));

	//	EXPECT_TRUE(test_program(nm, "let gfunc,ffunc = "
	//		                         "    lambda (int<4> a)->unit {"
	//		                         "         ffunc(6);"
	//		                         "         gfunc(5);"
	//		                         "    },"
	//		                         "    lambda (int<4> b)->unit {"
	//		                         "         ffunc(5);"
	//		                         "         gfunc(10);"
	//		                         "    };"
	//		                         "unit main() { ffunc(12); }"));

	//	EXPECT_TRUE(test_program(nm, "let int = int<4>;"
	//		                         "let f = function (ref<int,f,f,plain> a) -> int {"
	//		                         "   return 5;"
	//		                         "};"
	//		                         "int main() {"
	//		                         "   f(0);"
	//		                         "   return 0;"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "let int = int<4>;"
	//		                         "let uint = uint<4>;"
	//		                         "let differentbla = function (ref<'b,f,f,plain> x) -> unit {"
	//		                         "    decl auto m = x;"
	//		                         "    decl auto l = m;"
	//		                         "};"
	//		                         "let bla = function (ref<'a,f,f,plain> f) -> unit {"
	//		                         "    let anotherbla = function (ref<'a,f,f,plain> x) -> unit {"
	//		                         "        decl auto m = x;"
	//		                         "    };"
	//		                         "    anotherbla(f);"
	//		                         "    differentbla(f);"
	//		                         "};"
	//		                         "int main() {"
	//		                         "    decl int x = 10;"
	//		                         "    bla(x);"
	//		                         "    return 0;"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "unit main() {"
	//		                         "    let class = struct name { int<2> a};"
	//		                         "    let collection = array<class, 10>;"
	//		                         "    decl ref<collection> x;"
	//		                         "    decl int<2> y;"
	//		                         "    x[5].a = y;"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "unit main()  {"
	//		                         "while ( false || true ) { 1+1; }"
	//		                         "}"));


	//	EXPECT_TRUE(test_program(nm, "unit main()  {"
	//		                         "while ( true || false ) { 1+1; }"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "unit main()  {"
	//		                         "while ( (false && true) || (true && false) ) { decl int<4> a = 5; }"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "unit main()  {"
	//		                         "while ( false || true && true ) { decl int<4> a = 5; }"
	//		                         "}"));

	//	EXPECT_TRUE(test_program(nm, "unit main()  {"
	//		                         "while ( false && true || true ) { decl int<4> a = 5; }"
	//		                         "}"));
	//}

} // parser
} // core
} // insieme
