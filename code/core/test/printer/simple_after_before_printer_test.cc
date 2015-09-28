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
#include <sstream>
#include <fstream>
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"


#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/parser3/ir_parser.h"
#include "../../include/insieme/core/ir_node.h"

namespace insieme {
	namespace core {
		namespace parser3 {

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
												  | PrettyPrinter::PRINT_ANNOTATIONS | PrettyPrinter::NO_LIST_SUGAR
												  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
												  |
												  /*PrettyPrinter::PRINT_LITERAL_TYPES |*/ PrettyPrinter::PRINT_DERIVED_IMPL);
					std::ostringstream ss;
					ss << printerA;
					auto type2 = builder.parseType(ss.str());
					if (type2) {
						PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
													  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
													  | PrettyPrinter::PRINT_ANNOTATIONS | PrettyPrinter::NO_LIST_SUGAR
													  | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY
													  |
													  /*PrettyPrinter::PRINT_LITERAL_TYPES |*/ PrettyPrinter::PRINT_DERIVED_IMPL);

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
				return false;
			}

			TEST(After_Before_Test, Types) {
			NodeManager nm;
			EXPECT_TRUE(test_type(nm, "int<8>"));
			EXPECT_TRUE(test_type(nm, "let int = int<5>; int"));
			EXPECT_TRUE(test_type(nm, "someweirdname<>"));
			EXPECT_TRUE(test_type(nm, "vector<int<4>, 4>"));
			EXPECT_TRUE(test_type(nm, "vector<'a, 4>"));
			EXPECT_TRUE(test_type(nm, "struct { int<4> a; int<5> b}"));
			EXPECT_TRUE(test_type(nm, "struct name { int<4> a; int<5> b}"));
			EXPECT_TRUE(test_type(nm, "struct { int<4> a; int<5> b;}"));

			EXPECT_TRUE(test_type(nm, "( int<4> , ref<int<4>>) -> int<4>"));
			EXPECT_TRUE(test_type(nm, "( int<4> , ref<int<4>>) => int<4>"));
			EXPECT_TRUE(test_type(nm, "(array<'elem,'n>, vector<uint<8>,'n>) -> 'elem"));

			EXPECT_TRUE(test_type(nm, "struct C { int<4> field; }"));
			EXPECT_TRUE(test_type(nm, "let papa, mama = t<11>, t<4>; struct name : [ papa, mama ] { int<4> a; int<5> b}"));
			EXPECT_TRUE(test_type(nm, "let papa = t<11>; struct name :[ papa ]{ int<4> a; int<5> b}"));

		}


		bool test_expression(NodeManager &nm, const std::string &x) {
			IRBuilder builder(nm);

			std::cout << " ============== TEST ============ " << std::endl;
			auto type1 = builder.parseExpr(x);

			if (type1) {
				//dumpColor(type1);
				//dumpText(type1);
				PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
											  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
											  | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
											  | PrettyPrinter::NO_EVAL_LAZY | PrettyPrinter::PRINT_DERIVED_IMPL);
				std::ostringstream ss;
				ss << printerA;
				auto type2 = builder.parseExpr(ss.str());
				if (type2) {
					PrettyPrinter printerB(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
												  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
												  | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES |
												  PrettyPrinter::NO_EVAL_LAZY
												  | PrettyPrinter::PRINT_DERIVED_IMPL);

					if (builder.normalize(type1) == builder.normalize(type2)) {
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

		EXPECT_TRUE(test_expression(nm, "param(123456789)"));

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

		EXPECT_TRUE(test_expression(nm, "lambda () -> unit { }"));
		EXPECT_TRUE(test_expression(nm, "lambda (int<4> a) -> unit { }"));
		EXPECT_TRUE(test_expression(nm, "lambda () -> unit { decl int<4> a = 1+1; }"));
		EXPECT_TRUE(test_expression(nm, "lambda (bool a) -> bool { return a; }"));

		EXPECT_TRUE(test_expression(nm, "lambda ('a _) -> bool { return true; }"));
		EXPECT_TRUE(test_expression(nm, "lambda ('a x) -> bool { return true; }"));
		EXPECT_TRUE(test_expression(nm, "lambda ('a x) -> 'a { return x+CAST('a) 3; }"));
		EXPECT_TRUE(test_expression(nm, "lambda ('a x) -> 'a { return(x+CAST('a) 3); }"));
		EXPECT_TRUE(test_expression(nm, "lambda ('a x) -> 'a { return x+CAST('a) 3; }"));
		EXPECT_TRUE(test_expression(nm, "lambda ('a x) => x+CAST('a) 3"));


		EXPECT_TRUE(test_expression(nm, "type_lit(int<4>)"));

		EXPECT_TRUE(test_expression(nm,
									"lambda (int<4> v, int<4> exp) -> int<4> { "
											"	let one = lambda(int<4> _)-> int<4> { return 4;  };"
											"	let two = lambda(int<4> x)-> int<4> { return x+5; };"
											"   return one(two(exp));"
											"}  "));

		EXPECT_TRUE(test_expression(nm, "100+200*300"));
		EXPECT_TRUE(test_expression(nm, "100-200*300"));
		EXPECT_TRUE(test_expression(nm, "100-200+300"));
		EXPECT_TRUE(test_expression(nm, "100-(200+300)"));
		EXPECT_TRUE(test_expression(nm, "100-200-300"));

	}


	bool test_statement(NodeManager &nm, const std::string &x) {
		IRBuilder builder(nm);

		std::cout << " ============== TEST ============ " << std::endl;
		auto type1 = builder.parseStmt(x);

		if (type1) {
			dumpColor(type1);
			PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
										  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
										  | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
										  | PrettyPrinter::NO_EVAL_LAZY | PrettyPrinter::PRINT_DERIVED_IMPL);
//            PrettyPrinter printerA(type1, PrettyPrinter::PRINT_DEREFS);

			std::ostringstream ss;
			std::cout << printerA << std::endl;
			ss << printerA;
			auto type2 = builder.parseStmt(ss.str());
			if (type2) {
				PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
											  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
											  | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
											  | PrettyPrinter::NO_EVAL_LAZY | PrettyPrinter::PRINT_DERIVED_IMPL);

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
		}
		else {
			std::cout << "[parsing type1 went wrong!]" << std::endl;
			return false;
		}
	}


	TEST(After_Before_Test, Statements) {

	NodeManager nm;

//	EXPECT_TRUE(test_statement(nm, "{ decl int<4> x = 0; x+1; }"));
//	EXPECT_TRUE(test_statement(nm, "{ decl auto x = 0; x+1; }"));

	EXPECT_TRUE(test_statement(nm, "if ( true ) {}"));
	EXPECT_TRUE(test_statement(nm, "if ( true ) {} else {}"));
	EXPECT_TRUE(test_statement(nm, "if ( true ) if ( false ) { } else 1 ;"));
	EXPECT_TRUE(test_statement(nm, "if ( true ) if ( false ) { } else 1 ; else 2; "));
	EXPECT_TRUE(test_statement(nm, "if( false ) { return 0; } else { return 1+2; }"));
	EXPECT_TRUE(test_statement(nm, "if( false ) { return 0; }"));
	EXPECT_TRUE(test_statement(nm, "while ( true ) 1+1;"));
	EXPECT_TRUE(test_statement(nm, "while ( false ) 1+1;"));
// additional function created, which is not a single statement anymore therefore not parseable
// this will work when used in programs (you will find the tests there)
//EXPECT_TRUE(test_statement(nm, "while ( false || true ) { 1+1; }"));
//EXPECT_TRUE(test_statement(nm, "while ( false || true || false) { 1+1; }"));
//EXPECT_TRUE(test_statement(nm, "while (( false || true ) && (true || false)) { 1+1; }"));
	EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3) 1+1;"));
	EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3: 2) 1+1;"));
	EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3: 2) { 1+1; }"));

	EXPECT_TRUE(test_statement(nm, "switch (2) { case 1: 1; case 2: 2; }"));

	EXPECT_TRUE(test_statement(nm, "try  {2;} catch( int<4> e) { 1+1; }"));
	EXPECT_TRUE(test_statement(nm, "try  {2;} catch( int<4> e) { 1+1; } catch (ref<int<4>> r) { 3+4; }"));

	EXPECT_TRUE(test_statement(nm, "{ }"));

	EXPECT_TRUE(test_statement(nm,
							   "{"
									   "    let class = struct name { int<2> a};"
									   "}"
	));

	EXPECT_TRUE(test_statement(nm,
							   "{"
									   "    let class = struct name { int<2> a};"
									   "    let collection = vector<class, 10>;"
									   "}"
	));

	EXPECT_TRUE(test_statement(nm,
							   "{"
									   "    let class = struct somenewname { int<2> a};"
									   "    let collection = vector<int<2>,5>;"
									   "    decl ref<collection> x;"
									   "    decl ref<somenewname> y;"
									   "}"
	));

	EXPECT_TRUE(test_statement(nm,
							   "{"
									   "    let class = struct somenewname { int<2> a};"
									   "    let collection = vector<int<2>,5>;"
									   "    decl ref<collection> x;"
									   "    decl ref<someoldname> y;"
									   "}"
	));

/*
*      As the let-declaration is printed first, it is not a single statement anymore and not
*      parsable via parseStmnt(). It will work when parsed with parseProgram()
*
EXPECT_TRUE(test_statement(nm,
"{"
"    let class = struct name { int<2> a};"
"    let collection = vector<class, 10>;"
"    decl ref<collection> x;"
"    decl int<2> y;"
"}"
));

EXPECT_TRUE(test_statement(nm,
"{"
"    let class = struct name { int<2> a};"
"    let collection = vector<class, 10>;"
"    decl ref<collection> x;"
"    decl int<2> y;"
"    x[5].a = y;"
"}"
));
*/  }


bool test_program(NodeManager &nm, const std::string &x) {

	IRBuilder builder1(nm);

	std::ofstream ostreamA, ostreamB;


	std::cout << " ============== TEST ============== " << std::endl;
	auto type1 = builder1.parseProgram(x);
	EXPECT_TRUE(checks::check(type1).empty()) << checks::check(type1);

	if (type1) {
		dumpColor(type1);
		PrettyPrinter printerA(type1, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
									  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
									  | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
									  | /*PrettyPrinter::NO_EVAL_LAZY    |*/ PrettyPrinter::PRINT_DERIVED_IMPL);
		std::ostringstream ss;
		ss << printerA;
		auto type2 = builder1.parseProgram(ss.str());
		if (type2) {
			PrettyPrinter printerB(type2, PrettyPrinter::OPTIONS_DEFAULT | PrettyPrinter::PRINT_CASTS
										  | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS
										  | PrettyPrinter::NO_LIST_SUGAR | PrettyPrinter::PRINT_ATTRIBUTES
										  | /*PrettyPrinter::NO_EVAL_LAZY    |*/ PrettyPrinter::PRINT_DERIVED_IMPL);

			if (builder1.normalize(type1) == builder1.normalize(type2)) {
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
	}
	else {
		std::cout << "[parsing type1 went wrong!]" << std::endl;
		return false;
	}
}

TEST(After_Before_Test, Let
) {
NodeManager mgr;

EXPECT_TRUE(test_program(mgr, "let int = int<4>; int main () { return 1; }"));
EXPECT_TRUE(test_program(mgr, "let int = int<4>; let long = int<8>; long main (ref<int,f,f> a) { return 1; }"));
EXPECT_TRUE(test_program(mgr, "let int , long = int<4> ,int<8>; int<4> main () { return 1; }"));
EXPECT_TRUE(test_program(mgr, "let f = lambda () -> unit { }; int<4> main () { f(); return 1; }"));
EXPECT_TRUE(test_program(mgr,
						 "let int = int<4>; let f = lambda (int a) -> int { return a; }; int<4> main () { f(1); return 1; }"));

EXPECT_TRUE(test_program(mgr, "let f,g = lambda()->unit{g();},lambda()->unit{f();}; unit main() { f(); }"));

EXPECT_TRUE(test_program(mgr, ""
		"let int = int<4>;"
		"let f = lambda (int a) -> int {"
		"   return 0;"
		"};"
		"let g = lambda (int a, int b) -> unit {};"
		"unit main() {"
		"   f(3);"
		"   decl int x = 4;"
		"   decl int<4> y = 2;"
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
EXPECT_TRUE(test_program(mgr, ""
		"let class = struct name { int<4> a; int<5> b};"
		"unit main() {"
		"   decl ref<class> x;"
		"   decl ref<int<4>> y;"
		"}"));
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

TEST(After_Before_Test, Program
) {
NodeManager nm;

EXPECT_TRUE(test_program(nm, ""
		"unit main() {}"));
EXPECT_TRUE(test_program(nm, ""
		"let int = int<4>;"
		"int main() {"
		"   return 4;"
		"}"));

EXPECT_TRUE(test_program(nm, ""
		"int<4> main(ref<int<4>,f,f> a) {"
		"   decl int <4> c = 5;"
		"   return 42;"
		"}"));
EXPECT_TRUE(test_program(nm, ""
		"let int = int<4>;"
		"let f = lambda()->unit{};"
		"int<4> main() {"
		"   f();"
		"   return 4;"
		"}"));
EXPECT_TRUE(test_program(nm, ""
		"let int = int<4>;"
		"let f = lambda(int a)->int{"
		"   return a;"
		"};"
		"unit main(ref<int<4>,f,f> a) {"
		"   f(6);"
		"}"));

EXPECT_TRUE(test_program(nm, ""
		"let int = int<4>;"
		"let f = function(ref<int,f,f> a)->unit{"
		"   a = 5;"
		"};"
		"unit main(ref<int<4>,f,f> a) {"
		"   decl int b = 6;"
		"   f(b);"
		"}"));
EXPECT_TRUE(test_program(nm, ""
		"let int = int<4>;"
		"let f = function(ref<int<4>,f,f> a)->int{"
		"   return *a;"
		"};"
		"unit main() {"
		"   decl int z = f(5);"
		"}"));

EXPECT_TRUE(test_program(nm, ""
		"let f = function (ref<int<4>,f,f> a) -> int<4> { "
		"   return *a; "
		"}; "
		"unit main (ref<int<8>,f,f> argc, ref<int<4>,f,f> argc2) {1;2;3;f(2);}"));
EXPECT_TRUE(test_program(nm, ""
		"unit main (ref<int<4>,f,f> a, ref<int<4>,f,f> b)  { "
		"   decl int <4> c = *a;"
		"   a = 5; "
		"}"));
EXPECT_TRUE(test_program(nm, ""
		"let int = int<4>; "
		"unit main (ref<int,f,f> a, ref<int,f,f> b) { 1+1; }"));
EXPECT_TRUE(test_program(nm,
						 "let int = int<4>; "
								 "let f = function (ref<int,f,f> a) ->int { "
								 "  return *a; "
								 "}; "
								 "unit main (ref<int,f,f> a, ref<int,f,f> b) { "
								 "  f(1); "
								 "}"));

EXPECT_TRUE(test_program(nm,
						 "let int = int<4>;"
								 "let fun000 = recFunc function_name {"
								 "      function_name = lambda() -> unit{"
								 "          v001();"
								 "      };"
								 "      v001 = lambda() -> unit {"
								 "          function_name();"
								 "      };"
								 "};"
								 "int main() { "
								 "      decl int x = 10;"
								 "      fun000();"
								 "      return 0; "
								 "}"
));

EXPECT_TRUE(test_program(nm,
						 "let ffunc = function (ref<int<4>,f,f> a)->int<4> {"
								 "         ffunc(*a);"
								 "         return 5;"
								 "};"
								 "let gfunc = lambda(int<4> a)->int<4> {"
								 "  return a;"
								 "};"
								 "unit main() { "
								 "  decl ref<int<4>,f,f> s = var(5);"
								 "  ffunc(*s);"
								 "  gfunc(4);"
								 " }"
));

EXPECT_TRUE(test_program(nm, ""
		"let int = int<4>;"
		"let f = lambda (int a) -> int {"
		"   return a;"
		"};"
		"int main() {"
		"   decl int a = 5;"
		"   f(4);"
		"   f(a);"
		"   return 0;"
		"}"));

EXPECT_TRUE(test_program(nm,
						 "let ffunc = function (ref<int<4>,f,f> a)->unit {"
								 "         ffunc(*a);"
								 "};"
								 "unit main() { "
								 "  decl ref<int<4>,f,f> s = var(5);"
								 "  ffunc(*s); "
								 "}"
));

EXPECT_TRUE(test_program(nm,
						 "let ffunc = lambda (int<4> a)->int<4> {"
								 "      return a;"
								 "    };"
								 "unit main() { ffunc(12); }"
));

EXPECT_TRUE(test_program(nm, ""
		"let int = int<4>;"
		"let f = lambda(int a) -> unit {};"
		"let g = lambda(int b) -> unit {"
		"   f(5);"
		"   f(b);"
		"};"
		"unit main() {"
		"   g(5);"
		"}"));

EXPECT_TRUE(test_program(nm,
						 "let ffunc,gfunc = "
								 "    function (ref<int<4>,f,f> a)->int<4> {"
								 "         ffunc(*a);"
								 "         gfunc(6);"
								 "         return 5;"
								 "    },"
								 "    function (ref<int<4>,f,f> b)->int<4> {"
								 "         ffunc(7);"
								 "         gfunc(*b);"
								 "         return 3;"
								 "    };"
								 "unit main() { ffunc(12); }"
));

EXPECT_TRUE(test_program(nm,
						 "let ffunc,gfunc = "
								 "    lambda (int<4> a)->unit {"
								 "         ffunc(a);"
								 "         gfunc(6);"
								 "    },"
								 "    lambda (int<4> b)->unit {"
								 "         ffunc(7);"
								 "         gfunc(b);"
								 "    };"
								 "unit main() { ffunc(12); }"
));

EXPECT_TRUE(test_program(nm,
						 "let gfunc,ffunc = "
								 "    lambda (int<4> a)->unit {"
								 "         ffunc(6);"
								 "         gfunc(5);"
								 "    },"
								 "    lambda (int<4> b)->unit {"
								 "         ffunc(5);"
								 "         gfunc(10);"
								 "    };"
								 "unit main() { ffunc(12); }"
));

EXPECT_TRUE(test_program(nm,
						 "let int = int<4>;"
								 "let f = function (ref<int,f,f> a) -> int {"
								 "   return 5;"
								 "};"
								 "int main() {"
								 "   f(0);"
								 "   return 0;"
								 "}"));

EXPECT_TRUE(test_program(nm,
						 "let int = int<4>;"
								 "let uint = uint<4>;"
								 "let differentbla = function (ref<'b,f,f> x) -> unit {"
								 "    decl auto m = x;"
								 "    decl auto l = m;"
								 "};"
								 "let bla = function (ref<'a,f,f> f) -> unit {"
								 "    let anotherbla = function (ref<'a,f,f> x) -> unit {"
								 "        decl auto m = x;"
								 "    };"
								 "    anotherbla(f);"
								 "    differentbla(f);"
								 "};"
								 "int main() {"
								 "    decl int x = 10;"
								 "    bla(x);"
								 "    return 0;"
								 "}"
));

EXPECT_TRUE(test_program(nm,
						 "unit main() {"
								 "    let class = struct name { int<2> a};"
								 "    let collection = array<class, 10>;"
								 "    decl ref<collection> x;"
								 "    decl int<2> y;"
								 "    x[5].a = y;"
								 "}"
));
/*
EXPECT_TRUE(test_program(nm,
"unit main()  {"
"while ( false || true ) { 1+1; }"
"}"
));


EXPECT_TRUE(test_program(nm,
"unit main()  {"
"while ( true || false ) { 1+1; }"
"}"
));

EXPECT_TRUE(test_program(nm,
"unit main()  {"
"while ( (false && true) || (true && false) ) { decl int<4> a = 5; }"
"}"
));

EXPECT_TRUE(test_program(nm,
						 "unit main()  {"
								 "while ( (false || true) && (true || false) ) { decl int<4> a = 5; }"
								 "}"
));

EXPECT_TRUE(test_program(nm,
"unit main()  {"
"while ( false || true && true ) { decl int<4> a = 5; }"
"}"
));

EXPECT_TRUE(test_program(nm,
"unit main()  {"
"while ( false && true || true ) { decl int<4> a = 5; }"
"}"
));
*/
}

} // parser3
} // core
} // insieme
