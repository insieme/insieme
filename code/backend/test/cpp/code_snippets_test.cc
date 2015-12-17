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

#include "insieme/core/ir_builder.h"
#include "insieme/core/frontend_ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace backend {


	TEST(CppSnippet, CppReference) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;
				
				def f : ( x : ref<int,f,f>, y : cpp_ref<int,f,f>, z : cpp_ref<int,t,f>, w : cpp_ref<int,t,t> ) -> int {
					return *x + *y + *z + *w;
				};
				
				int main() {
					var ref<int> i = ref_var_init(12);
					var cpp_ref<int,f,f> j = ref_cast(i, type_lit(f), type_lit(f), type_lit(cpp_ref));
					var cpp_ref<int,t,f> k = ref_cast(i, type_lit(t), type_lit(f), type_lit(cpp_ref));
					var cpp_ref<int,t,t> l = ref_cast(i, type_lit(t), type_lit(t), type_lit(cpp_ref));
					f(i,j,k,l);
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, CppRValueReference) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;
				
				def g : () -> int { return 12; };

				def f : ( y : cpp_rref<int,f,f>, z : cpp_rref<int,t,f>, w : cpp_rref<int,t,t> ) -> int {
					return *y + *z + *w;
				};
				
				int main() {
					f(g(),g(),g());
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, MemberFunctions) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;
				
				def struct Math {
				
					lambda id : (a : int)->int {
						return a;
					}
				
					lambda sum : (a : int, b : int)->int {
						return a + b;
					}
				};
				
				int main() {
					var ref<Math> m;
					
					print("%d\n", m->id(12));
					print("%d\n", m->sum(12, 14));
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, TrivialClass) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;
				
				def struct A { };
				
				int main() {
					var ref<A> a;
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
		EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

		// check definition of six essential functions
		EXPECT_FALSE(containsSubString(code, "A()"));

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, RecursiveClass) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;
				
				def struct A { x : ref<A>; };
				
				int main() {
					var ref<A> a;
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
		EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

		// check definition of six essential functions
		EXPECT_FALSE(containsSubString(code, "A()"));

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, MutualRecursiveClass) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;
				
				decl struct A;
				decl struct B;

				def struct A { x : ref<B>; };
				def struct B { x : ref<A>; };
				
				int main() {
					var ref<A> a;
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
		EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

		// make sure no constructors are defined
		EXPECT_FALSE(containsSubString(code, "A()"));
		EXPECT_FALSE(containsSubString(code, "B()"));

		// also, the definition of B is missing
		EXPECT_FALSE(containsSubString(code, "struct B {"));


		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, TheEssentialSix) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;
				
				def struct A {
				    ctor( ) {} 
				    ctor( x : int ) {}
				};
				
				int main() {
					var ref<A> a;
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
		EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

		// check definition of six essential functions
		EXPECT_PRED2(containsSubString, code, "A() = default;");						// default constructor
		EXPECT_PRED2(containsSubString, code, "A(const A& p2) = default;");				// default copy constructor
		EXPECT_PRED2(containsSubString, code, "A(A&& p2) = default;");					// default move constructor
		EXPECT_PRED2(containsSubString, code, "~A() = default;");						// default destructor
		EXPECT_PRED2(containsSubString, code, "A& operator=(const A& p2) = default;");		// default assignment
		EXPECT_PRED2(containsSubString, code, "A& operator=(A&& p2) = default;");			// default move assignment

		EXPECT_PRED2(containsSubString, code, "A(int32_t p2);");						// user defined constructor declaration
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x) { }");					// user defined constructor definition

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, TheEssentialSixMutualRecursion) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;
				
				decl struct A;
				decl struct B;

				def struct A {
 					data : ref<B>;
 					ctor( ) {} 
					ctor( x : int ) {}
				};

				def struct B {
 					data : ref<A>;
 					ctor( ) {}
					ctor( x : int ) {}
				};
				
				int main() {
					var ref<A> a;
					var ref<B> b;
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
		EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

		// check definition of six essential functions
		EXPECT_PRED2(containsSubString, code, "A() = default;");						// default constructor
		EXPECT_PRED2(containsSubString, code, "A(const A& p2) = default;");				// default copy constructor
		EXPECT_PRED2(containsSubString, code, "A(A&& p2) = default;");					// default move constructor
		EXPECT_PRED2(containsSubString, code, "~A() = default;");						// default destructor
		EXPECT_PRED2(containsSubString, code, "A& operator=(const A& p2) = default;");		// default assignment
		EXPECT_PRED2(containsSubString, code, "A& operator=(A&& p2) = default;");			// default move assignment

		EXPECT_PRED2(containsSubString, code, "A(int32_t p2);");						// user defined constructor declaration
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x) { }");					// user defined constructor definition


		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(CppSnippet, Constructors) {
			core::NodeManager manager;
			core::IRBuilder builder(manager);

			// create a code fragment including some member functions
			core::ProgramPtr program = builder.parseProgram(R"(
					alias int = int<4>;

					def struct A {
						x : int;
	 					ctor( ) {
	 						this->x = 12;
	 					} 
						ctor( x : int ) {
	 						this->x = x;
	 					} 
						ctor( x : int , y : int ) {
							this->x = x + y;
						}
					};
/*
					def f : A::( x : int, y : int, z : int ) {
						this->x = x + y + z;
					};
*/				
					int main() {
						var ref<A> a;
						// var ref<A> b = f(ref_var(type_lit(A)));
						return 0;
					}
			)");

			ASSERT_TRUE(program);
			std::cout << "Program: " << dumpColor(program) << std::endl;
			EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

			// use sequential backend to convert into C++ code
			auto converted = sequential::SequentialBackend::getDefault()->convert(program);
			ASSERT_TRUE((bool)converted);
			std::cout << "Converted: \n" << *converted << std::endl;

			// check presence of relevant code
			auto code = toString(*converted);
			EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
			EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

			// check definition of six essential functions
			EXPECT_PRED2(containsSubString, code, "A();");
			EXPECT_PRED2(containsSubString, code, "A::A() : x(12) { }");

			EXPECT_PRED2(containsSubString, code, "A(int32_t p2);");
			EXPECT_PRED2(containsSubString, code, "A::A(int32_t x) : x(x) { }");

			EXPECT_PRED2(containsSubString, code, "A(int32_t p2, int32_t p3);");
			EXPECT_PRED2(containsSubString, code, "A::A(int32_t x, int32_t y) : x(x + y) { }");


			// try compiling the code fragment
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
			compiler.addFlag("-c"); // do not run the linker
			EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
		}


	TEST(CppSnippet, Counter) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment implementing a counter class
		core::ProgramPtr program = builder.parseProgram(
		    R"(
				alias int = int<4>;
				
				def struct Counter {
				
					value : int;
					
					lambda reset : ()->unit {
						value = 0;
					}
					
					lambda inc : ()->int {
						value = value + 1;
						return *value;
					}
					
					lambda dec : ()->int {
						value = value - 1;
						return *value;
					}
					
					lambda get : ()->int {
						return *value;
					}
					
					lambda set : (x : int)->unit {
						value = x;
					}
					
					lambda p : ()-> unit {
						print("%d\n", get());
					}
				};
				
				int main() {
					var ref<Counter> c;
					c->reset();
					c->p();
					c->inc();
					c->p();
					c->inc();
					c->p();
					c->dec();
					c->p();
					c->set(14);
					c->p();
					return 0;
				}
				)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << *program << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, DISABLED_Inheritance) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment implementing a counter class
		core::ProgramPtr program = builder.parseProgram(
		    R"(
				alias int = int<4>;
				
				def struct A {
					x : int;
				};
				
				def struct B : [A] {
					y : int;
				};
				
				def struct C : [B] {
					z : int;
				};
				
				int main() {
				
					// -------- handle an instance of A --------
					var ref<A> a;
					a.x = 1;
					
					
					// -------- handle an instance of B --------
					var ref<B> b;
					
					// direct access
					b.as(A).x = 1;
					b.y = 2;
					
					// indirect access of A's x
					auto bA = b.as(A);
					bA.x = 3;
					
					
					// -------- handle an instance of C --------
					var ref<C> c;
					
					// access B's A's x
					c.as(B).as(A).x = 1;
				
					// access B's y
					c.as(B).y = 2;
					
					// access C's z
					c.z = 3;
					
					print("x = %d\n", *(c.as(B).as(A).x));
					print("y = %d\n", *(c.as(B).y));
					print("z = %d\n", *c.z);
					
					return 0;
				}
				)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << core::printer::PrettyPrinter(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		// std::cout << "Converted: \n" << *converted << std::endl;

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

//	TEST(CppSnippet, ClassMetaInfo) {
//		core::NodeManager mgr;
//		core::FrontendIRBuilder builder(mgr);
//
//		// create a class type
//		auto counterType = builder.parseType("let Counter = struct { int<4> value; }; Counter");
//		ASSERT_TRUE(counterType);
//
//		// create symbol map for remaining task
//		std::map<string, core::NodePtr> symbols;
//		symbols["Counter"] = counterType;
//
//		auto parseExpr = [&](const string& code) { return builder.parseExpr(code, symbols).as<core::LambdaExprPtr>(); };
//		auto parseType = [&](const string& code) { return builder.parseType(code, symbols).as<core::FunctionTypePtr>(); };
//
//		// add member functions to meta info
//		core::ClassMetaInfo info;
//
//
//		// ------- Constructor ----------
//
//		// default
//		info.addConstructor(parseExpr("lambda ctor Counter::() { }"));
//
//		// with value
//		info.addConstructor(parseExpr("lambda ctor Counter::(int<4> x) { this.value = x; }"));
//
//		// copy constructor
//		info.addConstructor(parseExpr("lambda ctor Counter::(ref<Counter> c) { this.value = *(c.value); }"));
//
//		// ------- member functions ----------
//
//		// a non-virtual, const function
//		info.addMemberFunction("get", parseExpr("lambda Counter::()->int<4> { return *this.value; }"), false, true);
//
//		// a non-virtual, non-const function
//		info.addMemberFunction("set", parseExpr("lambda Counter::(int<4> x)->unit { this.value = x; }"), false, false);
//
//		// a virtual, const function
//		info.addMemberFunction("print", parseExpr(R"(lambda Counter::()->unit { print("%d\n", *this.value); })"), true, true);
//
//		// a virtual, non-const function
//		info.addMemberFunction("clear", parseExpr("lambda Counter::()->unit { }"), true, false);
//
//		// a pure virtual, non-const function
//		info.addMemberFunction("dummy1", builder.getPureVirtual(parseType("method Counter::()->int<4>")), true, false);
//
//		// a pure virtual, const function
//		info.addMemberFunction("dummy2", builder.getPureVirtual(parseType("method Counter::()->int<4>")), true, true);
//
//		// std::cout << info << "\n";
//
//		// attach
//		core::setMetaInfo(counterType, info);
//
//		// verify proper construction
//		EXPECT_TRUE(core::checks::check(counterType).empty()) << core::checks::check(counterType);
//
//		// ------------ create code using the counter type --------
//
//		auto prog = builder.parseProgram("int<4> main() { decl ref<ref<Counter>> c; return *((*c).value); }", symbols);
//
//		EXPECT_TRUE(core::checks::check(prog).empty());
//
//		// generate code
//		auto targetCode = sequential::SequentialBackend::getDefault()->convert(prog);
//		ASSERT_TRUE((bool)targetCode);
//
//		// check generated code
//		string code = toString(*targetCode);
//		EXPECT_PRED2(containsSubString, code, "Counter();");
//		EXPECT_PRED2(containsSubString, code, "Counter(int32_t p2);");
//		EXPECT_PRED2(containsSubString, code, "Counter(Counter* p2);");
//
//		EXPECT_PRED2(containsSubString, code, "int32_t get() const;");
//		EXPECT_PRED2(containsSubString, code, "void set(int32_t p2);");
//		EXPECT_PRED2(containsSubString, code, "virtual void print() const;");
//		EXPECT_PRED2(containsSubString, code, "virtual void clear();");
//
//		EXPECT_PRED2(containsSubString, code, "virtual int32_t dummy1() =0;");
//		EXPECT_PRED2(containsSubString, code, "virtual int32_t dummy2() const =0;");
//
//		//		std::cout << *targetCode;
//
//		// try compiling the code fragment
//		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
//		compiler.addFlag("-c"); // do not run the linker
//		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
//
//
//		// -------------------------------------------- add destructor -----------------------------------------
//
//		info.setDestructor(parseExpr("lambda ~Counter::() {}"));
//		core::setMetaInfo(counterType, info);
//		EXPECT_TRUE(core::checks::check(counterType).empty()) << core::checks::check(counterType);
//
//		targetCode = sequential::SequentialBackend::getDefault()->convert(prog);
//		ASSERT_TRUE((bool)targetCode);
//
//		//		std::cout << *targetCode;
//
//		// check generated code
//		code = toString(*targetCode);
//		EXPECT_PRED2(containsSubString, code, "~Counter();");
//		EXPECT_PRED2(notContainsSubString, code, "virtual ~Counter();");
//		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
//
//
//		// ---------------------------------------- add virtual destructor -----------------------------------------
//
//		info.setDestructorVirtual();
//		core::setMetaInfo(counterType, info);
//		EXPECT_TRUE(core::checks::check(counterType).empty()) << core::checks::check(counterType);
//
//		targetCode = sequential::SequentialBackend::getDefault()->convert(prog);
//		ASSERT_TRUE((bool)targetCode);
//
//		//		std::cout << *targetCode;
//
//		// check generated code
//		code = toString(*targetCode);
//		EXPECT_PRED2(containsSubString, code, "virtual ~Counter();");
//		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
//	}
//
//
//	TEST(CppSnippet, VirtualFunctionCall) {
//		core::NodeManager mgr;
//		core::FrontendIRBuilder builder(mgr);
//
//		std::map<string, core::NodePtr> symbols;
//
//		// create a class A with a virtual function
//		core::TypePtr classA = builder.parseType("let A = struct { }; A");
//		symbols["A"] = classA;
//
//		auto funType = builder.parseType("method A::(int<4>)->int<4>", symbols).as<core::FunctionTypePtr>();
//
//		core::ClassMetaInfo infoA;
//		infoA.addMemberFunction("f", builder.getPureVirtual(funType), true);
//		core::setMetaInfo(classA, infoA);
//
//		// create a class B
//		core::TypePtr classB = builder.parseType("let B = struct : A { }; B", symbols);
//		symbols["B"] = classB;
//
//		core::ClassMetaInfo infoB;
//		infoB.addMemberFunction("f", builder.parseExpr("lambda B::(int<4> x)->int<4> { return x + 1; }", symbols).as<core::LambdaExprPtr>(), true);
//		core::setMetaInfo(classB, infoB);
//
//		auto res = builder.parseProgram(R"(
//				let f = expr lit("f" : method A::(int<4>)->int<4>);
//
//				let ctorB1 = lambda ctor B::() { };
//				let ctorB2 = lambda ctor B::(int<4> x) { };
//
//				int<4> main() {
//					decl ref<A> x = ctorB1(new(undefined(B)));
//					decl ref<A> y = ctorB2(new(undefined(B)), 5);
//					x->f(3);
//					delete(x);
//					return 0;
//				})",
//		                                symbols);
//
//		ASSERT_TRUE(res);
//
//		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);
//
//		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
//		ASSERT_TRUE((bool)targetCode);
//
//		// std::cout << *targetCode;
//
//		// check generated code
//		auto code = toString(*targetCode);
//		EXPECT_PRED2(containsSubString, code, "(*x).f(3);");
//
//		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
//		compiler.addFlag("-c"); // do not run the linker
//		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
//	}

	//TODO this seems to hang in an endless loop
	/*TEST(CppSnippet, ConstructorCall) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		// create a example code using all 3 ctor variants
		auto res = builder.parseProgram(
		    R"(
					alias int = int<4>;
					
					def struct A {
					
						x : int;
						
						ctor () {
							x = 0;
						}
						
						ctor (x : int) {
							this.x = x;
						}
					};
					
					int main() {
						// on stack
						var ref<A> a1 = A::(ref_var(undefined(A)));
						var ref<A> a2 = A::(ref_var(undefined(A)), 1);

						// on heap
						var ref<A> a3 = A::(ref_new(undefined(A)));
						var ref<A> a4 = A::(ref_new(undefined(A)), 1);

						// in place
						var ref<A> a5; A::(a5);
						var ref<A> a6; A::(a6, 1);

						return 0;
					}
				)");

		ASSERT_TRUE(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		// std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "A a1;");
		EXPECT_PRED2(containsSubString, code, "A a2((1));");
		EXPECT_PRED2(containsSubString, code, "A* a3 = new A();");
		EXPECT_PRED2(containsSubString, code, "A* a4 = new A(1);");
		EXPECT_PRED2(containsSubString, code, "new (&a5) A();");
		EXPECT_PRED2(containsSubString, code, "new (&a6) A(1);");

		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}*/

	//TODO this seems to hang in an endless loop
	/*TEST(CppSnippet, DestructorCall) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.parseProgram(
		    R"(
					alias int = int<4>;
					
					def struct A {
					
						x : int;
						
						ctor (x : int) {
							this.x = x;
							print("Creating: %d\n", x);
						}
						
						dtor () {
							print("Clearing: %d\n", *x);
							x = 0;
						}
					};
					
					int main() {
					
						// create an un-initialized memory location
						var ref<A> a = A::(ref_var(undefined(A)), 1);
						
						// init using in-place constructor
						A::(a, 2);
					
						// invoke destructor
						A::~(a);
					
						return 0;
					}
				)");

		ASSERT_TRUE(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		// std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "(*a).A::~A()");

		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}*/

//	TEST(CppSnippet, ArrayConstruction) {
//		core::NodeManager mgr;
//		core::IRBuilder builder(mgr);
//
//		std::map<string, core::NodePtr> symbols;
//		symbols["createArray"] = mgr.getLangExtension<core::lang::IRppExtensions>().getArrayCtor();
//
//		auto res = builder.parseProgram(
//		    R"(
//					let int = int<4>;
//
//					let A = struct { int x; };
//
//					let ctorA = lambda ctor A::() {
//						this.x = 4;
//					};
//
//					int main() {
//
//						// create an array of objects of type A on the stack
//						decl ref<array<A,1>> a = createArray(ref_var, ctorA, 5u);
//
//						// create an array of objects of type A on the heap
//						decl ref<array<A,1>> b = createArray(ref_new, ctorA, 5u);
//
//						// update an element
//						a[3].x = 12;
//						b[3].x = 12;
//
//						return 0;
//					}
//				)",
//		    symbols);
//
//		ASSERT_TRUE(res);
//		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);
//
//		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
//		ASSERT_TRUE((bool)targetCode);
//
//		// std::cout << *targetCode;
//
//		// check generated code
//		auto code = toString(*targetCode);
//		EXPECT_PRED2(containsSubString, code, "A a[5u];");
//		EXPECT_PRED2(containsSubString, code, "A* b = new A[5u];");
//
//		EXPECT_PRED2(containsSubString, code, "a[3].x = 12;");
//		EXPECT_PRED2(containsSubString, code, "b[3].x = 12;");
//
//
//		// check whether ctor is present!
//		EXPECT_PRED2(containsSubString, code, "A();");
//		EXPECT_PRED2(containsSubString, code, "A::A() : x(4) {");
//
//		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
//		compiler.addFlag("-c"); // do not run the linker
//		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
//	}
//
//	TEST(CppSnippet, VectorConstruction) {
//		core::NodeManager mgr;
//		core::IRBuilder builder(mgr);
//
//		std::map<string, core::NodePtr> symbols;
//		symbols["createVector"] = mgr.getLangExtension<core::lang::IRppExtensions>().getVectorCtor();
//
//		auto res = builder.parseProgram(
//		    R"(
//					let int = int<4>;
//
//					let A = struct { int x; };
//
//					let ctorA = lambda ctor A::() {
//						this.x = 4;
//					};
//
//					int main() {
//
//						let size = expr lit("not important" : intTypeParam<5>);
//
//						// create an array of objects of type A on the stack
//						decl ref<array<A,1>> a = createVector(ref_var, ctorA, size);
//
//						// create an array of objects of type A on the heap
//						decl ref<array<A,1>> b = createVector(ref_new, ctorA, size);
//
////						// create an array of objects of type A on the stack
////						decl ref<vector<A,5>> c = createVector(ref_var, ctorA, size);
////
////						// create an array of objects of type A on the heap
////						decl ref<vector<A,5>> d = createVector(ref_new, ctorA, size);
//
//						// update an element
//						a[3].x = 12;
//						b[3].x = 12;
////						c[3].x = 12;
////						d[3].x = 12;
//
//						return 0;
//					}
//				)",
//		    symbols);
//
//		// TODO: also support ref<vector<X,y>> as the value type
//
//		ASSERT_TRUE(res);
//		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);
//
//		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
//		ASSERT_TRUE((bool)targetCode);
//
//		// std::cout << *targetCode;
//
//		// check generated code
//		auto code = toString(*targetCode);
//		EXPECT_PRED2(containsSubString, code, "A a[5u];");
//		EXPECT_PRED2(containsSubString, code, "A* b = new A[5u];");
//		//		EXPECT_PRED2(containsSubString, code, "A c[5u];");
//		//		EXPECT_PRED2(containsSubString, code, "A* d = new A[5u];");
//
//		EXPECT_PRED2(containsSubString, code, "a[3].x = 12;");
//		EXPECT_PRED2(containsSubString, code, "b[3].x = 12;");
//		//		EXPECT_PRED2(containsSubString, code, "c[3].x = 12;");
//		//		EXPECT_PRED2(containsSubString, code, "d[3].x = 12;");
//
//
//		// check whether ctor is present!
//		EXPECT_PRED2(containsSubString, code, "A();");
//		EXPECT_PRED2(containsSubString, code, "A::A() : x(4) {");
//
//		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
//		compiler.addFlag("-c"); // do not run the linker
//		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
//	}

	//TODO asserts in a semantic check
	/*TEST(CppSnippet, InitializerList) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					alias int = int<4>;

					def struct B { };
					def struct A : [B] {

						x : int;
						y : int;
						z : int;

						ctor (y : int) {
							this.y = y;
							for ( int i = 0 .. 10 ) {
								this.y = 1;
								this.z = 3;
							}
							this.x = 4;
							this.z = 2;
						}
					};

					int main() {

						// call constructor
						var ref<A> a = A::(ref_new(undefined(A)), 10);

						return 0;
					}
				)"));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		//		std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "A a((10));");
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t y) : x(4), y(y) {");
		EXPECT_PRED2(containsSubString, code, "(*this).z = 2;");

		EXPECT_PRED2(notContainsSubString, code, "(*this).x = 4;");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}*/


	TEST(CppSnippet, DISABLED_InitializerList2) {
		// something including a super-constructor call

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					alias int = int<4>;

					def struct A {

						x : int;

						ctor (x : int) {
							this.x = x;
						}
					};

					def struct B : [A] {

						y : int;

						ctor (x : int, y : int) {
							A::(this, x);
							this.y = y;
						}
					}

					int main() {

						// call constructor
						var ref<B> b = B::(ref_var(undefined(B)), 1, 2);

						return 0;
					}
				)"));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		//		std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "B b((1), (2));");
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x) : x(x) {");
		EXPECT_PRED2(containsSubString, code, "B::B(int32_t x, int32_t y) : A(x), y(y) {");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, DISABLED_InitializerList3) {
		// something including a super-constructor call

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					alias int = int<4>;

					def struct A {

						x : int;
						y : int;

						ctor (x : int, y : int) {
							this.x = x;
							this.y = y;
						}
					};

					def struct B : [A] {

						z : int;

						ctor (x : int, y : int, z : int) {
							A::(this, x, y + z);
							this.z = z;
						}
					};

					int main() {

						// call constructor
						var ref<B> b = B::(ref_var(undefined(B)), 1, 2, 3);

						return 0;
					}
				)"));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		//		std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "B b((1), (2), (3));");
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x, int32_t y) : x(x), y(y) {");
		EXPECT_PRED2(containsSubString, code, "B::B(int32_t x, int32_t y, int32_t z) : A(x, y + z), z(z) {");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	//TODO asserts in a semantic check
	/*TEST(CppSnippet, DISABLED_InitializerList4) {
		// something including a non-parameter!

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					alias int = int<4>;

					def struct A {

						x : int;
						y : int;

						ctor (x : int, y : int) {
							this.x = x;
							this.y = this.x + y;
						}
					};

					int main() {

						// call constructor
						var ref<A> b = A::(ref_var(undefined(A)), 1, 2);

						return 0;
					}
				)"));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		//		std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "A b((1), (2));");
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x, int32_t y) : x(x), y((*this).x + y) {");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}*/

	//TODO try/catch not supported by the parser atm
	/*TEST(CppSnippet, DISABLED_Exceptions) {
		// something including a non-parameter!

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					let short = int<2>;
					let int = int<4>;

					int main() {

						try {

							throw 4;

						} catch (int x) {
							print("Catched integer %d\n", x);
						} catch (short y) {
							print("Catched short %d\n", y);
						} catch (ref<short> z) {
							print("Catched short %d\n", *z);
//						} catch (any a) {
//							print("Catched something!\n");
						 }

						return 0;
					}
				)"));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		// std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "throw 4;");
		EXPECT_PRED2(containsSubString, code, "} catch(int32_t x) {");
		EXPECT_PRED2(containsSubString, code, "} catch(int16_t y) {");
		EXPECT_PRED2(containsSubString, code, "} catch(int16_t* z) {");
//		EXPECT_PRED2(containsSubString, code, "} catch(...) {");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}*/


	TEST(CppSnippet, DISABLED_StaticVariableConst) {
		// something including a non-parameter!

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto& ext = mgr.getLangExtension<core::lang::StaticVariableExtension>();

		std::map<string, core::NodePtr> symbols;
		symbols["init"] = ext.getInitStaticConst();

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					alias int = int<4>;
					let a = lit("a":ref<struct __static_var { initialized : bool; value : int; }>);

					int main() {

						init(a, 5);

						return 0;
					}
				)",
		    symbols));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		// std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, " static int32_t a = 5");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, DISABLED_StaticVariable) {
		// something including a non-parameter!

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto& ext = mgr.getLangExtension<core::lang::StaticVariableExtension>();

		std::map<string, core::NodePtr> symbols;
		symbols["init"] = ext.getInitStaticLazy();

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					alias int = int<4>;
					let a = expr lit("a":ref<struct __static_var { bool initialized; int value; }>);

					int main() {

						init(a, lambda ()=> 1);
						init(a, lambda ()=> 2);

						return 0;
					}
				)",
		    symbols));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		// std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "static int32_t a = __insieme_type_");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, DISABLED_StaticVariableSingle) {
		// something including a non-parameter!

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto& ext = mgr.getLangExtension<core::lang::StaticVariableExtension>();

		std::map<string, core::NodePtr> symbols;
		symbols["init"] = ext.getInitStaticLazy();

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					alias int = int<4>;
					let a = expr lit("a":ref<struct __static_var { bool initialized; int value; }>);

					int main() {

						init(a, lambda ()=> 2);

						return 0;
					}
				)",
		    symbols));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		// std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, " static int32_t a = 2;");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, DISABLED_StaticVariableDefaultCtor) {
		// something including a non-parameter!

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto& ext = mgr.getLangExtension<core::lang::StaticVariableExtension>();

		std::map<string, core::NodePtr> symbols;
		symbols["init"] = ext.getInitStaticLazy();

		auto res = builder.normalize(builder.parseProgram(
		    R"(
					alias int = int<4>;
					let A = struct A {};
					let a = expr lit("a":ref<struct __static_var { bool initialized; A value; }>);
					let ctorA = lambda ctor A::() { };

					int main() {

						init(a, lambda ()=> *(ctorA(var(undefined(A)))));

						return 0;
					}
				)",
		    symbols));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		// std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, " static A a;");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

} // namespace backend
} // namespace insieme
