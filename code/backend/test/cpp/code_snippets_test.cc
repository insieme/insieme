/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#include "insieme/core/checks/full_check.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/utils/logging.h"


namespace insieme {
namespace backend {


	TEST(CppSnippet, CppReference) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;

				def f = ( x : int, y : cpp_ref<int,f,f>, z : cpp_ref<int,t,f>, w : cpp_ref<int,t,t> ) -> int {
					return x + y + z + w;
				};

				int main() {
					var ref<int> i = 12;
					var cpp_ref<int,f,f> j = ref_cast(i, type_lit(f), type_lit(f), type_lit(cpp_ref));
					var cpp_ref<int,t,f> k = ref_cast(i, type_lit(t), type_lit(f), type_lit(cpp_ref));
					var cpp_ref<int,t,t> l = ref_cast(i, type_lit(t), type_lit(t), type_lit(cpp_ref));
					f(*i,j,k,l);
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

				def g = () -> int { return 12; };

				def f = ( y : cpp_rref<int,f,f>, z : cpp_rref<int,t,f>, w : cpp_rref<int,t,t> ) -> int {
					return y + z + w;
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

	TEST(CppSnippet, ReferenceParameter) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
				def IMP_test = function (v1 : ref<int<4>,f,f,cpp_ref>) -> unit { };
				int<4> function IMP_main (){
					var ref<int<4>,f,f,plain> bla = 2;
					IMP_test(ref_kind_cast(bla, type_lit(cpp_ref)));
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

		// check absence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(notContainsSubString, code, "&bla");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, ReferenceVariableDeclaration) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
				int<4> function IMP_main () {
					var ref<int<4>,f,f,plain> bla = 2;
					var ref<int<4>,f,f,cpp_ref> alb = bla;
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
		EXPECT_PRED2(containsSubString, code, "int32_t& alb = bla;");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, ReferenceVariableUse) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
				def IMP_test = function (v1 : ref<int<4>,f,f,cpp_ref>) -> unit { };
				int<4> function IMP_main () {
					var ref<int<4>,f,f,plain> bla = 2;
					var ref<int<4>,f,f,cpp_ref> alb = bla;
					IMP_test(alb);
					bla + alb;
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
		EXPECT_PRED2(containsSubString, code, "IMP_test(alb)");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, ReferenceVariableMethodUse) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
				def struct A {
					lambda foo = () -> unit {}
				};

				int<4> function IMP_main () {
					var ref<A> a;
					var ref<A,f,f,cpp_ref> b = a;
					b.foo();
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
		auto code = utils::removeCppStyleComments(toString(*converted));
		EXPECT_PRED2(notContainsSubString, code, "*");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, RefPtrDecl) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		//int i;
		//int* i_ptr = &i;
		//int& i_ref = i;

		//int j = *i_ptr;
		//int* j_ptr = &i_ref;
		//int& j_ref = *i_ptr;

		core::ProgramPtr program = builder.normalize(builder.parseProgram(R"(
			int<4> function IMP_main (){
				var ref<int<4>,f,f,plain> i;
				var ref<ptr<int<4>>,f,f,plain> i_ptr = ptr_from_ref(i);
				var ref<int<4>,f,f,cpp_ref> i_ref = i;
				var ref<int<4>,f,f,plain> j = *ptr_to_ref(*i_ptr);
				var ref<ptr<int<4>>,f,f,plain> j_ptr = ptr_from_ref(ref_cast(i_ref, type_lit(f), type_lit(f), type_lit(plain)));
				var ref<int<4>,f,f,cpp_ref> j_ref = ptr_to_ref(*i_ptr);
				return 0;
			}
		)"));

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check absence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "int32_t* j_ptr = (int32_t*)(&i_ref);");
		EXPECT_PRED2(containsSubString, code, "int32_t& j_ref = *i_ptr;");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, RefPtrFun) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		//int i;
		//int* i_ptr = &i;
		//int& i_ref = i;
		//take_ref(*i_ptr);
		//take_ptr(&i_ref);

		core::ProgramPtr program = builder.parseProgram(R"(
			def IMP_take_ref = (a: ref<int<4>,f,f,cpp_ref>) -> unit {};
			def IMP_take_ptr = (a: ptr<int<4>>) -> unit {};
			int<4> function IMP_main() {
				var ref<int<4>,f,f,plain> v0 = v0;
				var ref<ptr<int<4>>,f,f,plain> v1 = ptr_from_ref(v0);
				var ref<int<4>,f,f,cpp_ref> v2 = v0;

				IMP_take_ref(ref_kind_cast(ptr_to_ref(*v1), type_lit(cpp_ref)));
				IMP_take_ptr(ptr_from_ref(ref_cast(v2, type_lit(f), type_lit(f), type_lit(plain))));
				return 0;
			}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check absence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "IMP_take_ref(*v1);");
		EXPECT_PRED2(containsSubString, code, "IMP_take_ptr((int32_t*)(&v2));");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, RefPtrRet) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
			def gen_ref = function () -> ref<int<4>,f,f,cpp_ref> {
				return ptr_to_ref(*lit("gr" : ref<ptr<int<4>>,f,f,plain>));
			};
			def gen_ptr = function () -> ptr<int<4>> {
				return *lit("gf" : ref<ptr<int<4>>,f,f,plain>);
			};
			int<4> function IMP_main() {
				gen_ref();
				gen_ptr();
				return 0;
			}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check absence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "return *gr;");
		EXPECT_PRED2(containsSubString, code, "return gf;");

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

					lambda id = (a : int)->int {
						return a;
					}

					lambda sum = (a : int, b : int)->int {
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
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
		EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

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

		// make sure the definition of B is missing
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
		EXPECT_PRED2(containsSubString, code, "A(A const& p2) = default;");				// default copy constructor
		EXPECT_PRED2(containsSubString, code, "A(A&& p2) = default;");					// default move constructor
		EXPECT_PRED2(containsSubString, code, "~A() = default;");						// default destructor
		EXPECT_PRED2(containsSubString, code, "A& operator=(A const& p2) = default;");		// default assignment
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
		EXPECT_PRED2(containsSubString, code, "A(A const& p2) = default;");				// default copy constructor
		EXPECT_PRED2(containsSubString, code, "A(A&& p2) = default;");					// default move constructor
		EXPECT_PRED2(containsSubString, code, "~A() = default;");						// default destructor
		EXPECT_PRED2(containsSubString, code, "A& operator=(A const& p2) = default;");		// default assignment
		EXPECT_PRED2(containsSubString, code, "A& operator=(A&& p2) = default;");			// default move assignment

		EXPECT_PRED2(containsSubString, code, "A(int32_t p2);");						// user defined constructor declaration
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x) { }");					// user defined constructor definition


		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(CppSnippet, DefaultOperators) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

//		struct S {
//		int a;
//			~S() {
//				5;
//			}
//		};
//
//		void j(S& s) {
//			s = {5};
//		}

		// create a code fragment including some member functions
		core::ProgramPtr programTrivial = builder.parseProgram(R"(
				def struct IMP_S {
					val : int<4>;
				};

				def IMP_j = function (v57 : ref<IMP_S,f,f,cpp_ref>) -> ref<IMP_S,f,f,cpp_ref> {
					return ref_kind_cast(v57, type_lit(plain)).IMP__operator_assign_(ref_kind_cast(<ref<IMP_S,f,f,plain>>(ref_temp(type_lit(IMP_S))) {5}, type_lit(cpp_ref))) materialize ;
				};

				int<4> main() {
					var ref<IMP_S> a;
					IMP_j(ref_kind_cast(a, type_lit(cpp_ref)));
					return 0;
				}
		)");
		core::ProgramPtr programNonTrivial = builder.parseProgram(R"(
				def struct IMP_S {
					val : int<4>;
					dtor() { 5;}
				};

				def IMP_j = function (v57 : ref<IMP_S,f,f,cpp_ref>) -> ref<IMP_S,f,f,cpp_ref> {
					return ref_kind_cast(v57, type_lit(plain)).IMP__operator_assign_(ref_kind_cast(<ref<IMP_S,f,f,plain>>(ref_temp(type_lit(IMP_S))) {5}, type_lit(cpp_ref))) materialize ;
				};

				int<4> main() {
					var ref<IMP_S> a;
					IMP_j(ref_kind_cast(a, type_lit(cpp_ref)));
					return 0;
				}
		)");

		for(auto program : {programTrivial, programNonTrivial}) {
			ASSERT_TRUE(program);
			//std::cout << "Program: " << core::printer::PrettyPrinter(program, core::printer::PrettyPrinter::PRINT_DEFAULT_MEMBERS) << std::endl;
			EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

			// use sequential backend to convert into C++ code
			auto converted = sequential::SequentialBackend::getDefault()->convert(program);
			ASSERT_TRUE((bool)converted);
			//std::cout << "Converted: \n" << *converted << std::endl;

			// check presence of relevant code
			auto code = toString(*converted);
			EXPECT_PRED2(containsSubString, code, "IMP_S& IMP_j(IMP_S& v57)");
			EXPECT_PRED2(containsSubString, code, "return v57.operator=((IMP_S){5});");
			EXPECT_PRED2(containsSubString, code, "IMP_S& operator=(IMP_S const& p2) = default;");

			// try compiling the code fragment
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
			compiler.addFlag("-c"); // do not run the linker
			EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
		}
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
						<ref<int>>(this.x){12};
					}
					ctor( x : int ) {
						<ref<int>>(this.x){x};
					}
					ctor( x : int , y : int ) {
						<ref<int>>(this.x){x + y};
					}
				};

				def A::ctor f = ( x : int, y : int, z : int ) {
					<ref<int>>(this.x){x + y + z};
				};

				int main() {
					var ref<A> a;
					var ref<A> b = f(b, 1, 2, 3);
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
		EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

		EXPECT_PRED2(containsSubString, code, "A();");
		EXPECT_PRED2(containsSubString, code, "A::A() : x(12) { }");

		EXPECT_PRED2(containsSubString, code, "A(int32_t p2);");
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x) : x(x) { }");

		EXPECT_PRED2(containsSubString, code, "A(int32_t p2, int32_t p3);");
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x, int32_t y) : x(x + y) { }");

		EXPECT_PRED2(containsSubString, code, "A(int32_t p2, int32_t p3, int32_t p4);");
		EXPECT_PRED2(containsSubString, code, "A::A(int32_t x, int32_t y, int32_t z) : x(x + y + z) { }");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, InterceptedConstructors) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto program = builder.normalize(builder.parseProgram(R"(
			def struct S {};
			int<4> main() {
				var ref<S> s0 = lit("S::ctor" : S::())(ref_decl(type_lit(ref<S,f,f,plain>)));
				var ref<S> s1 = S::(ref_decl(type_lit(ref<S,f,f,plain>)));
				var ref<S> s2;
				return 0;
			}
		)"));

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "S s0;");
		EXPECT_PRED2(containsSubString, code, "S s1;");
		EXPECT_PRED2(containsSubString, code, "S s2;");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, ConstructorsInitConstructor) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(builder.parseProgram(R"(
			alias int = int<4>;

			def struct B { };
			def struct A {
				b : B;
				ctor () {
					B::(b);
				}
			};
			def struct C {
				ctor(a : int) {}
			};
			def struct D {
				c : C;
				ctor () {
					C::(c, 43);
				}
			};

			def struct E {
				a : A;
				b : B;
				c : C;
				d : D;
				ctor () {
					A::(a);
					B::(b);
					C::(c, 3);
					D::(d);
				}
			};

			int main() {
				var ref<A> a = A::(a);
				var ref<D> d = D::(d);
				var ref<E> e = E::(e);
				return 0;
			}
		)"));

		std::cout << " ------------- Parsing Done -----------------------\n";

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		std::cout << " -------------- Checks Done -----------------------\n";

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);
		//std::cout << *targetCode;

		std::cout << " ----------- Conversion Done ----------------------\n";

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "A::A() : b() { }");
		EXPECT_PRED2(containsSubString, code, "D::D() : c(43) { }");
		EXPECT_PRED2(notContainsSubString, utils::removeCppStyleComments(code), "*");
		EXPECT_PRED2(notContainsSubString, code, "new ");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, ConstructorsChained) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(builder.parseProgram(R"(
			decl ctor:ChainedConstructor::(int<4>);
			def struct ChainedConstructor {
				a : int<4>;
				b : real<4>;
				ctor function () {
					ChainedConstructor::(this, 5);
				}
				ctor function (v1 : ref<int<4>,f,f,plain>) {
					<ref<int<4>,f,f,plain>>((this).a) {*v1};
					<ref<real<4>,f,f,plain>>((this).b) {2.0E+0f};
				}
			};

			int<4> main() {
				var ref<ChainedConstructor,f,f,plain> v0 = ChainedConstructor::(v0);
				return 0;
			}
		)"));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);
		//std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "ChainedConstructor::ChainedConstructor() : ChainedConstructor(5) { }");
		EXPECT_PRED2(notContainsSubString, utils::removeCppStyleComments(code), "*");
		EXPECT_PRED2(notContainsSubString, code, "new ");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, ConstructorsBase) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.normalize(builder.parseProgram(R"(
			def struct Base {
				a : int<4>;
				ctor function (v1 : ref<int<4>,f,f,plain>) {
					<ref<int<4>,f,f,plain>>((this).a) {*v1};
				}
			};
			def struct Derived: [ public Base ] {
				ctor function () {
					Base::(ref_parent_cast(this, type_lit(Base)), 5);
				}
			};

			int<4> main() {
				var ref<Derived,f,f,plain> v0 = Derived::(v0);
				return 0;
			}
		)"));

		ASSERT_TRUE(res);
		EXPECT_TRUE(core::checks::check(res).empty()) << core::checks::check(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);
		//std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "Derived::Derived() : Base(5) { }");
		EXPECT_PRED2(notContainsSubString, utils::removeCppStyleComments(code), "*");
		EXPECT_PRED2(notContainsSubString, code, "new ");

		// check whether code is compiling
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, Globals) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
			def struct A {
				x : int<4>;
				ctor function (v1 : ref<int<4>,f,f,plain>) {
					x = *v1;
				}
			};
			def struct Trivial {};
			int<4> main() {
				Trivial::(lit("x" : ref<Trivial,f,f,plain>));
				A::(lit("a" : ref<A,f,f,plain>), 5);
				lit("x" : ref<Trivial,f,f,plain>);
				lit("a" : ref<A,f,f,plain>);
				return 0;
			}
		)");

		ASSERT_TRUE(program);
		//std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(notContainsSubString, code, "new"); // no heap mem allocation in this program
		EXPECT_PRED2(containsSubString, code, "A a(5);");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(CppSnippet, Destructor) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;

				def struct A {
					x : int;
					dtor() = default;
				};

				def struct B {
					x : int;
					dtor() { x = 12; }
				};

				def struct C {
					x : int;
					dtor virtual () { x = 12; }
				};

				int main() {
					var ref<A> a;
					var ref<B> b;
					var ref<C> c;
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

		EXPECT_PRED2(containsSubString, code, "struct A");		    // struct definition
		EXPECT_PRED2(containsSubString, code, "A a;");			    // variable definition
		EXPECT_PRED2(notContainsSubString, code, "~A() {");			// don't implement destructor for A

		EXPECT_PRED2(containsSubString, code, "struct B");			// struct definition
		EXPECT_PRED2(containsSubString, code, "B b;");				// variable definition
		EXPECT_PRED2(containsSubString, code, "~B();");				// destructor for B
		EXPECT_FALSE(containsSubString(code, "virtual ~B();"));		// not virtual

		EXPECT_PRED2(containsSubString, code, "struct C");			// struct definition
		EXPECT_PRED2(containsSubString, code, "C c;");				// variable definition
		EXPECT_PRED2(containsSubString, code, "virtual ~C();");		// destructor for B


		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}


	TEST(CppSnippet, MemberFunctionsExausted) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;

				def struct A {
					x : int;

					lambda f1 = () -> unit { 10; }
					const lambda f2 = () -> unit { 11; }
					const volatile lambda f3 = () -> unit { 12; }
					volatile lambda f4 = () -> unit { 13; }

					virtual lambda f5 = () -> unit { 14; }
					virtual const lambda f6 = () -> unit { 15; }
					virtual const volatile lambda f7 = () -> unit { 16; }
					virtual volatile lambda f8 = () -> unit { 17; }

				};

/*
				def f = A::( x : int, y : int, z : int )->unit {
					this->x = x + y + z;
				};
*/

				int main() {
					var ref<A> a;
					// a.f9();
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
		EXPECT_PRED2(containsSubString, code, "void f1();");
		EXPECT_PRED2(containsSubString, code, "void A::f1() {");

		EXPECT_PRED2(containsSubString, code, "void f2() const;");
		EXPECT_PRED2(containsSubString, code, "void A::f2() const {");

		EXPECT_PRED2(containsSubString, code, "void f3() const volatile;");
		EXPECT_PRED2(containsSubString, code, "void A::f3() const volatile {");

		EXPECT_PRED2(containsSubString, code, "void f4() volatile;");
		EXPECT_PRED2(containsSubString, code, "void A::f4() volatile {");


		EXPECT_PRED2(containsSubString, code, "virtual void f5();");
		EXPECT_PRED2(containsSubString, code, "void A::f5() {");

		EXPECT_PRED2(containsSubString, code, "virtual void f6() const;");
		EXPECT_PRED2(containsSubString, code, "void A::f6() const {");

		EXPECT_PRED2(containsSubString, code, "virtual void f7() const volatile;");
		EXPECT_PRED2(containsSubString, code, "void A::f7() const volatile {");

		EXPECT_PRED2(containsSubString, code, "virtual void f8() volatile;");
		EXPECT_PRED2(containsSubString, code, "void A::f8() volatile {");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, MemberFunctionsRecursive) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;

				decl struct A;
				decl r : A::()->unit;
				decl f : A::()->unit;
				decl g : A::()->unit;

				def struct A {
					x : int;

					lambda r = () -> unit { r(); }
					lambda f = () -> unit { g(); }
					lambda g = () -> unit { f(); }

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

		// check definition of the recursive functions
		EXPECT_PRED2(containsSubString, code, "void r();");
		EXPECT_PRED2(containsSubString, code, "void A::r() {");

		EXPECT_PRED2(containsSubString, code, "void f();");
		EXPECT_PRED2(containsSubString, code, "void A::f() {");

		EXPECT_PRED2(containsSubString, code, "void g();");
		EXPECT_PRED2(containsSubString, code, "void A::g() {");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, PureVirtualMemberFunctions) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		// create a code fragment including some member functions
		core::ProgramPtr program = builder.parseProgram(R"(
				alias int = int<4>;

				def struct A {
					x : int;

					pure virtual f1 : () -> unit
					pure virtual const f2 : () -> unit
					pure virtual const volatile f3 : () -> unit
					pure virtual volatile f4 : () -> unit

				};

				def struct B : [public A] {

					virtual lambda f1 = () -> unit { }
					virtual const lambda f2 = () -> unit { }
					virtual const volatile lambda f3 = () -> unit { }
					virtual volatile lambda f4 = () -> unit { }

				};

				int main() {
					var ref<B> a;
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
		EXPECT_PRED2(containsSubString, code, "B a;");			// variable definition

		EXPECT_PRED2(containsSubString, code, "virtual void f1() =0;");
		EXPECT_PRED2(containsSubString, code, "virtual void f2() const =0;");
		EXPECT_PRED2(containsSubString, code, "virtual void f3() const volatile =0;");
		EXPECT_PRED2(containsSubString, code, "virtual void f4() volatile =0;");

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

					lambda reset = ()->unit {
						value = 0;
					}

					lambda inc = ()->int {
						value = value + 1;
						return *value;
					}

					lambda dec = ()->int {
						value = value - 1;
						return *value;
					}

					lambda get = ()->int {
						return *value;
					}

					lambda set = (x : int)->unit {
						value = x;
					}

					lambda p = ()-> unit {
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

	TEST(CppSnippet, Enum) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"( using "ext.enum";
				int<4> function IMP_main () {
					var ref<(type<enum_def<IMP_Bla,uint<8>,enum_entry<IMP_Bla_colon__colon_A,0>>>, uint<8>),f,f,plain> v0 = (type_lit(enum_def<IMP_Bla,uint<8>,enum_entry<IMP_Bla_colon__colon_A,0>>), 0ul);
					enum_to_int(*v0)==num_cast(5, type_lit(uint<8>));
					return 0;
				}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check presence of relevant code
		auto code = toString(*converted);
		EXPECT_PRED2(containsSubString, code, "enum Bla : uint64_t { BlaA=0 }");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, ImplicitInitConstructorSemantics) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
			def struct IMP_A {
				i : int<4>;
				lambda IMP_f = () -> real<4> { return lit("1.0E+0":real<4>); }
			};
			int<4> main() {
				var ref<IMP_A,f,f,plain> v0 = IMP_A::(v0);
				var ref<IMP_A,f,f,plain> v1 = ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref));
				var ref<IMP_A,f,f,plain> v2 = ref_cast(IMP_A::(ref_temp(type_lit(IMP_A)), ref_kind_cast(v0, type_lit(cpp_ref))), type_lit(f), type_lit(f), type_lit(cpp_rref));
				return 0;
			}
		)");
		// source:
		// A a;
		// A b = a;
		// A c = A(a);

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check C++ code for absence of any pointers/derefs
		auto codeString = toString(*converted);
		codeString = insieme::utils::removeCppStyleComments(codeString);
		EXPECT_PRED2(notContainsSubString, codeString, "*");
		EXPECT_PRED2(containsSubString, codeString, "IMP_A(IMP_A const& p2) = default");
		EXPECT_PRED2(containsSubString, codeString, "IMP_A() = default");
		EXPECT_PRED2(containsSubString, codeString, "IMP_A(IMP_A&& p2) = default");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, ImplicitCallArgConstructorSemantics) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::ProgramPtr program = builder.parseProgram(R"(
			def struct IMP_A {
				i : int<4>;
				lambda IMP_f = () -> real<4> { return lit("1.0E+0":real<4>); }
			};
			def consume = (a : IMP_A) -> unit { };
			int<4> main() {
				consume(ref_cast(IMP_A::(ref_temp(type_lit(IMP_A))), type_lit(t), type_lit(f), type_lit(cpp_ref)));
				consume(ref_cast(IMP_A::(ref_temp(type_lit(IMP_A))), type_lit(f), type_lit(f), type_lit(cpp_rref)));
				return 0;
			}
		)");

		ASSERT_TRUE(program);
		// std::cout << "Program: " << dumpColor(program) << std::endl;
		EXPECT_TRUE(core::checks::check(program).empty()) << core::checks::check(program);

		// use sequential backend to convert into C++ code
		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		ASSERT_TRUE((bool)converted);
		//std::cout << "Converted: \n" << *converted << std::endl;

		// check C++ code for absence of any pointers/derefs
		auto codeString = toString(*converted);
		codeString = insieme::utils::removeCppStyleComments(codeString);
		EXPECT_PRED2(notContainsSubString, codeString, "*");
		EXPECT_PRED2(containsSubString, codeString, "IMP_A(IMP_A const& p2) = default");
		EXPECT_PRED2(containsSubString, codeString, "IMP_A() = default");
		EXPECT_PRED2(containsSubString, codeString, "IMP_A(IMP_A&& p2) = default");

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

	TEST(CppSnippet, ConstructorCall) {
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
						var ref<A> a1 = A::(a1);
						var ref<A> a2 = A::(a2, 1);

						// on heap
						var ref<A> a1r = A::(ref_new(type_lit(A)));
						var ref<A> a2r = A::(ref_new(type_lit(A)), 1);

						// on heap
						var ptr<A> a3 = ptr_from_ref(A::(ref_new(type_lit(A))));
						var ptr<A> a4 = ptr_from_ref(A::(ref_new(type_lit(A)), 1));

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
		EXPECT_PRED2(containsSubString, code, "A* a1r = new A();");
		EXPECT_PRED2(containsSubString, code, "A* a2r = new A(1);");
		EXPECT_PRED2(containsSubString, code, "A* a3 = new A();");
		EXPECT_PRED2(containsSubString, code, "A* a4 = new A(1);");
		EXPECT_PRED2(containsSubString, code, "new (&a5) A();");
		EXPECT_PRED2(containsSubString, code, "new (&a6) A(1);");

		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, NewDelete) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		auto res = builder.parseProgram(R"(
			def struct IMP_SimplestConstructor { };
			def struct IMP_SlightlyLessSimpleConstructor {
				i : int<4>;
				ctor function (v1 : ref<int<4>,f,f,plain>) {
					(this).i = *v1;
				}
			};
			int<4> main() {

				var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_ref(ref_new(type_lit(int<4>)));
				ref_delete(ptr_to_ref(*i));
				var ref<ptr<int<4>,f,f>,f,f,plain> j = ptr_from_ref(ref_new_init(42));
				ref_delete(ptr_to_ref(*j));

				var ref<ptr<IMP_SimplestConstructor>,f,f,plain> o1 = ptr_from_ref(IMP_SimplestConstructor::(ref_new(type_lit(IMP_SimplestConstructor))));
				ref_delete(IMP_SimplestConstructor::~(ptr_to_ref(*o1)));

				var ref<ptr<IMP_SlightlyLessSimpleConstructor>,f,f,plain> o2 = ptr_from_ref(IMP_SlightlyLessSimpleConstructor::(ref_new(type_lit(IMP_SlightlyLessSimpleConstructor)), 42));
				ref_delete(IMP_SlightlyLessSimpleConstructor::~(ptr_to_ref(*o2)));

				return 0;
			}
		)");

		ASSERT_TRUE(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		//std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "int32_t* i = (int32_t*)malloc(sizeof(int32_t));");
		EXPECT_PRED2(containsSubString, code, "free(i);");
		EXPECT_PRED2(containsSubString, code, "int32_t* j = _ref_new___insieme_type_2(42);");
		EXPECT_PRED2(containsSubString, code, "free(j);");
		EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* o1 = new IMP_SimplestConstructor();");
		EXPECT_PRED2(containsSubString, code, "delete o1;");
		EXPECT_PRED2(containsSubString, code, "IMP_SlightlyLessSimpleConstructor* o2 = new IMP_SlightlyLessSimpleConstructor(42);");
		EXPECT_PRED2(containsSubString, code, "delete o2;");

		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

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
						var ref<B> b = B::(b, 1, 2);

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
						var ref<B> b = B::(b, 1, 2, 3);

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
						var ref<A> b = A::(b, 1, 2);

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
