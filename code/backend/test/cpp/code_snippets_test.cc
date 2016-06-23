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
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {

	// global because it increases the testing speed by ~ factor 2
	core::NodeManager mgr;
	core::IRBuilder builder(mgr);

	TEST(CppSnippet, CppReference) {
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
		core::ProgramPtr program = builder.parseProgram(R"(
				def IMP_test = function (v1 : ref<int<4>,f,f,cpp_ref>) -> unit { };
				int<4> function IMP_main () {
					var ref<int<4>,f,f,plain> bla = 2;
					var ref<int<4>,f,f,cpp_ref> alb = bla;
					IMP_test(alb);
					bla + alb;
					alb = 8;
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
		auto code = utils::removeCppStyleComments(toString(*converted));
		EXPECT_PRED2(containsSubString, code, "IMP_test(alb)");
		EXPECT_PRED2(notContainsSubString, code, "*");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, ReferenceVariableMethodUse) {
		core::ProgramPtr program = builder.parseProgram(R"(
				def struct A {
					lambda foo = () -> unit {}
				};

				def struct B {
					v : int<4>;
				};

				def fun = (b : ref<B,t,f,cpp_ref>) -> unit {
					auto a = b.v;
				};

				int<4> function IMP_main () {
					var ref<A> a;
					var ref<A,f,f,cpp_ref> a2 = a;
					a2.foo();
					var ref<B> b;
					fun(ref_kind_cast(b,type_lit(cpp_ref)));
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
		auto code = utils::removeCppStyleComments(toString(*converted));
		EXPECT_PRED2(notContainsSubString, code, "*");
		EXPECT_PRED2(containsSubString, code, "int32_t const& a = b.v;");

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, RefPtrDecl) {
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

		// try compiling the code fragment
		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*converted, compiler));
	}

	TEST(CppSnippet, TheEssentialSix) {
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

				def A:: f = ctor ( x : int, y : int, z : int ) {
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

	TEST(CppSnippet, ConstructorCall) {
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

	TEST(CppSnippet, NewDeleteArrayFixed) {
		auto res = builder.parseProgram(R"(
			def struct IMP_SimplestConstructor { };
			int<4> main() {
				var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_array(ref_new(type_lit(array<int<4>,50>)));
				ref_delete(ptr_to_array(*i));
				var ref<ptr<int<4>>,f,f,plain> j = ptr_from_array(<ref<array<int<4>,50>,f,f,plain>>(ref_new(type_lit(array<int<4>,50>))) {1, 2, 3});
				ref_delete(ptr_to_array(*j));
				var ref<ptr<array<int<4>,3>>,f,f,plain> k = ptr_from_array(ref_new(type_lit(array<array<int<4>,3>,50>)));
				ref_delete(ptr_to_array(*k));

				var ref<ptr<IMP_SimplestConstructor>,f,f,plain> o1 = ptr_from_array(<ref<array<IMP_SimplestConstructor,3>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,3>))) {});
				ref_delete(ptr_to_array(*o1));
				var ref<IMP_SimplestConstructor,f,f,plain> v0 = IMP_SimplestConstructor::(ref_decl(type_lit(ref<IMP_SimplestConstructor,f,f,plain>)));
				var ref<ptr<IMP_SimplestConstructor>,f,f,plain> o2 = ptr_from_array(<ref<array<IMP_SimplestConstructor,3>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,3>))) {ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref))});
				ref_delete(ptr_to_array(*o2));
				return 0;
			}
		)");

		ASSERT_TRUE(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		//std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "int32_t* i = new int32_t[50];");
		EXPECT_PRED2(containsSubString, code, "delete[] i;");
		EXPECT_PRED2(containsSubString, code, "int32_t* j = new int32_t[50]{1, 2, 3};");
		EXPECT_PRED2(containsSubString, code, "delete[] j;");
		EXPECT_PRED2(containsSubString, code, "__insieme_type_2* k = new __insieme_type_2[50];");
		EXPECT_PRED2(containsSubString, code, "delete[] k;");

		EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* o1 = new IMP_SimplestConstructor[3];");
		EXPECT_PRED2(containsSubString, code, "delete[] o1;");
		EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* o2 = new IMP_SimplestConstructor[3]{(IMP_SimplestConstructor const&)v0, (IMP_SimplestConstructor const&)v0};");
		EXPECT_PRED2(containsSubString, code, "delete[] o2;");

		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, NewDeleteArrayVariable) {
		auto res = builder.parseProgram(R"(
			def struct IMP_SimplestConstructor { };
			def new_arr_fun_0 = function (v0 : ref<uint<inf>,f,f,plain>) -> ptr<int<4>> {
				var uint<inf> bla = *v0;
				return ptr_from_array(ref_new(type_lit(array<int<4>,#bla>)));
			};
			def new_arr_fun_1 = function (v0 : ref<uint<inf>,f,f,plain>, v1 : ref<int<4>,t,f,cpp_ref>, v2 : ref<int<4>,t,f,cpp_ref>, v3 : ref<int<4>,t,f,cpp_ref>) -> ptr<int<4>> {
				var uint<inf> v4 = *v0;
				return ptr_from_array(<ref<array<int<4>,#v4>,f,f,plain>>(ref_new(type_lit(array<int<4>,#v4>))) {*v1, *v2, *v3});
			};
			def new_arr_fun_2 = function (v0 : ref<uint<inf>,f,f,plain>) -> ptr<IMP_SimplestConstructor> {
				var uint<inf> v1 = *v0;
				return ptr_from_array(<ref<array<IMP_SimplestConstructor,#v1>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,#v1>))) {});
			};
			def new_arr_fun_3 = function (v0 : ref<uint<inf>,f,f,plain>, v1 : ref<IMP_SimplestConstructor,t,f,cpp_ref>, v2 : ref<IMP_SimplestConstructor,t,f,cpp_ref>) -> ptr<IMP_SimplestConstructor> {
				var uint<inf> v3 = *v0;
				return ptr_from_array(<ref<array<IMP_SimplestConstructor,#v3>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,#v3>))) {*v1, *v2});
			};
			int<4> main() {
				{
					var ref<int<4>,f,f,plain> v0 = 50;
					var ref<ptr<int<4>>,f,f,plain> v1 = new_arr_fun_0(num_cast(*v0+5, type_lit(uint<inf>)));
					ref_delete(ptr_to_array(*v1));
				}
				{
					var ref<int<4>,f,f,plain> v0 = 50;
					var ref<ptr<int<4>>,f,f,plain> v1 = new_arr_fun_1(num_cast(*v0+5, type_lit(uint<inf>)), 1, 2, 3);
					ref_delete(ptr_to_array(*v1));
				}
				{
					var ref<int<4>,f,f,plain> v0 = 30;
					var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v1 = new_arr_fun_2(num_cast(*v0+5, type_lit(uint<inf>)));
					ref_delete(ptr_to_array(*v1));
				}
				{
					var ref<int<4>,f,f,plain> v0 = 50;
					var ref<IMP_SimplestConstructor,f,f,plain> v1 = IMP_SimplestConstructor::(ref_decl(type_lit(ref<IMP_SimplestConstructor,f,f,plain>)));
					var ref<IMP_SimplestConstructor,f,f,plain> v2 = IMP_SimplestConstructor::(ref_decl(type_lit(ref<IMP_SimplestConstructor,f,f,plain>)));
					var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v3 = new_arr_fun_3(num_cast(*v0+5, type_lit(uint<inf>)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v2, type_lit(t), type_lit(f), type_lit(cpp_ref)));
					ref_delete(ptr_to_array(*v3));
				}
				return 0;
			}
		)");

		ASSERT_TRUE(res);

		auto targetCode = sequential::SequentialBackend::getDefault()->convert(res);
		ASSERT_TRUE((bool)targetCode);

		//std::cout << *targetCode;

		// check generated code
		auto code = toString(*targetCode);
		EXPECT_PRED2(containsSubString, code, "int32_t* v1 = new_arr_fun_0((uint64_t)(v0 + 5));");
		EXPECT_PRED2(containsSubString, code, "return new int32_t[v1];");
		EXPECT_PRED2(containsSubString, code, "int32_t* var_3 = new_arr_fun_1((uint64_t)(var_2 + 5), 1, 2, 3);");
		EXPECT_PRED2(containsSubString, code, "return new int32_t[v4]{v1, v2, v3};");
		EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* var_5 = new_arr_fun_2((uint64_t)(var_4 + 5))");
		EXPECT_PRED2(containsSubString, code, "return new IMP_SimplestConstructor[v1];");
		EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* v3 = new_arr_fun_3((uint64_t)(var_6 + 5), (IMP_SimplestConstructor const&)var_7, (IMP_SimplestConstructor const&)v2);");
		EXPECT_PRED2(containsSubString, code, "return new IMP_SimplestConstructor[v3]{v1, v2};");

		utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
		compiler.addFlag("-c"); // do not run the linker
		EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	}

	TEST(CppSnippet, DISABLED_InitializerList2) {
		// something including a super-constructor call
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

	TEST(CppSnippet, DISABLED_StaticVariableConst) {
		// something including a non-parameter!
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
