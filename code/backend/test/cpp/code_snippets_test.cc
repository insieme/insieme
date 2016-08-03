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

#include "../code_snippets_test_utils.h"

namespace insieme {
namespace backend {

	// global because it increases the testing speed by ~ factor 2
	core::NodeManager mgr;
	core::IRBuilder builder(mgr);

	TEST(CppSnippet, CppReference) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			;
		})
	}

	TEST(CppSnippet, CppRValueReference) {
		DO_TEST(R"(
			alias int = int<4>;

			def g = () -> int { return 12; };

			def f = ( y : cpp_rref<int,f,f>, z : cpp_rref<int,t,f>, w : cpp_rref<int,t,t> ) -> int {
				return y + z + w;
			};

			int main() {
				f(g(),g(),g());
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			;
		})
	}

	TEST(CppSnippet, ReferenceParameter) {
		DO_TEST(R"(
			def IMP_test = function (v1 : ref<int<4>,f,f,cpp_ref>) -> unit { };
			int<4> function IMP_main (){
				var ref<int<4>,f,f,plain> bla = 2;
				IMP_test(ref_kind_cast(bla, type_lit(cpp_ref)));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
				EXPECT_PRED2(notContainsSubString, code, "&bla");
		})
	}

	TEST(CppSnippet, ReferenceMember) {
		DO_TEST(R"(
			def struct IMP_RefMember {
				mem : ref<int<4>,t,f,cpp_ref>;
				ctor function () {
					<ref<int<4>,t,f,cpp_ref>>(*(this).mem) {0};
					*(this).mem;
				}
			};
			def struct IMP_A {
				x : int<4>;
				y : ref<int<4>,f,f,cpp_ref>;
			};
			int<4> main() {
				var ref<IMP_RefMember,f,f,plain> refmem = IMP_RefMember::(ref_decl(type_lit(ref<IMP_RefMember,f,f,plain>)));

				var ref<int<4>,f,f,plain> main_int = 0;
				var ref<IMP_A,t,f,plain> a_instance = <ref<IMP_A,f,f,plain>>(ref_decl(type_lit(ref<IMP_A,t,f,plain>))) {1, main_int};
				a_instance.x;
				*a_instance.y;

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "const IMP_A a_instance = {1, main_int};");
			EXPECT_PRED2(containsSubString, code, "a_instance.x;");
			EXPECT_PRED2(containsSubString, code, "a_instance.y;");
			EXPECT_PRED2(containsSubString, code, "IMP_RefMember::IMP_RefMember() : mem(0)");
			EXPECT_PRED2(containsSubString, code, "(*this).mem;");
		})
	}

	TEST(CppSnippet, ReferenceMemberPlainMemberInit) {
		DO_TEST(R"(
			def struct IMP_D {};
			def struct IMP_E {
				x : int<4>;
				dD : IMP_D;
				dr : ref<IMP_D,f,f,cpp_ref>;
				cdr : ref<IMP_D,t,f,cpp_ref>;
				ctor function () {
					IMP_D::((this).dD);
					<ref<IMP_D,f,f,cpp_ref>>(*(this).dr) {(this).dD};
					<ref<IMP_D,t,f,cpp_ref>>(*(this).cdr) {*(this).dr};
				}
			};
			// Inspire Program
			int<4> function IMP_main (){
				var ref<IMP_E,f,f,plain> v90 = IMP_E::(ref_decl(type_lit(ref<IMP_E,f,f,plain>)));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "IMP_E::IMP_E() : dD(), dr((*this).dD), cdr((*this).dr) { }");
		})
	}

	TEST(CppSnippet, RefArrayReturn) {
		DO_TEST(R"(
			def IMP_test = function () -> ref<array<int<4>,1>,f,f,cpp_ref> {
				auto x = <ref<array<int<4>,1>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,1>,f,f,plain>))){};
				return x;
			};
			int<4> main() {
				IMP_test();
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			;
		})
	}

	TEST(CppSnippet, RefArrayParameter) {
		DO_TEST(R"(
			def IMP_takeT_char__lbracket_5_rbracket__returns_void = function (v0 : ref<array<char,5>,t,f,cpp_ref>) -> unit { };
			def IMP_takeT_int__lbracket_3_rbracket__returns_void = function (v0 : ref<array<int<4>,3>,t,f,cpp_ref>) -> unit { };
			int<4> main() {
				IMP_takeT_char__lbracket_5_rbracket__returns_void(ref_kind_cast(lit(""test"" : ref<array<char,5>,t,f,plain>), type_lit(cpp_ref)));
				var ref<array<int<4>,3>,f,f,plain> v0 = <ref<array<int<4>,3>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3>,f,f,plain>))) {1, 2, 3};
				IMP_takeT_int__lbracket_3_rbracket__returns_void(ref_kind_cast(v0, type_lit(cpp_ref)));
				return 0;
			}
		)", true, utils::compiler::Compiler::getDefaultCppCompiler(), {
			;
		})
	}

	TEST(CppSnippet, ReferenceVariableDeclaration) {
		DO_TEST(R"(
			int<4> function IMP_main () {
				var ref<int<4>,f,f,plain> bla = 2;
				var ref<int<4>,f,f,cpp_ref> alb = bla;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "int32_t& alb = bla;");
		})
	}

	TEST(CppSnippet, ReferenceVariableUse) {
		DO_TEST(R"(
			def IMP_test = function (v1 : ref<int<4>,f,f,cpp_ref>) -> unit { };
			int<4> function IMP_main () {
				var ref<int<4>,f,f,plain> bla = 2;
				var ref<int<4>,f,f,cpp_ref> alb = bla;
				IMP_test(alb);
				bla + alb;
				alb = 8;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "IMP_test(alb)");
			EXPECT_PRED2(notContainsSubString, code, "*");
		})
	}

	TEST(CppSnippet, ReferenceLazyUse) {
		DO_TEST(R"(
			def struct S {
				mem : int<4>;
			};

			int<4> main() {
				var ref<S> y;
				var ref<S,t,f,cpp_ref> x = y;
				x.mem == 0 && x.mem == 1;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "x.mem == 0 && x.mem == 1;");
		})
	}

	TEST(CppSnippet, ReferenceVariableMethodUse) {
		DO_TEST(R"(
			def struct A {
				lambda foo = () -> unit {}
			};

			def struct B {
				v : int<4>;
			};

			def fun = (b : ref<B,t,f,cpp_ref>) -> unit {
				var ref<int<4>,t,f,cpp_ref> a = b.v;
			};

			int<4> function IMP_main () {
				var ref<A> a;
				var ref<A,f,f,cpp_ref> a2 = a;
				a2.foo();
				var ref<B> b;
				fun(ref_kind_cast(b,type_lit(cpp_ref)));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(notContainsSubString, code, "*");
			EXPECT_PRED2(containsSubString, code, "int32_t const& a = b.v;");
		})
	}

	TEST(CppSnippet, ReferenceConstructorCall) {
		DO_TEST(R"(
			def struct IMP_S {
			};

			int<4> function IMP_main () {
				var ref<IMP_S,t,f,cpp_ref> v324 = IMP_S::(ref_cast(ref_decl(type_lit(ref<IMP_S,t,f,cpp_ref>)), type_lit(f), type_lit(f), type_lit(plain)));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "v324 = IMP_S()");
			EXPECT_PRED2(notContainsSubString, code, "new");
			EXPECT_PRED2(notContainsSubString, code, "alloca(");
		})
	}

	TEST(CppSnippet, RefPtrDecl) {
		//int i;
		//int* i_ptr = &i;
		//int& i_ref = i;

		//int j = *i_ptr;
		//int* j_ptr = &i_ref;
		//int& j_ref = *i_ptr;

		DO_TEST(R"(
			int<4> function IMP_main (){
				var ref<int<4>,f,f,plain> i;
				var ref<ptr<int<4>>,f,f,plain> i_ptr = ptr_from_ref(i);
				var ref<int<4>,f,f,cpp_ref> i_ref = i;
				var ref<int<4>,f,f,plain> j = *ptr_to_ref(*i_ptr);
				var ref<ptr<int<4>>,f,f,plain> j_ptr = ptr_from_ref(ref_cast(i_ref, type_lit(f), type_lit(f), type_lit(plain)));
				var ref<int<4>,f,f,cpp_ref> j_ref = ptr_to_ref(*i_ptr);
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "int32_t* j_ptr = (int32_t*)(&i_ref);");
			EXPECT_PRED2(containsSubString, code, "int32_t& j_ref = *i_ptr;");
		})
	}

	TEST(CppSnippet, RefPtrFun) {
		//int i;
		//int* i_ptr = &i;
		//int& i_ref = i;
		//take_ref(*i_ptr);
		//take_ptr(&i_ref);

		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "IMP_take_ref(*v1);");
			EXPECT_PRED2(containsSubString, code, "IMP_take_ptr((int32_t*)(&v2));");
		})
	}

	TEST(CppSnippet, RefPtrRet) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "return *gr;");
			EXPECT_PRED2(containsSubString, code, "return gf;");
		})
	}

	TEST(CppSnippet, MemberFunctions) {
		DO_TEST(R"(
			alias int = int<4>;

			def struct Math {
				lambda id = (a : int)->int {
					return a;
				}

				lambda sum = (a : int, b : int)->int {
					return a + b;
				}
			};

			def struct IMP_B {
				function IMP_m = () -> unit { }
			};
			def IMP_makeB = function () -> IMP_B {
				return <ref<IMP_B,f,f,plain>>(ref_decl(type_lit(ref<IMP_B,f,f,plain>))) {};
			};

			int main() {
				var ref<Math> m;

				print("%d\n", m->id(12));
				print("%d\n", m->sum(12, 14));
				IMP_makeB() materialize .IMP_m();
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "IMP_makeB().m()");
		})
	}

	TEST(CppSnippet, TrivialClass) {
		DO_TEST(R"(
			alias int = int<4>;

			def struct A { };

			int main() {
				var ref<A> a;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
			EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition
		})
	}

	TEST(CppSnippet, RecursiveClass) {
		DO_TEST(R"(
			alias int = int<4>;

			def struct A { x : ref<A>; };

			int main() {
				var ref<A> a;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
			EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition
		})
	}

	TEST(CppSnippet, MutualRecursiveClass) {
		DO_TEST(R"(
			alias int = int<4>;

			decl struct A;
			decl struct B;

			def struct A { value : int<4>; next : ref<B>; lambda getNext = () -> ref<B> { var ref<B> x; return x; } };
			def struct B { value : int<4>; next : ref<A>; lambda getNext = () -> ref<A> { var ref<A> x; return x; } };

			int main() {
				var ref<A> a;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
			EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition
		})
	}

	TEST(CppSnippet, TheEssentialSix) {
		DO_TEST(R"(
			alias int = int<4>;

			def struct A {
					ctor( ) {}
					ctor( x : int ) {}
			};

			int main() {
				var ref<A> a;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
			EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

			// check definition of six essential functions
			EXPECT_PRED2(containsSubString, code, "A() = default;");                       // default constructor
			EXPECT_PRED2(containsSubString, code, "A(A const& p2) = default;");            // default copy constructor
			EXPECT_PRED2(containsSubString, code, "A(A&& p2) = default;");                 // default move constructor
			EXPECT_PRED2(containsSubString, code, "~A() = default;");                      // default destructor
			EXPECT_PRED2(containsSubString, code, "A& operator=(A const& p2) = default;"); // default assignment
			EXPECT_PRED2(containsSubString, code, "A& operator=(A&& p2) = default;");      // default move assignment

			EXPECT_PRED2(containsSubString, code, "A(int32_t p2);");                       // user defined constructor declaration
			EXPECT_PRED2(containsSubString, code, "A::A(int32_t x) { }");                  // user defined constructor definition
		})
	}

	TEST(CppSnippet, TheEssentialSixMutualRecursion) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
			EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

			// check definition of six essential functions
			EXPECT_PRED2(containsSubString, code, "A() = default;");                       // default constructor
			EXPECT_PRED2(containsSubString, code, "A(A const& p2) = default;");            // default copy constructor
			EXPECT_PRED2(containsSubString, code, "A(A&& p2) = default;");                 // default move constructor
			EXPECT_PRED2(containsSubString, code, "~A() = default;");                      // default destructor
			EXPECT_PRED2(containsSubString, code, "A& operator=(A const& p2) = default;"); // default assignment
			EXPECT_PRED2(containsSubString, code, "A& operator=(A&& p2) = default;");      // default move assignment

			EXPECT_PRED2(containsSubString, code, "A(int32_t p2);");                       // user defined constructor declaration
			EXPECT_PRED2(containsSubString, code, "A::A(int32_t x) { }");                  // user defined constructor definition
		})
	}

	TEST(CppSnippet, DefaultOperatorsTrivial) {
//		struct S {
//		int a;
//		};
//
//		void j(S& s) {
//			s = {5};
//		}

		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "IMP_S& IMP_j(IMP_S& v57)");
			EXPECT_PRED2(containsSubString, code, "return v57.operator=(INS_INIT(IMP_S){5});");
			EXPECT_PRED2(containsSubString, code, "IMP_S& operator=(IMP_S const& p2) = default;");
		})
	}

	TEST(CppSnippet, DefaultOperatorsNonTrivial) {
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

		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "IMP_S& IMP_j(IMP_S& v57)");
			EXPECT_PRED2(containsSubString, code, "return v57.operator=(INS_INIT(IMP_S){5});");
			EXPECT_PRED2(containsSubString, code, "IMP_S& operator=(IMP_S const& p2) = default;");
		})
	}

	TEST(CppSnippet, ConversionOperator) {
		DO_TEST(R"(
			def struct IMP_ConversionOperator {
				x : int<4>;
				function IMP__conversion_operator_int = () -> int<4> {
					return *(this).x;
				}
				function IMP__conversion_operator_int_space__star_ = () -> ptr<int<4>> {
					return ptr_from_ref((this).x);
				}
			};
			int<4> main() {
				var ref<IMP_ConversionOperator,f,f,plain> cv = IMP_ConversionOperator::(ref_decl(type_lit(ref<IMP_ConversionOperator,f,f,plain>)));
				var ref<int<4>,f,f,plain> a = cv.IMP__conversion_operator_int();
				var ref<ptr<int<4>>,f,f,plain> b = cv.IMP__conversion_operator_int_space__star_();
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "int32_t a = cv.operator int32_t();");
			EXPECT_PRED2(containsSubString, code, "int32_t* b = cv.operator int32_t*();");
		})
	}

	TEST(CppSnippet, Constructors) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
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
		})
	}

	TEST(CppSnippet, InterceptedConstructors) {
		DO_TEST(R"(
			def struct S {};
			int<4> main() {
				var ref<S> s0 = lit("S::ctor" : S::())(ref_decl(type_lit(ref<S,f,f,plain>)));
				var ref<S> s1 = S::(ref_decl(type_lit(ref<S,f,f,plain>)));
				var ref<S> s2;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "S s0;");
			EXPECT_PRED2(containsSubString, code, "S s1;");
			EXPECT_PRED2(containsSubString, code, "S s2;");
		})
	}

	TEST(CppSnippet, ConstructorsInitConstructor) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "A::A() : b() { }");
			EXPECT_PRED2(containsSubString, code, "D::D() : c(43) { }");
			EXPECT_PRED2(notContainsSubString, utils::removeCppStyleComments(code), "*");
			EXPECT_PRED2(notContainsSubString, code, "new ");
		})
	}

	TEST(CppSnippet, ConstructorsChained) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "ChainedConstructor::ChainedConstructor() : ChainedConstructor(5) { }");
			EXPECT_PRED2(notContainsSubString, utils::removeCppStyleComments(code), "*");
			EXPECT_PRED2(notContainsSubString, code, "new ");
		})
	}

	TEST(CppSnippet, ConstructorsBase) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "Derived::Derived() : Base(5) { }");
			EXPECT_PRED2(notContainsSubString, utils::removeCppStyleComments(code), "*");
			EXPECT_PRED2(notContainsSubString, code, "new ");
		})
	}

	TEST(CppSnippet, ConstructorsBaseMove) {
		DO_TEST(R"(
			decl IMP_move : (ref<IMP_Derived,f,f,cpp_ref>) -> ref<IMP_Derived,f,f,cpp_rref>;
			def struct IMP_Base {
			};
			def struct IMP_Derived : [ public IMP_Base ] {
				ctor function () {
					IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)));
				}
				ctor function (v1 : ref<IMP_Derived,f,f,cpp_rref>) {
					IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)), ref_parent_cast(IMP_move(ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(IMP_Base)));
				}
			};
			def IMP_move = function (v86 : ref<IMP_Derived,f,f,cpp_ref>) -> ref<IMP_Derived,f,f,cpp_rref> {
				return ref_cast(v86, type_lit(f), type_lit(f), type_lit(cpp_rref));
			};
			int<4> function IMP_main (){
				var ref<IMP_Derived,f,f,plain> v0 = IMP_Derived::(ref_decl(type_lit(ref<IMP_Derived,f,f,plain>)));
				var ref<IMP_Derived,f,f,plain> v1 = IMP_move(ref_kind_cast(v0, type_lit(cpp_ref)));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, ": IMP_Base((IMP_Base&&)IMP_move(v1))");
		})
	}

	TEST(CppSnippet, ReturnThisInConstructor) {
		DO_TEST(R"(
			def struct IMP_ConstructorReturn {
				ctor () {
					return this in ref<ref<IMP_ConstructorReturn>>;
				}
				dtor () {
					return this in ref<ref<IMP_ConstructorReturn>>;
				}
				lambda IMP_f = () -> unit {
					return;
				}
			};
			int<4> main() {
				var ref<IMP_ConstructorReturn,f,f,plain> v0 = IMP_ConstructorReturn::(ref_decl(type_lit(ref<IMP_ConstructorReturn,f,f,plain>)));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "return;");
			EXPECT_PRED2(notContainsSubString, code, "return this");
		})
	}

	TEST(CppSnippet, Globals) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(notContainsSubString, code, "new"); // no heap mem allocation in this program
			EXPECT_PRED2(containsSubString, code, "A a(5);");
		})
	}


	TEST(CppSnippet, Destructor) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "struct A");      // struct definition
			EXPECT_PRED2(containsSubString, code, "A a;");          // variable definition
			EXPECT_PRED2(notContainsSubString, code, "~A() {");     // don't implement destructor for A

			EXPECT_PRED2(containsSubString, code, "struct B");      // struct definition
			EXPECT_PRED2(containsSubString, code, "B b;");          // variable definition
			EXPECT_PRED2(containsSubString, code, "~B();");         // destructor for B
			EXPECT_FALSE(containsSubString(code, "virtual ~B();")); // not virtual

			EXPECT_PRED2(containsSubString, code, "struct C");      // struct definition
			EXPECT_PRED2(containsSubString, code, "C c;");          // variable definition
			EXPECT_PRED2(containsSubString, code, "virtual ~C();"); // destructor for B
		})
	}


	TEST(CppSnippet, MemberFunctionsExausted) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
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
		})
	}

	TEST(CppSnippet, MemberFunctionsRecursive) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
			EXPECT_PRED2(containsSubString, code, "A a;");			// variable definition

			// check definition of the recursive functions
			EXPECT_PRED2(containsSubString, code, "void r();");
			EXPECT_PRED2(containsSubString, code, "void A::r() {");

			EXPECT_PRED2(containsSubString, code, "void f();");
			EXPECT_PRED2(containsSubString, code, "void A::f() {");

			EXPECT_PRED2(containsSubString, code, "void g();");
			EXPECT_PRED2(containsSubString, code, "void A::g() {");
		})
	}

	TEST(CppSnippet, PureVirtualMemberFunctions) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "struct A");		// struct definition
			EXPECT_PRED2(containsSubString, code, "B a;");			// variable definition

			EXPECT_PRED2(containsSubString, code, "virtual void f1() =0;");
			EXPECT_PRED2(containsSubString, code, "virtual void f2() const =0;");
			EXPECT_PRED2(containsSubString, code, "virtual void f3() const volatile =0;");
			EXPECT_PRED2(containsSubString, code, "virtual void f4() volatile =0;");
		})
	}


	TEST(CppSnippet, Counter) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			;
		})
	}

	TEST(CppSnippet, Enum) {
		DO_TEST(R"(
			using "ext.enum";
			int<4> function IMP_main () {
				var ref<(type<enum_def<IMP_Bla,uint<8>,enum_entry<IMP_Bla_colon__colon_A,0>>>, uint<8>),f,f,plain> v0 = (type_lit(enum_def<IMP_Bla,uint<8>,enum_entry<IMP_Bla_colon__colon_A,0>>), 0ul);
				enum_to_int(*v0)==num_cast(5, type_lit(uint<8>));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "enum Bla : uint64_t { BlaA=0 }");
		})
	}

	TEST(CppSnippet, ImplicitInitConstructorSemantics) {
		// A a;
		// A b = a;
		// A c = A(a);

		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(notContainsSubString, code, "*");
			EXPECT_PRED2(containsSubString, code, "IMP_A(IMP_A const& p2) = default");
			EXPECT_PRED2(containsSubString, code, "IMP_A() = default");
			EXPECT_PRED2(containsSubString, code, "IMP_A(IMP_A&& p2) = default");
		})
	}

	TEST(CppSnippet, ImplicitCallArgConstructorSemantics) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(notContainsSubString, code, "*");
			EXPECT_PRED2(containsSubString, code, "IMP_A(IMP_A const& p2) = default");
			EXPECT_PRED2(containsSubString, code, "IMP_A() = default");
			EXPECT_PRED2(containsSubString, code, "IMP_A(IMP_A&& p2) = default");
		})
	}

	TEST(CppSnippet, Inheritance) {
		DO_TEST(R"(
			def struct IMP_Base {
				a : int<4>;
				function IMP_foo = () -> unit {
					(this).a = 5;
				}
			};
			def struct IMP_Derived : [ public IMP_Base ] {
				b : int<4>;
				function IMP_bar = () -> unit {
					ptr_to_ref(ptr_parent_cast(ptr_from_ref(this), type_lit(IMP_Base))).a = 6;
					(this).b = 6;
				}
			};
			def struct IMP_DerivedSpecifyingFoo : [ public IMP_Derived ] {
				c : int<4>;
				function IMP_foo = () -> unit {
					ptr_to_ref(ptr_parent_cast(ptr_from_ref(this), type_lit(IMP_Base))).a = 7;
					(this).c = 7;
				}
			};
			int<4> main() {
				var ref<IMP_Base,f,f,plain> v0 = IMP_Base::(ref_decl(type_lit(ref<IMP_Base,f,f,plain>)));
				var ref<IMP_Derived,f,f,plain> v1 = IMP_Derived::(ref_decl(type_lit(ref<IMP_Derived,f,f,plain>)));
				var ref<IMP_DerivedSpecifyingFoo,f,f,plain> v2 = IMP_DerivedSpecifyingFoo::(ref_decl(type_lit(ref<IMP_DerivedSpecifyingFoo,f,f,plain>)));

				// value
				ref_parent_cast(v1, type_lit(IMP_Base)).IMP_foo();

				// pointer
				var ref<ptr<IMP_Derived>,f,f,plain> v_ptr = ptr_from_ref(v1);
				ptr_to_ref(ptr_parent_cast(*v_ptr, type_lit(IMP_Base))).IMP_foo();

				// cpp reference
				var ref<IMP_Derived,f,f,cpp_ref> v_cppref = v1;
				ref_parent_cast(v_cppref, type_lit(IMP_Base)).IMP_foo();

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "(*(IMP_Base*)(&v1)).foo();");
			EXPECT_PRED2(containsSubString, code, "IMP_Derived* v_ptr = &v1;");
			EXPECT_PRED2(containsSubString, code, "(*(IMP_Base*)v_ptr).foo();");
			EXPECT_PRED2(containsSubString, code, "IMP_Derived& v_cppref = v1;");
			EXPECT_PRED2(containsSubString, code, "((IMP_Base&)v_cppref).foo();");
			EXPECT_PRED2(containsSubString, code, "(*(IMP_Base*)this).a = 6;");
			EXPECT_PRED2(containsSubString, code, "(*(IMP_Base*)this).a = 7;");
		})
	}

	TEST(CppSnippet, ConstructorCall) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "A a1;");
			EXPECT_PRED2(containsSubString, code, "A a2((1));");
			EXPECT_PRED2(containsSubString, code, "A* a1r = new A();");
			EXPECT_PRED2(containsSubString, code, "A* a2r = new A(1);");
			EXPECT_PRED2(containsSubString, code, "A* a3 = new A();");
			EXPECT_PRED2(containsSubString, code, "A* a4 = new A(1);");
			EXPECT_PRED2(containsSubString, code, "new (&a5) A();");
			EXPECT_PRED2(containsSubString, code, "new (&a6) A(1);");
		})
	}

	TEST(CppSnippet, NewDelete) {
		DO_TEST(R"(
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "int32_t* i = (int32_t*)malloc(sizeof(int32_t));");
			EXPECT_PRED2(containsSubString, code, "free(i);");
			EXPECT_PRED2(containsSubString, code, "int32_t* j = _ref_new___insieme_type_2(42);");
			EXPECT_PRED2(containsSubString, code, "free(j);");
			EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* o1 = new IMP_SimplestConstructor();");
			EXPECT_PRED2(containsSubString, code, "delete o1;");
			EXPECT_PRED2(containsSubString, code, "IMP_SlightlyLessSimpleConstructor* o2 = new IMP_SlightlyLessSimpleConstructor(42);");
			EXPECT_PRED2(containsSubString, code, "delete o2;");
		})
	}

	TEST(CppSnippet, NewDeleteArrayFixed) {
		DO_TEST(R"(
			def struct IMP_SimplestConstructor { };
			int<4> main() {
				var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_array(<ref<array<int<4>,50>,f,f,plain>>(ref_new(type_lit(array<int<4>,50>))) {});
				ref_delete(ptr_to_array(*i));
				var ref<ptr<int<4>>,f,f,plain> j = ptr_from_array(<ref<array<int<4>,50>,f,f,plain>>(ref_new(type_lit(array<int<4>,50>))) {1, 2, 3});
				ref_delete(ptr_to_array(*j));
				var ref<ptr<array<int<4>,3>>,f,f,plain> k = ptr_from_array(<ref<array<array<int<4>,3>,50>,f,f,plain>>(ref_new(type_lit(array<array<int<4>,3>,50>))) {});
				ref_delete(ptr_to_array(*k));

				var ref<ptr<IMP_SimplestConstructor>,f,f,plain> o1 = ptr_from_array(<ref<array<IMP_SimplestConstructor,3>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,3>))) {});
				ref_delete(ptr_to_array(*o1));
				var ref<IMP_SimplestConstructor,f,f,plain> v0 = IMP_SimplestConstructor::(ref_decl(type_lit(ref<IMP_SimplestConstructor,f,f,plain>)));
				var ref<ptr<IMP_SimplestConstructor>,f,f,plain> o2 = ptr_from_array(<ref<array<IMP_SimplestConstructor,3>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,3>))) {ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref))});
				ref_delete(ptr_to_array(*o2));

				// arrays created with ref_new not nested inside an init expr should be allocated with malloc and free'd with free
				var ref<ptr<int<4>,f,f>,f,f,plain> i_malloc = ptr_from_array(ref_new(type_lit(array<int<4>,50>)));
				ref_delete(ptr_to_ref(*i_malloc));
				var ref<ptr<IMP_SimplestConstructor>,f,f,plain> o_malloc = ptr_from_array(ref_new(type_lit(array<IMP_SimplestConstructor,3>)));
				ref_delete(ptr_to_ref(*o_malloc));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
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

			// arrays created with ref_new not nested inside an init expr should be allocated with malloc and free'd with free
			EXPECT_PRED2(containsSubString, code, "int32_t* i_malloc = (int32_t*)malloc(sizeof(int32_t) * 50);");
			EXPECT_PRED2(containsSubString, code, "free(i_malloc);");
			EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* o_malloc = (IMP_SimplestConstructor*)malloc(sizeof(IMP_SimplestConstructor) * 3);");
			EXPECT_PRED2(containsSubString, code, "free(o_malloc);");
		})
	}

	TEST(CppSnippet, NewDeleteArrayVariable) {
		DO_TEST(R"(
			def struct IMP_SimplestConstructor { };
			def new_arr_fun_0 = function (v0 : ref<uint<inf>,f,f,plain>) -> ptr<int<4>> {
				var uint<inf> bla = *v0;
				return ptr_from_array(<ref<array<int<4>,#bla>,f,f,plain>>(ref_new(type_lit(array<int<4>,#bla>))) {});
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
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "int32_t* v1 = new_arr_fun_0((uint64_t)(v0 + 5));");
			EXPECT_PRED2(containsSubString, code, "return new int32_t[v1];");
			EXPECT_PRED2(containsSubString, code, "int32_t* var_3 = new_arr_fun_1((uint64_t)(var_2 + 5), 1, 2, 3);");
			EXPECT_PRED2(containsSubString, code, "return new int32_t[v4]{v1, v2, v3};");
			EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* var_5 = new_arr_fun_2((uint64_t)(var_4 + 5))");
			EXPECT_PRED2(containsSubString, code, "return new IMP_SimplestConstructor[v1];");
			EXPECT_PRED2(containsSubString, code, "IMP_SimplestConstructor* v3 = new_arr_fun_3((uint64_t)(var_6 + 5), (IMP_SimplestConstructor const&)var_7, (IMP_SimplestConstructor const&)v2);");
			EXPECT_PRED2(containsSubString, code, "return new IMP_SimplestConstructor[v3]{v1, v2};");
		})
	}

	TEST(CppSnippet, ArrayStack) {
		DO_TEST(R"(
			def struct IMP_A {};
			int<4> main() {
				{
					var ref<array<IMP_A,3>,f,f,plain> i = <ref<array<IMP_A,3>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_A,3>,f,f,plain>))) {};
				}
				{
					var ref<IMP_A,f,f,plain> a = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
					var ref<IMP_A,f,f,plain> b = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
					var ref<array<IMP_A,3>,f,f,plain> v2 = <ref<array<IMP_A,3>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_A,3>,f,f,plain>))) {ref_cast(a, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(b, type_lit(t), type_lit(f), type_lit(cpp_ref))};
				}
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "{}");
			EXPECT_PRED2(containsSubString, code, "{{(IMP_A const&)a, (IMP_A const&)b}}");
		})
	}

	TEST(CppSnippet, LambdaBasic) {
		DO_TEST(R"(
			def struct lambda_class {
				capture_0 : int<4>;
				capture_1 : ref<int<4>,f,f,cpp_ref>;
				const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> unit {
					5;
				}
			};
			int<4> main() {
				var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
				var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
				var ref<lambda_class,f,f,plain> v2 = <ref<lambda_class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(lambda_class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {*v0, v1};
				<ref<lambda_class,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(lambda_class)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {*v0, v1}.IMP__operator_call_(42);
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "(lambda_class){v0, v1}.operator()(42);");
		})
	}

	TEST(CppSnippet, LambdaFunPtrConversion) {
		DO_TEST(R"(
			def __any_string__invoke = function (p1 : ref<int<4>>) -> unit {
				p1;
			};
			def struct __any_string__class {
				const function IMP__conversion_operator_void_space__lparen__star__rparen__lparen_int_rparen_ = () -> ptr<(int<4>) -> unit,t,f> {
					return ptr_of_function(__any_string__invoke);
				}
				const function IMP__operator_call_ = (p1 : ref<int<4>>) -> unit {
					p1;
				}
			};
			int<4> main() {
				var ref<ptr<(int<4>) -> unit,t,f>,f,f,plain> v0 = <ref<__any_string__class,f,f,plain>>(ref_temp(type_lit(__any_string__class))) {}.IMP__conversion_operator_void_space__lparen__star__rparen__lparen_int_rparen_();
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "__insieme_type_2 v0 = INS_INIT(__any_string__class){}.operator __insieme_type_2()");
		})
	}

	TEST(CppSnippet, InitializationOptions) {
		DO_TEST(R"(
			def struct IMP_D {
				i : int<4>;
			};
			def IMP_dfun = function (v0 : ref<IMP_D,f,f,plain>) -> unit { };
			int<4> main() {
				var ref<IMP_D,f,f,plain> v0 = <ref<IMP_D,f,f,plain>>(ref_decl(type_lit(ref<IMP_D,f,f,plain>))) {1};
				var ref<IMP_D,f,f,plain> v1 = <ref<IMP_D,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(IMP_D)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {1};
				IMP_dfun(<ref<IMP_D,f,f,plain>>(ref_decl(type_lit(ref<IMP_D,f,f,plain>))) {1});
				IMP_dfun(<ref<IMP_D,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(IMP_D)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {1});
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			;
		})
	}

	TEST(CppSnippet, DISABLED_StaticVariableConst) {
		DO_TEST(R"(
			using "ext.static";
			alias int = int<4>;
			let a = lit("a":ref<struct __static_var { initialized : bool; value : int; }>);

			int main() {

				static_init_lazy(a, 5);

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, " static int32_t a = 5");
		})
	}

	TEST(CppSnippet, DISABLED_StaticVariable) {
		DO_TEST(R"(
			using "ext.static";
			alias int = int<4>;
			let a = expr lit("a":ref<struct __static_var { bool initialized; int value; }>);

			int main() {

				init(a, lambda ()=> 1);
				init(a, lambda ()=> 2);

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, "static int32_t a = __insieme_type_");
		})
	}

	TEST(CppSnippet, DISABLED_StaticVariableSingle) {
		DO_TEST(R"(
			using "ext.static";
			alias int = int<4>;
			let a = expr lit("a":ref<struct __static_var { bool initialized; int value; }>);

			int main() {

				init(a, lambda ()=> 2);

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, " static int32_t a = 2;");
		})
	}

	TEST(CppSnippet, DISABLED_StaticVariableDefaultCtor) {
		DO_TEST(R"(
			using "ext.static";
			alias int = int<4>;
			let A = struct A {};
			let a = expr lit("a":ref<struct __static_var { bool initialized; A value; }>);
			let ctorA = lambda ctor A::() { };

			int main() {

				init(a, lambda ()=> *(ctorA(var(undefined(A)))));

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultCppCompiler(), {
			EXPECT_PRED2(containsSubString, code, " static A a;");
		})
	}

} // namespace backend
} // namespace insieme
