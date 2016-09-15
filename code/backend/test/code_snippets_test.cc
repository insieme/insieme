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

#include <fstream>

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/config.h"

#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/compiler/compiler.h"

#include "code_snippets_test_utils.h"

namespace insieme {
namespace backend {

	// global because it increases the testing speed by ~ factor 2
	core::NodeManager mgr;
	core::IRBuilder builder(mgr);

	TEST(FunctionCall, SimpleFunctions) {
		DO_TEST(R"(
			alias int = int<4>;

			def f = (a : int, b : int)->int {
				return a + b * a;
			};

			int<4> main() {
				f(12,15);
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(FunctionCall, SimpleVarDecl) {
		DO_TEST(R"(
			int<4> main() {
				var ref<int<4>,f,f,plain> c;
				var ref<int<4>,f,f,plain> v = 3;
				return c+v;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(FunctionCall, Templates) {
		DO_TEST(R"(
			int<4> main() {
				(dtype : type<'a>, size : type<'s>)->unit {
					var ref<'a> v0;
					var int<'s> v1 = lit("0":int<'s>);
				}(type_lit(real<4>), type_lit(8));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(FunctionCall, Pointwise) {
		DO_TEST(R"(
			unit main() {
				auto v1 = *<ref<array<int<4>,4>>>{1,2,3,4};
				auto v2 = *<ref<array<int<4>,4>>>{5,6,7,8};
				array_pointwise(int_add)(v1,v2);
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_FALSE(code.find("<?>") != string::npos);
		})
	}

	TEST(FunctionCall, TypeLiterals) {
		DO_TEST(R"(int<4> main() {
			(stype : type<'a>)->int<4> {
				return 5;
			} (type_lit(real<4>));
			return 0;
		}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(containsSubString, code, "fun_1()");
		})
	}

	TEST(FunctionCall, GenericFunctionAndTypeLiteral) {
		DO_TEST(R"(
			int<4> main() {
				(data : ref<array<'a,'l>>)->uint<8> {
					return sizeof(type_lit('a));
				} (ref_null(type_lit(array<real<4>,12>),type_lit(f),type_lit(f)));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(containsSubString, code, "sizeof(float)");
		})
	}

	TEST(FunctionCall, RefNewCalls) {
		DO_TEST(R"(
			int<4> main() {
				var ref<int<4>> var_5017 = ref_new(type_lit(int<4>));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(containsSubString, code, "int32_t* var_5017 = (int32_t*)malloc(sizeof(int32_t))");
		})
	}

	TEST(FunctionCall, FixedSizedArrayInit) {
		DO_TEST(R"(
			decl call_vector : (ref<array<uint<8>,3>>)->unit ;
			int<4> main() {
				call_vector(<ref<array<uint<8>,3>>>{0ul,0ul,0ul});
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "call_vector((uint64_t(*)[3])(&INS_INIT(__insieme_type_2){{0ul, 0ul, 0ul}}))");
		})
	}

	TEST(Literals, BoolLiterals) {
		DO_TEST(R"(int<4> main() {
			true;
			false;
			return 0;
		})", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "true");
			EXPECT_PRED2(containsSubString, code, "false");
		})
	}

	TEST(Literals, NumericLiterals) {
		DO_TEST(R"(int<4> main() {
			lit("1" : int<1>) * lit("-1" : int<1>);
			lit("2" : uint<1>);
			lit("3" : int<2>) * lit("-3" : int<2>);
			lit("4" : uint<2>);
			5 * -5;
			6u;
			7l * -7l;
			8ul;
			9ll *  -9ll;
			10ull;
			11f * -11f;
			12.0f * -12.0f;
			13d * -13d;
			14.0d * -14.0d;
			15.0 * -15.0;
			return 0;
		})", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			// casts to smaller types
			EXPECT_PRED2(containsSubString, code, "(int8_t)1 * (int8_t)-1;");
			EXPECT_PRED2(containsSubString, code, "(uint8_t)2;");
			EXPECT_PRED2(containsSubString, code, "(int16_t)3 * (int16_t)-3;");
			EXPECT_PRED2(containsSubString, code, "(uint16_t)4;");

			// no casts to larger and floating point types - the corresponding literals should be used
			EXPECT_PRED2(containsSubString, code, "5 * -5;");
			EXPECT_PRED2(containsSubString, code, "6u;");
			EXPECT_PRED2(containsSubString, code, "7l * -7l;");
			EXPECT_PRED2(containsSubString, code, "8ul;");
			EXPECT_PRED2(containsSubString, code, "9ll * -9ll;");
			EXPECT_PRED2(containsSubString, code, "10ull;");
			EXPECT_PRED2(containsSubString, code, "11.0f * -11.0f;");
			EXPECT_PRED2(containsSubString, code, "12.0f * -12.0f;");
			EXPECT_PRED2(containsSubString, code, "13.0 * -13.0;");
			EXPECT_PRED2(containsSubString, code, "14.0 * -14.0;");
			EXPECT_PRED2(containsSubString, code, "15.0 * -15.0;");

			EXPECT_PRED2(notContainsSubString, code, "(long)");
			EXPECT_PRED2(notContainsSubString, code, "(int64_t)");
			EXPECT_PRED2(notContainsSubString, code, "(unsigned long)");
			EXPECT_PRED2(notContainsSubString, code, "(uint64_t)");
			EXPECT_PRED2(notContainsSubString, code, "(long long)");
			EXPECT_PRED2(notContainsSubString, code, "(int128_t)");
			EXPECT_PRED2(notContainsSubString, code, "(unsigned long long)");
			EXPECT_PRED2(notContainsSubString, code, "(uint128_t)");
			EXPECT_PRED2(notContainsSubString, code, "(float)");
			EXPECT_PRED2(notContainsSubString, code, "(double)");
		})
	}

	TEST(Parallel, NestedLambdaTypeDeduction) {
		core::ProgramPtr program = builder.parseProgram(R"(
			alias int = int<4>;
			alias uint = uint<4>;

			def differentbla = (x : 'b) -> unit {
				auto m = x;
				auto l = m;
			};

			def bla = (f : 'a) -> unit {
				let anotherbla = (x : 'a) -> unit {
					auto m = x;
				};
				anotherbla(f);
				differentbla(f);
				parallel(job { auto l = f; });
			};

			int main() {
				// some bla
				var int x = 10;
				bla(x);
				return 0;
			})");
		LOG(DEBUG) << "Program: " << *program << std::endl;

		auto converted = sequential::SequentialBackend::getDefault()->convert(program);
		LOG(DEBUG) << "Converted Seq: \n" << *converted << std::endl;
		auto converted_rt = runtime::RuntimeBackend::getDefault()->convert(program);
		LOG(DEBUG) << "Converted Run: \n" << *converted_rt << std::endl;
	}

	TEST(Arrays, Allocation) {
		DO_TEST(R"(
			alias int = int<4>;
			alias uint = uint<4>;

			int main() {

				// get a size for the variable-sized arrays
				var uint<inf> size = num_cast(12,type_lit(uint<inf>));

				// create two fixed-sized arrays on the stack and the heap
				var ref<array<int,10>> a;
				var ref<array<int,10>> b = ref_new(type_lit(array<int,10>));
				var ref<array<int,10>> c = ref_new_init(*a);

				var ref<array<int,inf>> e = ref_null(type_lit(array<int,inf>),type_lit(f),type_lit(f));

				// use the arrays
				a[2] = 123;
				b[4] = 321;
				c[1] = 456;
				e[7] = 123;

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "__insieme_type_1 a;");
			EXPECT_PRED2(containsSubString, code, "__insieme_type_1* b = (__insieme_type_1*)malloc(sizeof(__insieme_type_1));");
		})
	}

	TEST(Arrays, VLAs) {
		DO_TEST(R"(
			int<4> main() {
				var ref<int<4>,f,f> v0 = 3;
				var uint<inf> l = num_cast(*v0,type_lit(uint<inf>));
				var ref<array<real<4>,#l>,f,f> arr;

				var ref<int<4>,f,f> v1 = 3;
				var ref<int<4>,f,f> v2 = 6;
				var ref<int<4>,f,f> v3 = 10;
				var uint<inf> l1 = num_cast(*v1+2, type_lit(uint<inf>));
				var uint<inf> l2 = num_cast(*v2, type_lit(uint<inf>));
				var uint<inf> l3 = num_cast(*v3+1, type_lit(uint<inf>));
				var ref<array<array<array<int<4>,#l3>,#l2>,#l1>,f,f> arr2;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "float arr[l];");
			EXPECT_PRED2(containsSubString, code, "int32_t arr2[l1][l2][l3];");
		})
	}

	TEST(FunctionCall, GenericFunctionsWithLazy) {
		DO_TEST(R"(
			def f = (a : ref<'a>, c : ('a)=>bool, v : ()=>'a)->ref<'a> {
				if(c(*a)) {
					a = v();
				}
				return a;
			};
			int<4> main() {
				var ref<int<4>> a = 1;
				var ref<real<4>> b = 2.0f;

				f(a, (a : int<4>)=> true, ()=>3);
				f(b, (b : real<4>)=> true, ()=>4.0f);

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(FunctionCall, PassLambdaToBind) {
		DO_TEST(R"(def f = ()->int<4> { return 4; };
			def g = (a : ()=>'a)->'a { return a(); };
			int<4> main() {
				g(f);
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(Debugging, DebugCodePrinting) {
		std::string irString = R"(
			alias int = int<4>;

			def f = ()->int { return 4; };
			def g = (x : int)->int { return x + 2; };

			int main() {
				return g(f());
			}
		)";

		{
			DO_TEST(irString, false, utils::compiler::Compiler::getDefaultC99Compiler(), {
				EXPECT_PRED2(notContainsSubString, code, "<?>");
				EXPECT_PRED2(notContainsSubString, code, "<a>");
				EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
				EXPECT_PRED2(notContainsSubString, code, "{\n    _ = function() -> int<4> {\n        return g(f());\n    };\n}");
			})
		}

		{
			auto backend = sequential::SequentialBackend::getDefault();
			backend->getConfiguration().addIRCodeAsComment = true;
			DO_TEST_WITH_BACKEND(irString, backend, false, utils::compiler::Compiler::getDefaultC99Compiler(), {
				EXPECT_PRED2(notContainsSubString, originalCode, "<?>");
				EXPECT_PRED2(notContainsSubString, originalCode, "<a>");
				EXPECT_PRED2(notContainsSubString, originalCode, "UNSUPPORTED");
				EXPECT_PRED2(containsSubString, originalCode, "{main = function () -> int<4> {\n        return g(f());\n    };\n}");
			})
		}
	}

	TEST(Types, RecursiveTypesSimple) {
		DO_TEST(R"(
			alias int = int<4>;

			def struct List { value : int<4>; next : ref<List>; };

			int main() {
				var ref<List> x;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(Types, RecursiveTypesMutual) {
		DO_TEST(R"(
			alias int = int<4>;

			decl struct B;
			def struct A { value : int<4>; next : ref<B>; };
			def struct B { value : int<4>; next : ref<A>; };

			int main() {
				var ref<A> x;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(Functions, RecursiveFunctionSimple) {
		DO_TEST(R"(
			alias int = int<4>;

			decl f : ()->unit;
			def f = ()->unit { f(); };

			int main() {
				f();
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(Functions, RecursiveFunctionMutual) {
		DO_TEST(R"(
			alias int = int<4>;

			decl f : ()->unit;
			decl g : ()->unit;

			def f = ()->unit { g(); };
			def g = ()->unit { f(); };

			int main() {
				f();
				g();
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(Functions, RecursiveFunctionEvenOdd) {
		DO_TEST(R"(
			alias int = int<4>;

			decl even : ( int )->bool;
			decl odd  : ( int )->bool;

			def even = ( x : int )->bool { return ( x == 0 ) ? true : odd(x-1); };
			def odd  = ( x : int )->bool { return ( x == 0 ) ? false : even(x-1); };

			int main() {
				even(10);
				odd(12);
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "<?>");
			EXPECT_PRED2(notContainsSubString, code, "<a>");
			EXPECT_PRED2(notContainsSubString, code, "UNSUPPORTED");
		})
	}

	TEST(Functions, FunTypeDecls) {
		DO_TEST(R"(
			int<4> main() {
				var ref<ptr<(int<4>) -> unit,t,f>,f,f,plain> v0;
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "typedef __insieme_type_1* __insieme_type_2;");
			EXPECT_PRED2(containsSubString, code, "__insieme_type_2 v0");
		})
	}

	TEST(Initialization, Array) {
		DO_TEST(R"(
			int<4> main() {
				var ref<array<int<4>,5>,f,f> v0 = <ref<array<int<4>,5>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5>,f,f,plain>))) {1,2,3,4,5};
				//int arr_all[5] = {1,2,3,4,5};

				var ref<array<int<4>,5>,f,f> v1 = <ref<array<int<4>,5>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5>,f,f,plain>))) {1,2};
				//int arr_partial[5] = {1,2};

				var ref<array<int<4>,5>,f,f> v2 = <ref<array<int<4>,5>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5>,f,f,plain>))) {0};
				//int arr_zero[5] = {0};

				var ref<array<int<4>,3>,f,f> v3 = <ref<array<int<4>,3>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3>,f,f,plain>))) {0,1,2};
				//int arr_implied[] = {0,1,2};

				var ref<array<array<int<4>,3>,2>,f,f> v4 = <ref<array<array<int<4>,3>,2>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3>,2>,f,f>))) {<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {1,2,3},<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {4,5,6}};
				//int arr_multi[2][3] = {{1,2,3}, {4,5,6}};

				var ref<array<array<int<4>,3>,2>,f,f,plain> v5 = <ref<array<array<int<4>,3>,2>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3>,2>,f,f,plain>))) {<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {1},<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {4,5}};
				//int arr_multi_partial[2][3] = {{1}, {4,5}};

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "v0 = {{1, 2, 3, 4, 5}}");
			EXPECT_PRED2(containsSubString, code, "v1 = {{1, 2}}");
			EXPECT_PRED2(containsSubString, code, "v2 = {{0}}");
			EXPECT_PRED2(containsSubString, code, "v3 = {{0, 1, 2}}");
			EXPECT_PRED2(containsSubString, code, "v4 = {{{{1, 2, 3}}, {{4, 5, 6}}}}");
			EXPECT_PRED2(containsSubString, code, "v5 = {{{{1}}, {{4, 5}}}}");
		})
	}

	TEST(Initialization, Struct) {
		DO_TEST(R"(
			def struct A { a : int<4>; b : real<4>; };
			def struct B { a : int<4>; b : real<4>; c : uint<4>; };
			def struct C { };
			int<4> main() {
				var ref<A> v0 = <ref<A>>(ref_decl(type_lit(ref<A>))){ 1, 1.0f };
				//{ struct { int a; float b; } sif = { 1, 1.0f }; }

				var ref<B> v1 = <ref<B>>(ref_decl(type_lit(ref<B>))){ 1, 1.0f, 2u };
				//{ struct { int a; float b; unsigned c; } sifc = { .a = 1, .c = 2u }; }

				var ref<C> v2 = <ref<C>>(ref_decl(type_lit(ref<C>))){};

				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "A v0 = {1, 1.0f}");
			EXPECT_PRED2(containsSubString, code, "B v1 = {1, 1.0f, 2u}");
			EXPECT_PRED2(containsSubString, code, "C v2 = {}");
		})
	}

	TEST(Initialization, StructTemp) {
		DO_TEST(R"(
			def struct A { a : int<4>; b : real<4>; };

			def f = (a : A) ->unit { };

			int<4> main() {
				f(*<ref<A>>(ref_temp(type_lit(A))){ 1, 1.0f });

				var ref<A> v0;
				v0 = *<ref<A>>(ref_temp(type_lit(A))){ 1, 1.0f };
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			;
		})
	}

	TEST(Initialization, Globals) {
		DO_TEST(R"(
			def struct S { x: int<4>; y: uint<4>; };
			int<4> main() {
				<ref<int<4>,f,f,plain>>(lit("initedGlobal" : ref<int<4>,f,f,plain>)) {5};
				<ref<S,f,f,plain>>(lit("y" : ref<S,f,f,plain>)) {1, 5u};
				<ref<array<S,3>,f,f,plain>>(lit("klaus_test" : ref<array<S,3>,t,f,plain>)) {<ref<S,f,f,plain>>(ref_temp(type_lit(S))) {1, 2u}, <ref<S,f,f,plain>>(ref_temp(type_lit(S))) {3, 4u}, <ref<S,f,f,plain>>(ref_temp(type_lit(S))) {5, 6u}};
				<ref<array<char,255>,f,f,plain>>(lit("char_arr" : ref<array<char,255>,f,f,plain>)) {'\0'};
				<ref<array<int<4>,2>,f,f,plain>>(lit("arr" : ref<array<int<4>,2>,f,f,plain>)) {42, 43};

				*lit("initedGlobal":ref<int<4>>);
				*lit("y":ref<S>);
				ptr_from_array(lit("klaus_test":ref<array<S,3>,t,f>));
				ptr_from_array(lit("char_arr":ref<array<char,255>,f,f>));
				ptr_from_array(lit("arr":ref<array<int<4>,2>,f,f>));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "{{{1, 2u}, {3, 4u}, {5, 6u}}}");
		})
	}

	TEST(Enum, Simple) {
		DO_TEST(R"(using "ext.enum";
			int<4> main() {
				var ref<(type<enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>>, int<4>),f,f,plain> v0 = (type_lit(enum_def<IMP_e,int<4>,enum_entry<IMP_e_colon__colon_A,0>,enum_entry<IMP_e_colon__colon_B,1>,enum_entry<IMP_e_colon__colon_C,2>>), 0);
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "e v0 = eA;");
		})
	}

	TEST(Pointer, Simple) {
		DO_TEST(R"(
			def fun = (i : ptr<int<4>>) -> unit {};
			def pfun = () -> ptr<int<4>> {
				var ref<int<4>,f,f> v0;
				return ptr_from_ref(v0);
			};
			def pfun2 = (a : ptr<int<4>>) -> int<4> {
				return *ptr_to_ref(a);
			};

			def struct A {
				m1 : ptr<int<4>>;
			};
			def pAfun = (p : A) -> int<4> {
				return *ptr_to_ref(p.m1);
			};

			int<4> main() {
				var ref<int<4>,f,f> v0;
				var ref<ptr<int<4>,f,f>,f,f> v1 = ptr_from_ref(v0);
				fun(ptr_from_ref(v0));
				fun(*v1);
				pfun();
				pfun2(*v1);
				pfun2(pfun());

				var ref<int<4>,f,f> s = *ptr_to_ref(pfun()) + *ptr_to_ref(pfun());

				var ref<A> a;
				pAfun(*a);
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "*pfun() + *pfun()");
		})
	}

	TEST(VariableDeclTypes, InitExpr) {
		DO_TEST(R"(
			def union u {
				i : int<4>;
			};
			int<4> function main (v69 : ref<int<4>,f,f,plain>, v70 : ref<ptr<ptr<char>>,f,f,plain>){
				var ref<u,f,f,plain> v71 = <ref<u,f,f,plain>>(ref_decl(type_lit(ref<u,f,f,plain>))) {1};
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "u v71 = {1};");
		})
	}

	TEST(Pointers, Simple) {
		DO_TEST(R"string(
			decl IMP___assert_fail : (ptr<char,t,f>, ptr<char,t,f>, uint<4>, ptr<char,t,f>) -> unit;
			int<4> function IMP_main (v9 : ref<int<4>,f,f,plain>, v10 : ref<ptr<ptr<char>>,f,f,plain>){
				var ref<ptr<int<4>>,f,f,plain> v11 = ptr_null(type_lit(int<4>), type_lit(f), type_lit(f));
				var ref<ptr<int<4>>,f,f,plain> v15 = *v11;
				ptr_eq(*v11, *v15) && ptr_ne(ptr_from_array("This is a semantic ERROR!"), ptr_null(type_lit(char), type_lit(f), type_lit(f)))?unit_consume(0):
					IMP___assert_fail(ptr_cast(ptr_from_array("a == b && \"This is a semantic ERROR!\""), type_lit(t), type_lit(f)), ptr_cast(ptr_from_array("s"), type_lit(t), type_lit(f)), num_cast(9, type_lit(uint<4>)), ptr_from_array("int main(int, char **)"));
				return 0;
			}
		)string", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "ptr_");
		})
	}

	TEST(Memory, MallocFree) {
		// char* a = (char*)malloc(sizeof(char) * 30);
		// free(a);
		DO_TEST(R"(
			def malloc_wrapper = (size : uint<8>) -> ptr<unit> {
				var uint<inf> si = size;
				return ptr_reinterpret(ptr_from_array(ref_new(type_lit(array<uint<1>,#si>))), type_lit(unit));
			};

			def free_wrapper = (trg : ptr<unit>) -> unit { ref_delete(ptr_to_ref(trg)); };

			int<4> main() {
				var ref<ptr<char>,f,f,plain> v0 = ptr_reinterpret(malloc_wrapper(sizeof(type_lit(char))*num_cast(30, type_lit(uint<8>))), type_lit(char));
				free_wrapper(ptr_reinterpret(*v0, type_lit(unit)));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "(char*)malloc_wrapper(sizeof(char) * (uint64_t)30)");
			EXPECT_PRED2(containsSubString, code, "free_wrapper((void*)v0)");
		})
	}

	TEST(Assignment, CStyle) {
		DO_TEST(R"(
			def c_style_assignment = (lhs : ref<'a,f,'b>, rhs : 'a) -> 'a { lhs = rhs; return *lhs; };
			int<4> function IMP_main() {
				var ref<int<4>,f,f,plain> v9 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
				var ref<int<4>,f,f,plain> v10 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
				var ref<int<4>,f,f,plain> v11 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
				v9 = c_style_assignment(v10, c_style_assignment(v11, 1));
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(containsSubString, code, "c_style_assignment");
			EXPECT_PRED2(notContainsSubString, code, "+=");
		})
	}

	TEST(Comma, Operator) {
		DO_TEST(R"(
			def comma_operator = (lhs : () => 'a, rhs : () => 'b) -> 'b { lhs(); return rhs(); };
			int<4> function IMP_main() {
				comma_operator(() -> int<4> { return comma_operator(() -> int<4> { return 2; }, () -> int<4> { return 3; }); }, () -> int<4> { return 4; });
				return 0;
			}
		)", false, utils::compiler::Compiler::getDefaultC99Compiler(), {
			EXPECT_PRED2(notContainsSubString, code, "comma_operator");
			EXPECT_PRED2(containsSubString, code, "2,3,4");
		})
	}

} // namespace backend
} // namespace insieme
