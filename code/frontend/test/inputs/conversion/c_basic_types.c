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

int main() {

	// BASE TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<char,f,f> v0;")
	char c;

	#pragma test expect_ir("var ref<uint<1>,f,f> v0;")
	unsigned char uc;
	#pragma test expect_ir("var ref<int<1>,f,f> v0;")
	signed char sc;

	#pragma test expect_ir("var ref<int<2>,f,f> v0;")
	short ss;
	#pragma test expect_ir("var ref<uint<2>,f,f> v0;")
	unsigned short us;

	#pragma test expect_ir("var ref<int<4>,f,f> v0;")
	int m;
	#pragma test expect_ir("var ref<int<4>,t,f> v0;")
	const int y;
	#pragma test expect_ir("var ref<int<4>,f,t> v0;")
	volatile int z;
	#pragma test expect_ir("var ref<int<4>,t,t> v0;")
	const volatile int q;

	#pragma test expect_ir("var ref<int<4>,t,f> v0;")
	int const y2;
	#pragma test expect_ir("var ref<int<4>,f,t> v0;")
	int volatile z2;
	#pragma test expect_ir("var ref<int<4>,t,t> v0;")
	int const volatile q2;

	#pragma test expect_ir("var ref<real<4>,f,f> v0;")
	float f1;
	#pragma test expect_ir("var ref<real<8>,f,f> v0;")
	double f2;
	#pragma test expect_ir("var ref<real<16>,f,f> v0;")
	long double f3;

	// BASE TYPE TYPEDEFS //////////////////////////////////////////////////////////////

	typedef float fluffy;
	typedef volatile float fluffier;

	#pragma test expect_ir("var ref<real<4>,f,f> v0;")
	fluffy fluff;

	#pragma test expect_ir("var ref<real<4>,f,t> v0;")
	fluffier fluffer;

	// POINTER TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<ptr<int<4>,f,f>,f,f> v0;")
	int* pi;

	#pragma test expect_ir("var ref<ptr<int<4>,t,f>,f,f> v0;")
	const int* pci;

	#pragma test expect_ir("var ref<ptr<int<4>,t,f>,f,f> v0;")
	int const * pci2;

	#pragma test expect_ir("var ref<ptr<int<4>,f,t>,f,f> v0;")
	volatile int* pvi;

	#pragma test expect_ir("var ref<ptr<int<4>,f,t>,f,f> v0;")
	int volatile * pvi2;

	#pragma test expect_ir("var ref<ptr<int<4>,t,t>,f,f> v0;")
	const volatile int* pcvi;

	#pragma test expect_ir("var ref<ptr<int<4>,f,f>,t,f> v0;")
	int *const cpi;

	#pragma test expect_ir("var ref<ptr<int<4>,t,f>,t,f> v0;")
	const int *const cpci;

	#pragma test expect_ir("var ref<ptr<ptr<int<4>,f,f>,f,f>,f,f> v0;")
	int** ppi;
	#pragma test expect_ir("var ref<ptr<ptr<ptr<int<4>,f,f>,f,f>,t,f>,f,f> v0;")
	int * *const * ppcpi;

	// FIXED SIZE ARRAY TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<array<real<4>,2u>,f,f> v0;")
	float arrf[2];
	#pragma test expect_ir("var ref<array<real<4>,2u>,t,f> v0;")
	const float arrcf[2];
	#pragma test expect_ir("var ref<array<real<4>,2u>,f,t> v0;")
	volatile float arrvf[2];

	#pragma test expect_ir("var ref<array<array<real<4>,5u>,2u>,f,f> v0;")
	float arrarrf[2][5];
	#pragma test expect_ir("var ref<array<array<array<real<4>,3u>,5u>,2u>,f,f> v0;")
	float arrarrarrf[2][5][3];

	#pragma test expect_ir("var ref<array<ptr<real<4>,f,f>,2u>,f,f> v0;")
	float* arrpf[2];
	#pragma test expect_ir("var ref<array<ptr<real<4>,t,f>,2u>,f,f> v0;")
	const float* arrpcf[2];
	#pragma test expect_ir("var ref<array<ptr<real<4>,t,f>,2u>,f,t> v0;")
	const float *volatile arrvpcf[2];

	// ENUM TYPES //////////////////////////////////////////////////////////////

	typedef enum { Bla, Alb } enum_t;
	#pragma test expect_ir(R"(
		var ref<(type<enum_def<__any_string__,int<4>,
				enum_entry<IMP_main_lparen__rparen__colon__colon__colon__colon_Bla,0>,
				enum_entry<IMP_main_lparen__rparen__colon__colon__colon__colon_Alb,1>>>, int<4>),f,f,plain> v0 =
			ref_decl(type_lit(ref<(type<enum_def<__any_string__,int<4>,
					enum_entry<IMP_main_lparen__rparen__colon__colon__colon__colon_Bla,0>,
					enum_entry<IMP_main_lparen__rparen__colon__colon__colon__colon_Alb,1>>>, int<4>),f,f,plain>));
	)")
	enum_t enu;
	enum { XY, ZR } bla;

	// STRUCT TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(
		def struct __any_string__struct {
			i : int<4>;
		};
		{
			var ref<__any_string__struct,f,f,plain> v0 = ref_decl(type_lit(ref<__any_string__struct,f,f,plain>));
		}
	)")
	{
		struct { int i; } swi_anon;
	}

	typedef struct swi_s { int i; } swi_t;
	#pragma test expect_ir(R"(
		def struct __any_string__ {
			i : int<4>;
		};
		var ref<__any_string__,f,f,plain> v0 = ref_decl(type_lit(ref<__any_string__,f,f,plain>));
	)")
	swi_t swi_1;
	#pragma test expect_ir(R"(
		def struct __any_string__ {
			i : int<4>;
		};
		var ref<__any_string__,f,f,plain> v0 = ref_decl(type_lit(ref<__any_string__,f,f,plain>));
	)")
	struct swi_s swi_2;

	typedef union { int i; } union_t;
	#pragma test expect_ir(R"(
		def union __any_string__ {
			i : int<4>;
		};
		var ref<__any_string__,f,f,plain> v0 = ref_decl(type_lit(ref<__any_string__,f,f,plain>));
	)")
	union_t uni;

	#pragma test expect_ir(R"(
		def struct __any_string__struct {
			a : int<4>;
			b : int<4>;
		};
		def union __any_string__union {
			__any_string__anonymous : __any_string__struct;
			v : array<int<4>,2u>;
		};
		{
			var ref<__any_string__union,f,f,plain> v0 = ref_decl(type_lit(ref<__any_string__union,f,f,plain>));
			v0.__any_string__anonymous.a;
		}
	)")
	{
		union {
			struct {
				int a; int b;
			};
			int v[2];
		} anonymous_inner;
		anonymous_inner.a;
	}
}
