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

int foo(int a) { return a; }

int main() {

	// BASE TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<char,f,f> v0 = num_cast(lit(\"'a'\":int<4>), type_lit(char));")
	char c = 'a';

	#pragma test expect_ir("var ref<uint<1>,f,f> v0 = num_cast(5, type_lit(uint<1>));")
	unsigned char uc = 5;
	#pragma test expect_ir("var ref<int<1>,f,f> v0 = num_cast(2, type_lit(int<1>));")
	signed char sc = 2;

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = 2;")
	int i = 2;

	#pragma test expect_ir(R"(var ref<(ref<array<char,inf>,f,f,plain>,int<8>),f,f,plain> v0 =
							  ptr_from_array(lit(""Hallo"":ref<array<char,6u>,f,f>));)")
	char* hallo = "Hallo";

	#pragma test expect_ir(R"(var ref<array<char,6u>,f,f,plain> v0 = lit(""Hallo"":ref<array<char,6u>,f,f>);)")
	char hallo2[6] = "Hallo";

	#pragma test expect_ir(R"(var ref<array<char,5u>,f,f,plain> v0 =
							  <ref<array<char,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<char,5u>,f,f,plain>))) {'a', 'b', 'c', '\n', '\0'};)")
	char abc[5] = { "abc\n" };

	#pragma test expect_ir(R"(var ref<array<char,5u>,f,f,plain> v0 =
							  <ref<array<char,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<char,5u>,f,f,plain>))) {'m', 'u', 'h', 'a', '\0'};)")
	char muha[5] = { "muha" };

	#pragma test expect_ir(R"(var ref<array<char,4u>,f,f,plain> v0 =
							  <ref<array<char,4u>,f,f,plain>>(ref_decl(type_lit(ref<array<char,4u>,f,f,plain>)))
							  {num_cast(lit("'b'":int<4>), type_lit(char)),
							   num_cast(lit("'c'":int<4>), type_lit(char)),
							   num_cast(lit("'d'":int<4>), type_lit(char))};)")
	char bcd[4] = { 'b', 'c', 'd' };

	// QUALIFIERS //////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<int<4>,t,f> v0 = 5;")
	const int ci = 5;

	#pragma test expect_ir("var ref<int<4>,f,t> v0 = 5;")
	volatile int vi = 5;

	#pragma test expect_ir("var ref<int<4>,t,t> v0 = 5;")
	const volatile int cvi = 5;

	// POINTER TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<ptr<int<4>,f,f>,f,f> v1 = ptr_from_ref(v0); }")
	{
		int i;
		int* pi = &i;
	}
	#pragma test expect_ir("{ var ref<int<4>,t,f> v0; var ref<ptr<int<4>,t,f>,f,f> v1 = ptr_from_ref(v0); }")
	{
		const int ci;
		const int* pci = &ci;
	}
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<ptr<int<4>,t,f>,f,f> v1 = ptr_cast(ptr_from_ref(v0), type_lit(t), type_lit(f)); }")
	{
		int i;
		const int* pci2 = &i;
	}

	// ARRAY TYPES /////////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<array<int<4>,5u>,f,f> v0 = <ref<array<int<4>,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5u>,f,f,plain>))) {1,2,3,4,5};")
	int arr_all[5] = {1,2,3,4,5};

	#pragma test expect_ir("var ref<array<int<4>,5u>,f,f> v0 = <ref<array<int<4>,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5u>,f,f,plain>))) {1,2};")
	int arr_partial[5] = {1,2};

	#pragma test expect_ir("var ref<array<int<4>,5u>,f,f> v0 = <ref<array<int<4>,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5u>,f,f,plain>))) {0};")
	int arr_zero[5] = {0};

	#pragma test expect_ir("var ref<array<int<4>,3u>,f,f> v0 = <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {0,1,2};")
	int arr_implied[] = {0,1,2};

	#pragma test expect_ir("var ref<array<array<int<4>,3u>,2u>,f,f> v0 = <ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f>))) {<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>>))) {1,2,3},<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>>))) {4,5,6}};")
	int arr_multi[2][3] = {{1,2,3}, {4,5,6}};

	#pragma test expect_ir("var ref<array<array<int<4>,3u>,2u>,f,f,plain> v0 = <ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f,plain>))) {<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>>))) {1},<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>>))) {4,5}};")
	int arr_multi_partial[2][3] = {{1}, {4,5}};

	// STRUCT TYPES //////////////////////////////////////////////////////////////

	// basic
	#pragma test expect_ir(R"(
		def struct __any_string__struct {
			a : int<4>;
			b : real<4>;
		};
		{
			var ref<__any_string__struct,f,f,plain> v0 = <ref<__any_string__struct,f,f,plain>>(ref_decl(type_lit(ref<__any_string__struct,f,f,plain>))) {1, 1.0E+0f};
		}
	)")
	{
		struct {
			int a; float b;
		} sif = { 1, 1.0f };
	}

	// implicit
	#pragma test expect_ir(R"(
		def struct __any_string__struct {
			a : int<4>;
			b : real<4>;
			c : uint<4>;
		};
		{
			var ref<__any_string__struct,f,f,plain> v0 = <ref<__any_string__struct,f,f,plain>>(ref_decl(type_lit(ref<__any_string__struct,f,f,plain>))) {1, 0.0f, 2u};
		}
	)")
	{
		struct {
			int a; float b; unsigned c;
		} sifc = { .a = 1, .c = 2u };
	}

	// explicit
	#pragma test expect_ir(R"(
		def struct __any_string__struct {
			a : int<4>;
			b : real<4>;
			c : uint<4>;
		};
		{
			var ref<__any_string__struct,f,f,plain> v0 = <ref<__any_string__struct,f,f,plain>>(ref_decl(type_lit(ref<__any_string__struct,f,f,plain>))) {1, 0.0E+0f, 2u};
		}
	)")
	{
		struct {
			int a; float b; unsigned c;
		} sifc2 = { 1, 0.0f, 2u };
	}

	// UNION TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(
		def union __any_string__union {
			a : int<4>;
			b : real<4>;
		};
		{
			var ref<__any_string__union,f,f,plain> v0 = <ref<__any_string__union,f,f,plain>>(ref_decl(type_lit(ref<__any_string__union,f,f,plain>))) {1};
		}
	)")
	{
		union {
			int a; float b;
		} uif = { 1 };
	}


	#pragma test expect_ir(R"(
		def union __any_string__union {
			a : int<4>;
			b : real<4>;
		};
		{
			var ref<__any_string__union,f,f,plain> v0 = <ref<__any_string__union,f,f,plain>>(ref_decl(type_lit(ref<__any_string__union,f,f,plain>))) {1};
		}
	)")
	{
		union {
			int a; float b;
		} uif = { .a = 1 };
	}

	#pragma test expect_ir(R"(
		def union __any_string__union {
			a : int<4>;
			b : real<4>;
		};
		{
			var ref<__any_string__union,f,f,plain> v0 = <ref<__any_string__union,f,f,plain>>(ref_decl(type_lit(ref<__any_string__union,f,f,plain>))) {1.0E+0f};
		}
	)")
	{
		union {
			int a; float b;
		} uif = { .b = 1.0f };
	}

	// NESTED INITIALIZERS //////////////////////////////////////////////////////

	#pragma test expect_ir(R"(
		def struct __any_string__struct_inner {
			inner1 : int<4>;
			inner2 : real<4>;
		};
		def union __any_string__union_inner {
			u1 : int<4>;
			u2 : real<4>;
		};
		def struct __any_string__struct {
			is : __any_string__struct_inner;
			iu : __any_string__union_inner;
		};
		{
			var ref<__any_string__struct,f,f,plain> v0 = <ref<__any_string__struct,f,f,plain>>(
					ref_decl(type_lit(ref<__any_string__struct,f,f,plain>))) {
							<ref<__any_string__struct_inner,f,f,plain>>(ref_decl(type_lit(ref<__any_string__struct_inner>))) {1, 2.0E+0f},
							<ref<__any_string__union_inner,f,f,plain>>(ref_decl(type_lit(ref<__any_string__union_inner>))) {3}};
		}
	)")
	{
		struct {
			struct { int inner1; float inner2; } is;
			union { int u1; float u2; } iu;
		} su = { { 1, 2.0f }, { 3 } };
	}

	#pragma test expect_ir(R"(
		def struct __any_string__struct {
			a : int<4>;
			b : uint<4>;
		};
		{
			var ref<array<__any_string__struct,2u>,f,f,plain> v0 = <ref<array<__any_string__struct,2u>,f,f,plain>>(
					ref_decl(type_lit(ref<array<__any_string__struct,2u>,f,f,plain>))) {
							<ref<__any_string__struct,f,f,plain>>(ref_decl(type_lit(ref<__any_string__struct>))) {1, 2u},
							<ref<__any_string__struct,f,f,plain>>(ref_decl(type_lit(ref<__any_string__struct>))) {3, 4u}};
		}
	)")
	{
		struct {
			int a; unsigned b;
		} su[2] = { { 1, 2u }, { 3, 4u } };
	}

	#pragma test expect_ir(R"(
		def union __any_string__union {
			u1 : array<char,2u>;
		};
		{
			var ref<__any_string__union,f,f,plain> v0 = <ref<__any_string__union,f,f,plain>>(
					ref_decl(type_lit(ref<__any_string__union,f,f,plain>))) {
							<ref<array<char,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<char,2u>>))) {num_cast(1, type_lit(char)), num_cast(2, type_lit(char))}};
		}
	)")
	{
		union {
			char u1[2];
		} us = { { 1, 2 } };
	}

	// BOOL CONVERSION //////////////////////////////////////////////////////

	#define BOOL_TO_INT "def bool_to_int = (b: bool) -> int<4> { if(b) {return 1;} else {return 0;} };"

	#pragma test expect_ir(BOOL_TO_INT,"{","var ref<int<4>,f,f> v0 = bool_to_int(1<2); }")
	{ int boolconv = 1 < 2; }
}
