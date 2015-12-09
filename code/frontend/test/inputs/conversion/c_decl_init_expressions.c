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

int foo(int a) { return a; }

int main() {
	
	// BASE TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("var ref<char,f,f> v0 = ref_var_init(num_cast(lit(\"'a'\":int<4>), type_lit(char)));")
	char c = 'a';

	#pragma test expect_ir("var ref<uint<1>,f,f> v0 = ref_var_init(num_cast(5, type_lit(uint<1>)));")
	unsigned char uc = 5;
	#pragma test expect_ir("var ref<int<1>,f,f> v0 = ref_var_init(num_cast(2, type_lit(int<1>)));")
	signed char sc = 2;
	
	#pragma test expect_ir("var ref<int<4>,f,f> v0 = ref_var_init(2);")
	int i = 2;
	
	#pragma test expect_ir(R"(var ref<(ref<array<char,inf>,f,f,plain>,int<8>),f,f,plain> v0 =
							  ref_var_init(ptr_from_array(lit(""Hallo"":ref<array<char,6>,f,f>)));)")
	char* hallo = "Hallo";
	
	#pragma test expect_ir(R"(var ref<array<char,6>,f,f,plain> v0 = lit(""Hallo"":ref<array<char,6>,f,f>);)")
	char hallo2[6] = "Hallo";
	
	// QUALIFIERS ////////////////////////////////////////////////////////////// 
	
	#pragma test expect_ir("var ref<int<4>,t,f> v0 = ref_cast(ref_var_init(5), type_lit(t), type_lit(f), type_lit(plain));")
	const int ci = 5;
	
	#pragma test expect_ir("var ref<int<4>,f,t> v0 = ref_cast(ref_var_init(5), type_lit(f), type_lit(t), type_lit(plain));")
	volatile int vi = 5;
	
	#pragma test expect_ir("var ref<int<4>,t,t> v0 = ref_cast(ref_var_init(5), type_lit(t), type_lit(t), type_lit(plain));")
	const volatile int cvi = 5;
		
	// POINTER TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<ptr<int<4>,f,f>,f,f> v1 = ref_var_init(ptr_from_ref(v0)); }")
	{
		int i;
		int* pi = &i;
	}
	#pragma test expect_ir("{ var ref<int<4>,t,f> v0; var ref<ptr<int<4>,t,f>,f,f> v1 = ref_var_init(ptr_from_ref(v0)); }")
	{
		const int ci;
		const int* pci = &ci;
	}
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<ptr<int<4>,t,f>,f,f> v1 = ref_var_init(ptr_cast(ptr_from_ref(v0), type_lit(t), type_lit(f))); }")
	{
		int i;
		const int* pci2 = &i;
	}

	// ARRAY TYPES /////////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<array<int<4>,5>,f,f> v0 = ref_var_init(array_create(type_lit(5), [1,2,3,4,5]));")
	int arr_all[5] = {1,2,3,4,5};
	
	#pragma test expect_ir("var ref<array<int<4>,5>,f,f> v0 = ref_var_init(array_create(type_lit(5), [1,2]));")
	int arr_partial[5] = {1,2};
	
	#pragma test expect_ir("var ref<array<int<4>,5>,f,f> v0 = ref_var_init(array_create(type_lit(5), [0]));")
	int arr_zero[5] = {0};
	
	#pragma test expect_ir("var ref<array<int<4>,3>,f,f> v0 = ref_var_init(array_create(type_lit(3), [0,1,2]));")
	int arr_implied[] = {0,1,2};
	
	#pragma test expect_ir("var ref<array<array<int<4>,3>,2>,f,f> v0 =",\
		"ref_var_init(array_create(type_lit(2), [array_create(type_lit(3), [1,2,3]), array_create(type_lit(3), [4,5,6])]));")
	int arr_multi[2][3] = {{1,2,3}, {4,5,6}};

	#pragma test expect_ir("var ref<array<array<int<4>,3>,2>,f,f> v0 =",\
		"ref_var_init(array_create(type_lit(2), [array_create(type_lit(3), [1]), array_create(type_lit(3), [4,5])]));")
	int arr_multi_partial[2][3] = {{1}, {4,5}};
	
	// STRUCT TYPES //////////////////////////////////////////////////////////////

	// basic
	#pragma test expect_ir("REGEX_S", R"(.*var ref<struct \{ a : int<4>; b : real<4>; \},f,f,plain> v0 = ref_var_init.*)")
	{ struct { int a; float b; } sif = { 1, 1.0f }; }

	// implicit
	#pragma test expect_ir("REGEX_S", R"(.*var ref<struct \{ a : int<4>; b : real<4>; c : uint<4>; \},f,f,plain> v0 =.*)")
	{ struct { int a; float b; unsigned c; } sifc = { .a = 1, .c = 2u }; }
	
	// explicit
	#pragma test expect_ir("REGEX_S", R"(.*var ref<struct \{ a : int<4>; b : real<4>; c : uint<4>; \},f,f,plain> v0 =.*)")
	{ struct { int a; float b; unsigned c; } sifc2 = { 1, 0.0f, 2u }; }
	
	// UNION TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expect_ir("REGEX_S", R"(.*var ref<union \{ a : int<4>; b : real<4>; \},f,f,plain> v0 = .*)")
	{ union { int a; float b; } uif = { 1 }; }


	#pragma test expect_ir("REGEX_S", R"(.*var ref<union \{ a : int<4>; b : real<4>; \},f,f,plain> v0 = .*)")
	{ union { int a; float b; } uif = { .a = 1 }; }
	
	#pragma test expect_ir("REGEX_S", R"(.*var ref<union \{ a : int<4>; b : real<4>; \},f,f,plain> v0 = .*)")
	{ union { int a; float b; } uif = { .b = 1.0f }; }
	
	// NESTED INITIALIZERS //////////////////////////////////////////////////////

	#pragma test expect_ir("REGEX_S", R"(.*ref<struct \{ is : struct \{ inner1 : int<4>; inner2 : real<4>; \}; iu : union \{ u1 : int<4>; u2 : real<4>; \}; \},f,f,plain> v0 = .*)")
	{ struct { struct { int inner1; float inner2; } is; union { int u1; float u2; } iu; } su = { { 1, 2.0f }, { 3 } }; }
	
	#pragma test expect_ir("REGEX_S", R"(.*ref<array<struct \{ a : int<4>; b : uint<4>;  \},2>,f,f,plain> v0 = .*)")
	{ struct { int a; unsigned b; } su[2] = { { 1, 2u }, { 3, 4u } }; }

	// BOOL CONVERSION //////////////////////////////////////////////////////
	
	#define BOOL_TO_INT "def bool_to_int : (b: bool) -> int<4> { if(b) {return 1;} else {return 0;} };"

	#pragma test expect_ir(BOOL_TO_INT,"{","var ref<int<4>,f,f> v0 = ref_var_init(bool_to_int(1<2)); }")
	{ int boolconv = 1 < 2; }
}
