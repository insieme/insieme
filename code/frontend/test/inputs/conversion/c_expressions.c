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
 *
 */
void nameCheck() {
	#pragma test expect_ir(R"({ ptr_from_array("nameCheck"); 1; })")
	{ __func__; 1; }
	#pragma test expect_ir("EXPR_TYPE",R"(ptr<char,t,f>)")
	__func__;
}

typedef struct { int i; } simple_struct;

simple_struct generate_struct() { return (simple_struct){0}; };

int main() {
	nameCheck();

	//===-------------------------------------------------------------------------------------------------------------------------------- UNARY OPERATORS ---===

	#pragma test expect_ir("int_not(3)")
	~3;

	#pragma test expect_ir("!(3!=0)")
	!3;

	#pragma test expect_ir("3")
	+3;

	#pragma test expect_ir("-3")
	-3;

	#pragma test expect_ir("{ var ref<int<4>,f,f> v1 = 0; ptr_from_ref(v1); }")
	{
		int x = 0;
		&x;
	}

	#pragma test expect_ir("{ var ref<ptr<int<4>,f,f>,f,f> v0; ptr_to_ref(*v0); }")
	{
		int* x;
		*x;
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v1 = 0; 0-v1; }")
	{
		int x = 0;
		-x;
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v1 = 0; gen_pre_inc(v1); }")
	{
		int v = 0;
		++v;
	}

	#pragma test expect_ir("{ var ref<uint<2>,f,f> v1 = num_cast(0, type_lit(uint<2>)); gen_post_inc(v1); }")
	{
		unsigned short v = 0;
		v++;
	}

	#pragma test expect_ir("{ var ref<char,f,f> v1 = num_cast(0, type_lit(char)); gen_pre_dec(v1); }")
	{
		char v = 0;
		--v;
	}

	#pragma test expect_ir("{ var ref<int<1>,f,f> v1 = num_cast(0, type_lit(int<1>)); gen_post_dec(v1); }")
	{
		signed char v = 0;
		v--;
	}

	//===------------------------------------------------------------------------------------------------------------------------------- BINARY OPERATORS ---===

	// COMMA OPERATOR //////////////////////////////////////////////////////////////

	#pragma test expect_ir("{ comma_operator(() -> int<4> { return 2; }, () -> int<4> { return 3; }); }")
	{ 2, 3; }
	#pragma test expect_ir("EXPR_TYPE", "int<4>")
	2, 3;

	#pragma test expect_ir("{ comma_operator(() -> int<4> { return comma_operator(() -> int<4> { return 2; }, () -> int<4> { return 3; }); }, () -> int<4> { return 4; }); }")
	{ 2, 3, 4; }

	#pragma test expect_ir("{ comma_operator(() -> int<4> { return 2; }, () -> real<8> { return lit(\"3.0E+0\":real<8>); }); }")
	{ 2, 3.0; }
	#pragma test expect_ir("EXPR_TYPE", "real<8>")
	2, 3.0;

	// MATH //////////////////////////////////////////////////////////////

	#pragma test expect_ir("int_add(1, 2)")
	1 + 2;

	#pragma test expect_ir("int_sub(3, 4)")
	3 - 4;

	#pragma test expect_ir("int_mul(5, 6)")
	5 * 6;

	#pragma test expect_ir("int_div(7, 8)")
	7 / 8;

	#pragma test expect_ir("int_mod(9, 10)")
	9 % 10;

	// BITS //////////////////////////////////////////////////////////////

	#pragma test expect_ir("int_lshift(11, 12)")
	11 << 12;

	#pragma test expect_ir("int_rshift(13, 14)")
	13 >> 14;

	#pragma test expect_ir("int_lshift(11, 12l)")
	11 << 12l;

	#pragma test expect_ir("int_rshift(13, 14ul)")
	13 >> 14ul;

	#pragma test expect_ir("int_and(15, 16)")
	15 & 16;

	#pragma test expect_ir("int_xor(17, 18)")
	17 ^ 18;

	#pragma test expect_ir("int_or(19, 20)")
	19 | 20;

	// LOGICAL ////////////////////////////////////////////////////////////

	#pragma test expect_ir("(0!=0) || (1!=0)")
	0 || 1;

	#pragma test expect_ir("(1!=0) && (0!=0)")
	1 && 0;

	// COMPARISON /////////////////////////////////////////////////////////

	#pragma test expect_ir("int_eq(1, 2)")
	1 == 2;

	#pragma test expect_ir("int_ne(1, 2)")
	1 != 2;

	#pragma test expect_ir("real_ne(lit(\"1.0E+0\":real<8>), lit(\"2.0E+0\":real<8>))")
	1.0 != 2.0;

	#pragma test expect_ir("int_lt(1, 2)")
	1 < 2;

	#pragma test expect_ir("int_gt(1, 2)")
	1 > 2;

	#pragma test expect_ir("int_le(1, 2)")
	1 <= 2;

	#pragma test expect_ir("int_ge(1, 2)")
	1 >= 2;

	// WITH DIFFERENT TYPES ///////////////////////////////////////////////////

	// this should work: num_cast(1, type_lit(real<8>))+2.0E+0
	#pragma test expect_ir("num_cast(1, type_lit(real<8>))+lit(\"2.0E+0\":real<8>)")
	1 + 2.0;

	#pragma test expect_ir("lit(\"3.0E+0\":real<8>)-num_cast(4, type_lit(real<8>))")
	3.0 - 4;

	#pragma test expect_ir("lit(\"5.0E+0\":real<4>)*num_cast(6, type_lit(real<4>))")
	5.0f * 6;

	#pragma test expect_ir("7u/num_cast(8, type_lit(uint<4>))")
	7u / 8;

	#pragma test expect_ir("9u+num_cast(10, type_lit(uint<4>))")
	9u + 10;

	// POINTER & ARRAYS ///////////////////////////////////////////////////////

	// one dimension

	#pragma test expect_ir("{ var ref<array<int<4>,5>,f,f> v0; ptr_subscript(ptr_from_array(v0), 1); }")
	{
		int a[5];
		a[1];
	}

	#pragma test expect_ir("{ var ref<array<int<4>,5>,f,f> v0; ptr_subscript(ptr_from_array(v0), num_cast(1ull, type_lit(int<8>))); }")
	{
		int a[5ull];
		a[1ull];
	}

	#pragma test expect_ir("{ var ref<array<int<4>,5>,f,f> v0; ptr_subscript(ptr_from_array(v0), -1); }")
	{
		int a[5];
		a[-1];
	}

	#pragma test expect_ir("{ var ref<array<int<4>,5>,f,f> v0; ptr_subscript(ptr_from_array(v0), 1); }")
	{
		int a[5];
		1[a];
	}

	#pragma test expect_ir("{ var ref<array<int<4>,1>,f,f> v0; ptr_to_ref(ptr_from_array(v0)); }")
	{
		int a[1];
		*a;
	}

	#pragma test expect_ir("{ var ref<ptr<int<4>,f,f>,f,f> v0; ptr_from_ref(v0); }")
	{
		int* a;
		&a;
	}

	#pragma test expect_ir("{ var ref<array<int<4>,5>,f,f> v0; ptr_from_ref(v0); }")
	{
		int a[5];
		&a;
	}

	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; ptr_add(*v0, 5); }")
	{
		void* a;
		a+5;
	}

	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; ptr_add(*v0, num_cast(5ull, type_lit(int<8>))); }")
	{
		void* a;
		a+5ull;
	}

	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; ptr_add(*v0, 5); }")
	{
		void* a;
		5+a;
	}

	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; ptr_post_inc(v0); ptr_post_dec(v0); ptr_pre_inc(v0); ptr_pre_dec(v0); }")
	{
		void* a;
		a++;
		a--;
		++a;
		--a;
	}

	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; ptr_sub(*v0, 5); }")
	{
		void* a;
		a-5;
	}

	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; var ref<ptr<unit,f,f>,f,f> v1; ptr_diff(*v0, *v1); }")
	{
		void *a, *b;
		a-b;
	}

	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; ptr_gt(*v0,*v0); ptr_lt(*v0,*v0); ptr_le(*v0,*v0); ptr_ge(*v0,*v0); }")
	{
		void* a;
		a>a;
		a<a;
		a<=a;
		a>=a;
	}

	// multidimensional

	#pragma test expect_ir("{ var ref<array<array<int<4>,3>,2>,f,f> v0; ptr_subscript(ptr_from_array(ptr_subscript(ptr_from_array(v0), 1)), 2); }")
	{
		int a[2][3];
		a[1][2];
	}

	// note: there are no rvalue arrays in C!
	#pragma test expect_ir("{ var ref<array<array<int<4>,3>,2>,f,f> v0; ptr_from_array(ptr_subscript(ptr_from_array(v0), 1)); }")
	{
		int a[2][3];
		a[1];
	}

	// COMPOUND //////////////////////////////////////////////////////////////

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_add(v1, 1); }")
	{
		int a = 1;
		a += 1;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_subtract(v1, 2); }")
	{
		int a = 1;
		a -= 2;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_divide(v1, 1); }")
	{
		int a = 1;
		a /= 1;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_multiply(v1, 5); }")
	{
		int a = 1;
		a *= 5;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_modulo(v1, 5); }")
	{
		int a = 1;
		a %= 5;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_bitwise_and(v1, 5); }")
	{
		int a = 1;
		a &= 5;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_bitwise_or(v1, 5); }")
	{
		int a = 1;
		a |= 5;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<char,f,f> v1 = num_cast(0, type_lit(char)); comp_assign_bitwise_or(v1, 1); }")
	{
		char a = 0;
		a |= 1;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_bitwise_xor(v1, 5); }")
	{
		int a = 1;
		a ^= 5;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_left_shift(v1, 5); }")
	{
		int a = 1;
		a <<= 5;
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; { var ref<int<4>,f,f> v1 = 1; comp_assign_right_shift(v1, 5); }")
	{
		int a = 1;
		a >>= 5;
	}

	// ASSIGNMENT //////////////////////////////////////////////////////////////

	#pragma test expect_ir("{ var ref<int<4>,f,f> v1; v1 = 5; }")
	{
		int a;
		a = 5;
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<int<4>,f,f> v1; v0 = c_style_assignment(v1, 1); }")
	{
		int a, b;
		a = b = 1;
	}

	//===------------------------------------------------------------------------------------------------------------------------------- TERNARY OPERATOR ---===

	#pragma test expect_ir("(1!=0)?2:3")
	1?2:3;

	//===------------------------------------------------------------------------------------------------------------------------------------ MEMBER EXPR ---===

	#pragma test expect_ir("REGEX_S", R"(.*var ref<struct \{ i : int<4>; \},f,f,plain> v0 =.*)")
	{
		struct {
			int i;
		} ts;
		ts.i;
	}

	#pragma test expect_ir("REGEX_S", R"(.*var ref<union \{ i : int<4>; \},f,f,plain> v0 =.*)")
	{
		union {
			int i;
		} tu;
		tu.i;
	}

	#pragma test expect_ir("REGEX_S", R"(.*var ref<ptr<struct \{ i : int<4>; \}>,f,f,plain> v0 =.*)")
	{
		struct {
			int i;
		} *ts;
		ts->i;
	}

	#pragma test expect_ir("REGEX_S", R"(.*var ref<ptr<union \{ i : int<4>; \}>,f,f,plain> v0 =.*)")
	{
		union {
			int i;
		} *ts;
		ts->i;
	}

	// check direct R-value access
	#pragma test expect_ir(R"(
		def struct IMP_simple_struct { i: int<4>; };
		def IMP_generate_struct = () -> IMP_simple_struct { return *<ref<IMP_simple_struct>>(ref_temp(type_lit(IMP_simple_struct))) {0} in IMP_simple_struct; };
		IMP_generate_struct().i+5
	)")
	generate_struct().i + 5;

	//===---------------------------------------------------------------------------------------------------------------------------------- MISCELLANEOUS ---===

	#pragma test expect_ir("sizeof(type_lit(real<8>))")
	sizeof(double);

	#pragma test expect_ir("sizeof(type_lit(char))")
	sizeof(char);

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; sizeof(type_lit(int<4>)); }")
	{
		int sizeof_int;
		sizeof(sizeof_int);
	}

	#pragma test expect_ir("{ var ref<array<char,8>,f,f> v0; sizeof(type_lit(array<char,8>)); }")
	{
		char char_arr[8];
		sizeof(char_arr);
	}

	// completely weird but possible compound init expr that can be assigned to
	typedef struct {
		unsigned data;
		int x, y;
	} Image;

	#pragma test expect_ir(R"(
		def struct __any_string__ {
			data : uint<4>;
			x : int<4>;
			y : int<4>;
		};
		{
			<ref<__any_string__,f,f,plain>>(ref_temp(type_lit(__any_string__))) {0u, 0, 0} = *<ref<__any_string__,f,f,plain>>(ref_temp(type_lit(__any_string__))) {1u, 1, 1};
		}
	)")
	{
		(Image){0u, 0, 0} = (Image){1u, 1, 1};
	}

	#pragma test expect_ir(R"(
		def struct __any_string__ {
			data : uint<4>;
			x : int<4>;
			y : int<4>;
		};
		{
			<ref<__any_string__,f,f,plain>>(ref_temp(type_lit(__any_string__))) {0u, 0, 0}.x = 1;
		}
	)")
	{
		(Image){0u, 0, 0}.x = 1;
	}

	int y = (Image){0u, 0, 0}.y;

	// bool to int conversion
	#pragma test expect_ir("{ bool_to_int(1<5)+17; }")
	{ (1 < 5) + 17; }

	// GNU statement expressions
	#pragma test expect_ir("REGEX_S", R"(decl .* : \(\) -> int<4>; def .* = function \(\) -> int<4> \{ var ref<int<4>,f,f,plain> v0 = 1; return \*v0; \}; .*\(\))")
	({ int x = 1; x; });

	#pragma test expect_ir("REGEX_S", R"(decl .* : \(\) -> int<8>; def .* = function \(\) -> int<8> \{ var ref<int<8>,f,f,plain> v0 = 5l; var ref<int<8>,f,f,plain> v1 = 2l; return \*v0\+\*v1; \}; .*\(\))")
	({ long x = 5L; long y = 2L; x+y; });

	#pragma test expect_ir("REGEX_S", R"(decl .* : \(ref<int<4>,f,f,plain>\) -> int<4>; def .* = function \(v0 : ref<ref<int<4>,f,f,plain>,f,f,plain>\) -> int<4> \{ var ref<int<4>,f,f,plain> v1 = 2; return \*\*v0\+\*v1; \}; \{ var ref<int<4>,f,f,plain> v0 = 5; .*\(v0\); \})")
	{
		int x = 5;
		({ int y = 2; x+y; });
	}
}
