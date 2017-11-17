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

template <typename T>
T plusOne(T v) {
	return v + 1;
}

template <int val>
int fib() {
	return fib<val - 1>() + fib<val - 2>();
}
template <>
int fib<0>() {
	return 1;
}
template <>
int fib<1>() {
	return 1;
}

template <int m, int n>
int bla() {
	return bla<m - 1, n - 1>() + 1;
}
template <>
int bla<0, 1>() {
	return 2;
}

template <int N>
int giveN() {
	return N;
}

template <typename T>
void takeT(const T& t) {}

template <typename T>
class tempClass {
	T val;
};

int main() {
	;

	#pragma test expect_ir(R"(
		def IMP_plusOne_unsigned_space_int_returns_unsigned_space_int = (v1 : uint<4>) -> uint<4> { return v1+num_cast(1, type_lit(uint<4>)); };
		IMP_plusOne_unsigned_space_int_returns_unsigned_space_int(1u))")
	plusOne(1u);
	#pragma test expect_ir(R"(
		def IMP_plusOne_int_returns_int = (v1 : int<4>) -> int<4> { return v1+1; };
		IMP_plusOne_int_returns_int(1))")
	plusOne(1);
	#pragma test expect_ir(R"(
		def IMP_plusOne_double_returns_double = (v1 : real<8>) -> real<8> { return v1+num_cast(1, type_lit(real<8>)); };
		IMP_plusOne_double_returns_double(lit("1.0E+0":real<8>)))")
	plusOne(1.0);

	#pragma test expect_ir(R"(
		def IMP_fib_0 = () -> int<4> { return 1; };
		def IMP_fib_1 = () -> int<4> { return 1; };
		def IMP_fib_2_returns_int = () -> int<4> { return IMP_fib_1()+IMP_fib_0(); };
		def IMP_fib_3_returns_int = () -> int<4> { return IMP_fib_2_returns_int()+IMP_fib_1(); };
		def IMP_fib_4_returns_int = () -> int<4> { return IMP_fib_3_returns_int()+IMP_fib_2_returns_int(); };
		IMP_fib_4_returns_int())")
	fib<4>();

	#pragma test expect_ir(R"(
		def IMP_bla_0_1 = () -> int<4> { return 2; };
		def IMP_bla_1_2_returns_int = () -> int<4> { return IMP_bla_0_1()+1; };
		def IMP_bla_2_3_returns_int = () -> int<4> { return IMP_bla_1_2_returns_int()+1; };
		IMP_bla_2_3_returns_int())")
	bla<2, 3>();

	#pragma test expect_ir(R"(
		decl struct IMP_tempClass_int;
		decl IMP_tempClass_int::val:int<4>;
		def struct IMP_tempClass_int {
			val : int<4>;
		};
		var ref<IMP_tempClass_int,f,f,plain> v0 = IMP_tempClass_int::(ref_decl(type_lit(ref<IMP_tempClass_int,f,f,plain>)));
	)")
	tempClass<int> aInt;

	#pragma test expect_ir(R"(
		decl struct IMP_tempClass_float;
		decl IMP_tempClass_float::val:real<4>;
		def struct IMP_tempClass_float {
			val : real<4>;
		};
		var ref<IMP_tempClass_float,f,f,plain> v0 = IMP_tempClass_float::(ref_decl(type_lit(ref<IMP_tempClass_float,f,f,plain>)));
	)")
	tempClass<float> aFloat;

	#pragma test expect_ir(R"(def IMP_giveN_5_returns_int = function () -> int<4> {
		return 5;
	};
	{
		IMP_giveN_5_returns_int();
	})")
	{
		giveN<5>();
	}

	#pragma test expect_ir(R"(
		def IMP_takeT_char_space__lbracket_5_rbracket__returns_void = function (v0 : ref<array<char,5u>,t,f,cpp_ref>) -> unit { };
		def IMP_takeT_int_space__lbracket_3_rbracket__returns_void = function (v0 : ref<array<int<4>,3u>,t,f,cpp_ref>) -> unit { };
		{
			IMP_takeT_char_space__lbracket_5_rbracket__returns_void(ref_kind_cast(lit(""test"" : ref<array<char,5u>,t,f,plain>), type_lit(cpp_ref)));
			var ref<array<int<4>,3u>,f,f,plain> v0 = <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1, 2, 3};
			IMP_takeT_int_space__lbracket_3_rbracket__returns_void(ref_kind_cast(v0, type_lit(cpp_ref)));
		}
	)")
	{
		takeT("test");
		int t[3] = {1, 2, 3};
		takeT(t);
	}

	return 0;
}
