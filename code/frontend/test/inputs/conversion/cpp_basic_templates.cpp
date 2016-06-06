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

template <typename T>
class tempClass {
	T val;
};

int main() {
	;

	#pragma test expect_ir(R"(
		def IMP_plusOne_unsigned_int_returns_unsigned_int = (v1 : uint<4>) -> uint<4> { return v1+num_cast(1, type_lit(uint<4>)); };
		IMP_plusOne_unsigned_int_returns_unsigned_int(1u))")
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

	return 0;
}
