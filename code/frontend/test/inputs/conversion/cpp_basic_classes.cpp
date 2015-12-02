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

struct A {
	int i;
	float f();
};

float A::f() {
	return 1.0f;
}

#define A_IR R"(
def struct IMP_A {
	i : int<4>;
	lambda IMP_f : () -> real<4> { return lit("1.0E+0":real<4>); }
};)"

struct B {
	int i;
	float f() { return 1.0f; }
};

#define B_IR R"(
def struct IMP_B {
	i : int<4>;
	lambda IMP_f : () -> real<4> { return lit("1.0E+0":real<4>); }
};)"

struct C {
	unsigned x;
	C() {
		x = 0u;
	}
	C(unsigned y) {
		x = y;
	}
};

#define C_IR R"(
def struct IMP_C {
	x : uint<4>;
	ctor() { cxx_style_assignment(x, 0u); }
	ctor(y : uint<4>) { cxx_style_assignment(x, y); }
};)"

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(A_IR, R"( { var ref<IMP_A> a = IMP_A::(ref_var(type_lit(IMP_A))); } )")
	{ A a; }

	#pragma test expect_ir(A_IR, R"( { var ref<IMP_A> a = IMP_A::(ref_var(type_lit(IMP_A))); a.IMP_f(); } )")
	{
		A a;
		a.f();
	}
	
	#pragma test expect_ir(B_IR,R"( { var ref<IMP_B> b = IMP_B::(ref_var(type_lit(IMP_B))); b.IMP_f(); } )")
	{ 
		B b;
		b.f();
	}
	
	#pragma test expect_ir(C_IR,R"( { var ref<IMP_C> c1 = IMP_C::(ref_var(type_lit(IMP_C))); } )")
	{
		C c1;
	}
	
	#pragma test expect_ir(C_IR,R"( { var ref<IMP_C> c = IMP_C::(ref_var(type_lit(IMP_C)), 6u); } )")
	{
		C c2(6u);
	}

	return 0;
}
