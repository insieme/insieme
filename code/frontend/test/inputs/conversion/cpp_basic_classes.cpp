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

#include "cpp_basic_classes.h"

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
	lambda IMP_f = () -> real<4> { return lit("1.0E+0":real<4>); }
};)"

struct B {
	int i;
	float f() { return 1.0f; }
};

#define B_IR R"(
def struct IMP_B {
	i : int<4>;
	lambda IMP_f = () -> real<4> { return lit("1.0E+0":real<4>); }
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
	ctor() { x = 0u; }
	ctor(y : uint<4>) { x = y; }
};)"

// check member use within struct member function
struct D1 {
	void bla() {}
	void test() {
		bla();
	}
};

// check member use within struct member function, inverse order
struct D2 {
	void test() {
		bla();
	}
	void bla() {}
};


struct VolatileConstructor {
	VolatileConstructor() = default;
	VolatileConstructor(const VolatileConstructor&) {}
	VolatileConstructor(volatile const VolatileConstructor&) {}
};

#define VOL_CONSTR_IR R"(
def struct IMP_VolatileConstructor {
	ctor() = default;
	ctor(v : ref<IMP_VolatileConstructor,t,f,cpp_ref>) { }
	ctor(v : ref<IMP_VolatileConstructor,t,t,cpp_ref>) { }
};)"

void InHeader::f(int i) {
	i = 0;
}

struct ClassWithStaticMethod {
	static void bla() {}
};

struct ClassWithConstAndNonConstMethodInline {
	void bla() {}
	void bla() const {}
};

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(A_IR, R"( { var ref<IMP_A> a = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>))); } )")
	{ A a; }

	// init options
	#pragma test expect_ir(A_IR, R"( {
		var ref<IMP_A,f,f,plain> v0 = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
		var ref<IMP_A,f,f,plain> v1 = ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref));
		var ref<IMP_A,f,f,plain> v2 = ref_cast(IMP_A::(ref_temp(type_lit(IMP_A)), ref_kind_cast(v0, type_lit(cpp_ref))), type_lit(f), type_lit(f), type_lit(cpp_rref));
	} )")
	{
		A a;
		A b = a;
		A c = A(a);
	}

	// non-default implicit init
	#pragma test expect_ir(VOL_CONSTR_IR, R"( {
		var ref<IMP_VolatileConstructor,f,t,plain> v0 = IMP_VolatileConstructor::(ref_cast(ref_decl(type_lit(ref<IMP_VolatileConstructor,f,t,plain>)), type_lit(f), type_lit(f), type_lit(plain)));
		var ref<IMP_VolatileConstructor,f,f,plain> v1 = ref_cast(v0, type_lit(t), type_lit(t), type_lit(cpp_ref));
		var ref<IMP_VolatileConstructor,f,f,plain> v2 = ref_cast(IMP_VolatileConstructor::(ref_temp(type_lit(IMP_VolatileConstructor)), ref_kind_cast(v0, type_lit(cpp_ref))), type_lit(t), type_lit(f), type_lit(cpp_ref));
	} )")
	{
		volatile VolatileConstructor a;
		VolatileConstructor b = a;
		VolatileConstructor c = VolatileConstructor(a);
	}

	// method call
	#pragma test expect_ir(A_IR, R"( { var ref<IMP_A> a = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>))); a.IMP_f(); } )")
	{
		A a;
		a.f();
	}

	// method call using pointer
	#pragma test expect_ir(A_IR, R"({
		var ref<IMP_A,f,f,plain> v0 = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
		var ref<ptr<IMP_A>,f,f,plain> v1 = ptr_from_ref(v0);
		ptr_to_ref(*v1).IMP_f();
	})")
	{
		A a, *b = &a;
		b->f();
	}

	#pragma test expect_ir(B_IR,R"( { var ref<IMP_B> b = IMP_B::(ref_decl(type_lit(ref<IMP_B>))); b.IMP_f(); } )")
	{
		B b;
		b.f();
	}

	#pragma test expect_ir(C_IR,R"( { var ref<IMP_C> c1 = IMP_C::(ref_decl(type_lit(ref<IMP_C>))); } )")
	{
		C c1;
	}

	#pragma test expect_ir(C_IR,R"( { var ref<IMP_C> c = IMP_C::(ref_decl(type_lit(ref<IMP_C>)), 6u); } )")
	{
		C c2(6u);
	}

	{
		D1 d1;
	}

	{
		D2 d2;
	}

	#pragma test expect_ir(R"(
	decl struct IMP_InHeader;
	decl IMP_f:IMP_InHeader::(int<4>) -> unit;
	def struct IMP_InHeader {
		function IMP_f = (v1 : ref<int<4>,f,f,plain>) -> unit {
			v1 = 0;
		}
	};
	{
		var ref<int<4>,f,f,plain> v0;
		var ref<IMP_InHeader,f,f,plain> v1 = IMP_InHeader::(ref_decl(type_lit(ref<IMP_InHeader>)));
		v1.IMP_f(*v0);
	})")
	{
		int x;
		InHeader a;
		a.f(x);
	}

	#pragma test expect_ir(R"(
	def IMP_ClassWithStaticMethod_colon__colon_bla = function () -> unit { };
	{
		IMP_ClassWithStaticMethod_colon__colon_bla();
	})")
	{
		ClassWithStaticMethod::bla();
	}

	#pragma test expect_ir(R"(
		def struct IMP_ClassWithConstAndNonConstMethodInline {
			function IMP_bla = () -> unit { }
			const function IMP_bla = () -> unit { }
		};
		{
			var ref<IMP_ClassWithConstAndNonConstMethodInline,f,f,plain> v0 = IMP_ClassWithConstAndNonConstMethodInline::(ref_decl(type_lit(ref<IMP_ClassWithConstAndNonConstMethodInline,f,f,plain>)));
			v0.IMP_bla();
			var ref<IMP_ClassWithConstAndNonConstMethodInline,t,f,cpp_ref> v1 = v0;
			v1.IMP_bla();
		}
	)")
	{
		ClassWithConstAndNonConstMethodInline c;
		c.bla();
		const ClassWithConstAndNonConstMethodInline& cr = c;
		cr.bla();
	}

	return 0;
}
