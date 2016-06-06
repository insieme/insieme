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


class TrivialConstructorExpr {
	int a;
	float b;
	unsigned c = 1u;
	const int d;
public:
	TrivialConstructorExpr() : a(5), d(42) {
		c = 7u;
	}

	TrivialConstructorExpr(int x) : a(x), b(3.0f), d(23) {
	}
};

#define TRIVIAL_CONSTRUCTOR_EXPR_IR R"(
def struct IMP_TrivialConstructorExpr {
    a : int<4>;
    b : real<4>;
    c : uint<4>;
    d : int<4>;
    ctor function () {
        <ref<int<4>,f,f,plain>>(this.a) {5};
        <ref<uint<4>,f,f,plain>>(this.c) {1u};
        <ref<int<4>,f,f,plain>>(this.d) {42};
        (this).c = 7u;
    }
    ctor function (v1 : ref<int<4>,f,f,plain>) {
        <ref<int<4>,f,f,plain>>(this.a) {*v1};
        <ref<real<4>,f,f,plain>>(this.b) {3.0E+0f};
        <ref<uint<4>,f,f,plain>>(this.c) {1u};
        <ref<int<4>,f,f,plain>>(this.d) {23};
    }
};
)"

struct NonTrivial {
	int a;
	virtual ~NonTrivial() { 42; }
	NonTrivial() { }
	NonTrivial(int i) : a(i) {  }
};

class NonTrivialConstructorExpr {
	NonTrivial x;
	NonTrivial y {7};
public:
	NonTrivialConstructorExpr() {}
	NonTrivialConstructorExpr(int i) : x(i) {}
};

#define NONTRIVIAL_CONSTRUCTOR_EXPR_IR R"(
def struct IMP_NonTrivial {
    a : int<4>;
    ctor function () { }
    ctor function (v1 : ref<int<4>,f,f,plain>) {
        <ref<int<4>,f,f,plain>>((this).a) {*v1};
    }
    dtor virtual function () {
        42;
    }
};
def struct IMP_NonTrivialConstructorExpr {
    x : IMP_NonTrivial;
    y : IMP_NonTrivial;
    ctor function () {
        IMP_NonTrivial::((this).x);
        IMP_NonTrivial::((this).y, 7);
    }
    ctor function (v1 : ref<int<4>,f,f,plain>) {
        IMP_NonTrivial::((this).x, *v1);
        IMP_NonTrivial::((this).y, 7);
    }
};
)"

class ChainedConstructor {
	int a;
	float b;
public:
	ChainedConstructor(int a) : a(a), b(2.0f) {
	}

	ChainedConstructor() : ChainedConstructor(5) {
	}
};

#define CHAINED_CONSTRUCTOR_EXPR_IR R"(
decl ctor:IMP_ChainedConstructor::(int<4>);
def struct IMP_ChainedConstructor {
    a : int<4>;
    b : real<4>;
    ctor function () {
        IMP_ChainedConstructor::(this, 5);
    }
    ctor function (v1 : ref<int<4>,f,f,plain>) {
        <ref<int<4>,f,f,plain>>((this).a) {*v1};
        <ref<real<4>,f,f,plain>>((this).b) {2.0E+0f};
    }
};)"



struct Base {
	int a;
	Base(int a) : a(a) {}
};

struct Derived : public Base {
	Derived() : Base(5) {}
};

#define BASE_CONSTRUCTOR_EXPR_IR R"(
def struct IMP_Base {
    a : int<4>;
    ctor function (v1 : ref<int<4>,f,f,plain>) {
        <ref<int<4>,f,f,plain>>((this).a) {*v1};
    }
};
def struct IMP_Derived: [ public IMP_Base ] {
    ctor function () {
        IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)), 5);
    }
};)"


int main() {

	#pragma test expect_ir(TRIVIAL_CONSTRUCTOR_EXPR_IR,R"(var ref<IMP_TrivialConstructorExpr,f,f,plain> v0 = IMP_TrivialConstructorExpr::(ref_decl(type_lit(ref<IMP_TrivialConstructorExpr,f,f,plain>)));)")
	TrivialConstructorExpr default_constructed;

	#pragma test expect_ir(TRIVIAL_CONSTRUCTOR_EXPR_IR,R"(var ref<IMP_TrivialConstructorExpr,f,f,plain> v0 = IMP_TrivialConstructorExpr::(ref_decl(type_lit(ref<IMP_TrivialConstructorExpr,f,f,plain>)), 19);)")
	TrivialConstructorExpr nondefault_constructed(19);


	#pragma test expect_ir(NONTRIVIAL_CONSTRUCTOR_EXPR_IR, R"(var ref<IMP_NonTrivialConstructorExpr,f,f,plain> v0 = IMP_NonTrivialConstructorExpr::(ref_decl(type_lit(ref<IMP_NonTrivialConstructorExpr,f,f,plain>)));)")
	NonTrivialConstructorExpr nt_default_constructed;

	#pragma test expect_ir(NONTRIVIAL_CONSTRUCTOR_EXPR_IR, R"(var ref<IMP_NonTrivialConstructorExpr,f,f,plain> v0 = IMP_NonTrivialConstructorExpr::(ref_decl(type_lit(ref<IMP_NonTrivialConstructorExpr,f,f,plain>)), 28);)")
	NonTrivialConstructorExpr nt_nondefault_constructed(28);


	#pragma test expect_ir(CHAINED_CONSTRUCTOR_EXPR_IR, R"(var ref<IMP_ChainedConstructor,f,f,plain> v0 = IMP_ChainedConstructor::(ref_decl(type_lit(ref<IMP_ChainedConstructor,f,f,plain>)));)")
	ChainedConstructor chained_constructed;


	#pragma test expect_ir(BASE_CONSTRUCTOR_EXPR_IR, R"(var ref<IMP_Derived,f,f,plain> v0 = IMP_Derived::(ref_decl(type_lit(ref<IMP_Derived,f,f,plain>)));)")
	Derived derived_constructed;

	return 0;
}
