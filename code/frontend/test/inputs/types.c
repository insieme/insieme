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

#include <stdlib.h>

// builtin types
void basic_type_test() {
#pragma test expected "decl ref<int<4>> v0 = ( var(1))"
	int a = 1;

	#pragma test expected "decl ref<int<8>> v0 = ( var(undefined(type<int<8>>)))"
	long b;

	#pragma test expected "decl ref<int<2>> v0 = ( var(65535))"
	short c = 0xFFFF;

	#pragma test expected "decl ref<char> v0 = ( var('a'))"
	char d = 'a';

	#pragma test expected "decl ref<ref<any>> v0 = ( var(undefined(type<ref<any>>)))"
	void* e;

	#pragma test expected "decl ref<real<4>> v0 = ( var(0.0E+0))"
	float f = 0.00f;

	#pragma test expected "decl ref<real<8>> v0 = ( var(undefined(type<real<8>>)))"
	double g;

	#pragma test expected "decl ref<real<16>> v0 = ( var(undefined(type<real<16>>)))"
	long double h;

	#pragma test expected "decl ref<vector<real<4>,3>> v0 = ( var(undefined(type<vector<real<4>,3>>)))"
	float v[3];

	#pragma test expected "decl ref<vector<vector<int<4>,2>,3>> v0 = ( var(undefined(type<vector<vector<int<4>,2>,3>>)))"
	int vv[3][2];

	#pragma test expected "decl ref<vector<real<4>,3>> v0 = ( var([0.0f, 0.0f, 0.0f]))"
	float vvv[3] = {0, 0, 0};

	#pragma test expected "decl ref<vector<vector<real<4>,1>,2>> v0 = ( var([[0.0f], [0.0f]]))"
	float vvvv[][1] = {{0}, {0}};

	#pragma test expected "decl ref<ref<array<int<4>,1>>> v0 = ( var(ref_reinterpret(ref_null, type<array<int<4>,1>>)))"
	int* b1 = 0;

	#pragma test expected                                                                                                                                      \
	    "decl ref<ref<array<ref<array<ref<array<int<4>,1>>,1>>,1>>> v0 = ( var(ref_reinterpret(ref_null, type<array<ref<array<ref<array<int<4>,1>>,1>>,1>>)))"
	int*** c1 = 0;

	#pragma test expected "(v100 := ref_reinterpret(ref_null, type<array<ref<array<ref<array<int<4>,1>>,1>>,1>>))"
	c1 = 0;

	#pragma test expected "decl ref<real<8>> v0 = ( var(3.1415926535897931E+0))"
	double pi = 3.14159265358979323846;

	//	#pragma test expected "decl ref<vector<char,10>> v0 =  var("Hello Mum")"
	//	char str[10] = "Hello Mum";
}

// Simple struct
#pragma test expected "struct Person <height:int<4>,age:int<4>>"
struct Person {
	// cppcheck-suppress unusedStructMember
	int height;
	// cppcheck-suppress unusedStructMember
	int age;
};

void test_func() {
#pragma test expected "decl ref<struct Person <height:int<4>,age:int<4>>> v0 = ( var(undefined(type<struct Person <height:int<4>,age:int<4>>>)))"
	struct Person p;

	#pragma test expected "decl ref<struct Person <height:int<4>,age:int<4>>> v0 = ( var(struct{height:=178, age:=28}))"
	struct Person p2 = {178, 28};
}

// Self recursive struct
#pragma test expected "rec 'PersonList.{'PersonList=struct PersonList <name:ref<array<char,1>>,age:int<4>,next:ref<array<'PersonList,1>>>}"
struct PersonList {
	// cppcheck-suppress unusedStructMember
	char* name;
	// cppcheck-suppress unusedStructMember
	int age;
	// cppcheck-suppress unusedStructMember
	struct PersonList* next;
};

// Mutual recursive struct
struct A;
struct B;
struct C;

#pragma test expected                                                                                                                                          \
    "rec 'A.{'A=struct A <b:ref<array<'B,1>>,c:ref<array<'C,1>>>,'B=struct B <b:ref<array<'C,1>>>,'C=struct C <a:ref<array<'A,1>>,b:ref<array<'B,1>>>}"
struct A {
	// cppcheck-suppress unusedStructMember
	struct B* b;
	// cppcheck-suppress unusedStructMember
	struct C* c;
};

#pragma test expected                                                                                                                                          \
    "rec 'B.{'A=struct A <b:ref<array<'B,1>>,c:ref<array<'C,1>>>,'B=struct B <b:ref<array<'C,1>>>,'C=struct C <a:ref<array<'A,1>>,b:ref<array<'B,1>>>}"
struct B {
	// cppcheck-suppress unusedStructMember
	struct C* b;
};

#pragma test expected                                                                                                                                          \
    "rec 'C.{'A=struct A <b:ref<array<'B,1>>,c:ref<array<'C,1>>>,'B=struct B <b:ref<array<'C,1>>>,'C=struct C <a:ref<array<'A,1>>,b:ref<array<'B,1>>>}"
struct C {
	// cppcheck-suppress unusedStructMember
	struct A* a;
	// cppcheck-suppress unusedStructMember
	struct B* b;
};

// A tricky mutual struct example
struct A1;
struct B1;
struct C1;
struct D1;

#pragma test expected                                                                                                                                          \
    "struct A1 <b:ref<array<rec 'B1.{'B1=struct B1 <b:ref<array<'C1,1>>>,'C1=struct C1 <b:ref<array<'B1,1>>,d:ref<array<struct D1 <val:int<4>>,1>>>},1>>>"
struct A1 {
	// cppcheck-suppress unusedStructMember
	struct B1* b;
};

#pragma test expected "rec 'B1.{'B1=struct B1 <b:ref<array<'C1,1>>>,'C1=struct C1 <b:ref<array<'B1,1>>,d:ref<array<struct D1 <val:int<4>>,1>>>}"
struct B1 {
	// cppcheck-suppress unusedStructMember
	struct C1* b;
};

#pragma test expected "rec 'C1.{'B1=struct B1 <b:ref<array<'C1,1>>>,'C1=struct C1 <b:ref<array<'B1,1>>,d:ref<array<struct D1 <val:int<4>>,1>>>}"
struct C1 {
	// cppcheck-suppress unusedStructMember
	struct B1* b;
	// cppcheck-suppress unusedStructMember
	struct D1* d;
};

#pragma test expected "struct D1 <val:int<4>>"
struct D1 {
	// cppcheck-suppress unusedStructMember
	int val;
};

void mem_alloc() {
#pragma test expected "decl ref<ref<array<int<4>,1>>> v0 = ( var(ref_reinterpret(malloc(4ul), type<array<int<4>,1>>)))"
	int* a = malloc(4);
	free(a); // make the static checks happy
}


// enum E{ ON, OFF=10 };

// void test_enum() {
//
//	COMMENTED AS LONG AS THERE IS NOT A BETTER WAY TO HANDLE THIS
//
//	#pragma test expected "decl
// ref<__insieme_enum<_enum_home_luis_insieme_base_code_frontend_test__inputs_types_c1911_20,__insieme_enum_constant__<_enumCtnt_home_luis_insieme_base_code_frontend_test__inputs_types_c1919_9,0>,__insieme_enum_constant__<_enumCtnt_home_luis_insieme_base_code_frontend_test__inputs_types_c19113_17,10>>>
// v0 =  var(_enumCtnt_home_luis_insieme_base_code_frontend_test__inputs_types_c1919_9)"
//	enum E a = ON;
//	#pragma test expected "decl
// ref<__insieme_enum<_enum_home_luis_insieme_base_code_frontend_test__inputs_types_c1911_20,__insieme_enum_constant__<_enumCtnt_home_luis_insieme_base_code_frontend_test__inputs_types_c1919_9,0>,__insieme_enum_constant__<_enumCtnt_home_luis_insieme_base_code_frontend_test__inputs_types_c19113_17,10>>>
// v0 =  var(_enumCtnt_home_luis_insieme_base_code_frontend_test__inputs_types_c19113_17)"
//	enum E b = OFF;
//}

int add(int a, int b) {
	return a + b;
}
int sub(int a, int b) {
	return a - b;
}

void fun_ptr() {
// test declaration, assignment and call of function pointers

#pragma test expected "decl ref<(int<4>, int<4>) -> int<4>> v0 = ( var(fun(int<4> v1, int<4> v2) -> int<4> {return (v1+v2);}))"
	int (*f)(int, int) = &add;

	#pragma test expected "(v100 := fun(int<4> v1, int<4> v2) -> int<4> {return (v1-v2);})"
	f = &sub;

	#pragma test expected "(v100)(3, 4)"
	f(3, 4);
}
