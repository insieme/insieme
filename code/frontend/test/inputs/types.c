/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

// builtin types
void basic_type_test() {
	#pragma test "ref<int<4>> v1 = ref.var(1)"
	int a = 1;

	#pragma test "ref<int<8>> v1 = ref.var(0)"
	long b;

	#pragma test "ref<int<2>> v1 = ref.var(cast<int<2>>(0xFFFF))"
	short c = 0xFFFF;

	#pragma test "ref<char> v1 = ref.var('a')"
	char d = 'a';

	#pragma test "ref<anyRef> v1 = ref.var(null)"
	void* e;

	#pragma test "ref<real<4>> v1 = ref.var(0.00f)"
	float f = 0.00f;

	#pragma test "ref<real<8>> v1 = ref.var(0.0)"
	double g;
	
	#pragma test "ref<vector<real<4>,3>> v1 = ref.var(undefined(vector<real<4>,3>))"
	float v[3];

    #pragma test "ref<vector<vector<int<4>,2>,3>> v1 = ref.var(undefined(vector<vector<int<4>,2>,3>))"
	int vv[3][2];

	#pragma test "ref<vector<real<4>,3>> v1 = ref.var({cast<real<4>>(0),cast<real<4>>(0),cast<real<4>>(0)})"
	float vvv[3] = { 0, 0, 0 };

	#pragma test "ref<vector<vector<real<4>,1>,2>> v1 = ref.var({{cast<real<4>>(0)},{cast<real<4>>(0)}})"
	float vvvv[][1] = { {0}, {0} };

	#pragma test "ref<ref<array<int<4>,1>>> v1 = ref.var(get.null(array<int<4>,1>))"
	int* b1 = 0;

	#pragma test "ref<ref<array<ref<array<ref<array<int<4>,1>>,1>>,1>>> v1 = ref.var(get.null(array<ref<array<ref<array<int<4>,1>>,1>>,1>))"
	int*** c1 = 0;

	#pragma test "ref.assign(v1, get.null(array<ref<array<ref<array<int<4>,1>>,1>>,1>))"
	c1 = 0; 

	//#pragma test "ref<vector<char,10>> v1 = ref.var(\"Hello Mum\")"
	//char str[10] = "Hello Mum";
}

// Simple struct
#pragma test "struct<height:int<4>,age:int<4>>"
struct Person {
	// cppcheck-suppress unusedStructMember
	int height;
	// cppcheck-suppress unusedStructMember
	int age;
};

void test_func() {
	#pragma test \
	"ref<struct<height:int<4>,age:int<4>>> v1 = ref.var(undefined(struct<height:int<4>,age:int<4>>))"
	struct Person p;

	#pragma test \
	"ref<struct<height:int<4>,age:int<4>>> v1 = ref.var(struct{height=178, age=28})"
	struct Person p2 = {178, 28};
}

// Self recursive struct
#pragma test "rec 'PersonList.{'PersonList=struct<name:ref<array<char,1>>,age:int<4>,next:ref<array<'PersonList,1>>>}"
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

#pragma test "rec 'A.{'A=struct<b:ref<array<'B,1>>,c:ref<array<'C,1>>>,'B=struct<b:ref<array<'C,1>>>,'C=struct<a:ref<array<'A,1>>,b:ref<array<'B,1>>>}"
struct A {
	// cppcheck-suppress unusedStructMember
	struct B* b;
	// cppcheck-suppress unusedStructMember
	struct C* c;
};

#pragma test "rec 'B.{'A=struct<b:ref<array<'B,1>>,c:ref<array<'C,1>>>,'B=struct<b:ref<array<'C,1>>>,'C=struct<a:ref<array<'A,1>>,b:ref<array<'B,1>>>}"
struct B {
	// cppcheck-suppress unusedStructMember
	struct C* b;
};

#pragma test "rec 'C.{'A=struct<b:ref<array<'B,1>>,c:ref<array<'C,1>>>,'B=struct<b:ref<array<'C,1>>>,'C=struct<a:ref<array<'A,1>>,b:ref<array<'B,1>>>}"
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

#pragma test "struct<b:ref<array<rec 'B1.{'B1=struct<b:ref<array<'C1,1>>>,'C1=struct<b:ref<array<'B1,1>>,d:ref<array<struct<val:int<4>>,1>>>},1>>>"
struct A1 {
	// cppcheck-suppress unusedStructMember
	struct B1* b;
};

#pragma test "rec 'B1.{'B1=struct<b:ref<array<'C1,1>>>,'C1=struct<b:ref<array<'B1,1>>,d:ref<array<struct<val:int<4>>,1>>>}"
struct B1 {
	// cppcheck-suppress unusedStructMember
	struct C1* b;
};

#pragma test "rec 'C1.{'B1=struct<b:ref<array<'C1,1>>>,'C1=struct<b:ref<array<'B1,1>>,d:ref<array<struct<val:int<4>>,1>>>}"
struct C1 {
	// cppcheck-suppress unusedStructMember
	struct B1* b;
	// cppcheck-suppress unusedStructMember
	struct D1* d;
};

#pragma test "struct<val:int<4>>"
struct D1 {
	// cppcheck-suppress unusedStructMember
	int val;
};

void* malloc(int);

void mem_alloc() {

	#pragma test "ref<ref<array<int<4>,1>>> v1 = ref.var(ref.new(array.create.1D(int<4>, uint.div(4, sizeof(int<4>)))))"
	int* a = malloc(4);
	free(a); // make the static checks happy
}

enum E{ ON, OFF=10 };

void test_enum() {
	#pragma test "ref<int<4>> v1 = ref.var(0)"
	enum E a = ON;
	#pragma test "ref<int<4>> v1 = ref.var(10)"
	enum E b = OFF;
}


int add(int a, int b) { return a+b; }
int sub(int a, int b) { return a-b; }

void fun_ptr() {

	// test declaration, assignment and call of function pointers

	#pragma test "ref<((int<4>,int<4>)->int<4>)> v1 = ref.var(rec v4.{v4=fun(int<4> v2, int<4> v3) {return int.add(v2, v3);}})"
	int(* f)(int,int) = &add;

	#pragma test "ref.assign(v4, rec v3.{v3=fun(int<4> v1, int<4> v2) {return int.sub(v1, v2);}})"
	f = &sub;

	#pragma test "ref.deref(v1)(3, 4)"
	f(3,4);

}
