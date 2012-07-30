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

// ignore warnings
#pragma GCC diagnostic ignored "-Wall"

void init_class() {
	#pragma test \
	"struct<a:int<4>,b:int<4>,c:int<4>>"
	class TheClass{
		public:
		int a;
		int b;
		int c;
	};

	#pragma test \
		"{ decl ref<struct<a:int<4>,b:int<4>,c:int<4>>> v1 = ( var(undefined(type<struct<a:int<4>,b:int<4>,c:int<4>>>))); fun(ref<struct<a:int<4>,b:int<4>,c:int<4>>> v2){ { }; return v2; }(v1);}"
	TheClass a;
}

//	#pragma test "struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>"
class C {
public:
	int m_a;
	int m_c;
	int m_b;
	C() : m_a(1), m_b(2) { m_c = m_a + m_b; }
	C(const C& c) { m_a = c.m_a; m_b = c.m_b; m_c = c.m_c; }
	C(int a) { m_a = a; m_b = 2; m_c = m_a + m_b; }
	C(int a, int b, int c = 3) { m_a=a; m_b=b; m_c=c; }
};

//#pragma test "fun(){ decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v1 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))); fun(ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v2){ ((v2->m_a) := 1); ((v2->m_b) := 2); { ((v2->m_c) := (( *(v2->m_a))+( *(v2->m_b)))); }; }(v1);}"
//"decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v3 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))) fun (ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v5, ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>> v6) { ((v5->m_a) := ( *(v6->m_a)); ((v5->m_b) := ( *(v6->m_b)); ((v5->m_c) := ( *(v6->m_c)); }(v1, v4);"
// #pragma test "fun(){ decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v1 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))); fun(ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v2){ ((v2->m_a) := 1); ((v2->m_b) := 2); { ((v2->m_c) := (( *(v2->m_a))+( *(v2->m_b)))); }; }(v1); decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v4 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))); fun(ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v5, ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v6){ { ((v6->m_a) := ( *(v5->m_a))); ((v6->m_b) := ( *(v5->m_b))); ((v6->m_c) := ( *(v5->m_c))); }; }(v1, v4);}"
void constr_class() {
	//"decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v1 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))) \
	//fun(ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v2){ \
	//	((v2->m_a) := 1); \
	//	((v2->m_b) := 2); \
	//	{ \
	//		((v2->m_c) := (( *(v2->m_a))+( *(v2->m_b)))); \
	//	}; \
	//}(v1);"
	C c1;

//	"decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v3 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))) \
//	fun (ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v5, ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>> v6) { \
//		 ((v6->m_a) := ( *(v5->m_a)); \
//	 	 ((v6->m_b) := ( *(v5->m_b)); \
//	 	 ((v6->m_c) := ( *(v5->m_c)); \
//	 }(v1, v3);"
	C c2(c1);
}

#pragma test "fun(){ decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v1 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))); fun(int<4> v2, ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v3){ { ((v3->m_a) := v2); ((v3->m_b) := 2); ((v3->m_c) := (( *(v3->m_a))+( *(v3->m_b)))); }; return v3; }(1, v1);}"
void constr_class2() {
//	#pragma test \
//	"decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v8 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))) \
//	fun (int<4> v9, ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v10) { \
//		 ((v10->m_a) := v9); \
//		 ((v10->m_b) := 2); \
//		 ((v10->m_c) := (( *(v10->m_a))+( *(v10->m_b)))); \
//	}(1, v8);"
	C c3(1);
}
#pragma test "fun(){ decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v1 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))); fun(int<4> v2, int<4> v3, int<4> v4, ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v5){ { ((v5->m_a) := v2); ((v5->m_b) := v3); ((v5->m_c) := v4); }; return v5; }(1, 2, 3, v1);}"
void constr_class3() {
//	#pragma test \
//	"decl ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v12 = ( var(undefined(type<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>>))) \
//	fun(int<4> v13, int<4> v14, int<4> v15, ref<struct<m_a:int<4>,m_c:int<4>,m_b:int<4>>> v16){ \
//		((v16->m_a) := v13); \
//		((v16->m_b) := v14); \
//		((v16->m_c) := v15); \
//	}(1, 2, 3, v12);"
	C c4(1, 2);
}

int calculate (int a){
	return a;
}

double calculate (double b){
	return b;
}

void func_overload() {

	#pragma test \
	 "fun(int<4> v1){ return v1;}(3)"
	calculate(3);

	#pragma test \
	"fun(real<8> v1){ return v1;}(4.4)"
	calculate(4.4);
}

class Base {
public:
	Base()              {   virt(); }
	virtual void virt() { ; }
};

class Derived : public Base {
public:
	Derived()           {  virt(); }
	virtual void virt() { ; }
};

// #pragma test "fun(){ decl ref<struct<>> v1 = ( var(undefined(type<struct<>>))); fun(ref<struct<>> v2){ fun(ref<struct<>> v3){ { }; }(v2); }(v1);}"
void simple_inheritance() {
	Derived d;
}

namespace ns
{
	namespace sn
	{
		typedef int a;
	}
}
ns::sn::a number = 3;

//#pragma test \
//"fun(){ decl ref<struct<cpp_cpp_number:int<4>>> v1 = ( new(struct{cpp_cpp_number:=3})) { decl ref<int<4>> v2 = ( var(5)); v2; (v1->cpp_cpp_number); };}"
void namespace_test() {
	{
		int number=5;
		number;
		::number;
	}
//	#pragma test "(v1->cpp_cpp_number);"
	::number;
//	#pragma test "(v1->cpp_cpp_number);"
	number;
}

class A {
public:
	int i;
    bool b;
    A() {i=9999999;}
    ~A() {i=0; b=false;}
};

A* a;
//#pragma test "fun(){ decl ref<ref<array<struct<i:int<4>,b:bool>,1>>> v1 = ( var(fun(){ decl ref<array<struct<i:int<4>,b:bool>,1>> v2 = ( new(array.create.1D(type<struct<i:int<4>,b:bool>>, 1))); fun(ref<struct<i:int<4>,b:bool>> v3){ (((v3[&0])->i) := 9999999); }(( var((( *v2)[0])))); return v2; }())); (((( *v1)[&0])->i) := 3); (((( *v1)[&0])->b) := false);}"
void new_test() {
	a = new A();
	a->i =3;
	a->b =false;
}
//#pragma test "fun(){ ( del(( *v1)));}"
void delete_test() {
	delete a;
}


class ThisClass {
	int m_a;
public:
	ThisClass() { this->m_a = 0; }
	void a(int a) {this->m_a = a; }
};

#pragma test \
		"fun(){ decl ref<struct<m_a:int<4>>> v1 = ( var(undefined(type<struct<m_a:int<4>>>))); fun(ref<struct<m_a:int<4>>> v2){ ((v2->m_a) := 0); return v2; }(v1); fun(int<4> v4, ref<struct<m_a:int<4>>> v5){ ((v5->m_a) := v4); }(5, v1);}"
void this_test() {
	ThisClass tc;
	tc.a(5);
}
