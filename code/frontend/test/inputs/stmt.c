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
//#pragma GCC diagnostic ignored "-Wall"

void decl_stmt_test() {
	#pragma test \
	"decl ref<int<4>> v0 = ( var(0))"
	int b = 0;

	#pragma test \
	"decl ref<uint<4>> v0 = ( var(undefined(type<uint<4>>)))"
	unsigned int c;

	#pragma test \
	"decl ref<real<4>> v0 = ( var(undefined(type<real<4>>)))"
	float d;

	#pragma test \
	"decl ref<real<4>> v0 = ( var(1.0000000f))"
	float e = 1.0f;
	// ...
}

// Simple struct
void binary_op_test() {

	int a = 0, b = 0;
	unsigned c;

	#pragma test \
	"(( *v100)+( *v101))"
	a + b;

	#pragma test \
	"(v100 := (( *v100)+( *v101)))"
	a += b;

	#pragma test \
	"(( *v100)-( *v101))"
	a - b;

	#pragma test \
	"(( *v100)-1u)"
	c - 1;

	#pragma test \
	"(v100 := (( *v100)-( *v101)))"
	a -= b;

	#pragma test \
	"fun(int<4> v1, int<4> v2) -> int<4> { (v1+1); return (v2-1);}(( *v100), ( *v101))"
	(a+1, b-1);
}

void unary_op_test() {

	#pragma test "decl ref<int<4>> v0 = ( var(0))"
	int a = 0;

	#pragma test "(!int.to.bool(( *v100)))"
	!a;

	#pragma test "( *v100)"
	+a;

	#pragma test "(0-( *v100))"
	-a;

	#pragma test "decl ref<ref<array<int<4>,1>>> v0 = ( var(scalar.to.array(v100)))"
	int* b = &a;

	#pragma test "( *(( *v100)&[0]))"
	*b;

	#pragma test "gen.post.inc(v100)"
	a++;

	#pragma test "gen.post.dec(v100)"
	a--;

	#pragma test "gen.pre.inc(v100)"
	++a;

	#pragma test "gen.pre.dec(v100)"
	--a;

	#pragma test "(( *v100)<<2)"
	a << 2;

	#pragma test "(int.to.uint(( *v100), 4)<<2)"
	(unsigned int)a << 2;
}

void array_test() {

	#pragma test "decl ref<ref<array<int<4>,1>>> v0 = ( var(undefined(type<ref<array<int<4>,1>>>)))"
	int* a;

	#pragma test "( *(( *v100)&[0u]))"
	a[0];

}


struct Person { int weigth; int age; };

void member_access_test() {

	#pragma test "decl ref<struct<weigth:int<4>,age:int<4>>> v0 = ( var(undefined(type<struct<weigth:int<4>,age:int<4>>>)))"
	struct Person p;

	#pragma test "(( *v100).weigth)"
	p.weigth;

	#pragma test "decl ref<ref<array<struct<weigth:int<4>,age:int<4>>,1>>> v0 = ( var(scalar.to.array(v100)))"
	struct Person* ptr = &p;

	#pragma test "(( *(( *v100)&[0])).age)"
	ptr->age;

	#pragma test "(((( *v100)&[0])->age) := 100)"
	ptr->age = 100;
}

void if_stmt_test() {

	int cond = 0;

	#pragma test \
	"if(int.to.bool(( *v100))) { (v100 := (( *v100)+1));} else { (v100 := (( *v100)-1));}"
	if(cond) {
		cond += 1;
	} else {
		cond -= 1;
	}

	#pragma test \
	"if((( *v100)==0)) { (v100 := (( *v100)+( *v100)));}"
	if(cond == 0) {
		cond += cond;
	}

	int a=1;
	#pragma test \
	"((int.to.bool(( *v100)))?bind(){fun(int<4> v1) -> int<4> { return (v1+1);}(( *v100))}:bind(){fun(int<4> v1) -> int<4> { return (v1-1);}(( *v100))})"
	a ? a+1 : a-1;

	#pragma test \
	"(((( *v100)==0))?bind(){fun(int<4> v1) -> int<4> { return (v1+1);}(( *v100))}:bind(){fun(int<4> v1) -> int<4> { return (v1-1);}(( *v100))})"
	a == 0 ? a+1 : a-1;

	#pragma test \
	"if(((( *v100)>0) && bind(){fun(int<4> v1) -> bool { return (v1!=1);}(( *v100))})) { }"
	if(cond > 0 && cond != 1) {	; }
}

void for_stmt_test() {

	int it = 0;
	int a = 1;

	// standard for loop
	#pragma test \
	"{ decl int<4> v0 = ((100-0)-1); for(decl int<4> v1 = 0 .. ((v0/abs(1))+1) : 1) { { }; };}"
	for(int i=0; i<100; i++) { ; }

	// for loop using a variable declared outside
	#pragma test \
	"{ decl int<4> v0 = ((100-0)-1); for(decl int<4> v1 = 0 .. ((v0/abs(1))+1) : 1) { (v100 := v1); (v101 := ((v1*1)+0)); }; (v100 := ((((v0/abs(1))+1)*1)+0));}"
	for(it=0; it<100; ++it) { a=it; }

	#pragma test \
	"{ decl int<4> v0 = ((100-( *v100))-1); for(decl int<4> v1 = 0 .. ((v0/abs(6))+1) : 1) { (v101 := v1); (v100 := ((v1*6)+( *v100))); }; (v101 := ((((v0/abs(6))+1)*6)+( *v100)));}"
	for(it=a; it<100; it+=6) { a=it; }

	#pragma test \
	"{ decl int<4> v0 = ((100-( *v100))-1); for(decl int<4> v1 = 0 .. ((v0/abs(1))+1) : 1) { { }; };}"
	for(; it<100; it+=1) { ; }

	#pragma test \
	"{ decl ref<int<4>> v0 = ( var(1)); decl ref<int<4>> v1 = ( var(2)); decl int<4> v2 = ((100-0)-1); for(decl int<4> v3 = 0 .. ((v2/abs(1))+1) : 1) { (v100 := ((v3*1)+0)); };}"
	for(int i=0,j=1,z=2; i<100; i+=1) { a=i; }

	// divission is not supported as for loop increment
	int mq, nq;
	#pragma test \
	"{ (v100 := 0); while((( *v101)>1)) { { }; fun(ref<int<4>> v1, ref<int<4>> v2) -> int<4> { gen.post.inc(v1); return (v2 := (( *v2)/2)); }(v100, v101); };}"
    for( mq=0; nq>1; mq++,nq/=2 ) ;
}

void switch_stmt_test() {

	int a=0;

	#pragma test \
	"{ decl int<4> v0 = ( *v100); switch(v0) { case 1: { break; } default: { } };}"
	switch(a) {
	case 1:
		break;
	}
	//decl int<a> v2 = CAST<int<a>>(( *v1));
	//switch(v2) {
	//	case 1: { }
	//	default: { }
	//};


	// EVIL CODE: are we sure the +1 is not ignored??
	//#pragma test  "{ decl int<4> v0 = (( *v100)+8); (v100 := (( *v100)+1)); switch(v0) { case 1: { } default: { } };}"
	#pragma test  "{ decl int<4> v0 = (( *v100)+8); switch(v0) { case 1: { break; } default: { } };}"
	switch(a+8) {
	a += 1;
	case 1:
		break;
	}

	#pragma test  \
	"{ decl int<4> v0 = (( *v100)+8); decl ref<int<4>> v1 = ( var(undefined(type<int<4>>))); switch(v0) { case 1: { (v1 := 1); break; } default: { } };}"
	switch(a+8) {
		int x;
	case 1:
		x = 1;
		break;
	}

	#pragma test  \
	"{ decl int<4> v0 = (( *v100)+8); decl ref<int<4>> v1 = ( var(undefined(type<int<4>>))); switch(v0) { case 1: { (v1 := 1); break; } default: { } };}"
	switch(a+8) {
		int x = 0;
	case 1:
		x = 1;
		break;
	}

	#pragma test \
	"{ decl int<4> v0 = ( *v100); switch(v0) { case 0: { break; gen.post.inc(v100); } default: { gen.post.inc(v100); } };}"
	switch(a) {
	case 0:
		break;
	default:
		a++;
	}

	#pragma test \
	"{ decl int<4> v0 = ( *v100); switch(v0) { case 1: { (v100 := (( *v100)+1)); break; { decl ref<int<4>> v1 = ( var(undefined(type<int<4>>))); (( *v1)+1); }; (( *v100)-1); break; } case 2: { { decl ref<int<4>> v1 = ( var(undefined(type<int<4>>))); (( *v1)+1); }; (( *v100)-1); break; } default: { (( *v100)-1); break; } };}"
	switch(a) {
	case 1:
		a+=1;
		break;
	case 2:
		{ int c; c+1; }
	default:
		a-1;
		break;
	}

	for(;;) {
	#pragma test \
	"{ decl int<4> v0 = ( *v100); switch(v0) { case 10: { break; (v100 := (( *v100)+10)); break; (v100 := 1); continue; gen.post.inc(v100); return unit; break; } case 2: { (v100 := 1); continue; gen.post.inc(v100); return unit; break; } case 3: { gen.post.inc(v100); return unit; break; } case 8: { (v100 := (( *v100)+10)); break; (v100 := 1); continue; gen.post.inc(v100); return unit; break; } default: { break; } };}"
	switch(a) {
		case 10:
			break;
		case 8:
			a += 10;
			break;
		case 2:
			a = 1;
			continue;
		case 3:
			a++;
			return;
		default:
			break;
	}
	}

	#pragma test \
	"{ decl int<4> v0 = ( *v100); switch(v0) { case 1: { break; } default: { } };}"
	switch(a) {
		case 0+1:
			break;
	}

	#pragma test \
	"{ decl int<4> v0 = ( *v100); switch(v0) { case 0: { { (v100 := 10); break; }; (v100 := 100); break; } case 1: { (v100 := 100); break; } default: { } };}"
	switch(a) {
		case 0: 
			{
				a = 10;	
				break;
			}
		case 1:
			a = 100;
			break;
	}

}


void while_stmt_test() {
	int it = 0;
	#pragma test \
	"while((( *v100)!=0)) { (v100 := (( *v100)-1));}"
	while(it != 0) { it-=1; }

	#pragma test \
	"{ decl ref<bool> v0 = ( var(false)); while(((!( *v0)) || bind(){fun(ref<int<4>> v1) -> bool { return (( *v1)<10); }(v100)})) { (v0 := true); { (v100 := (( *v100)+1)); }; };}"
	do{ it+=1; }while(it < 10);
}

int g(int x) ;

#pragma test \
	"recFun v0 { v0 = fun(int<4> v1) -> int<4> { return v2((v1-1)); }; v2 = fun(int<4> v3) -> int<4> { return v0((v3+1)); };}"
int f(int x) {
	return g(x-1);
}

#pragma test \
	"recFun v0 { v0 = fun(int<4> v1) -> int<4> { return v2((v1+1)); }; v2 = fun(int<4> v3) -> int<4> { return v0((v3-1)); };}"
int g(int x) {
	return f(x+1);
}
//recFun v1 {
//    v2 = fun(int<4> v4) {
//        return v1((v4+1));
//    };
//    v1 = fun(int<4> v3) {
//        return v2((v3-1));
//    };
//}

int a(int x) ;
int b(int x) ;
int c(int x) ;

#pragma test \
	"recFun v0 { v0 = fun(int<4> v1) -> int<4> { return v2((v1-1)); }; v2 = fun(int<4> v3) -> int<4> { return v4((v3+1)); }; v4 = fun(int<4> v5) -> int<4> { return v0((v5*2)); };}"
int a(int x) {
	return b(x-1);
}

#pragma test \
	"recFun v0 { v0 = fun(int<4> v1) -> int<4> { return v2((v1+1)); }; v2 = fun(int<4> v3) -> int<4> { return v4((v3*2)); }; v4 = fun(int<4> v5) -> int<4> { return v0((v5-1)); };}"
int b(int x) {
	return c(x+1);
}

#pragma test \
	"recFun v0 { v0 = fun(int<4> v1) -> int<4> { return v2((v1*2)); }; v2 = fun(int<4> v3) -> int<4> { return v4((v3-1)); }; v4 = fun(int<4> v5) -> int<4> { return v0((v5+1)); };}"
int c(int x) {
	return a(x*2);
}



void rec_function_call_test() {
	#pragma test \
	"recFun v0 { v0 = fun(int<4> v1) -> int<4> { return v2((v1-1)); }; v2 = fun(int<4> v3) -> int<4> { return v0((v3+1)); };}(10)"
	f(10);
}

void evil(int** anything) { }

void vector_stmt_test() {

	#pragma test "decl ref<vector<int<4>,5>> v0 = ( var(undefined(type<vector<int<4>,5>>)))"
	int a[5];

	#pragma test \
	"( *(ref.vector.to.ref.array(v100)&[0u]))"
	a[0];

	#pragma test \
	"((ref.vector.to.ref.array(v100)&[0u]) := 1)"
	a[0] = 1;

	#pragma test \
	"decl ref<vector<vector<int<4>,3>,2>> v0 = ( var(undefined(type<vector<vector<int<4>,3>,2>>)))"
	int b[2][3];

	#pragma test \
	"( *(ref.vector.to.ref.array((ref.vector.to.ref.array(v100)&[0u]))&[0u]))"
	b[0][0];

	#pragma test \
	"((ref.vector.to.ref.array((ref.vector.to.ref.array(v100)&[1u]))&[1u]) := 0)"
	b[1][1] = 0;

	#pragma test \
	"decl ref<vector<int<4>,10>> v0 = ( var(vector.init.partial(([0]), 10)))"
	int vec[10] = {0};

	#pragma test \
	"decl ref<ref<array<int<4>,1>>> v0 = ( var(ref.vector.to.ref.array(( var(vector.init.partial(([0,5,10]), 10))))))"
	int *vec_ptr = (int[10]) {0, 5, 10};

	#pragma test \
	"fun(ref<array<ref<array<int<4>,1>>,1>> v1) -> unit { }(ref.reinterpret(ref.vector.to.ref.array(v100), type<array<ref<array<int<4>,1>>,1>>))"
	evil(b);
}

void* vf(void* ptr) { return ptr; }

void init_expr() {

	// #pragma test "decl ref<array<int<4>,1>> v1 = ( var(CAST<array<int<4>,1>>(null)))"
	int* a = 0;

	#pragma test \
	"( *(ref.vector.to.ref.array(( var([1, 2, 3])))&[1u]))"
	((int[3]) {1,2,3})[1];

	struct Person p;
	#pragma test \
	"(v100 := struct{weigth:=10, age:=20})"
	p = (struct Person) {10, 20};

	// #pragma test "fun(ref<array<'a,1>> v2){ return ( *v2);}(v1)"
	vf(a);

}


