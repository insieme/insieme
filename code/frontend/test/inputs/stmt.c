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

// Simple struct
void binary_op_test() {

	int a = 0;
	unsigned c;

	#pragma test "ref<int<4>> v1 = ref.var(0)"
	int b = 0;

	#pragma test "int.add(ref.deref(v2), ref.deref(v1))"
	a + b;

	#pragma test "ref.assign(v2, int.add(ref.deref(v2), ref.deref(v1)))"
	a += b;

	#pragma test "int.sub(ref.deref(v2), ref.deref(v1))"
	a - b;

	#pragma test "uint.sub(ref.deref(v1), cast<uint<4>>(1))"
	c - 1;

	#pragma test "ref.assign(v2, int.sub(ref.deref(v2), ref.deref(v1)))"
	a -= b;

	#pragma test "fun(ref<int<4>> v3, ref<int<4>> v4){ {int.add(ref.deref(v4), 1); return int.sub(ref.deref(v3), 1);} }(v1, v2)"
	(a+1, b-1);

	#pragma test "fun(ref<int<4>> v3){ {int<4> v2 = ref.deref(v3); ref.assign(v3, int.add(ref.deref(v3), cast<int<4>>(1))); return v2;} }(v1)"
	a++;

	#pragma test "fun(ref<int<4>> v3){ {int<4> v2 = ref.deref(v3); ref.assign(v3, int.sub(ref.deref(v3), cast<int<4>>(1))); return v2;} }(v1)"
	a--;

	#pragma test "fun(ref<int<4>> v2){ {ref.assign(v2, int.add(ref.deref(v2), cast<int<4>>(1))); ref.deref(v2);} }(v1)"
	++a;

	#pragma test "fun(ref<int<4>> v2){ {ref.assign(v2, int.sub(ref.deref(v2), cast<int<4>>(1))); ref.deref(v2);} }(v1)"
	--a;
}

void unary_op_test() {

	#pragma test "ref<int<4>> v1 = ref.var(0)"
	int a = 0;

//	#pragma test "bool.not(a)" TBD
	!a;

	#pragma test "v1"
	+a;

//	#pragma test "a" TBD
	-a;

	// #pragma test "ref<array<ref<int<4>>,1>> v2 = ref.var(v1)"
	int* b = &a;

//	#pragma test "subscript_single(ref.deref(v1), 0)"
	*b;
}

void if_stmt_test() {

	int cond = 0;

	#pragma test "if(cast<bool>(ref.deref(v1))) {ref.assign(v1, int.add(ref.deref(v1), 1));} else {ref.assign(v1, int.sub(ref.deref(v1), 1));}"
	if(cond) {
		cond += 1;
	} else {
		cond -= 1;
	}

	#pragma test "if(int.eq(ref.deref(v1), 0)) {ref.assign(v1, int.add(ref.deref(v1), ref.deref(v1)));} else {}"
	if(cond == 0) {
		cond += cond;
	}

	int a=1;
	#pragma test "fun(ref<int<4>> v2){ {if(cast<bool>(ref.deref(v2))) int.add(ref.deref(v2), 1) else int.sub(ref.deref(v2), 1);} }(v1)"
	a ? a+1 : a-1;

	#pragma test "fun(ref<int<4>> v2){ {if(int.eq(ref.deref(v2), 0)) int.add(ref.deref(v2), 1) else int.sub(ref.deref(v2), 1);} }(v1)"
	a == 0 ? a+1 : a-1;
}

void for_stmt_test() {

	#pragma test "ref<int<4>> v1 = ref.var(0)"
	int it = 0;
	int a = 1;

	// standard for loop
	#pragma test "for(ref<int<4>> v1 = ref.var(0) .. 100 : 1) {{};}"
	for(int i=0; i<100; i++) { ; }

	// for loop using a variable declared outside
	#pragma test "{for(ref<int<4>> v3 = ref.var(0) .. 100 : 1) {ref.assign(v2, ref.deref(v3));}; ref.assign(v1, 100);}"
	for(it=0; it<100; ++it) { a=it; }

	#pragma test "{for(ref<int<4>> v3 = ref.var(ref.deref(v2)) .. 100 : 6) {ref.assign(v2, ref.deref(v3));}; ref.assign(v1, 100);}"
	for(it=a; it<100; it+=6) { a=it; }

	#pragma test "while(int.lt(ref.deref(v1), 100)) {{{};}; ref.assign(v1, int.add(ref.deref(v1), 1));}"
	for(; it<100; it+=1) { ; }

	#pragma test "{ref<int<4>> v3 = ref.var(1); ref<int<4>> v4 = ref.var(2); for(ref<int<4>> v1 = ref.var(0) .. 100 : 1) {ref.assign(v2, ref.deref(v1));};}"
	for(int i=0,j=1,z=2; i<100; i+=1) { a=i; }

	int mq, nq;
	#pragma test "{ref.assign(v1, 0); while(int.gt(ref.deref(v2), 1)) {{}; fun(ref<int<4>> v5, ref<int<4>> v6){ {fun(ref<int<4>> v4){ {int<4> v3 = ref.deref(v4); ref.assign(v4, int.add(ref.deref(v4), cast<int<4>>(1))); return v3;} }(v5); return ref.assign(v6, int.div(ref.deref(v6), 2));} }(v1, v2);};}"
    for( mq=0; nq>1; mq++,nq/=2 ) ;
}

void switch_stmt_test() {

	int a=0;

	#pragma test "{int<a> v2 = cast<int<a>>(ref.deref(v1)); switch(v2) [ case 1: break | default: {} ];}"
	switch(a) {
	case 1:
		break;
	}

	// EVIL CODE
	#pragma test "{int<a> v2 = cast<int<a>>(int.add(ref.deref(v1), 8)); ref.assign(v1, int.add(ref.deref(v1), 1)); switch(v2) [ case 1: break | default: {} ];}"
	switch(a+8) {
	a += 1;
	case 1:
		break;
	}

	#pragma test "{int<a> v2 = cast<int<a>>(ref.deref(v1)); switch(v2) [ case 0: break | default: fun(ref<int<4>> v4){ {int<4> v3 = ref.deref(v4); ref.assign(v4, int.add(ref.deref(v4), cast<int<4>>(1))); return v3;} }(v1) ];}"
	switch(a) {
	case 0:
		break;
	default:
		a++;
	}

	#pragma test "{int<a> v2 = cast<int<a>>(ref.deref(v1)); break; break; break; switch(v2) [ case 2: {ref<int<4>> v5 = ref.var(0); fun(ref<int<4>> v7){ {int<4> v6 = ref.deref(v7); ref.assign(v7, int.add(ref.deref(v7), cast<int<4>>(1))); return v6;} }(v5);} | case 1: ref.assign(v1, int.add(ref.deref(v1), 1)) | default: fun(ref<int<4>> v4){ {int<4> v3 = ref.deref(v4); ref.assign(v4, int.sub(ref.deref(v4), cast<int<4>>(1))); return v3;} }(v1) ];}"
	switch(a) {
	case 1:
		a+=1;
		break;
	case 2:
		{  int c; c++; }
		break;
	default:
		a--;
		break;
	}
}


void while_stmt_test() {
	int it = 0;
	#pragma test "while(int.ne(ref.deref(v1), 0)) {ref.assign(v1, int.sub(ref.deref(v1), 1));}"
	while(it != 0) { it-=1; }
}

#pragma test "rec v1.{v2=fun(int<4> v4){ {return v1(int.add(v4, 1));} }, v1=fun(int<4> v3){ {return v2(int.sub(v3, 1));} }}"
int f(int x) {
	return g(x-1);
}

#pragma test "rec v1.{v2=fun(int<4> v4){ {return v1(int.sub(v4, 1));} }, v1=fun(int<4> v3){ {return v2(int.add(v3, 1));} }}"
int g(int x) {
	return f(x+1);
}

void rec_function_call_test() {
	#pragma test "rec v1.{v2=fun(int<4> v4){ {return v1(int.add(v4, 1));} }, v1=fun(int<4> v3){ {return v2(int.sub(v3, 1));} }}(10)"
	f(10);
}

void evil(void* anything) { }

void vector_stmt_test() {

	#pragma test "ref<vector<ref<int<4>>,5>> v1 = ref.var({ref.var(0),ref.var(0),ref.var(0),ref.var(0),ref.var(0)})"
	int a[5];

	#pragma test "subscript_single(ref.deref(v1), 0)"
	a[0];

	#pragma test "ref.assign(subscript_single(ref.deref(v1), 0), 1)"
	a[0] = 1;

	int b[2][2];

	#pragma test "subscript_single(subscript_single(ref.deref(v1), 0), 0)"
	b[0][0];

	#pragma test "ref.assign(subscript_single(subscript_single(ref.deref(v1), 1), 1), 0)"
	b[1][1] = 0;

	// #pragma test "fun(array<ref<'a>,1> v2){ {} }(v1)"
	evil(b);
}

