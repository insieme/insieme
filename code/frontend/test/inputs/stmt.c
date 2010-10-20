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

	#pragma test "ref<int<4>> v1 = 0"
	int b = 0;

	#pragma test "int.add(ref.deref(v2), ref.deref(v1))"
	a + b;

	#pragma test "ref.assign(v2, int.add(ref.deref(v2), ref.deref(v1)))"
	a += b;

	#pragma test "int.sub(ref.deref(v2), ref.deref(v1))"
	a - b;

	#pragma test "ref.assign(v2, int.sub(ref.deref(v2), ref.deref(v1)))"
	a -= b;

	#pragma test "fun(ref<int<4>> v3, ref<int<4>> v4){ {int.add(ref.deref(v4), 1); return int.sub(ref.deref(v3), 1);} }(v1, v2)"
	(a+1, b-1);

	#pragma test "fun(ref<int<4>> v6){ {int<4> v5 = ref.deref(v6); ref.assign(v6, int.add(ref.deref(v6), 1)); return v5;} }(v2)"
	a++;

	#pragma test "fun(ref<int<4>> v8){ {int<4> v7 = ref.deref(v8); ref.assign(v8, int.sub(ref.deref(v8), 1)); return v7;} }(v2)"
	a--;

	#pragma test "fun(ref<int<4>> v9){ {ref.assign(v9, int.add(ref.deref(v9), 1)); ref.deref(v9);} }(v2)"
	++a;

	#pragma test "fun(ref<int<4>> v10){ {ref.assign(v10, int.sub(ref.deref(v10), 1)); ref.deref(v10);} }(v2)"
	--a;
}

void unary_op_test() {

	#pragma test "ref<int<4>> v11 = 0"
	int a = 0;

//	#pragma test "bool.not(a)" TBD
	!a;

	#pragma test "v11"
	+a;

//	#pragma test "a" TBD
	-a;

	#pragma test "ref<ref<int<4>>> v12 = v11"
	int* b = &a;

	#pragma test "ref.deref(v12)"
	*b;
}

void if_stmt_test() {

	int cond = 0;

	#pragma test "if(v13) {ref.assign(v13, int.add(ref.deref(v13), 1));} else {ref.assign(v13, int.sub(ref.deref(v13), 1));}"
	if(cond) {
		cond += 1;
	} else {
		cond -= 1;
	}

	#pragma test "if(int.eq(ref.deref(v13), 0)) {ref.assign(v13, int.add(ref.deref(v13), ref.deref(v13)));} else {}"
	if(cond == 0) {
		cond += cond;
	}
}

void for_stmt_test() {

	#pragma test "ref<int<4>> v14 = 0"
	int it = 0;
	int a;

	// standard for loop
	#pragma test "for(ref<int<4>> v15 = 0 .. 100 : 1) {{};}"
	for(int i=0; i<100; i++) { ; }

	// for loop using a variable declared outside
	#pragma test "{for(int<4> v17 = 0 .. 100 : 1) {ref.assign(v16, ref.deref(v17));}; ref.assign(v14, 100);}"
	for(it=0; it<100; ++it) { a=it; }

	#pragma test "while(int.lt(ref.deref(v14), 100)) {{{};}; ref.assign(v14, int.add(ref.deref(v14), 1));}"
	for(; it<100; it+=1) { ; }

	#pragma test "{ref<int<4>> v19 = 1; ref<int<4>> v20 = 2; for(ref<int<4>> v18 = 0 .. 100 : 1) {ref.assign(v16, ref.deref(v18));};}"
	for(int i=0,j=1,z=2; i<100; i+=1) { a=i; }

	int mq, nq;
	#pragma test "{ref.assign(v21, 0); while(int.gt(ref.deref(v22), 1)) {{}; fun(ref<int<4>> v25, ref<int<4>> v26, ref<int<4>> v27){ {fun(ref<int<4>> v27){ {int<4> v23 = ref.deref(v27); ref.assign(v27, int.add(ref.deref(v27), 1)); return v23;} }(v25); return ref.assign(v26, int.div(ref.deref(v26), 2));} }(v21, v22, v24);};}"
    for( mq=0; nq>1; mq++,nq/=2 ) ;
}

void while_stmt_test() {

	int it = 0;

	#pragma test "while(int.ne(ref.deref(v28), 0)) {ref.assign(v28, int.sub(ref.deref(v28), 1));}"
	while(it != 0) { it-=1; }

}



