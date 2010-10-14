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

	#pragma test "ref<int<4>> b = 0"
	int b = 0;

	#pragma test "int.add(a, b)"
	a + b;

	#pragma test "ref.assign(a, int.add(a, b))"
	a += b;

	#pragma test "int.sub(a, b)"
	a - b;

	#pragma test "ref.assign(a, int.sub(a, b))"
	a -= b;

	#pragma test "fun(ref<int<4>> a, ref<int<4>> b){ {int.add(a, 1); return int.sub(b, 1);} }(a, b)"
	(a+1, b-1);

	#pragma test "fun(ref<int<4>> a){ {int<4> __tmp = ref.deref(a); ref.assign(a, int.add(a, 1)); return __tmp;} }(a)"
	a++;

	#pragma test "fun(ref<int<4>> a){ {int<4> __tmp = ref.deref(a); ref.assign(a, int.sub(a, 1)); return __tmp;} }(a)"
	a--;

	#pragma test "fun(ref<int<4>> a){ {ref.assign(a, int.add(a, 1)); ref.deref(a);} }(a)"
	++a;

	#pragma test "fun(ref<int<4>> a){ {ref.assign(a, int.sub(a, 1)); ref.deref(a);} }(a)"
	--a;
}

void unary_op_test() {

	#pragma test "ref<int<4>> a = 0"
	int a = 0;

//	#pragma test "bool.not(a)" TBD
	!a;

	#pragma test "a"
	+a;

//	#pragma test "a" TBD
	-a;

	#pragma test "ref<ref<int<4>>> b = a"
	int* b = &a;

	#pragma test "ref.deref(b)"
	*b;
}

void if_stmt_test() {

	int cond = 0;

	#pragma test "if(cond) {ref.assign(cond, int.add(cond, 1));} else {ref.assign(cond, int.sub(cond, 1));}"
	if(cond) {
		cond += 1;
	} else {
		cond -= 1;
	}

	#pragma test "if(int.eq(cond, 0)) {ref.assign(cond, int.add(cond, cond));} else {}"
	if(cond == 0) {
		cond += cond;
	}
}

void for_stmt_test() {

	int it = 0;

	// standard for loop
	#pragma test "for(ref<int<4>> i = 0 .. 100 : 1) {{};}"
	for(int i=0; i<100; i++) { ; }

	// for loop using a variable declared outside
	#pragma test "{for(ref<int<4>> __it = 0 .. 100 : 1) {{};}; ref.assign(it, __it);}"
	for(it=0; it<100; ++it) { ; }

	#pragma test "while(int.lt(it, 100)) {{{};}; ref.assign(it, int.add(it, 1));}"
	for(; it<100; it+=1) { ; }

	#pragma test "{ref<int<4>> j = 1; ref<int<4>> z = 2; for(ref<int<4>> i = 0 .. 100 : 1) {{};};}"
	for(int i=0,j=1,z=2; i<100; i+=1) { ; }

	int mq, nq;
	#pragma test "{ref.assign(mq, 0); "\
				  "while(int.gt(nq, 1)) {{}; "\
				  	  "fun(ref<int<4>> mq, ref<int<4>> nq){ "\
					  	  "{fun(ref<int<4>> mq){ {int<4> __tmp = ref.deref(mq); ref.assign(mq, int.add(mq, 1)); return __tmp;} }(mq); "\
					  	  "return ref.assign(nq, int.div(nq, 2));} }(ref<int<4>> mq, nq)"\
				  ";};}"
    for( mq=0; nq>1; mq++,nq/=2 ) ;
}

void while_stmt_test() {

	int it = 0;

	#pragma test "while(int.ne(it, 0)) {ref.assign(it, int.sub(it, 1));}"
	while(it != 0) { it-=1; }

}



