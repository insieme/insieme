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

// this test pretends to instaciate all the posible types defined in the OCL headers



// check what kind of macros we have
#ifndef INSIEME
	#define INSIEME
#endif 

#include "CL/cl_platform.h"

#ifdef __VEC__
    #error "__VEC__ macro found" 
#endif 
#ifdef __AVX__
    #error "__AVX__ macro found" 
#endif 
#ifndef __GNUC__
    #error "__GNU__ macro found" 
#endif 
#ifdef __STRICT_ANSI__
    #error "__STRICT_ANSI__ macro found" 
#endif 



int main(int argc, char **argv) {

	// basic types
	{
		#pragma test "decl ref<int<1>> v0 = ( var(undefined(type<int<1>>)))" 
		cl_char     var1;
		#pragma test "decl ref<uint<1>> v0 = ( var(undefined(type<uint<1>>)))" 
		cl_uchar    var2;
		#pragma test "decl ref<int<2>> v0 = ( var(undefined(type<int<2>>)))" 
		cl_short    var3;
		#pragma test "decl ref<uint<2>> v0 = ( var(undefined(type<uint<2>>)))" 
		cl_ushort   var4;
		#pragma test "decl ref<int<4>> v0 = ( var(undefined(type<int<4>>)))" 
		cl_int		var5;
		#pragma test "decl ref<uint<4>> v0 = ( var(undefined(type<uint<4>>)))" 
		cl_uint		var6;
		#pragma test "decl ref<int<8>> v0 = ( var(undefined(type<int<8>>)))" 
		cl_long 	var7;
		#pragma test "decl ref<uint<8>> v0 = ( var(undefined(type<uint<8>>)))" 
		cl_ulong 	var8;
		#pragma test "decl ref<uint<2>> v0 = ( var(undefined(type<uint<2>>)))" 
		cl_half 	var9;
		#pragma test "decl ref<real<4>> v0 = ( var(undefined(type<real<4>>)))" 
		cl_float 	var10;
		#pragma test "decl ref<real<8>> v0 = ( var(undefined(type<real<8>>)))" 
		cl_double 	var11;
	}

	// basic types with initilization
	{
		#pragma test "decl ref<int<1>> v0 = ( var(1))" 
		cl_char     var1 = 1;
		#pragma test "decl ref<uint<1>> v0 = ( var(1u))" 
		cl_uchar    var2 = 1;
		#pragma test "decl ref<int<2>> v0 = ( var(1))" 
		cl_short    var3 = 1;
		#pragma test "decl ref<uint<2>> v0 = ( var(1u))" 
		cl_ushort   var4 = 1;
		#pragma test "decl ref<int<4>> v0 = ( var(1))" 
		cl_int		var5 = 1;
		#pragma test "decl ref<uint<4>> v0 = ( var(1u))" 
		cl_uint		var6 = 1;
		#pragma test "decl ref<int<8>> v0 = ( var(1l))" 
		cl_long 	var7 = 1;
		#pragma test "decl ref<uint<8>> v0 = ( var(1ul))" 
		cl_ulong 	var8 = 1;
		#pragma test "decl ref<uint<2>> v0 = ( var(1u))" 
		cl_half 	var9 = 1;
		#pragma test "decl ref<real<4>> v0 = ( var(1.0f))" 
		cl_float 	var10 = 1;
		#pragma test "decl ref<real<8>> v0 = ( var(1.0))" 
		cl_double 	var11 = 1;
	}

	// cl_char2
	{
		#pragma test \
		"decl ref<union<s:vector<int<1>,2>,__m1:struct<x:int<1>,y:int<1>>,__m2:struct<s0:int<1>,s1:int<1>>,__m3:struct<lo:int<1>,hi:int<1>>>> v0 = ( var(undefined(type<union<s:vector<int<1>,2>,__m1:struct<x:int<1>,y:int<1>>,__m2:struct<s0:int<1>,s1:int<1>>,__m3:struct<lo:int<1>,hi:int<1>>>>)))" 
		cl_char2 var;

		#pragma test \
		"decl ref<union<s:vector<int<1>,2>,__m1:struct<x:int<1>,y:int<1>>,__m2:struct<s0:int<1>,s1:int<1>>,__m3:struct<lo:int<1>,hi:int<1>>>> v0 = ( var(union{AP(s):=[char.to.int('a', 1), char.to.int('b', 1)]}))" 
		cl_char2 initVar = {'a','b'};

		#pragma test \
		"((ref.vector.to.ref.array((v100->s))&[0u]) := 1)"
		initVar.s[0] = 1;

		#pragma test \
		"(((v100->__m1)->x) := 1)"
		initVar.x = 1;
		
		#pragma test \
		"(((v100->__m2)->s0) := 1)"
		initVar.s0 = 1;

		#pragma test \
		"(((v100->__m3)->lo) := 1)"
		initVar.lo = 1;

		#pragma test \
		"(v100 := union{AP(s):=[1, 2]})"
		initVar = (cl_char2) { 1, 2 };
	}

	// short 3 has a diff behaviour
	{
		#pragma test \
		"decl ref<union<s:vector<int<2>,4>,__m1:struct<x:int<2>,y:int<2>,z:int<2>,w:int<2>>,__m2:struct<s0:int<2>,s1:int<2>,s2:int<2>,s3:int<2>>,__m3:struct<lo:union<s:vector<int<2>,2>,__m1:struct<x:int<2>,y:int<2>>,__m2:struct<s0:int<2>,s1:int<2>>,__m3:struct<lo:int<2>,hi:int<2>>>,hi:union<s:vector<int<2>,2>,__m1:struct<x:int<2>,y:int<2>>,__m2:struct<s0:int<2>,s1:int<2>>,__m3:struct<lo:int<2>,hi:int<2>>>>>> v0 = ( var(undefined(type<union<s:vector<int<2>,4>,__m1:struct<x:int<2>,y:int<2>,z:int<2>,w:int<2>>,__m2:struct<s0:int<2>,s1:int<2>,s2:int<2>,s3:int<2>>,__m3:struct<lo:union<s:vector<int<2>,2>,__m1:struct<x:int<2>,y:int<2>>,__m2:struct<s0:int<2>,s1:int<2>>,__m3:struct<lo:int<2>,hi:int<2>>>,hi:union<s:vector<int<2>,2>,__m1:struct<x:int<2>,y:int<2>>,__m2:struct<s0:int<2>,s1:int<2>>,__m3:struct<lo:int<2>,hi:int<2>>>>>>)))"
		cl_short3 var;

		#pragma test \
		"decl ref<union<s:vector<int<2>,4>,__m1:struct<x:int<2>,y:int<2>,z:int<2>,w:int<2>>,__m2:struct<s0:int<2>,s1:int<2>,s2:int<2>,s3:int<2>>,__m3:struct<lo:union<s:vector<int<2>,2>,__m1:struct<x:int<2>,y:int<2>>,__m2:struct<s0:int<2>,s1:int<2>>,__m3:struct<lo:int<2>,hi:int<2>>>,hi:union<s:vector<int<2>,2>,__m1:struct<x:int<2>,y:int<2>>,__m2:struct<s0:int<2>,s1:int<2>>,__m3:struct<lo:int<2>,hi:int<2>>>>>> v0 = ( var(union{AP(s):=[1, 2, 3, 4]}))"
		cl_short3 initVar = {1,2,3,4};

		#pragma test \
		"((ref.vector.to.ref.array((v100->s))&[0u]) := 1)"
		initVar.s[0] = 1;

		#pragma test \
		"(((v100->__m1)->x) := 1)"
		initVar.x = 1;
		
		#pragma test \
		"(((v100->__m2)->s0) := 1)"
		initVar.s0 = 1;

		#pragma test \
		"(((v100->__m3)->lo) := union{AP(s):=[1, 2]})"
		initVar.lo = (cl_short2){1,2};

		#pragma test \
		"(v100 := union{AP(s):=[1, 2, 3, 4]})"
		initVar = (cl_short3){1,2,3,4};
	}

	return 0;
}
