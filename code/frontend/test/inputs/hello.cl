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

#include "ocl_device.h"

//char4 as_char4(int);

float4 subfunction(float4 a) {
	float b = cos(a.z);
	return (float4)(b, get_local_id(1), a.w, a.y);
}

#pragma insieme mark
__kernel void hello(__global short *src, __global float4 *dst, __local float *l, int factor, short2 vector){
#pragma insieme datarange (dst = __insieme_ocl_globalId : __insieme_ocl_globalId), \
	                      (src = __insieme_ocl_globalId : __insieme_ocl_globalId), \
	                      (l = 0 : __insieme_ocl_globalSize)
{
	__local float ll[4];
	ll[get_local_id(0)] = dst[get_global_id(0)].z;
	short bs1 = bitselect(src[0], src[1], src[2]);
	float4 bs2 = bitselect(dst[0], dst[1], dst[2]);

	float af = as_float(factor);
	char4 ac = as_char4(factor);
	short8 as = as_short8(bs2);

	float4 a = cos((float4)(l[3]));
	float4* b = (float4*)src;
	int4 n = (int4)3;
	int4 m;// = (n & ~(a > b[0])) | n;
	b = (float4*)src ;
	float f = 7.0f;
	subfunction(a);
	float4 c = native_divide(a, b[3]);
	short t[4]; 
	short* x = t + 7lu;

	char4 d = convert_char4(a);
	a = convert_float4(d);
	
	float16 sixteen;

#pragma insieme iterations 7
	for(int i = 0; i < factor; ++i)
		dst[0] = a - sixteen.sA5c8;
	dst[1] = b[1] / c.wzyx;
	dst[2] = (float)src[0] + b[0];
	dst[3] = 5.0f + c;
	dst[4] = c * (float)factor;
	dst[5] = (a + c) * 2.0f;
	dst[6] = (float4)(6.0f) + c.z;
	int i = get_global_id(0);
	dst[i].x += src[i] * factor;
}}
