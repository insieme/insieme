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

#ifndef NO_INSIEME  // must be set when using the kernel function outside of the insieme compiler
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void constantMemArg(__constant float* c) {
    float element = c[0];
}

#pragma insieme mark
__kernel void globalMemArg(__global float* g) {
//    __global float* privateGptr = g;
    float element = g[0];
//    __global float4* privateGvec = (float4*)g;

}

#pragma insieme mark
__kernel void localMemArg(__local int* l) {
    int element = l[0];
}


#pragma insieme mark
__kernel void privateMemArg(short p) {
    short copy = p;
}


#pragma insieme mark
__kernel void allMemArg(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    ga[0] = (float)gb[0];
}

#pragma insieme mark
__kernel void simpleCalc(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    l[gb[0]] = 3.3f;
    ga[pa] = c[1] * l[gb[0]];
}


#pragma insieme mark
__kernel void getId(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    uint gid = get_global_id(0);
    ga[gid] = (float)gid;
}

#pragma insieme mark
__kernel void getSize(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    ga[0] = (float)get_global_size(0);
    ga[1] = (float)get_global_size(1);
    ga[2] = (float)get_global_size(2);
    ga[3] = (float)get_local_size(0);
    ga[4] = (float)get_local_size(1);
    ga[5] = (float)get_local_size(2);
    ga[6] = (float)get_num_groups(0);
    ga[7] = (float)get_num_groups(1);
    ga[8] = (float)get_num_groups(2);
}


#pragma insieme mark
__kernel void branch(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    if(pa == pb)
        ga[0] = c[0];
}

#pragma insieme mark
__kernel void access3D(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    uint gid[3];
    gid[0] = get_global_id(0);
    gid[1] = get_global_id(1);
    gid[2] = get_global_id(2);
    // gb is used to pass the offsets of the linearized 3D array ga and gb
    uint gid3 = gid[0] * gb[2] * gb[1] + gid[1] * gb[2] + gid[2];

    ga[gid3] = c[gid3];
}

#pragma insieme mark
__kernel void barriers(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    uint lid = get_local_id(0);
    uint gid = get_global_id(0);

    l[lid] = 7.0f;//(float)c[gid];
    barrier(CLK_LOCAL_MEM_FENCE);
    ga[gid] = l[lid];
}
/* init zero missing, so no in kernel local variables possible
#pragma insieme mark
__kernel void localMem(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    uint gid = get_global_id(0);
    uint lid = get_local_id(0);
    __local int inKernelLocal[258];

    l[lid] = c[gid];
    inKernelLocal[lid-1] = gb[gid];
    barrier(CLK_LOCAL_MEM_FENCE);

    ga[gid] = l[gid+1] + inKernelLocal[gid];
}
*/

/*
 * Copyright 1993-2009 NVIDIA Corporation.  All rights reserved.
 *
 * NVIDIA Corporation and its licensors retain all intellectual property and
 * proprietary rights in and to this software and related documentation.
 * Any use, reproduction, disclosure, or distribution of this software
 * and related documentation without an express license agreement from
 * NVIDIA Corporation is strictly prohibited.
 *
 */

 // OpenCL Kernel Function for element by element vector addition
#pragma insieme mark
__kernel void VectorAdd(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
    // get index into global data array
    int iGID = get_global_id(0) * gb[1] * gb[2] + get_global_id(1) * gb[2] + get_global_id(2);
    int iNumElements = gb[0] * gb[1] * gb[2] - 17;

    // bound check (equivalent to the limit on a 'for' loop for standard/serial C code
    if (iGID >= iNumElements)
    {
        return;
    }

    // add the vector elements
    ga[iGID] = c[iGID] + gb[iGID];
}

