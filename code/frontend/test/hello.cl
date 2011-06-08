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

uint toll(uint x){
    return x;
}

#pragma insieme mark
__kernel void hello(__global float* g, __local float* l) {
    float x =  0.5;
    __local float y;// = g;
//    __global float4* p = (float4*)g;
    y = x;//
 //   toll(i);
    x = 2.0f;

    int gid = get_global_id(0);
/*    uint lid = get_local_id(0);
    l[lid] = g[gid];
    l[2*lid] = g[gid+i];
/*
    barrier(CLK_LOCAL_MEM_FENCE);
    x.x = l[i];
    x.y = native_sin(l[lid+i]);

    x = x+y;

    g[gid] = x.x * x.y;*/
}
