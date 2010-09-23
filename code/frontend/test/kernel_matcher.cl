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

//OpenCL definitions
/*#define __private __attribute__((address_space(0))) //default value
#define private __attribute__((address_space(0)))
#define __local __attribute__((address_space(1)))
#define local __attribute__((address_space(1)))
#define __global __attribute__((address_space(2)))
#define global __attribute__((address_space(2)))
#define __constant __attribute__((address_space(3)))
#define constant __attribute__((address_space(3)))*/

#define __private __attribute__((annotate("__private"))) //default value
#define private __attribute__((annotate("__private")))
#define __local __attribute__((annotate("__local")))
#define local __attribute__((annotate("__local")))
#define __global __attribute__((annotate("__global")))
#define global __attribute__((annotate("__global")))
#define __constant __attribute__((annotate("__constant")))
#define constant __attribute__((annotate("__constant")))

#define __kernel __attribute__((annotate("__kernel")))
#define kernel __attribute__((annotate("__kernel")))

typedef __attribute__((ext_vector_type(4))) int int4;

__kernel __attribute__((reqd_work_group_size(1,2,3))) void kfct(__global float * a, __constant int* c)
{
    local float* b;
    __private int* d;
    struct local __attribute__((packed)) X
    {
        int e[3] __attribute__((aligned(8)));
        const short s __attribute__((packed));
    };
    
    int x __attribute__ ((aligned(2)));
    __attribute__((aligned(8))) int y;

    int4 t = {1, 2, 3, 4};
    t.x = 5;
    int o = t.s0;
    b[0] = a[0];

    __private float fltarr[5];


    return;
}
