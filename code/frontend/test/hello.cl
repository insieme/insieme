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

#pragma insieme mark
__kernel void hello(__global double* g, __local float* l, int i) {
    float2 x = (float2)0;
    float2 y = {i, i+0.5};

    int gid = get_global_id(0);
    unsigned int lid = get_local_id(0);
    l[lid] = g[gid];
    l[2*lid] = g[gid+i];

    barrier(CLK_LOCAL_MEM_FENCE);
    x.x = l[i];
    x.y = native_sin(l[lid+i]);

    x = x+y;

    g[gid] = x.x * x.y;
}
/*
unsigned char
galoisMultiplication(unsigned char a, unsigned char b)
{
    unsigned char p = 0;
    for(unsigned int i=0; i < 8; ++i)
    {
        if((b&1) == 1)
        {
            p^=a;
        }
        unsigned char hiBitSet = (a & 0x80);
        a <<= 1;
        if(hiBitSet == 0x80)
        {
            a ^= 0x1b;
        }
        b >>= 1;
    }
    return p;
}

inline uchar4
sbox(__global uchar * SBox, uchar4 block)
{
    return (uchar4)(SBox[block.x], SBox[block.y], SBox[block.z], SBox[block.w]);
}

uchar4
mixColumns(__local uchar4 * block, __private uchar4 * galiosCoeff, unsigned int j)
{
    unsigned int bw = 4;

    uchar x, y, z, w;

    x = galoisMultiplication(block[0].x, galiosCoeff[(bw-j)%bw].x);
    y = galoisMultiplication(block[0].y, galiosCoeff[(bw-j)%bw].x);
    z = galoisMultiplication(block[0].z, galiosCoeff[(bw-j)%bw].x);
    w = galoisMultiplication(block[0].w, galiosCoeff[(bw-j)%bw].x);

    for(unsigned int k=1; k< 4; ++k)
    {
        x ^= galoisMultiplication(block[k].x, galiosCoeff[(k+bw-j)%bw].x);
        y ^= galoisMultiplication(block[k].y, galiosCoeff[(k+bw-j)%bw].x);
        z ^= galoisMultiplication(block[k].z, galiosCoeff[(k+bw-j)%bw].x);
        w ^= galoisMultiplication(block[k].w, galiosCoeff[(k+bw-j)%bw].x);
    }

    return (uchar4)(x, y, z, w);
}


#pragma insieme mark
uchar4
shiftRows(uchar4 row, unsigned int j)
{
    uchar4 r = row;
    for(uint i=0; i < j; ++i)
    {
        r = r.yzwx;
    }
    return r;
}

#pragma insieme mark
__kernel
void AESEncrypt(__global  uchar4  * output  ,
                __global  uchar4  * input   ,
                __global  uchar4  * roundKey,
                __global  uchar   * SBox    ,
                __local   uchar4  * block0  ,
                __local   uchar4  * block1  ,
                const     uint      width   ,
                const     uint     rounds   )

{

    __private uchar4 galiosCoeff[4];
     galiosCoeff[0] = (uchar4)(2, 0, 0, 0);
     galiosCoeff[1] = (uchar4)(3, 0, 0, 0);
     galiosCoeff[2] = (uchar4)(1, 0, 0, 0);
     galiosCoeff[3] = (uchar4)(1, 0, 0, 0);

    unsigned int blockIdx = get_group_id(0);
    unsigned int blockIdy = get_group_id(1);

    unsigned int localIdx = get_local_id(0);
    unsigned int localIdy = get_local_id(1);

    unsigned int globalIndex = (((blockIdy * width/4) + blockIdx) * 4 )+ (localIdy);
    unsigned int localIndex  = localIdy;

    block0[localIndex]  = input[globalIndex];

    block0[localIndex] ^= roundKey[localIndex];

    for(unsigned int r=1; r < rounds; ++r)
    {
        block0[localIndex] = sbox(SBox, block0[localIndex]);

        block0[localIndex] = shiftRows(block0[localIndex], localIndex);

        barrier(CLK_LOCAL_MEM_FENCE);
//        block1[localIndex]  = mixColumns(block0, galiosCoeff, localIndex);

        barrier(CLK_LOCAL_MEM_FENCE);
        block0[localIndex] = block1[localIndex]^roundKey[r*4 + localIndex];
    }
    block0[localIndex] = sbox(SBox, block0[localIndex]);

    block0[localIndex] = shiftRows(block0[localIndex], localIndex);

    output[globalIndex] =  block0[localIndex]^roundKey[(rounds)*4 + localIndex];
}*/
