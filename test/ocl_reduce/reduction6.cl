#ifdef INSIEME
#include "/home/klaus/insieme/code/frontend/test/inputs/ocl_device.h"
#endif

/*
 * Node: Nvidia modified version to support Insieme
 *
 * Copyright 1993-2010 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

// The following defines are set during runtime compilation, see reduction.cpp
// #define T float
#define blockSize 128u
#define nIsPow2 1

/*
    This version adds multiple elements per thread sequentially.  This reduces the overall
    cost of the algorithm while keeping the work complexity O(n) and the step complexity O(log n).
    (Brent's Theorem optimization)
*/
#ifdef INSIEME
#pragma insieme mark
#endif
__kernel void reduce6(__global float *g_idata, __global float *g_odata, unsigned int n, __local volatile float* sdata)
{
    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = get_local_id(0);
    unsigned int i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);
    unsigned int gridSize = blockSize*2*get_num_groups(0);
    sdata[tid] = 0;
    
    // we reduce multiple elements per thread.  The number is determined by the 
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < n)
    {         
        sdata[tid] += g_idata[i];
        // ensure we don't read out of bounds -- this is optimized away for powerOf2 sized arrays
        if (nIsPow2 == 1 || i + blockSize < n) 
            sdata[tid] += g_idata[i+blockSize];  
        i += gridSize;
    } 

    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    if (blockSize >= 512u) { if (tid < 256u) { sdata[tid] += sdata[tid + 256]; } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 256u) { if (tid < 128u) { sdata[tid] += sdata[tid + 128]; } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 128u) { if (tid <  64u) { sdata[tid] += sdata[tid +  64]; } barrier(CLK_LOCAL_MEM_FENCE); }
    
    if (tid < 32)
    {
        if (blockSize >=  64u) { sdata[tid] += sdata[tid + 32]; }
        if (blockSize >=  32u) { sdata[tid] += sdata[tid + 16]; }
        if (blockSize >=  16u) { sdata[tid] += sdata[tid +  8]; }
        if (blockSize >=   8u) { sdata[tid] += sdata[tid +  4]; }
        if (blockSize >=   4u) { sdata[tid] += sdata[tid +  2]; }
        if (blockSize >=   2u) { sdata[tid] += sdata[tid +  1]; }
    }
   
    // write result for this block to global mem 
    if (tid == 0) g_odata[get_group_id(0)] = sdata[0]; 
}
