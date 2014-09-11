#include "vecops.hh"

#define localRange 256

#pragma insieme mark
__kernel __attribute__((reqd_work_group_size (localRange,1,1))) void sparsedot1_kernel(
    __global const struct svm_node* px, __global struct svm_node* py,
    __global dtype* result, __global const int* pyLength, const ulong x, const ulong y)
{
    __local dtype sdata[localRange];

    int groupIdx = get_group_id(0);
    int groupSize = get_local_size(0);
    int localIdx = get_local_id(0);
    py += groupIdx*(x+1);

    dtype tmp = 0;

    // Reduce multiple elements per thread
//    for(unsigned int k = 0, k < 1000; ++k);
    for (unsigned int i = localIdx; i < x; i += groupSize)
    {
        int yIdx = i <= pyLength[groupIdx] ? i : pyLength[groupIdx]-1;

        int step = px[i].index > py[yIdx].index ? 1 : -1;
//        int step = (px[i].index > py[yIdx].index) * 2 - 1;
//printf("i %f, j %d\n", px[i].value, py[pyLength[groupIdx]-1].value);

        for(int j = i; j >= 0 && j < pyLength[groupIdx] && px[i].index * step >= py[j].index * step; j += step)
        {
           if(px[i].index == py[j].index)
            {
                tmp += py[j].value * px[i].value;
            }
        }
//printf("\tworked\n");
    }


/*working for dense vectors
    for (size_t i = localIdx; i < x; i += groupSize)
        tmp += py[i].value * px[i].value;
*/
    sdata[localIdx] = tmp;
    barrier(CLK_LOCAL_MEM_FENCE);

    // Do reduction in shared memory
    if (groupSize >= 512) {
        if (localIdx < 256) { sdata[localIdx] += sdata[localIdx + 256]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 256) {
        if (localIdx < 128) { sdata[localIdx] += sdata[localIdx + 128]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 128) {
        if (localIdx <  64) { sdata[localIdx] += sdata[localIdx +  64]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 64) {
        if (localIdx <  32) { sdata[localIdx] += sdata[localIdx +  32]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 32) {
        if (localIdx <  16) { sdata[localIdx] += sdata[localIdx +  16]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 16) {
        if (localIdx <  8) { sdata[localIdx] += sdata[localIdx +  8]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 8) {
        if (localIdx <  4) { sdata[localIdx] += sdata[localIdx +  4]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 4) {
        if (localIdx <  2) { sdata[localIdx] += sdata[localIdx +  2]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 2) {
        if (localIdx <  1) { sdata[localIdx] += sdata[localIdx +  1]; } barrier(CLK_LOCAL_MEM_FENCE);
    }

    // Write result to global memory
    if(localIdx == 0) {
        result[groupIdx] = sdata[0];
	}
}




__kernel __attribute__((reqd_work_group_size (localRange,1,1))) void sparsedot2_kernel(
    __global const dtype* px, __global struct svm_node* py,
    __global dtype* result, __global const int* pyLength, const ulong x, const ulong y)
{
    __local dtype sdata[localRange];

    int groupIdx = get_group_id(0);
    int groupSize = get_local_size(0);
    int localIdx = get_local_id(0);
    py += groupIdx*(x+1);
 //   int c = 0;

    // Reduce multiple elements per thread
//    for(unsigned int j = 0; j < 20; ++j)
    {
    dtype tmp = 0;
 
    
    for (unsigned int i = localIdx; i < x; i += groupSize)
        tmp += py[i].value * px[py[i].index];

    sdata[localIdx] = tmp;
    barrier(CLK_LOCAL_MEM_FENCE);

    // Do reduction in shared memory
    if (groupSize >= 1024) {
        if (localIdx < 512) { sdata[localIdx] += sdata[localIdx + 512]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 512) {
        if (localIdx < 256) { sdata[localIdx] += sdata[localIdx + 256]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 256) {
        if (localIdx < 128) { sdata[localIdx] += sdata[localIdx + 128]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 128) {
        if (localIdx <  64) { sdata[localIdx] += sdata[localIdx +  64]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
#ifndef NVIDIA
// general code
    if (groupSize >= 64) {
        if (localIdx <  32) { sdata[localIdx] += sdata[localIdx +  32]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 32) {
        if (localIdx <  16) { sdata[localIdx] += sdata[localIdx +  16]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 16) {
        if (localIdx <  8) { sdata[localIdx] += sdata[localIdx +  8]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 8) {
        if (localIdx <  4) { sdata[localIdx] += sdata[localIdx +  4]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 4) {
        if (localIdx <  2) { sdata[localIdx] += sdata[localIdx +  2]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 2) {
        if (localIdx <  1) { sdata[localIdx] += sdata[localIdx +  1]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
#else
// NVIDIA only code
    if (localIdx < 32)
    {
        if (groupSize >=  64) { sdata[localIdx] += sdata[localIdx + 32]; }
        if (groupSize >=  32) { sdata[localIdx] += sdata[localIdx + 16]; }
        if (groupSize >=  16) { sdata[localIdx] += sdata[localIdx +  8]; }
        if (groupSize >=   8) { sdata[localIdx] += sdata[localIdx +  4]; }
        if (groupSize >=   4) { sdata[localIdx] += sdata[localIdx +  2]; }
        if (groupSize >=   2) { sdata[localIdx] += sdata[localIdx +  1]; }
    }
#endif
    }
    // Write result to global memory
 //   sdata[localIdx] += c;
    if(localIdx == 0)
        result[groupIdx] =  sdata[0];

}


#define vecwidth 8
#if vecwidth == 4
#define vectype float4
#else
#define vectype float8
#endif
__kernel __attribute__((reqd_work_group_size (localRange,1,1))) void sparsedot4_kernel(
    __global const dtype* px, __global struct svm_node* py,
    __global dtype* result, __global const int* pyLength, const ulong x, const ulong y)
{
    __local vectype sdata[localRange];

    int groupIdx = get_group_id(0);
    int groupSize = get_local_size(0);
    int localIdx = get_local_id(0);
    py += groupIdx*(x+1);
 //   int c = 0;

    // Reduce multiple elements per thread
//    for(unsigned int j = 0; j < 1000; ++j)
    {
    vectype tmp = (vectype)0;
 
    
    for (unsigned int i = localIdx*vecwidth; i < x; i += groupSize*vecwidth ){
        tmp.s0 += py[i+0].value * px[py[i+0].index];
        tmp.s1 += py[i+1].value * px[py[i+1].index];
        tmp.s2 += py[i+2].value * px[py[i+2].index];
        tmp.s3 += py[i+3].value * px[py[i+3].index];
#if vecwidth == 8
        tmp.s4 += py[i+4].value * px[py[i+4].index];
        tmp.s5 += py[i+5].value * px[py[i+5].index];
        tmp.s6 += py[i+6].value * px[py[i+6].index];
        tmp.s7 += py[i+7].value * px[py[i+7].index];
#endif
    }

    sdata[localIdx] = tmp;
    barrier(CLK_LOCAL_MEM_FENCE);

    // Do reduction in shared memory
    if (groupSize >= 512) {
        if (localIdx < 256) { sdata[localIdx] += sdata[localIdx + 256]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 256) {
        if (localIdx < 128) { sdata[localIdx] += sdata[localIdx + 128]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 128) {
        if (localIdx <  64) { sdata[localIdx] += sdata[localIdx +  64]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
#ifndef NVIDIA
// general code
    if (groupSize >= 64) {
        if (localIdx <  32) { sdata[localIdx] += sdata[localIdx +  32]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 32) {
        if (localIdx <  16) { sdata[localIdx] += sdata[localIdx +  16]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 16) {
        if (localIdx <  8) { sdata[localIdx] += sdata[localIdx +  8]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 8) {
        if (localIdx <  4) { sdata[localIdx] += sdata[localIdx +  4]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 4) {
        if (localIdx <  2) { sdata[localIdx] += sdata[localIdx +  2]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 2) {
        if (localIdx <  1) { sdata[localIdx] += sdata[localIdx +  1]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
#else
// NVIDIA only code
    if (localIdx < 32)
    {
        if (groupSize >=  64) { sdata[localIdx] += sdata[localIdx + 32]; }
        if (groupSize >=  32) { sdata[localIdx] += sdata[localIdx + 16]; }
        if (groupSize >=  16) { sdata[localIdx] += sdata[localIdx +  8]; }
        if (groupSize >=   8) { sdata[localIdx] += sdata[localIdx +  4]; }
        if (groupSize >=   4) { sdata[localIdx] += sdata[localIdx +  2]; }
        if (groupSize >=   2) { sdata[localIdx] += sdata[localIdx +  1]; }
    }
#endif
    }
    // Write result to global memory
 //   sdata[localIdx] += c;
    if(localIdx == 0)
        result[groupIdx] =  sdata[0].s0 + sdata[0].s2 + sdata[0].s2 + sdata[0].s3
#if vecwidth == 8
            + sdata[0].s4 + sdata[0].s5 + sdata[0].s6 + sdata[0].s7;
#endif
            ;
            

}

/*
__kernel __attribute__((reqd_work_group_size (localRange,1,1))) void sparsedot3_kernel(
    __read_only image2d_t px, __global struct svm_node* py,
    __global dtype* result, __global const int* pyLength, const ulong x, const ulong y,
    const unsigned int rows, const unsigned int rowLength)
{
    __local dtype sdata[localRange];

    int groupIdx = get_group_id(0);
    int groupSize = get_local_size(0);
    int localIdx = get_local_id(0);
    py += groupIdx*(x+1);
    sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE | CLK_FILTER_NEAREST | CLK_ADDRESS_NONE;

    // Reduce multiple elements per thread
    dtype tmp = 0;
    int cnt = 0;

        for (unsigned int i = localIdx; i < x; i += groupSize)
        {
            float4 voxel = read_imagef(px, sampler, (int2)(py[i].index-py[i].index/rowLength*rowLength, py[i].index/rowLength));
            tmp += voxel.x * py[i].value;
        }
   



    sdata[localIdx] = tmp;
    barrier(CLK_LOCAL_MEM_FENCE);

    // Do reduction in shared memory
    if (groupSize >= 512) {
        if (localIdx < 256) { sdata[localIdx] += sdata[localIdx + 256]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 256) {
        if (localIdx < 128) { sdata[localIdx] += sdata[localIdx + 128]; } barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (groupSize >= 128) {
        if (localIdx <  64) { sdata[localIdx] += sdata[localIdx +  64]; } barrier(CLK_LOCAL_MEM_FENCE);
    }

    if (localIdx < 32)
    {
        if (groupSize >=  64) { sdata[localIdx] += sdata[localIdx + 32]; }
        if (groupSize >=  32) { sdata[localIdx] += sdata[localIdx + 16]; }
        if (groupSize >=  16) { sdata[localIdx] += sdata[localIdx +  8]; }
        if (groupSize >=   8) { sdata[localIdx] += sdata[localIdx +  4]; }
        if (groupSize >=   4) { sdata[localIdx] += sdata[localIdx +  2]; }
        if (groupSize >=   2) { sdata[localIdx] += sdata[localIdx +  1]; }
    }

    // Write result to global memory
    if(localIdx == 0)
        result[groupIdx] =  sdata[0];

}
*/
