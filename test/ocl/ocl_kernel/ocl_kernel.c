// TODO positive definition check fucks up intetgration tests shuld be fixed 
#ifndef NO_INSIEME  //should be set when using the kernel function with the insieme compiler
#include "../../../code/frontend/test/inputs/ocl_device.h"
#endif

#pragma insieme mark
__kernel void constantMemArg(__constant float* c) {
    float element = c[0];
}

#pragma insieme mark
__kernel void globalMemArg(__global float* g) {
    __global float* tt = &g[0];
    __global float* tx = &g[3];
    float element = g[0];
//    __global float4* privateGvec = (float4*)g;

    // Not supported - decl + assign
    // __global float* t;
    // t = &g[0];
}

#pragma insieme mark
__kernel void localMemArg(__local int* l) {
    // __local int element; // BUG
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
    uint gid = get_global_id(0) + get_group_id(0);// * get_local_size(0);
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

    l[lid] = (float)c[gid];
    barrier(CLK_LOCAL_MEM_FENCE);
    ga[gid] = l[lid];
}
//* init zero missing, so no in kernel local variables possible
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

#pragma insieme mark
__kernel void vectorTest(__constant float* c, __global float* ga, __global int* gb, __local float* l, uint pa, int pb ) {
	float4 zero = (float4)(0);
	float4 two = (float4)(2);
	float4 three = (float4)(3);
	zero = zero + two;

	uchar4 four = (uchar4) (5);
	float4 convf = convert_float4(four);
	float4 res4 = convf * two;
	four = convert_uchar4(res4 + zero);

	float f3[3] = {0, 1, 2};
	f3[0] = 3;
	f3[1] = 4;
	f3[2] = 5;
	//float* fp = &f3[2]; FIXME: BUG in the backend
	//*fp = 5;
}

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

