#include "../../../frontend/test/ocl_device.h"

#pragma insieme mark
__kernel void constantMemArg(__constant double* c) {
    double element = c[0];
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
__kernel void allMemArg(__constant double* c, __global float* g, __local int* l, short p) {
    int element = l[0];
}

