#ifdef INSIEME
#include "ocl_device.h"
#endif
#pragma insieme mark
__kernel void lin_reg(__global float* input1, __global float* input2, __global float* alpha, __global float* beta, __global float* output, int num_elements) {
	int gid = get_global_id(0);
	if (gid >= num_elements) return;

        float a = alpha[gid];
        float b = beta[gid];
        float error = 0;

        for(int i=0; i<num_elements; i++)
        {
                float e = (a * input1[i] + b) - input2[i];
                error += e * e;
        }
        output[gid] = error;
}
