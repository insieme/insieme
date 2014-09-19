#ifdef INSIEME
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void vec_add(__global int* input1, __global int* input2, __global int* output, int num_elements) {
	int gid = get_global_id(0);
	if (gid >= num_elements) return;
	output[gid] = (input1[gid] + input2[gid]);
}

#pragma insieme mark
__kernel void vec_mul2(__global int* input1, __global int* input2, int num_elements) {
	int gid = get_global_id(0);
	if (gid >= num_elements) return;
	input2[gid] = (input1[gid] * 2);
}
