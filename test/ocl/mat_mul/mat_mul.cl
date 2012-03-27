#ifdef INSIEME
#include "ocl_device.h"
#endif
#pragma insieme mark
__kernel void mat_mul(__global int* input1, __global int* input2, __global int* output, int num_elements, int width) {
	int gid = get_global_id(0);
	if (gid >= num_elements) return;
	int tx = gid % width;
	int ty = gid / width;
	int sum = 0;
	for (int k = 0; k < width; ++k) { sum += input1[tx * width + k] * input2[k * width + ty]; }
	output[tx * width + ty] = sum;
}
