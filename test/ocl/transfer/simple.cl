#ifdef INSIEME
#include "ocl_device.h"
#endif
#pragma insieme mark
__kernel void simple(__global int* input, __global int* output, int num_elements) {
	int gid = get_global_id(0);
	if (gid >= num_elements) return;
	output[gid] = input[gid];
}
