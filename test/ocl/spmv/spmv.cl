#ifdef INSIEME
#include "ocl_device.h"
#endif
#pragma insieme mark
__kernel void spmv(__global int* rowdel, __global int* vec, __global int* val, __global int* col, __global int* output, int num_elements) {
	int gid = get_global_id(0);
	if (gid >= num_elements) return;
	int sum = 0;
	int start = rowdel[gid];
	int stop  = rowdel[gid+1];
	for (int j = start; j < stop; ++j) {
		int c = col[j];
		sum += val[j] * vec[c];
	}
	output[gid] = sum;
}
