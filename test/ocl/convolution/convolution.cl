#ifdef INSIEME
#include "ocl_device.h"
#endif
#pragma insieme mark
__kernel void convolution(__global int* input, __constant int* mask, __global int* output, int num_elements, int width) {
	int gid = get_global_id(0);
	if (gid >= num_elements) return;
        int tx = gid % width;
        int ty = gid / width;
	if (tx == 0 || ty == 0 || tx == width-1 || ty == width-1) {
		output[gid] = 0;
		return;	
	}
	int sum = 0;

	int square = 3;
	
	int tmpx = tx - 1;
	int tmpy = ty - 1;
	for (int r = 0; r < square; ++r) {
		for (int c = 0; c < square; ++c) {
			sum += mask[r * square + c] * input[(tmpy + r ) * width + tmpx + c];
                }
        }
	output[gid] = sum;
}
