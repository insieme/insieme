#ifdef INSIEME
#include "ocl_device.h"
#endif
#pragma insieme mark
__kernel void convolution(__global int* input, __constant int* mask, __global int* output, int num_elements, int width, int mask_width) {
	int gid = get_global_id(0);
	if (gid >= num_elements) return;
        int tx = gid % width;
        int ty = gid / width;
	int offset = mask_width/2;
	if (tx < offset || ty < offset || tx >= (width-offset) || ty >= (width-offset)) {
		output[gid] = 0;
		return;	
	}
	int sum = 0;

	int tmpx = tx - offset;
	int tmpy = ty - offset;
	for (int r = 0; r < mask_width; ++r) {
		for (int c = 0; c < mask_width; ++c) {
			sum += mask[r * mask_width + c] * input[(tmpy + r ) * width + tmpx + c];
                }
        }
	output[gid] = sum;
}
