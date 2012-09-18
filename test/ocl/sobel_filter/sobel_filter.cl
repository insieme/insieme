#ifdef INSIEME
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void sobel_filter(__global uchar4* inputImage, __global uchar4* outputImage, int num_elements, uint width) {
        int gid = get_global_id(0);
        if (gid >= num_elements) return;

        int tx = gid % width;
        int ty = gid / width;

	if( tx >= 1 && tx < (width-1) && ty >= 1 && ty < num_elements/width - 1) {
		float4 i00 = convert_float4(inputImage[gid - 1 - width]);
		float4 i10 = convert_float4(inputImage[gid - width]);
		float4 i20 = convert_float4(inputImage[gid + 1 - width]);
		float4 i01 = convert_float4(inputImage[gid - 1]);
		float4 i11 = convert_float4(inputImage[gid]);
		float4 i21 = convert_float4(inputImage[gid + 1]);
		float4 i02 = convert_float4(inputImage[gid - 1 + width]);
		float4 i12 = convert_float4(inputImage[gid + width]);
		float4 i22 = convert_float4(inputImage[gid + 1 + width]);
		float4 two = (float4)2;

		float4 Gx =   i00 + two * i10 + i20 - i02  - two * i12 - i22;
		float4 Gy =   i00 - i20 + two * i01 - two * i21 + i02  -  i22;

		outputImage[gid] = convert_uchar4(hypot(Gx, Gy)/two);
	}
}
