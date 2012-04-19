#ifdef INSIEME
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void sobel_filter(__global uchar4* inputImage, __global uchar4* outputImage, uint width, uint height) {
	uint id = get_global_id(0);
	if(id > (width*height)) return;

	uint x = id % width;
	uint y = id / width;

	float4 Gx = (float4)(0);
	float4 Gy = Gx;
	
	int c = x + y * width;

	if( x >= 1 && x < (width-1) && y >= 1 && y < height - 1) {
		float4 i00 = convert_float4(inputImage[c - 1 - width]);
		float4 i10 = convert_float4(inputImage[c - width]);
		float4 i20 = convert_float4(inputImage[c + 1 - width]);
		float4 i01 = convert_float4(inputImage[c - 1]);
		float4 i11 = convert_float4(inputImage[c]);
		float4 i21 = convert_float4(inputImage[c + 1]);
		float4 i02 = convert_float4(inputImage[c - 1 + width]);
		float4 i12 = convert_float4(inputImage[c + width]);
		float4 i22 = convert_float4(inputImage[c + 1 + width]);
		float4 two = (float4)(2);		


		Gx =   i00 + two * i10 + i20 - i02  - two * i12 - i22;
		Gy =   i00 - i20 + two * i01 - two * i21 + i02  -  i22;

		outputImage[c] = convert_uchar4(hypot(Gx, Gy)/two);
	}
}
