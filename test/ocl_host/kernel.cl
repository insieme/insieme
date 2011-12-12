#include "ocl_device.h"
#pragma insieme mark
__kernel void fun(__global int *src, __global int *dst, int factor){
		int i = get_global_id(0);
		dst[i] = src[i] * factor;


}
