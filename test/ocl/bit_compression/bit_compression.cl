#ifdef INSIEME
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void bit_compression(__global uint4* input, __global uint* num_bits, __global uint* output, int length) {
	int gid = get_global_id(0);
	if(gid >= length) return;
	
	uint4 in = input[gid];
	uint bits = num_bits[gid];
	uint tmp = 0;
	if (bits == 2) {
		tmp |= (in.x << (32-bits)) & 3221225472;
		tmp |= (in.y << (28-bits)) &  805306368;
		tmp |= (in.z << (24-bits)) &  201326592;
		tmp |= (in.w << (20-bits)) &   50331648;
	} else if (bits == 4) {
		tmp |= (in.x << (32-bits)) & 4026531840;
                tmp |= (in.y << (28-bits)) &  251658240;
                tmp |= (in.z << (24-bits)) &   15728640;
                tmp |= (in.w << (20-bits)) &     983040;
	} else if (bits == 8) {
                tmp |= (in.x << (32-bits)) & 4278190080;
                tmp |= (in.y << (28-bits)) &   16711680;
                tmp |= (in.z << (24-bits)) &      65280;
                tmp |= (in.w << (20-bits)) &        255;
	}
	output[gid] = tmp;
}

