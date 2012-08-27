#ifdef INSIEME
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void bit_compression(__global uint4* input, __global uint* num_bits, __global uint* output, int length) {
	int gid = get_global_id(0);
	if(gid >= length) return;
	
	uint4 in = input[gid];
	int bits = num_bits[gid];
	uint tmp = 0;
	if (bits == 2) {
		tmp |= (in.x << (32-bits)) & 3221225472u;
		tmp |= (in.y << (28-bits)) &  805306368u;
		tmp |= (in.z << (24-bits)) &  201326592u;
		tmp |= (in.w << (20-bits)) &   50331648u;
	} else if (bits == 4) {
		tmp |= (in.x << (32-bits)) & 4026531840u;
                tmp |= (in.y << (28-bits)) &  251658240u;
                tmp |= (in.z << (24-bits)) &   15728640u;
                tmp |= (in.w << (20-bits)) &     983040u;
	} else if (bits == 8) {
                tmp |= (in.x << (32-bits)) & 4278190080u;
                tmp |= (in.y << (28-bits)) &   16711680u;
                tmp |= (in.z << (24-bits)) &      65280u;
                tmp |= (in.w << (20-bits)) &        255u;
	}
	output[gid] = tmp;
}

