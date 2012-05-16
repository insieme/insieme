#ifdef INSIEME
#include "ocl_device.h"
#endif
#pragma insieme mark
__kernel void sinewave(__global float4* output, int num_elements) {
        int gid = get_global_id(0);
        if (gid >= num_elements) return;

	float time = 5.0f;
	float u = gid*2.0f - 1.0f;
	float v = gid*3.0f - 2.0f;
        float w = gid*4.0f - 3.0f;
        float z = gid*5.0f - 4.0f;
	float freq = 4.0f;
	for (int i = 0; i < 50; ++i) {
		u = sin(u*freq + time) * cos(v*freq + time) * 0.5f + sin(w*freq + time) * cos(z*freq + time) * 0.5f;
       		v = sin(u*freq + time) * cos(v*freq + time) * 0.5f + sin(w*freq + time) * cos(z*freq + time) * 0.5f;
        	w = sin(u*freq + time) * cos(v*freq + time) * 0.5f + sin(w*freq + time) * cos(z*freq + time) * 0.5f;
        	z = sin(u*freq + time) * cos(v*freq + time) * 0.5f + sin(w*freq + time) * cos(z*freq + time) * 0.5f;
	}
	
	float4 out = (float4)(u, v, w, z);
	output[gid] = out;
}
