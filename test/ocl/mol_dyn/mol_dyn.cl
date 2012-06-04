#ifdef INSIEME
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void mol_dyn(__global float4* input, __global int* neigh, __global float4* output, 
			int num_elements, int neighCount, int cutsq, int lj1, int lj2, int inum) { // remeber const
	int gid = get_global_id(0);
	if (gid >= num_elements) return;

	float4 ipos = input[gid];
	float4 f = {0.0f, 0.0f, 0.0f, 0.0f};
	int j = 0;
    	while (j < neighCount) {
		int jidx = neigh[j*inum + gid];
        	float4 jpos = input[jidx];

        	// Calculate distance
        	float delx = ipos.x - jpos.x;
        	float dely = ipos.y - jpos.y;
       		float delz = ipos.z - jpos.z;
        	float r2inv = delx*delx + dely*dely + delz*delz;

        	// If distance is less than cutoff, calculate force
        	if (r2inv < cutsq) {
			r2inv = 10.0f/r2inv;
			float r6inv = r2inv * r2inv * r2inv;
			float forceC = r2inv*r6inv*(lj1*r6inv - lj2);

			f.x += delx * forceC;
			f.y += dely * forceC;
			f.z += delz * forceC;
        	}
        	j++;
	}
	output[gid] = f;
}
