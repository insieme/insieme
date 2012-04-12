#pragma OPENCL EXTENSION cl_khr_fp64: enable

#ifdef INSIEME
#include "ocl_device.h"
#endif

#include "n_body.h"

#pragma insieme mark
__kernel void n_body(__global body* B_read, __global body* B_write, int size) {
	uint gid = get_global_id(0);
	
	if(gid >= size)
		return;

	force F = triple_zero(); // set forces to zero
	for(int k=0; k<size; k++) {
		if(gid!=k) {
			triple dist = SUB(B_read[k].pos, B_read[gid].pos);
			double r = ABS(dist);
			double f = (B_read[gid].m * B_read[k].m) / (r*r);
			force cur = MULS(NORM(dist), f);
			F = ADD(F, cur);
		}
	}

	B_write[gid].m = B_read[gid].m;
	B_write[gid].v = ADD(B_read[gid].v, DIVS(F, B_read[gid].m));
	B_write[gid].pos = ADD(B_read[gid].pos, B_read[gid].v);
}
