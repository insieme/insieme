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
	
	body bgid = B_read[gid];
	for(int k=0; k<size; k++) {
		body bk = B_read[k];
		triple dist = SUB(bk.pos, bgid.pos);
                double r = ABS(dist);
                double f = (gid == k) ? 0 : (B_read[gid].m * B_read[k].m) / (r*r);
                force cur = MULS(NORM(dist), f);
                F = ADD(F, cur);
	}

	B_write[gid].m = bgid.m;
	B_write[gid].v = ADD(bgid.v, DIVS(F, bgid.m));
	B_write[gid].pos = ADD(bgid.pos, bgid.v);
}
