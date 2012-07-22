#ifdef INSIEME
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void reduce(
	__global float16* buffer,
  __const int chunkSize, 
  __const int length, 
  __global float* result) 
{ 
	int gid = get_global_id(0);
	
	if(gid >= length)
		return;
	
/*	int global_index = gid * chunkSize;
	float acc = INFINITY; 
	int upper_bound = (gid + 1) * chunkSize; 
	int nElemin = length * chunkSize;
	if (upper_bound > nElemin) upper_bound = nElemin;
	while (global_index < upper_bound) { 
	   float element = buffer[global_index];
	   acc = (acc<element) ? acc : element; 
	   global_index++; 
	}
	*/
	
	float16 val = buffer[gid];
	
	float acc = min(val.s0, val.s1);
	acc = min(acc, val.s2);
	acc = min(acc, val.s3);
	acc = min(acc, val.s4);
	acc = min(acc, val.s5);
	acc = min(acc, val.s6);
	acc = min(acc, val.s7);
	acc = min(acc, val.s8);
	acc = min(acc, val.s9);
	acc = min(acc, val.sA);
	acc = min(acc, val.sB);
	acc = min(acc, val.sC);
	acc = min(acc, val.sD);
	acc = min(acc, val.sE);
	acc = min(acc, val.sF);
	 
	result[gid] = acc; 
}
