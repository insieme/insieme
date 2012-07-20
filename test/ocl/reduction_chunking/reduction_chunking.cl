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
	int nElemax = length * chunkSize;
	if (upper_bound > nElemax) upper_bound = nElemax;
	while (global_index < upper_bound) { 
	   float element = buffer[global_index];
	   acc = (acc<element) ? acc : element; 
	   global_index++; 
	}
	*/
	
	float16 val = buffer[gid];
	
	float acc = max(val.s0, val.s1);
	acc = max(acc, val.s2);
	acc = max(acc, val.s3);
	acc = max(acc, val.s4);
	acc = max(acc, val.s5);
	acc = max(acc, val.s6);
	acc = max(acc, val.s7);
	acc = max(acc, val.s8);
	acc = max(acc, val.s9);
	acc = max(acc, val.sA);
	acc = max(acc, val.sB);
	acc = max(acc, val.sC);
	acc = max(acc, val.sD);
	acc = max(acc, val.sE);
	acc = max(acc, val.sF);
	 
	result[gid] = acc; 
}
