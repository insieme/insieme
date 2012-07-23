#ifdef INSIEME
#include "ocl_device.h"
#endif

#pragma insieme mark
__kernel void geo_mean(
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
	
	float mean = log(val.s0) + log(val.s1) + log(val.s2) + log(val.s3) + log(val.s4) + log(val.s5) + log(val.s6) + log(val.s7) + 
		log(val.s8) + log(val.s9) + log(val.sA) + log(val.sB) + log(val.sC) + log(val.sD) + log(val.sE) + log(val.sF);
	mean /= chunkSize;
	 
	result[gid] = pow(10,mean); 
}
