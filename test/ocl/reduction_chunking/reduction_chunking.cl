#ifdef INSIEME
#include "ocl_device.h"
#endif
#pragma insieme mark
__kernel void reduce(
	__global float* buffer,
  __const int chunkSize, 
  __const int length, 
  __global float* result) 
{ 
	int gid = get_global_id(0);
	int global_index = gid * chunkSize;
	float acc = INFINITY; 
	int upper_bound = (gid + 1) * chunkSize; 
	if (upper_bound > length) upper_bound = length;
	while (global_index < upper_bound) { 
	   float element = buffer[global_index];
	   acc = (acc<element) ? acc : element; 
	   global_index++; 
	} 
	result[gid] = acc; 
}
