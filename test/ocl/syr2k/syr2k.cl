/**
 * syr2k.cl: This file is part of the PolyBench/GPU 1.0 test suite.
 *
 *
 * Contact: Scott Grauer-Gray <sgrauerg@gmail.com>
 * Louis-Noel Pouchet <pouchet@cse.ohio-state.edu>
 * Web address: http://www.cse.ohio-state.edu/~pouchet/software/polybench/GPU
 */

#ifdef INSIEME
#include "ocl_device.h"
#pragma insieme mark
#endif
__kernel void syr2k(__global float *a, __global float *b, __global float *c, float alpha, float beta, int m, int n) 
{    
   	int j = get_global_id(0) % n;
	int i = get_global_id(0) / n;

	if ((i < n))
	{
		c[get_global_id(0)] *= beta;
		
		int k;
		float tmp = 0;
		for(k = 0; k < m; k++)
		{
			tmp += alpha * a[i * m + k] * b[j * m + k] 
			                     + alpha * b[i * m + k] * a[j * m + k];
		}
		 c[get_global_id(0)] += tmp;
	}
}


