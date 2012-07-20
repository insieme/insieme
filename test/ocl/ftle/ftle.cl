#ifdef INSIEME
#include "ocl_device.h"
#endif
#include "LinearAlgebra.cl"

#pragma insieme mark
__kernel void computeFTLE( __global float2* flowMap, int width, float2 dataCellSize, float advectionTime, __global float * output, int num_elements) {
        int gid = get_global_id(0);
        if (gid >= num_elements) return;
        int tx = gid % width; 
        int ty = gid / width; 

        if( tx >= 1 && tx < (width-1) && ty >= 1 && ty < num_elements/width - 1) {
		float2 left   = flowMap[gid -1];
		float2 right  = flowMap[gid + 1];
		float2 top    = flowMap[gid - width];
		float2 bottom = flowMap[gid + width];
	
		float2 delta2;
		delta2.x = 2.0f * dataCellSize.x;
		delta2.y = 2.0f * dataCellSize.y;
	
		fmat2 jacobi;
		jacobi[0][0] = (right.x  - left.x) / delta2.x;
		jacobi[0][1] = (bottom.x - top.x)  / delta2.y;
		jacobi[1][0] = (right.y  - left.y) / delta2.x;
		jacobi[1][1] = (bottom.y - top.y)  / delta2.y;
	
		fmat2 jacobiT;
		fmat2 cauchy;
		fmat2 cauchySymm;
	
		fmat2trp(jacobi, jacobiT);
		fmat2mul(jacobiT, jacobi, cauchy);
		fmat2symm(cauchy, cauchySymm);
	
		fvec2 eigenvalues;
	
		fmat2eigenvalues(cauchySymm, eigenvalues);
	
		float maxEigenvalue = fmax(eigenvalues[0], eigenvalues[1]);
	
		output[gid] = 1.0 / fabs(advectionTime) * log(sqrt(maxEigenvalue));
    }
}
