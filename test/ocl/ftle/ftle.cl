#ifdef INSIEME
#include "ocl_device.h"
#endif

float4 float4mul(float4 a, float4 b) {
	float4 c;
	c.x = a.x * b.x + a.y * b.y;
	c.y = a.x * b.y + a.y * b.w;
	c.z = a.z * b.x + a.w * b.z;
	c.w = a.z * b.y + a.w * b.w;
	return c;
}

float4 float4trp(float4 a) {
	float4 b = a;
        float x;
        x = b.y;
        b.y =  b.z;
        b.z = x;
	return b;
}

float4 float4symm(float4 a) {
        float4 b = float4trp(a);
	b = b + a;
	b = b * 0.5f;
	return b;
}

float2 float4invariants(float4 m) {
	float2 pqr;
        pqr.x = m.x * m.w - m.y * m.z;
        pqr.y = -(m.x + m.w);
	return pqr;
}


float2 float2squareroots(float2 a) {
        float discrim, root;
	float2 b;
        discrim = a.y * a.y - 4 * a.x;

        if (discrim >= 0) {
                root = sqrt(discrim);
                b.x = (-a.y - root) / 2.0f;
                b.y = (-a.y + root) / 2.0f;
        } else {
                root = sqrt(-discrim);
                b.x = -a.x / 2.0f;
                b.y = root / 2.0f;
        }
	return b;
}


float2 float4eigenvalues(float4 m) {
        float2 pqr;
        pqr = float4invariants(m);
        return (float2squareroots(pqr));
}

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
	
		float4 jacobi;
		jacobi.x = (right.x  - left.x) / delta2.x;
		jacobi.y = (bottom.x - top.x)  / delta2.y;
		jacobi.z = (right.y  - left.y) / delta2.x;
		jacobi.w = (bottom.y - top.y)  / delta2.y;
	
		float4 jacobiT, cauchy, cauchySymm;
		jacobiT = float4trp(jacobi);
		cauchy = float4mul(jacobiT, jacobi);
		cauchySymm = float4symm(cauchy);
	
		float2 eigenvalues;
		eigenvalues = float4eigenvalues(cauchySymm);
		eigenvalues = eigenvalues + float4eigenvalues(cauchySymm);
		eigenvalues = eigenvalues + float4eigenvalues(cauchySymm);
		eigenvalues = eigenvalues + float4eigenvalues(cauchySymm);
		float maxEigenvalue = max(eigenvalues.x, eigenvalues.y);
		
		output[gid] = 1.0 / fabs(advectionTime) * log(sqrt(maxEigenvalue));
    }
}
