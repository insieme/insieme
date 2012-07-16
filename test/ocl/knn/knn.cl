#ifdef INSIEME
#include "ocl_device.h"
#endif


#define F4 0

#pragma insieme mark
__kernel void knn(
    const __global float* ref, const __global float* query, 
    __global float* dist, __global int* neighbours, 
    const int numRef, const int numQuery)
//    , const int dim)
{
    size_t gid = get_global_id(0);

    if(gid >= numQuery)
        return;

    size_t queryOffset = gid /* dim*/;

	size_t curNeighbour = 0;
	float curDist = MAXFLOAT;

    for(int i = 0; i < numRef; ++i)
    {
        float privateDist = 0;
        size_t refOffset = i /* dim*/;

#if F4
        float4 tmpDist = {0,0,0,0};
        int d;
        for(d = 0; d < dim-3; d+=4)
        {
/* Cypress code */
            float4 a = {ref[refOffset + d], ref[refOffset + d+1], ref[refOffset + d+2], ref[refOffset + d+3]};
            float4 b = {query[queryOffset + d], query[queryOffset + d+1], query[queryOffset + d+2], query[queryOffset + d+3]};
            float4 t = a - b;//(float4){ref[refOffset + d], ref[refOffset + d+1], ref[refOffset + d+2], ref[refOffset + d+3]} - 
                       //(float4){query[queryOffset + d], query[queryOffset + d+1], query[queryOffset + d+2], query[queryOffset + d+3]};
//            a = t * t;
            tmpDist += t * t;
//            privateDist = a.s0 + a.s1 + a.s2 + a.s3;*/
        }

        for(; d < dim; d++)
        {
            float t = ref[refOffset + d] - query[queryOffset + d];
            privateDist += t * t; 
        }
        privateDist = tmpDist.s0 + tmpDist.s1 + tmpDist.s2 + tmpDist.s3; 
#else
  //      for(int d = 0; d < dim; d++)
        {
            float t = ref[refOffset/* + d*/] - query[queryOffset/* + d*/];
            privateDist += t * t; 
        }
#endif        

        if(privateDist < curDist)
        {
            curDist = privateDist;
            curNeighbour = i;
        }
    }
    
    dist[gid] = sqrt(curDist);
    neighbours[gid] = curNeighbour;
}

