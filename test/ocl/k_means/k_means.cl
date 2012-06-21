#ifdef INSIEME
#include "ocl_device.h"
#endif

#ifndef FLT_MAX
#define FLT_MAX 500000.0
#endif

#pragma insieme mark
__kernel void k_means( __global float* features, __global float* clusters, __global int* membership, int num_elements, int nclusters, int nfeatures) {
        int gid = get_global_id(0);
        if (gid >= num_elements) return;
	int index = 0;
	float min_dist = FLT_MAX;
	for (int i = 0; i < nclusters; i++) {
		float dist = 0;
		for (int l = 0; l < nfeatures; l++) {
			dist += (features[l * num_elements + gid] - clusters[i * nfeatures + l]) * (features[l * num_elements + gid] - clusters[i * nfeatures + l]);
		}
		if (dist < min_dist) {
			min_dist = dist;
			index = gid;
		}
	}
	membership[gid] = index;
}
