#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

#ifndef PATH
#define PATH "./"
#endif

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);
        icl_print_args(args);

	chdir(PATH);

    int size = args->size;
	int nfeatures = 2;
	int nclusters = 3;
	int feature_size = nfeatures * size;
	int cluster_size = nclusters * nfeatures;

	float* features = (float*) malloc(sizeof(float) * feature_size);
	float* clusters = (float*) malloc(sizeof(float) * cluster_size);
	int* membership = (int *)malloc(sizeof(int) * size);
	
	for(int i=0; i < feature_size; ++i) {
		features[i] = 2.0f;
	}
    for(int i=0; i < cluster_size; ++i) {
            clusters[i] = 1.0f;
    }
   for(int i=0; i < size; ++i) {
            membership[i] = 0.0f;
    }

	
    icl_init_devices(args->device_type);

	icl_start_energy_measurement();

    if (icl_get_num_devices() != 0) {
        icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "k_means.cl", "k_means", "", ICL_SOURCE);
		
		icl_buffer* buf_features = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(float) * feature_size);
		icl_buffer* buf_clusters = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(float) * cluster_size);
		icl_buffer* buf_membership = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		for (int i = 0; i < args->loop_iteration; ++i) {
			icl_write_buffer(buf_features, CL_TRUE, sizeof(float) * feature_size, &features[0], NULL, NULL);
			icl_write_buffer(buf_clusters, CL_TRUE, sizeof(float) * cluster_size, &clusters[0], NULL, NULL);

			icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 6,
												(size_t)0, (void *)buf_features,
												(size_t)0, (void *)buf_clusters,
												(size_t)0, (void *)buf_membership,
												sizeof(cl_int), (void *)&size,
												sizeof(cl_int), (void *)&nclusters,
												sizeof(cl_int), (void *)&nfeatures);
		
			icl_read_buffer(buf_membership, CL_TRUE, sizeof(int) * size, &membership[0], NULL, NULL);
		}
		
		icl_release_buffers(3, buf_features, buf_clusters, buf_membership);
		icl_release_kernel(kernel);
	}
	
	icl_stop_energy_measurement();
	
    if (args->check_result) {
        printf("======================\n= K-means Done\n");
	unsigned int check = 1;
		for(unsigned int x = 0; x < size; ++x) {
			int index = 0;
			float min_dist = 500000.0f;
			for (int i = 0; i < nclusters; i++) {
				float dist = 0;
				for (int l = 0; l < nfeatures; l++) {
		    		dist += (features[l * size + x] - clusters[i * nfeatures + l]) * (features[l * size + x] - clusters[i * nfeatures + l]);
				}
				if (dist < min_dist) {
		    		min_dist = dist;
		    		index = x;
				}
			}
			if(membership[x] != index) {
				check = 0;
				printf("= fail at %d, expected %d / actual %d\n", x, index, membership[x]);
				break;
			}
		}
		printf("======================\n");
		printf("Result check: %s\n", check ? "OK" : "FAIL");
    } else {
		printf("Result check: OK\n");
    }

	icl_release_args(args);
	icl_release_devices();
	free(features);
	free(clusters);
	free(membership);
}
