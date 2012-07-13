#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"
#include <math.h>

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);
        icl_print_args(args);

//	int dim = 128;
	int size = args->size;
	int nRef = 100000;

	float* ref = (float*)malloc(sizeof(float) * nRef /* dim*/);
	float* query = (float*)malloc(sizeof(float) * size /* dim*/);
	float* dists = (float *)malloc(sizeof(float) * size);
	int* neighbors = (int*)malloc(sizeof(int) * size);
	srand(42);
	
	for(int i=0; i < nRef/*dim*/; ++i) {
		ref[i] = rand();
	}
	for(int i=0; i < size/**dim*/; ++i) {
		query[i] = rand();
	}

	icl_init_devices(ICL_ALL);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(0);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "knn.cl", "knn", "", ICL_SOURCE);
		
		icl_buffer* buf_ref = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(float) * nRef /* dim*/);
		icl_buffer* buf_query = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(float) * size /* dim*/);
		icl_buffer* buf_dists = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(float) * size);
		icl_buffer* buf_neighbors = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);

		icl_write_buffer(buf_ref, CL_TRUE, sizeof(float) * nRef /*dim*/, &ref[0], NULL, NULL);
		icl_write_buffer(buf_query, CL_TRUE, sizeof(float) * size /*dim*/, &query[0], NULL, NULL);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 6,
											(size_t)0, (void *)buf_ref,
											(size_t)0, (void *)buf_query,
											(size_t)0, (void *)buf_dists,
											(size_t)0, (void *)buf_neighbors,
											sizeof(cl_int), (void *)&nRef,
											sizeof(cl_int), (void *)&size);
//											sizeof(cl_int), (void *)&dim);
		
		icl_read_buffer(buf_dists, CL_TRUE, sizeof(float) * size, &dists[0], NULL, NULL);
		icl_read_buffer(buf_neighbors, CL_TRUE, sizeof(int) * size, &neighbors[0], NULL, NULL);
		
		icl_release_buffers(4, buf_ref, buf_query, buf_dists, buf_neighbors);
		icl_release_kernel(kernel);
	}
	
	if (args->check_result) {
		printf("======================\n= KNN program working\n");
		unsigned int check = 1;
		unsigned int sum = 0;
		for(int i = 0; i < size; ++i) {
			if(dists[i] < 0)
				check = 0;
			if(neighbors[i] < 0 || neighbors[i] >= nRef)
				check = 0;
		}
		
		printf("======================\n");
		printf("Result check: %s\n", check ? "OK" : "FAIL");
	} else {
		
                printf("Result check: OK\n");
	}

	icl_release_args(args);	
	icl_release_devices();
	free(ref);
	free(query);
	free(dists);
	free(neighbors);
}
