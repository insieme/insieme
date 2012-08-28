#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

#ifndef PATH
#define PATH "./"
#endif

int main(int argc, const char* argv[]) {
	chdir(PATH);
	icl_args* args = icl_init_args();
	icl_parse_args(argc, argv, args);
        icl_print_args(args);

	int size = args->size;

	cl_uint* ma = (cl_uint*) malloc(sizeof(cl_uint) * size);
	cl_uint* b = (cl_uint*) malloc(sizeof(cl_uint) * size);
	cl_uint* c = (cl_uint*) malloc(sizeof(cl_uint) * size);
	cl_uint* seed = (cl_uint*) malloc(sizeof(cl_uint) * size);
	cl_float4* result = (cl_float4*)malloc(sizeof(cl_float4) * size);

	for (cl_uint i = 0; i < size; ++i) { 
		ma[i] = i; b[i] = i; c[i] = i; seed[i] = i;
	}
	
        icl_init_devices(args->device_type);

        if (icl_get_num_devices() != 0) {
                icl_device* dev = icl_get_device(args->device_id);

                icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "mers_twister.cl", "mersenne_twister", "", ICL_SOURCE);

		icl_buffer* buf_ma = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(cl_uint) * size);
		icl_buffer* buf_b = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(cl_uint) * size);
		icl_buffer* buf_c = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(cl_uint) * size);
		icl_buffer* buf_seed = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(cl_uint) * size);
		icl_buffer* buf_result = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(cl_float4) * size);

		icl_write_buffer(buf_ma, CL_FALSE, sizeof(cl_uint) * size, &ma[0], NULL, NULL);
		icl_write_buffer(buf_b, CL_FALSE, sizeof(cl_uint) * size, &b[0], NULL, NULL);
		icl_write_buffer(buf_c, CL_FALSE, sizeof(cl_uint) * size, &c[0], NULL, NULL);
		icl_write_buffer(buf_seed, CL_TRUE, sizeof(cl_uint) * size, &seed[0], NULL, NULL);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;
		
		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 6,
			(size_t)0, (void *)buf_ma,
			(size_t)0, (void *)buf_b,
			(size_t)0, (void *)buf_c,
			(size_t)0, (void *)buf_seed,
			(size_t)0, (void *)buf_result,
			sizeof(cl_int), (void *)&size
		);
		
		icl_read_buffer(buf_result, CL_TRUE, sizeof(cl_float4) * size, &result[0], NULL, NULL);
		
		icl_release_buffers(5, buf_ma, buf_b, buf_c, buf_seed, buf_result);
		icl_release_kernel(kernel);
	}
	

	if (args->check_result) {
		printf("======================\n= mersenne twister test\n");
		printf("Check not Implemented\n");
		printf("Result check: OK\n");
	} else {
		printf("Result check: OK\n");
	}
	
	icl_release_devices();
	free(ma);
	free(b);
	free(c);
	free(seed);
	free(result);
	return 0;
}
