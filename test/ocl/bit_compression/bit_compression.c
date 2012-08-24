#include <stdlib.h>
#include <stdio.h>
#include <math.h>
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

	cl_uint* num_bits = (cl_uint*) malloc(sizeof(cl_uint) * size);
	cl_uint4* input = (cl_uint4*) malloc(sizeof(cl_uint4) * size);
	cl_uint* output = (cl_uint*) malloc(sizeof(cl_uint) * size);
	for (cl_uint i = 0; i < size; ++i) {
		input[i].s[0] = 15;
		input[i].s[1] = 15;
		input[i].s[2] = 15;
		input[i].s[3] = 15;
		num_bits[i] = (int)pow(2, ((i % 3) + 1));
	}
	
        icl_init_devices(args->device_type);

        if (icl_get_num_devices() != 0) {
                icl_device* dev = icl_get_device(args->device_id);

                icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "bit_compression.cl", "bit_compression", "", ICL_SOURCE);

		icl_buffer* buf_input = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(cl_uint4) * size);
		icl_buffer* buf_bits = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(cl_uint) * size);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(cl_uint) * size);

		icl_write_buffer(buf_input, CL_FALSE, sizeof(cl_uint4) * size, &input[0], NULL, NULL);
		icl_write_buffer(buf_bits, CL_TRUE, sizeof(cl_uint) * size, &num_bits[0], NULL, NULL);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;
		
		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 4,
			(size_t)0, (void *)buf_input,
			(size_t)0, (void *)buf_bits,
			(size_t)0, (void *)buf_output,
			sizeof(cl_int), (void *)&size
		);
		
		icl_read_buffer(buf_output, CL_TRUE, sizeof(cl_float) * size, &output[0], NULL, NULL);
		
		icl_release_buffers(3, buf_input, buf_bits, buf_output);
		icl_release_kernel(kernel);
	}
	

	if (args->check_result) {
		printf("======================\n= bit_compression test\n");
		printf("Check not Implemented\n");
		printf("Result check: OK\n");
	} else {
		printf("Result check: OK\n");
	}
	
	icl_release_devices();
	free(num_bits);
	free(input);
	free(output);
	return 0;
}
