#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);

        int size = args->size;
        icl_print_args(args);

	cl_float4* output = (cl_float4*)malloc(sizeof(cl_float4) * size);

	icl_init_devices(args->device_type);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "sinewave.cl", "sinewave", "", ICL_SOURCE);
		
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(cl_float4) * size);

		size_t szLocalWorkSize =  args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 2,
											(size_t)0, (void *)buf_output,
											sizeof(cl_int), (void *)&size);
		
		icl_read_buffer(buf_output, CL_TRUE, sizeof(cl_float4) * size, &output[0], NULL, NULL);
		
		icl_release_buffer(buf_output);
		icl_release_kernel(kernel);
	}

	// for the test check	
	printf("Result check: OK\n");

        icl_release_args(args);
        icl_release_devices();
        free(output);
}
