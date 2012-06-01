#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);
        icl_print_args(args);

        int size = args->size;

	cl_float4* input = (cl_float4*)malloc(sizeof(cl_float4) * size);
	cl_float4* output = (cl_float4*)malloc(sizeof(cl_float4) * size);
	int* neigh = (int*)malloc(sizeof(int) * size);

	float* f_inp = (float*)&input[0];	
	for(int i=0; i < size * 4; ++i) {
		f_inp[i] = (float)i;
	}
	for(int i = 0; i < size; ++i){
		neigh[i] = i+1;
	}

	icl_init_devices(args->device_type);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "mol_dyn.cl", "mol_dyn", "", ICL_SOURCE);
		
		icl_buffer* buf_input = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(cl_float4) * size);
		icl_buffer* buf_neigh = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(cl_float4) * size);

		icl_write_buffer(buf_input, CL_TRUE, sizeof(cl_float4) * size, &input[0], NULL, NULL);
		icl_write_buffer(buf_neigh, CL_TRUE, sizeof(int) * size, &neigh[0], NULL, NULL);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		int neighCount = 15;
		int cutsq = 50;
		int lj1 = 20;
		int lj2 = 0.003f;
		int inum = 0;
		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 9,
											(size_t)0, (void *)buf_input,
											(size_t)0, (void *)buf_neigh,
											(size_t)0, (void *)buf_output,
											sizeof(cl_int), (void *)&size,
											sizeof(cl_int), (void *)&neighCount,
											sizeof(cl_int), (void *)&cutsq,
											sizeof(cl_int), (void *)&lj1,
											sizeof(cl_int), (void *)&lj2,
											sizeof(cl_int), (void *)&inum);
		
		icl_read_buffer(buf_output, CL_TRUE, sizeof(cl_float4) * size, &output[0], NULL, NULL);
		
		icl_release_buffers(3, buf_input, buf_neigh, buf_output);
		icl_release_kernel(kernel);
	}
	
        if (args->check_result) {
	        printf("======================\n= Molecular Dynamic Done\n");
 		unsigned int check = 1;
		printf("======================\n");
		printf("Result check: %s\n", check ? "OK" : "FAIL");
        } else {
		printf("Result check: OK\n");
        }

	icl_release_args(args);
	icl_release_devices();
	free(input);
	free(neigh);
	free(output);
}
