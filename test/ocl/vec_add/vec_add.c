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

	int* input1 = (int*)malloc(sizeof(int) * size);
	int* input2 = (int*) malloc(sizeof(int) * size);
	int* output = (int *)malloc(sizeof(int) * size);
	
	for(int i=0; i < size; ++i) {
		input1[i] = i;
		input2[i] = i*2;
	}

	icl_init_devices(ICL_ALL);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(0);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "vec_add.cl", "vec_add", "", ICL_SOURCE);
		
		icl_buffer* buf_input1 = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_input2 = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);

		icl_write_buffer(buf_input1, CL_TRUE, sizeof(int) * size, &input1[0], NULL, NULL);
		icl_write_buffer(buf_input2, CL_TRUE, sizeof(int) * size, &input2[0], NULL, NULL);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 4,
											(size_t)0, (void *)buf_input1,
											(size_t)0, (void *)buf_input2,
											(size_t)0, (void *)buf_output,
											sizeof(cl_int), (void *)&size);
		
		icl_read_buffer(buf_output, CL_TRUE, sizeof(int) * size, &output[0], NULL, NULL);
		
		icl_release_buffers(3, buf_input1, buf_input2, buf_output);
		icl_release_kernel(kernel);
	}
	
        if (args->check_result) {
	        printf("======================\n= Vector Addition Done\n");
 		unsigned int check = 1;
		for(unsigned int i = 0; i < size; ++i) {
			if(output[i] != i*3/2) {
				check = 0;
 				printf("= fail at %d, expected %d / actual %d", i, i*3/2, output[i]);
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
	free(input1);
	free(input2);
	free(output);
}
