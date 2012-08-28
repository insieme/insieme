#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <unistd.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

#ifndef PATH
#define PATH "./"
#endif

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);

	chdir(PATH);

	int width = (int)floor(sqrt(args->size));
	args->size = width * width;
        int size = args->size;
        icl_print_args(args);

	int* input1 = (int*)malloc(sizeof(int) * size);
	int* input2 = (int*) malloc(sizeof(int) * size);
	int* output = (int *)malloc(sizeof(int) * size);
	
	for(int i=0; i < size; ++i) {
		input1[i] = i;
		input2[i] = i;
	}

	icl_init_devices(args->device_type);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "mat_mul.cl", "mat_mul", "", ICL_SOURCE);
		
		icl_buffer* buf_input1 = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_input2 = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);

		icl_write_buffer(buf_input1, CL_TRUE, sizeof(int) * size, &input1[0], NULL, NULL);
		icl_write_buffer(buf_input2, CL_TRUE, sizeof(int) * size, &input2[0], NULL, NULL);
		
		size_t szLocalWorkSize =  args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 5,
											(size_t)0, (void *)buf_input1,
											(size_t)0, (void *)buf_input2,
											(size_t)0, (void *)buf_output,
											sizeof(cl_int), (void *)&size,
											sizeof(cl_int), (void *)&width);
		
		icl_read_buffer(buf_output, CL_TRUE, sizeof(int) * size, &output[0], NULL, NULL);
		
		icl_release_buffers(3, buf_input1, buf_input2, buf_output);
		icl_release_kernel(kernel);
	}
	
        if (args->check_result) {
		printf("======================\n= Matrix Multiplication Done\n");
		unsigned int check = 1;
		for(unsigned int i = 0; i < size; ++i) {
			int sum = 0;
			int x = i % width;
			int y = i / width;
			for(unsigned int k = 0; k < width; ++k)
				sum += input1[y * width + k] * input2[k * width +  x];
		
			if(output[i] != sum) {
				check = 0;
				printf("= fail at %d, expected %d / actual %d\n", i, sum, output[i]);
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
