#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
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

	int mask_width = 22;
	int mask_size = mask_width * mask_width;
	
	int* input  = (int*)malloc(sizeof(int) * size);
	int* mask   = (int*) malloc(sizeof(int) * mask_size);
	int* output = (int*)malloc(sizeof(int) * size);

	for(int i=0; i < mask_size; ++i) mask[i] = 1;
	mask[mask_size/2] = 0;
	
	for(int i=0; i < size; ++i) {
		input[i] = 1;//rand() % 10;
	}
	
	icl_init_devices(args->device_type);
	
	icl_start_energy_measurement();

	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "convolution.cl", "convolution", "", ICL_SOURCE);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		for (int i = 0; i < args->loop_iteration; ++i) {
			icl_buffer* buf_input  = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
			icl_buffer* buf_mask   = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * mask_size);
			icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);
		
			icl_write_buffer(buf_input, CL_TRUE, sizeof(int) * size, &input[0], NULL, NULL);
			icl_write_buffer(buf_mask, CL_TRUE, sizeof(int) * mask_size, &mask[0], NULL, NULL);

			icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 6,
												(size_t)0, (void *)buf_input,
												(size_t)0, (void *)buf_mask,
												(size_t)0, (void *)buf_output,
												sizeof(cl_int), (void *)&size,
												sizeof(cl_int), (void *)&width,
												sizeof(cl_int), (void *)&mask_width);
		
			icl_read_buffer(buf_output, CL_TRUE, sizeof(int) * size, &output[0], NULL, NULL);
			icl_release_buffers(3, buf_input, buf_mask, buf_output);
		}
		
		icl_release_kernel(kernel);
	}
	
	icl_stop_energy_measurement();
	
    if (args->check_result) {
            printf("======================\n= Convolution Done\n");
            unsigned int check = 1;
            for(unsigned int i = 0; i < size; ++i) {
                    int x = i % width;
                    int y = i / width;
		int sum = 0;
		int offset = mask_width/2;
		if (!(x < offset || y < offset || x >= width-offset || y >= width-offset)) {
    			int tmpx = x - offset;
    			int tmpy = y - offset;
    			for (int r = 0; r < mask_width; ++r) {
            			for (int c = 0; c < mask_width; ++c) {
                    			sum += mask[r * mask_width + c] * input[(tmpy + r ) * width + tmpx + c];
            			}
    			}
		}

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
	free(input);
	free(mask);
	free(output);
}
