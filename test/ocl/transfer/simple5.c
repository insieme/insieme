#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);
        icl_print_args(args);

	int size = args->size;

        icl_init_devices(args->device_type);

        if (icl_get_num_devices() != 0) {
                icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "simple.cl", "simple", "", ICL_SOURCE);
	
		icl_timer* time = icl_init_timer(ICL_MILLI); // INIT TIMER
		icl_start_timer(time); // START TIMER
		int* input = (int*)malloc(sizeof(int) * size);
        	int* output = (int *)malloc(sizeof(int) * size);

		icl_buffer* pinned_buf_input =  icl_create_buffer(dev,  CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* pinned_buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);
		
		int* pinned_mem_input = icl_map_buffer(pinned_buf_input, CL_TRUE, CL_MAP_WRITE, sizeof(int) * size, NULL, NULL);
		icl_stop_timer(time); // STOP TIMER
                for(int i=0; i < size; ++i) {
                        input[i] = i;
                }

		icl_start_timer(time); // START TIMER
		memcpy(pinned_mem_input, input, sizeof(int) * size);	
		icl_unmap_buffer(pinned_buf_input, pinned_mem_input, NULL, NULL);

		icl_stop_timer(time); // STOP TIMER
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 3,
											(size_t)0, (void *)pinned_buf_input,
											(size_t)0, (void *)pinned_buf_output,
											sizeof(cl_int), (void *)&size);
		icl_start_timer(time); // START TIMER
		
		int* pinned_mem_output = icl_map_buffer(pinned_buf_output, CL_TRUE, CL_MAP_READ, sizeof(int) * size, NULL, NULL);
		memcpy(output, pinned_mem_output, sizeof(int) * size);

		icl_unmap_buffer(pinned_buf_output, pinned_mem_output, NULL, NULL);

		printf("TIME: %f\n", icl_stop_timer(time)); // STOP TIMER
		icl_release_timer(time); // RELEASE TIMER

		
		icl_stop_timer(time); // STOP TIMER
	
		if (args->check_result) {
			printf("======================\n= Simple program working\n");
			unsigned int check = 1;
			for(unsigned int i = 0; i < size; ++i) {
				if(input[i] != output[i]) {
					check = 0;
					printf("= fail at %d, expected %d / actual %d", i, i, output[i]);
					break;
				}
			}
			printf("======================\n");
			printf("Result check: %s\n", check ? "OK" : "FAIL");
		} else {
			printf("Result check: OK\n");
		}

		icl_release_buffers(2, pinned_buf_input, pinned_buf_output);
		icl_release_kernel(kernel);
	}
	
	

	icl_release_args(args);	
	icl_release_devices();
}
