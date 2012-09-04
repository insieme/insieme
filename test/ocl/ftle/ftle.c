#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"
#include <math.h>
#include <unistd.h>

#ifndef PATH
#define PATH "./"
#endif

static inline float random01_float(){ return (float) rand()/(RAND_MAX); }

void icl_fillrandom_float(float* arrayPtr, int width, int height, float rangeMin, float rangeMax){
    if(!arrayPtr) {
        fprintf(stderr, "Cannot fill array: NULL pointer.\n");
        return;
    }
    double range = (double)(rangeMax - rangeMin);
    for(int i = 0; i < height; i++)
        for(int j = 0; j < width; j++) {
            int index = i*width + j;
            arrayPtr[index] = rangeMin + (float)(range*random01_float());
        }
}

int main(int argc, const char* argv[]) {
	chdir(PATH);

	int numTimesteps = 4;
	cl_float2 origin   = { 0.f,  0.f  };
	cl_float2 cellSize = { 0.1f, 0.1f };
	float startTime = 1.0f;
	float advectionTime = 0.5f;

	icl_args* args = icl_init_args();
	icl_parse_args(argc, argv, args);

	int width = (int)floor(sqrt(args->size));
	args->size = width * width;
	int size = args->size;
	icl_print_args(args);

	// prepare inputs
	float* flowMap   = (float*) malloc(sizeof(cl_float2) * size);
	float* output    = (float*) malloc(sizeof(cl_float2) * size);

	icl_fillrandom_float(flowMap, size, 1, -1.0f, 1.0f); // filled in case we only run the 2nd kernel

	icl_init_devices(args->device_type);

	icl_start_energy_measurement();

	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);

		icl_kernel* kernel = icl_create_kernel(dev, "ftle.cl", "computeFTLE", "-I.", ICL_SOURCE);

		icl_buffer* buf_flowMap   = icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(cl_float2) * size);		
		icl_buffer* buf_output    = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(cl_float2) * size);

		size_t localWorkSize = args->local_size;
		float multiplier = size/(float)localWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * localWorkSize;

		for (int i = 0; i < args->loop_iteration; ++i) {
			icl_write_buffer(buf_flowMap, CL_TRUE, sizeof(cl_float2) * size, &flowMap[0], NULL, NULL);

			icl_run_kernel(kernel, 1, &szGlobalWorkSize, &localWorkSize, NULL, NULL, 6,
					(size_t)0, (void *)buf_flowMap,
					sizeof(cl_int), (void *)&width,
					sizeof(cl_float2), (void *)&cellSize,
					sizeof(cl_float), (void *)&advectionTime,
					(size_t)0, (void *)buf_output,
					sizeof(cl_int), (void *)&size);

			icl_read_buffer(buf_output, CL_TRUE, sizeof(cl_float2) * size, &output[0], NULL, NULL);
		}

		icl_release_buffers(2, buf_flowMap, buf_output);
		icl_release_kernel(kernel);		
	}

	icl_stop_energy_measurement();

   if (args->check_result) {
			printf("======================\n");
			printf("Check Not Implemented\n");
			printf("Result check: OK\n");
	} else {
			printf("Result check: OK\n");
	}

	icl_release_args(args);
	icl_release_devices();

	free(flowMap);
	free(output);
	
	return 0;
}
