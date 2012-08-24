#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"
#include <math.h>

#ifndef PATH
#define PATH "./"
#endif

void out_float_hbuffer(const float *hostbuf, size_t bsize){
	size_t i;
	for(i=0; i< bsize; i++){
		printf("%f ", hostbuf[i]);		
	}
	printf("\n");
}

void fillrandom_float(float* arrayPtr, int width, int height, float rangeMin, float rangeMax){
    if(!arrayPtr) {
        fprintf(stderr, "Cannot fill array: NULL pointer.\n");
        return;
    }
	srand(7);
    double range = (double)(rangeMax - rangeMin);     
    for(int i = 0; i < height; i++)
        for(int j = 0; j < width; j++) {
            int index = i*width + j;
            arrayPtr[index] = rangeMin + (float)(range*((float)rand() / RAND_MAX)); 	
        }    
}	

int main(int argc, const char* argv[]) 
{
	chdir(PATH);
	icl_args* args = icl_init_args();
	icl_parse_args(argc, argv, args);
        icl_print_args(args);

	int size = args->size;

	// this is the size of chunking - so far as big as the local size	
	int chunkSize = 16;

	cl_float16* input = (cl_float16*)malloc(sizeof(cl_float16) * size);
	float* output = (float*)malloc(sizeof(float) * size);
	
	fillrandom_float((float*)input,size, chunkSize, 0.001f ,100000.f);

        icl_init_devices(args->device_type);

        if (icl_get_num_devices() != 0) {
                icl_device* dev = icl_get_device(args->device_id);

                icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "geometric_mean.cl", "geo_mean", "", ICL_SOURCE);
		
		icl_buffer* buf_input = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(cl_float16) * size);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(float) * size);

		icl_write_buffer(buf_input, CL_FALSE, sizeof(cl_float16) * size, &input[0], NULL, NULL);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;
		

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 4,
			(size_t)0, (void *)buf_input,
			sizeof(cl_int), (void *)&chunkSize,
			sizeof(cl_int), (void *)&size,
			(size_t)0, (void *)buf_output
		);
		
		icl_read_buffer(buf_output, CL_TRUE, sizeof(float) * size, &output[0], NULL, NULL);
		
		icl_release_buffers(2, buf_input, buf_output);
		icl_release_kernel(kernel);
	}
	
//	printf("Chunks' minimum \n");
//	out_float_hbuffer(output, size);

	if (args->check_result) {
		printf("======================\n= Geometric mean test\n");
		unsigned int check = 1;
		float host_mean = 0.0f;
		float* testInput = (float*)input;
		for(unsigned int i = 0; i < size*chunkSize; ++i) 
			host_mean = host_mean + log(testInput[i]);
		host_mean /= size*chunkSize;
		host_mean = pow(2.718281828459045235f, host_mean);	
		
		printf("Host mean is %f\n", host_mean);

		float device_mean = 0.0f;
		for(unsigned int i = 0; i < size; ++i) 
			device_mean = device_mean + log(output[i]);

		device_mean /= size;
		device_mean = pow(2.718281828459045235f, device_mean);			
		printf("Device mean is %f\n", device_mean);		

		printf("Result check: %s\n", fabs(device_mean - host_mean) < 1.0f ? "OK" : "FAIL");
	} else {
		printf("Result check: OK\n");
	}
	
	icl_release_devices();
	free(input);
	free(output);

	#ifdef _MSC_VER
	icl_prompt();
	#endif

	return 0;
}
