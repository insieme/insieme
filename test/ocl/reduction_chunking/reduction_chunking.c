#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

int main(int argc, char* argv[]) 
{
	icl_arguments args;
	args = default_arguments;
	args.checkResults = true;
	args.size = 1024;
	icl_parse_argument(argc, argv, &args);	
	icl_print_arguments(&args);	

	int size = args.size;

	// this is the size of chunking - so far as big as the local size	
	int chunkSize = args.groupSize;
	int chunkNum = args.size / chunkSize;
	printf("Reduction with chunking - %d chunks of %d elements\n", chunkNum, chunkSize);

	float* input = (float*)malloc(sizeof(float) * size);
	float* output = (float*)malloc(sizeof(float) * chunkNum);
	
	icl_fillrandom_float(input,size,1,0.f,100000.f);
	icl_fill_float(output,chunkNum,1,0.f);

	icl_init_devices(ICL_ALL);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(0);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "reduction_chunking.cl", "reduce", "", ICL_SOURCE);
		
		icl_buffer* buf_input = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(float) * size);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(float) * chunkNum);

		icl_write_buffer(buf_input, CL_FALSE, sizeof(float) * size, &input[0], NULL, NULL);
		
		size_t szLocalWorkSize = args.groupSize;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;
		

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 4,
			(size_t)0, (void *)buf_input,
			sizeof(cl_int), (void *)&chunkSize,
			sizeof(cl_int), (void *)&args.size,
			(size_t)0, (void *)buf_output
		);
		
		icl_read_buffer(buf_output, CL_TRUE, sizeof(float) * chunkNum, &output[0], NULL, NULL);
		
		icl_release_buffers(2, buf_input, buf_output);
		icl_release_kernel(kernel);
	}
	
	printf("Chunks' minimum \n");
	icl_out_float_hbuffer(output, chunkNum);

	if (args.checkResults) {
		printf("======================\n= Reduction test\n");
		unsigned int check = 1;
		float host_min = 100000.f;
		for(unsigned int i = 0; i < size; ++i) 
			if(input[i] < host_min)	host_min = input[i];
		printf("Host minimum is %f\n", host_min);

		float device_min = 100000.f;
		for(unsigned int i = 0; i < chunkNum; ++i) 
			if(output[i] < device_min)	device_min = output[i];			
		printf("Device minimum is %f\n", device_min);		

		printf("Result check: %s\n", (device_min == host_min) ? "OK" : "FAIL");
	} 
	
	icl_release_devices();
	free(input);
	free(output);

	#ifdef _MSC_VER
	icl_prompt();
	#endif

	return 0;
}
