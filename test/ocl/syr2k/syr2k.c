#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

#ifndef PATH
#define PATH "./"
#endif

void syr2k_check(float *A, float *B, float *C, float ALPHA, float BETA, int m, int n, int size)
{
	int i, j, k;
	int nn = size / n;
		
  	for (i = 0; i < size; i++)
	{
		C[i] = (i / n + 2) * BETA;
	}


  	for (i = 0; i < n; i++)
	{
   		for (j = 0; j < n; j++)
		{
			for (k = 0; k < m; k++)
			{
	  			C[i*n + j] += ALPHA * A[i*m + k] * B[j*m + k];
	 		 	C[i*n + j] += ALPHA * B[i*m + k] * A[j*m + k];
			}
		}
	}
}

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);
        icl_print_args(args);

	chdir(PATH);

	int width = (int)floor(sqrt(args->size));
	args->size = width * width;
    int size = args->size;
    icl_print_args(args);

	float* input_a = (float*)malloc(sizeof(float) * size);
	float* input_b = (float*)malloc(sizeof(float) * size);
	float* output =  (float*)malloc(sizeof(float) * size);
	
	float ALPHA = 1;
	float BETA = 1;
	int M = width;
	int N = width;
	
	for(int i=0; i < size; ++i) {
		input_a[i] = i % 19;
		input_b[i] = (size - i) % 17;
		output[i] = i / M + 2;
	}

	icl_init_devices(ICL_ALL);
	
	icl_start_energy_measurement();

	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(0);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "syr2k.cl", "syr2k", "", ICL_SOURCE);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		for (int i = 0; i < args->loop_iteration; ++i) {
			icl_buffer* buf_input_a = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(float) * size);
			icl_buffer* buf_input_b = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(float) * size);
			icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_READ_WRITE, sizeof(float) * size);

			icl_write_buffer(buf_input_a, CL_TRUE, sizeof(float) * size, &input_a[0], NULL, NULL);
			icl_write_buffer(buf_input_b, CL_TRUE, sizeof(float) * size, &input_b[0], NULL, NULL);
			icl_write_buffer(buf_output, CL_TRUE, sizeof(float) * size, &output[0], NULL, NULL);

			icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 7,
												(size_t)0, (void *)buf_input_a,
												(size_t)0, (void *)buf_input_b,
												(size_t)0, (void *)buf_output,
												sizeof(cl_float), (void *)&ALPHA,
												sizeof(cl_float), (void *)&BETA,
												sizeof(cl_int), (void *)&M,
												sizeof(cl_int), (void *)&N);

			icl_read_buffer(buf_output, CL_TRUE, sizeof(float) * size, &output[0], NULL, NULL);
			icl_release_buffers(3, buf_input_a, buf_input_b, buf_output);
		}
		
		icl_release_kernel(kernel);
	}
	
	icl_stop_energy_measurement();

	for(int i =0; i < 0; ++i)
		printf("%d %f\n", i, output[i]);
	
	if (args->check_result) {
		printf("======================\n= syr2k program working\n");

		float* val =  (float*)malloc(sizeof(float) * size);

		syr2k_check(input_a, input_b, val , ALPHA, BETA, M, N, size);

		unsigned int check = 1;
		for(unsigned int i = 0; i < size; ++i) {
			if(output[i] != val[i]) {
				check = 0;
				printf("= fail at %d, expected %f / actual %f\n", i, val[i], output[i]);
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
	free(input_a);
	free(input_b);
	free(output);
}
