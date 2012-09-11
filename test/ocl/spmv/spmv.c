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

        int size = args->size;
        icl_print_args(args);

	int* row_b = (int*)malloc(sizeof(int) * size); // size represents the number of rows in the matrix
	int* row_e = (int*)malloc(sizeof(int) * size);
	int* vec = (int*)malloc(sizeof(int) * size); // vector for the multiplication
	int* output = (int*)malloc(sizeof(int) * size); // output data
	
	chdir(PATH);

	srand(42);
	//int min = rsize* 1 / 100; // min number of value for row
	//int max = rsize* 3 / 100; // max..
	row_b[0] = 0;
	row_e[0] = 20;
	for(int i=1; i < size; ++i) {
		//int rnd_value = rand() % (max - min) + min;
		row_b[i] = row_b[i-1] + 20;
		row_e[i] = row_e[i-1] + 20;
		vec[i] = 1;
	}
	
	int len = row_e[size-1];
	int* val = (int*)malloc(sizeof(int) * len); // values of the matrix
	int* col = (int*)malloc(sizeof(int) * len); // in wich column is the corrisponding value

	for(int i=0; i < len; ++i){
		val[i] = 2;
		col[i] = rand() % (size-1);
	}
	
	icl_init_devices(args->device_type);
	
	icl_start_energy_measurement();

	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "spmv.cl", "spmv", "", ICL_SOURCE);
		
		icl_buffer* buf_row_b = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_row_e = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_vec = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_val = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * len);
		icl_buffer* buf_col = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * len);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);
		
		size_t szLocalWorkSize =  args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		for (int i = 0; i < args->loop_iteration; ++i) {
			icl_write_buffer(buf_row_b, CL_TRUE, sizeof(int) * size, &row_b[0], NULL, NULL);
			icl_write_buffer(buf_row_e, CL_TRUE, sizeof(int) * size, &row_e[0], NULL, NULL);
			icl_write_buffer(buf_vec, CL_TRUE, sizeof(int) * size, &vec[0], NULL, NULL);
			icl_write_buffer(buf_val, CL_TRUE, sizeof(int) * len, &val[0], NULL, NULL);
			icl_write_buffer(buf_col, CL_TRUE, sizeof(int) * len, &col[0], NULL, NULL);

			icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 7,
												(size_t)0, (void *)buf_row_b,
												(size_t)0, (void *)buf_row_e,
												(size_t)0, (void *)buf_vec,
												(size_t)0, (void *)buf_val,
												(size_t)0, (void *)buf_col,
												(size_t)0, (void *)buf_output,
												sizeof(cl_int), (void *)&size);
		
			icl_read_buffer(buf_output, CL_TRUE, sizeof(int) * size, &output[0], NULL, NULL);
		}
		
		icl_release_buffers(6, buf_row_b, buf_row_e, buf_vec, buf_val, buf_col, buf_output);
		icl_release_kernel(kernel);
	}
	
	icl_stop_energy_measurement();
	
    if (args->check_result) {
	printf("======================\n= Sparse Matrix-Vector Multiplication  Done\n");
	unsigned int check = 1;
	for(unsigned int i = 0; i < size; ++i) {
		int sum = 0;
		int start = row_b[i];
		int stop =  row_e[i];
		for (int j = start; j < stop; ++j){
			int c = col[j];
			sum += val[j] * vec[c];
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
    free(row_b);
	free(row_e);
    free(vec);
    free(val);
	free(col);
	free(output);
}
