#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);

        int size = args->size;
        icl_print_args(args);

	int* rowdel = (int*)malloc(sizeof(int) * size); // size represents the number of rows in the matrix
	int* vec = (int*)malloc(sizeof(int) * size); // vector for the multiplication
	int* output = (int*)malloc(sizeof(int) * size); // output data
	
	int rsize = size-2;
	
	srand(42);
	//int min = rsize* 1 / 100; // min number of value for row
	//int max = rsize* 3 / 100; // max..
	rowdel[0] = 0;
	rowdel[size-1] = 0;
	rowdel[size] = 0;

	for(int i=1; i < rsize; ++i) {
		//int rnd_value = rand() % (max - min) + min;
		rowdel[i] = rowdel[i-1] + 20;
		vec[i] = 1;
	}
	
	int len = rowdel[rsize-1];
	int* val = (int*)malloc(sizeof(int) * len); // values of the matrix
	int* col = (int*)malloc(sizeof(int) * len); // in wich column is the corrisponding value

	for(int i=0; i < len; ++i){
		val[i] = 2;
		col[i] = rand() % (rsize-1);
	}
	
	icl_init_devices(args->device_type);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "spmv.cl", "spmv", "", ICL_SOURCE);
		
		icl_buffer* buf_rowdel = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_vec = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * size);
		icl_buffer* buf_val = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * len);
		icl_buffer* buf_col = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(int) * len);
		icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);

		icl_write_buffer(buf_rowdel, CL_TRUE, sizeof(int) * size, &rowdel[0], NULL, NULL);
		icl_write_buffer(buf_vec, CL_TRUE, sizeof(int) * size, &vec[0], NULL, NULL);
		icl_write_buffer(buf_val, CL_TRUE, sizeof(int) * len, &val[0], NULL, NULL);
		icl_write_buffer(buf_col, CL_TRUE, sizeof(int) * len, &col[0], NULL, NULL);
		
		size_t szLocalWorkSize =  args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 6,
											(size_t)0, (void *)buf_rowdel,
											(size_t)0, (void *)buf_vec,
											(size_t)0, (void *)buf_val,
											(size_t)0, (void *)buf_col,
											(size_t)0, (void *)buf_output,
											sizeof(cl_int), (void *)&rsize);
		
		icl_read_buffer(buf_output, CL_TRUE, sizeof(int) * size, &output[0], NULL, NULL);
		
		icl_release_buffers(5, buf_rowdel, buf_vec, buf_val, buf_col, buf_output);
		icl_release_kernel(kernel);
	}
	
        if (args->check_result) {
		printf("======================\n= Sparse Matrix-Vector Multiplication  Done\n");
		unsigned int check = 1;
		for(unsigned int i = 0; i < rsize; ++i) {
			int sum = 0;
			int start = rowdel[i];
			int stop =  rowdel[i+1];
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
        free(rowdel);
        free(vec);
        free(val);
	free(col);
	free(output);
}
