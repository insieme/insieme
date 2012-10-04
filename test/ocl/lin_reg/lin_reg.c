#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

#ifndef PATH
#define PATH "./"
#endif 

int float_compare(const void* elem1, const void* elem2) {
        if(*(const float*)elem1 < *(const float*)elem2) return -1;
        return *(const float*)elem1 > *(const float*)elem2;
}

inline float random01_float() { 
	return (float) rand()/(RAND_MAX);
}

void fill_random_float(float* arrayPtr, int width, int height, float rangeMin, float rangeMax){
    double range = (double)(rangeMax - rangeMin);
    for(int i = 0; i < height; i++)
        for(int j = 0; j < width; j++) {
            int index = i*width + j;
            arrayPtr[index] = rangeMin + (float)(range * random01_float());
        }
}

bool compare_float(const float *refData, const float *data, const int length, const float epsilon) {
    float error = 0.0f;
    float ref = 0.0f;

    for(int i = 1; i < length; ++i) {
        float diff = refData[i] - data[i];
        error += diff * diff;
        ref += refData[i] * refData[i];
    }

    float normRef = sqrtf((float) ref);
    if (fabs(ref) < 1e-7f) {
        return false;
    }
    float normError = sqrtf((float) error);
    error = normError / normRef;

    return error < epsilon;
}

int main(int argc, const char* argv[]) {
    icl_args* args = icl_init_args();
    icl_parse_args(argc, argv, args);
    icl_print_args(args);

	chdir(PATH);

    int size = args->size;

    float* input1 = (float*) malloc(sizeof(float) * size);
    float* input2 = (float*) malloc(sizeof(float) * size);
    float* alpha  = (float*) malloc(sizeof(float) * size);
    float* beta   = (float*) malloc(sizeof(float) * size);
    float* output = (float*) malloc(sizeof(float) * size);

    fill_random_float(input2, size, 1, -1.0f, 1.0f);
    qsort(input2, size, sizeof(float), float_compare);
    float step = 2.0f / size;
    for(int i=0; i < size; i++) 
	input1[i] = -1.0f + i * step;

    fill_random_float(alpha, size, 1, -1.0f, 1.0f);
    fill_random_float(beta, size, 1, -1.0f, 1.0f);

    icl_init_devices(args->device_type);

	icl_start_energy_measurement();

    if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "lin_reg.cl", "lin_reg", "", ICL_SOURCE);

		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		for (int i = 0; i < args->loop_iteration; ++i) {
			icl_buffer* buf_input1 = icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(float) * size);
			icl_buffer* buf_input2 = icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(float) * size);
			icl_buffer* buf_alpha  = icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(float) * size);
			icl_buffer* buf_beta   = icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(float) * size);
			icl_buffer* buf_output = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(float) * size);

			icl_write_buffer(buf_input1, CL_TRUE, sizeof(float) * size, &input1[0], NULL, NULL);
			icl_write_buffer(buf_input2, CL_TRUE, sizeof(float) * size, &input2[0], NULL, NULL);
			icl_write_buffer(buf_alpha, CL_TRUE, sizeof(float) * size, &alpha[0], NULL, NULL);
			icl_write_buffer(buf_beta, CL_TRUE, sizeof(float) * size, &beta[0], NULL, NULL);

			icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 6,
												(size_t)0, (void *)buf_input1,
												(size_t)0, (void *)buf_input2,
			                								(size_t)0, (void *)buf_alpha,
											                (size_t)0, (void *)buf_beta,
												(size_t)0, (void *)buf_output,
												sizeof(cl_int), (void *)&size);
			icl_read_buffer(buf_output, CL_TRUE, sizeof(float) * size, &output[0], NULL, NULL);
			icl_release_buffers(5, buf_input1, buf_input2, buf_alpha, buf_beta, buf_output);
		}

		icl_release_kernel(kernel);
	}
	
	icl_stop_energy_measurement();

    if (args->check_result) {
        printf("======================\n= Linear Regression Done\n");
		float* output2 = (float *)malloc(sizeof(float) * size);
		for(unsigned int j = 0; j < size; ++j) {
			const int gid = j;
			float a = alpha[gid];
			float b = beta[gid];
			float error = 0;
			for(int i=0; i<size; i++) {
				float e = (a * input1[i] + b) - input2[i];
				error += e * e;
			}
			output2[gid] = error;
		}
                
	    bool check = compare_float(output, output2, size, 0.000001);
		printf("======================\n");
		printf("Result check: %s\n", check ? "OK" : "FAIL");
		free(output2);
    } else {
		printf("Result check: OK\n");
    }

	icl_release_args(args);
	icl_release_devices();
	free(input1);
	free(input2);
	free(alpha);
	free(beta);
	free(output);
}
