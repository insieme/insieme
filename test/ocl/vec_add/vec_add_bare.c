#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

size_t getGlobalWorkSize(int size, size_t local) {
	float multiplier=size/(float)local;
	if (multiplier>(int)multiplier) multiplier+=1;
	return (int)multiplier*local;
}

int main(int argc, const char *argv[]) {
	icl_args *args= icl_init_args();
	icl_parse_args(argc, argv, args);
	icl_print_args(args);

	int size=args->size;
	int vecsz=sizeof(int)*size;

	// initialize input
	int *input1=(int*)malloc(vecsz), *input2=(int*)malloc(vecsz), *output=(int*)malloc(vecsz);
	for (int i= 0; i < size; ++i) {
		input1[i]= i;
		input2[i]= i * 2;
	}

	icl_init_devices(ICL_ALL);
	if (icl_get_num_devices() != 0) {
		icl_device *dev=icl_get_device(0);
		icl_print_device_short_info(dev);

		icl_kernel *kernel=icl_create_kernel(dev, "vec_add.cl", "vec_add", "", ICL_SOURCE);

		size_t szLocalWorkSize =args->local_size,
			   szGlobalWorkSize=getGlobalWorkSize(size, szLocalWorkSize);

		for (int i= 0; i < args->loop_iteration; ++i) {
			// Cannot use variable vecsz here, as the OCL backend does not handle this construct properly.
			icl_buffer *buf_input1=icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(int)*size),
					   *buf_input2=icl_create_buffer(dev, CL_MEM_READ_ONLY,  sizeof(int)*size),
					   *buf_output=icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int)*size);

			icl_write_buffer(buf_input1, CL_TRUE, vecsz, &input1[0], NULL, NULL);
			icl_write_buffer(buf_input2, CL_TRUE, vecsz, &input2[0], NULL, NULL);

			icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 4,
						   (size_t)0, (void *)buf_input1, (size_t)0, (void *)buf_input2, (size_t)0, (void *)buf_output,
						   sizeof(cl_int), (void *)&size);

			icl_read_buffer(buf_output, CL_TRUE, vecsz, &output[0], NULL, NULL);
			icl_release_buffers(3, buf_input1, buf_input2, buf_output);
		}

		icl_release_kernel(kernel);
	}

	icl_release_args(args);
	icl_release_devices();
	free(input1);
	free(input2);
	free(output);
}
