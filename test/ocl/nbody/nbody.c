#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

#define ADD(T1,T2) 		(cl_float4) {{ (T1).s[0] + (T2).s[0], (T1).s[1] + (T2).s[1], (T1).s[2] + (T2).s[2], 0.0f }}
#define EQ(T1,T2) 		(fabs((T1).s[0]-(T2).s[0]) < eps && fabs((T1).s[1]-(T2).s[1]) < eps && fabs((T1).s[2]-(T2).s[2]) < eps)

#define eps 1
#define M 1
#define SPACE_SIZE 100

float rand_val(float min, float max) {
	return (rand() / (float) RAND_MAX) * (max - min) + min;
}

cl_float4 triple_rand() {
	return (cl_float4) {{
		rand_val(-SPACE_SIZE,SPACE_SIZE),
		rand_val(-SPACE_SIZE,SPACE_SIZE),
		rand_val(-SPACE_SIZE,SPACE_SIZE),
		0.0f
	}};
}

void triple_print(cl_float4 t) {
	printf("(%f,%f,%f)", t.s[0], t.s[1], t.s[2]);
}

cl_float4 triple_zero() {
	return (cl_float4) {{0.0f, 0.0f, 0.0f, 0.0f}};
}

int main(int argc, const char* argv[]) {
    icl_args* args = icl_init_args();
    icl_parse_args(argc, argv, args);
    icl_print_args(args);

	int size = args->size;
	float deltaTime = 1;
	float epsSqr = 0.00001;

	cl_float4* pos = (cl_float4*)malloc(sizeof(cl_float4) * size); // the list of positions
	cl_float4* vel = (cl_float4*)malloc(sizeof(cl_float4) * size); // the list of velocities

	// distribute bodies in space (randomly)
	for(int i=0; i<size; i++) {
		pos[i] = triple_rand();
		vel[i] = triple_zero();
	}

	icl_init_devices(args->device_type);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(args->device_id);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "nbody.cl", "nbody", "", ICL_SOURCE);
		
		icl_buffer* buf_pos = icl_create_buffer(dev, CL_MEM_READ_WRITE, sizeof(cl_float4) * size);
		icl_buffer* buf_vel = icl_create_buffer(dev, CL_MEM_READ_WRITE, sizeof(cl_float4) * size);
		icl_buffer* buf_newPos = icl_create_buffer(dev, CL_MEM_READ_WRITE, sizeof(cl_float4) * size);
		icl_buffer* buf_newVel = icl_create_buffer(dev, CL_MEM_READ_WRITE, sizeof(cl_float4) * size);

		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		for(int i=0; i<M; i++) {
			icl_write_buffer(buf_pos, CL_TRUE, sizeof(cl_float4) * size, &pos[0], NULL, NULL);
			icl_write_buffer(buf_vel, CL_TRUE, sizeof(cl_float4) * size, &vel[0], NULL, NULL);

			icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 7,
											(size_t)0, (void *)buf_pos,
											(size_t)0, (void *)buf_vel,
											sizeof(cl_int), (void *)&size,
											sizeof(cl_float), (void *)&deltaTime,
											sizeof(cl_float), (void *)&epsSqr,
											(size_t)0, (void *)buf_newPos,
											(size_t)0, (void *)buf_newVel);
											
			icl_read_buffer(buf_newPos, CL_TRUE, sizeof(cl_float4) * size, &pos[0], NULL, NULL);
			icl_read_buffer(buf_newVel, CL_TRUE, sizeof(cl_float4) * size, &vel[0], NULL, NULL);
		}
		
		
		icl_release_buffers(4, buf_vel, buf_pos, buf_newVel, buf_newPos);
		icl_release_kernel(kernel);
	}
	
	if (args->check_result) {
		printf("======================\n= N Body Simulation working\n");
		cl_float4 sum = triple_zero();
		for(int i=0; i<size; i++) {
			sum = ADD(sum, vel[i]);
		}

		unsigned int check = EQ(sum, triple_zero());

		printf("======================\n");
		printf("Result check: %s\n", check ? "OK" : "FAIL");
	} else {
		printf("Checking disabled!\n");
        	printf("Result check: OK\n");
	}

	icl_release_args(args);	
	icl_release_devices();
	free(pos);
	free(vel);
	return 0;
}
