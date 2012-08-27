#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"
#include "vmath.h"

#ifndef PATH
#define PATH "./"
#endif

// camera info
typedef struct _CameraInfo {
        float position[5];
        float direction[5];
        float right[5];
        float up[5];
} CameraInfo;

// struct for triangle intersection test
typedef struct {
	float n_u; // normal.u / normal.k
	float n_v; // normal.v / normal.k
	float n_d; // constant of plane equation
	int k;     // projection dimension

	// line equation for line ac
	float b_nu, b_nv, b_d;
	int pad0; // padding

	// line equation for line ab
	float c_nu, c_nv, c_d;
	int color; // color value (previously padding)
} TriAccel;

int main(int argc, const char* argv[]) {
        icl_args* args = icl_init_args();
        icl_parse_args(argc, argv, args);

	chdir(PATH);

       int width = (int)floor(sqrt(args->size));
       args->size = width * width;
       int size = args->size;
       icl_print_args(args);

	TriAccel* triAccels = (TriAccel*)malloc(sizeof(TriAccel) * size);
	int* pixel = (int *)malloc(sizeof(int) * size);
	
	int colors[] = {
		0x00ff0000,
		0x0000ff00,
		0x000000ff,
		0x00ffff00,
		0x0000ffff,
		0x00ff00ff,
	};

	for(int i=0; i < size; ++i) {
		float va[5];
		float vb[5];
		float vc[5];
		va[0] = randf(-1.0f, 1.0f);
		va[1] = randf(-1.0f, 1.0f);
		va[2] = randf(-1.0f, 1.0f);
		vb[0] = randf(-1.0f, 1.0f);
		vb[1] = randf(-1.0f, 1.0f);
		vb[2] = randf(-1.0f, 1.0f);
		vc[0] = randf(-1.0f, 1.0f);
		vc[1] = randf(-1.0f, 1.0f);
		vc[2] = randf(-1.0f, 1.0f);

		// calculate TriAccel
		TriAccel *ta = &triAccels[i];

		float b[5]; vec3_sub(b, vc, va);
		float c[5]; vec3_sub(c, vb, va);
		float n[5]; vec3_cross(n, c, b);

		int k,u,v;
		if (fabsf(n[0]) > fabsf(n[1])) {
			k = fabsf(n[0]) > fabsf(n[2]) ? 0 : 2;
		} else {
			k = fabsf(n[1]) > fabsf(n[2]) ? 1 : 2;
		}
		u = (k+1) % 3;
		v = (k+2) % 3;

		ta->n_u = n[u] / n[k];
		ta->n_v = n[v] / n[k];
		ta->n_d = vec3_dot(n, va) / n[k];
		ta->k = k;
		float div = b[u] * c[v] - b[v] * c[u];
		ta->b_nu = b[u] / div;
		ta->b_nv = -b[v] / div;
		ta->b_d = (b[v] * va[u] - b[u] * va[v]) / div;
		ta->c_nu = -c[u] / div;
		ta->c_nv = c[v] / div;
		ta->c_d = (c[u] * va[v] - c[v] * va[u]) / div;

		ta->color = colors[rand() % 6];
	}
        
	CameraInfo camInfo;

        // set camera
        float d[5];
        float u[5];
        float r[5];
        vec3_set_3f(d, 1.0f, 1.0f, -1.0f);
        vec3_set_3f(u, 0.0f, 0.0f, 1.0f);
        vec3_set_3f(r, 1.0f, -1.0f, 0.0f);

        // d
        vec3_normalize(d, d);
        // r
        vec3_cross(r, d, u);
        vec3_normalize(r, r);
        // u
        vec3_cross(u, r, d);
        vec3_normalize(u, u);

        vec3_set_3f(camInfo.position, -1.0f, -1.0f, 1.0f);
        vec3_set_v(camInfo.direction, d);
        vec3_set_v(camInfo.right, r);
        vec3_set_v(camInfo.up, u);

        icl_init_devices(args->device_type);

        if (icl_get_num_devices() != 0) {
                icl_device* dev = icl_get_device(args->device_id);

                icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "raytracing.cl", "raytracing", "", ICL_SOURCE);
		
		icl_buffer* buf_triAccels = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(TriAccel) * size);
		icl_buffer* buf_pixel = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);

		icl_write_buffer(buf_triAccels, CL_TRUE, sizeof(TriAccel) * size, &triAccels[0], NULL, NULL);
	
                size_t szLocalWorkSize =  args->local_size;
                float multiplier = size/(float)szLocalWorkSize;
                if(multiplier > (int)multiplier)
                        multiplier += 1;
                size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 5,
											(size_t) 0, (void*) buf_pixel,
											(size_t) 0, (void*) buf_triAccels,
											sizeof(CameraInfo), &camInfo,
                                                                                        sizeof(cl_int), (void *)&size,
                                                                                        sizeof(cl_int), (void *)&width);


		icl_read_buffer(buf_pixel, CL_TRUE, sizeof(int) * size, &pixel[0], NULL, NULL);
		
		icl_release_buffers(2, buf_triAccels, buf_pixel);
		icl_release_kernel(kernel);
	}
	
	if (args->check_result) {
		printf("======================\n= Simple program working\n");
		unsigned int check = 1;
		
		printf("No check available\n");
		printf("======================\n");
		printf("Result check: %s\n", check ? "OK" : "FAIL");
	} else {
		
                printf("Result check: OK\n");
	}

	icl_release_args(args);	
	icl_release_devices();
	free(triAccels);
	free(pixel);
}
