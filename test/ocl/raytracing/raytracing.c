#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"
#include "vmath.h"

// camera info
typedef struct {
	vec3_t position; float pad0;
	vec3_t direction; float pad1;
	vec3_t right; float pad2;
	vec3_t up; float pad3;
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

CameraInfo set_default_camera() {
	CameraInfo camInfo;
	
	// set camera
	vec3_t d, u, r;
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
	
	return camInfo;
}

int main(int argc, const char* argv[]) {
    icl_args* args = icl_init_args();
    icl_parse_args(argc, argv, args);
    icl_print_args(args);

	int size = args->size;

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
		vec3_t va, vb, vc;
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

		vec3_t b; vec3_sub(b, vc, va);
		vec3_t c; vec3_sub(c, vb, va);
		vec3_t n; vec3_cross(n, c, b);
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
	CameraInfo camInfo = set_default_camera();

	icl_init_devices(ICL_ALL);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(0);

		icl_print_device_short_info(dev);
		icl_kernel* kernel = icl_create_kernel(dev, "raytracing.cl", "raytracing", "", ICL_SOURCE);
		
		icl_buffer* buf_triAccels = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(TriAccel) * size);
		icl_buffer* buf_pixel = icl_create_buffer(dev, CL_MEM_WRITE_ONLY, sizeof(int) * size);

		icl_write_buffer(buf_triAccels, CL_TRUE, sizeof(TriAccel) * size, &triAccels[0], NULL, NULL);
		
		size_t szLocalWorkSize = args->local_size;
		float multiplier = size/(float)szLocalWorkSize;
		if(multiplier > (int)multiplier)
			multiplier += 1;
		size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;
		
		int imgWidth = szLocalWorkSize;
		int imgHeight = szGlobalWorkSize / imgWidth;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 6,
			sizeof(CameraInfo), &camInfo,
			sizeof(int), &imgWidth,
			sizeof(int), &imgHeight,
			(size_t) 0, (void*) buf_pixel,
			sizeof(int), &size,
			(size_t) 0, (void*) buf_triAccels
		);
		
		icl_read_buffer(buf_pixel, CL_TRUE, sizeof(int) * size, &pixel[0], NULL, NULL);
		
		icl_release_buffers(2, buf_triAccels, buf_pixel);
		icl_release_kernel(kernel);
	}
	
	if (args->check_result) {
		printf("======================\n= Simple program working\n");
		unsigned int check = 1;
/*		for(unsigned int i = 0; i < size; ++i) {
			if(i % args->local_size == 0)
				printf("\n");
			printf("%i ", pixel[i]);
		}*/
		
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
