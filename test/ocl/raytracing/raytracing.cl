#ifdef INSIEME
#include "ocl_device.h"
#endif
#define EPSILON_DET 0.0f
#define EPSILON_EDGE 0.00001f

////////////////////////////////////////////////////////////////////////////////
// camera information

typedef struct {
	float position[5];
	float direction[5];
	float right[5];
	float up[5];
} CameraInfo;


////////////////////////////////////////////////////////////////////////////////
// TriAccel structure by Wald, with added color

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


////////////////////////////////////////////////////////////////////////////////
// single ray and single ray hit
typedef struct {
	float ov[5]; // origin
	float dv[5]; // direction
} Ray;

typedef struct {
	float t; // distance from ray origin
	float u; // barycentric coordinates of hit
	float v;
	int triId; // index of hit triangle
} RayHit;


////////////////////////////////////////////////////////////////////////////////
//
// main rendering kernel
//
////////////////////////////////////////////////////////////////////////////////

#pragma insieme mark
__kernel void raytracing( __global int *imgPixels, __global TriAccel *triAccels, const CameraInfo camera, int num_elements, int width) {
	// calculate pixel position
        int pixel = get_global_id(0);
        if (pixel >= num_elements) return;
        int pixelX = pixel % width;
        int pixelY = pixel / width;

	// generate primary ray
	
	float x = (float) pixelX / (float) width - 0.5f;
	float y = (float) pixelY / (float) width - 0.5f;
				
	Ray ray;
	ray.ov[0] = normalize(camera.direction[0] + x * camera.right[0] + y * camera.up[0]);
	ray.ov[1] = normalize(camera.direction[1] + x * camera.right[1] + y * camera.up[1]);
	ray.ov[2] = normalize(camera.direction[2] + x * camera.right[2] + y * camera.up[2]);
	
	// traces a single ray and returns the resulting color in 'color'
	RayHit nearestHit;
	nearestHit.triId = -1;
	nearestHit.t = MAXFLOAT;

	for (int i = 0; i < num_elements; i++) {
		__global TriAccel *triaccel = &triAccels[i];

		// accelerated single ray intersection test by Wald
		// 'hit' is updated if an intersection with a smaller hit distance is found
		unsigned int modulo[5] = {0,1,2,0,1};
		const int k = triaccel->k;
		const int ku = modulo[k+1];
		const int kv = modulo[k+2];
	
		const float nd = 1.0f / (ray.dv[k] + triaccel->n_u * ray.dv[ku] + triaccel->n_v * ray.dv[kv]);
		const float f = (triaccel->n_d - ray.ov[k] - triaccel->n_u * ray.ov[ku] - triaccel->n_v * ray.ov[kv]) * nd;
	
		// check for valid distance
		if (!(nearestHit.t > f && f > 0.00001f)) return;
	
		// compute hitpoint positions on uv plane
		const float hu = (ray.ov[ku] + f * ray.dv[ku]);
		const float hv = (ray.ov[kv] + f * ray.dv[kv]);
	
		// check first barycentric coordinate
		const float lambda = (hv * triaccel->b_nu + hu * triaccel->b_nv + triaccel->b_d);
		if (lambda < -EPSILON_EDGE) return;
	
		// check second barycentric coordinate
		const float mue = (hv * triaccel->c_nu + hu * triaccel->c_nv + triaccel->c_d);
		if (mue < -EPSILON_EDGE) return;
	
		// check third barycentric coordinate
		if (lambda + mue > 1.0f + EPSILON_EDGE) return;
	
		// check everything instead of trying to exit early.
		//if (!(nearestHit.t > f && f > 0.00001f) || lambda < -EPSILON_EDGE || mue < EPSILON_EDGE || lambda+mue > 1.0f + EPSILON_EDGE) return;
	
		// have a valid hitpoint here. store it.
		nearestHit.t = f;
		nearestHit.triId = i;
		nearestHit.u = lambda;
		nearestHit.v = mue;
	
	}
	
	int thisPixel = 0;
	if(nearestHit.triId >= 0)
		thisPixel = triAccels[nearestHit.triId].color;
	imgPixels[pixel] = thisPixel;
}
