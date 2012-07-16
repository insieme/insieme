#ifdef INSIEME
#include "ocl_device.h"
#endif
#define EPSILON_DET 0.0f
#define EPSILON_EDGE 0.00001f

////////////////////////////////////////////////////////////////////////////////
// camera information

typedef struct {
	float3 position;
	float3 direction;
	float3 right;
	float3 up;
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
// scene information

typedef struct {
	int numTris;
	__global TriAccel *triAccels;
} SceneInfo;


////////////////////////////////////////////////////////////////////////////////
// single ray and single ray hit
/*
typedef struct {
	union { float3 o; float ov[3]; }; // origin
	union { float3 d; float dv[3]; }; // direction
} Ray;
*/

typedef struct {
	float ov[3]; // origin
	float dv[3]; // direction
} Ray;

typedef struct {
	float t; // distance from ray origin
	float2 uv; // barycentric coordinates of hit
	int triId; // index of hit triangle
} RayHit;


////////////////////////////////////////////////////////////////////////////////
// accelerated single ray intersection test by Wald
// 'hit' is updated if an intersection with a smaller hit distance is found

void intersectTriAccel(__global TriAccel *acc, const int triId, const Ray *ray, RayHit *hit) {
	unsigned int modulo[5] = {0,1,2,0,1};
	const int k = acc->k;
	const int ku = modulo[k+1];
	const int kv = modulo[k+2];
	
	const float nd = 1.0f / (ray->dv[k] + acc->n_u * ray->dv[ku] + acc->n_v * ray->dv[kv]);
	const float f = (acc->n_d - ray->ov[k] - acc->n_u * ray->ov[ku] - acc->n_v * ray->ov[kv]) * nd;
	
	// check for valid distance
	if (!(hit->t > f && f > 0.00001f)) return;
	
	// compute hitpoint positions on uv plane
	const float hu = (ray->ov[ku] + f * ray->dv[ku]);
	const float hv = (ray->ov[kv] + f * ray->dv[kv]);
	
	// check first barycentric coordinate
	const float lambda = (hv * acc->b_nu + hu * acc->b_nv + acc->b_d);
	if (lambda < -EPSILON_EDGE) return;
	
	// check second barycentric coordinate
	const float mue = (hv * acc->c_nu + hu * acc->c_nv + acc->c_d);
	if (mue < -EPSILON_EDGE) return;
	
	// check third barycentric coordinate
	if (lambda + mue > 1.0f + EPSILON_EDGE) return;
	
	// check everything instead of trying to exit early.
	//if (!(hit->t > f && f > 0.00001f) || lambda < -EPSILON_EDGE || mue < EPSILON_EDGE || lambda+mue > 1.0f + EPSILON_EDGE) return;
	
	// have a valid hitpoint here. store it.
	hit->t = f;
	hit->triId = triId;
	hit->uv = (float2)(lambda, mue);
}


////////////////////////////////////////////////////////////////////////////////
// traces a single ray and returns the resulting color in 'color'

void traceRay(const Ray *ray, const SceneInfo *scene, __global int *pixel) {
	RayHit nearestHit;
	nearestHit.triId = -1;
	nearestHit.t = MAXFLOAT;

	for (int i = 0; i < scene->numTris; i++) {
		__global TriAccel *triaccel = &scene->triAccels[i];
		intersectTriAccel(triaccel, i, ray, &nearestHit);
	}
	
	*pixel = nearestHit.triId >= 0 ? scene->triAccels[nearestHit.triId].color : 0;
}

////////////////////////////////////////////////////////////////////////////////
//
// main rendering kernel
//
////////////////////////////////////////////////////////////////////////////////
#pragma insieme mark
__kernel void raytracing(
	// camera
	const CameraInfo camera,
	// image
	int imgWidth, int imgHeight,
	__global int *imgPixels,
	// scene
	int numTris,
	__global TriAccel *triAccels
)
{
	// calculate pixel position
	const int pixel = get_global_id(0);
	const int pixelX = pixel % imgWidth;
	const int pixelY = pixel / imgWidth;
	if (pixelY >= imgHeight) return; // nothing to do
	
	// generate primary ray
	Ray ray;
	{
		float x = (float) pixelX / (float) imgWidth - 0.5f;
		float y = (float) pixelY / (float) imgHeight - 0.5f;
 		//ray.o = camera.position;
		ray.ov[0] = camera.position.x;
		ray.ov[1] = camera.position.y;
		ray.ov[2] = camera.position.z;
				
		//ray.d = normalize(camera.direction + x * camera.right + y * camera.up);
		float3 dir = normalize(camera.direction + x * camera.right + y * camera.up);
		ray.ov[0] = dir.x;
		ray.ov[1] = dir.y;
		ray.ov[2] = dir.z;
	}
	
	// scene information
	SceneInfo scene;
	scene.numTris = numTris;
	scene.triAccels = triAccels;
	
	// trace ray and write color
	traceRay(&ray, &scene, &imgPixels[pixel]);
}