#ifndef VMATH_H
#define VMATH_H

#include <math.h>

typedef float vec3_t[3];

static inline float randf(float low, float up) {
	return fmod(rand(), (up - low)) + low;
}

static inline void vec3_set_f(vec3_t dst, const float f) {
	dst[0] = f;
	dst[1] = f;
	dst[2] = f;
}

static inline void vec3_set_3f(vec3_t dst, const float x, const float y, const float z) {
	dst[0] = x;
	dst[1] = y;
	dst[2] = z;
}

static inline void vec3_set_v(vec3_t dst, const vec3_t a) {
	dst[0] = a[0];
	dst[1] = a[1];
	dst[2] = a[2];
}

#define VEC3_OP(NAME,OP) \
static inline void vec3_ ## NAME (vec3_t dst, const vec3_t a, const vec3_t b) {\
	dst[0] = a[0] OP b[0];\
	dst[1] = a[1] OP b[1];\
	dst[2] = a[2] OP b[2];\
}\
static inline void vec3_ ## NAME ## _vf (vec3_t dst, const vec3_t a, const float b) {\
	dst[0] = a[0] OP b;\
	dst[1] = a[1] OP b;\
	dst[2] = a[2] OP b;\
}\
static inline void vec3_ ## NAME ## _fv (vec3_t dst, const float a, const vec3_t b) {\
	dst[0] = a OP b[0];\
	dst[1] = a OP b[1];\
	dst[2] = a OP b[2];\
}

VEC3_OP(add,+)
VEC3_OP(sub,-)
VEC3_OP(mul,*)
VEC3_OP(div,/)

#undef VEC3_OP

static inline float vec3_dot(const vec3_t a, const vec3_t b) {
	return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

static inline float vec3_length(const vec3_t a) {
	return sqrtf(a[0] * a[0] + a[1] * a[1] + a[2] * a[2]);
}

static inline void vec3_normalize(vec3_t dst, const vec3_t a) {
	vec3_div_vf(dst, a, vec3_length(a));
}

static inline void vec3_cross(vec3_t dst, const vec3_t a, const vec3_t b) {
	dst[0] = a[1] * b[2] - a[2] * b[1];
	dst[1] = a[2] * b[0] - a[0] * b[2];
	dst[2] = a[0] * b[1] - a[1] * b[0];
}

static inline void vec3_min(vec3_t dst, const vec3_t a, const vec3_t b) {
	dst[0] = a[0] < b[0] ? a[0] : b[0];
	dst[1] = a[1] < b[1] ? a[1] : b[1];
	dst[2] = a[2] < b[2] ? a[2] : b[2];
}

static inline void vec3_max(vec3_t dst, const vec3_t a, const vec3_t b) {
	dst[0] = a[0] > b[0] ? a[0] : b[0];
	dst[1] = a[1] > b[1] ? a[1] : b[1];
	dst[2] = a[2] > b[2] ? a[2] : b[2];
}

#endif
