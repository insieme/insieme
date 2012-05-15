#ifdef INSIEME
#include "ocl_device.h"
#endif

#define _SINGLE_
#define SIZE 1


// The kernel to be selected is a function of the defines specified when
// the program is built (see clBuildProgram in the bsop.c code for more info).

#ifdef _SINGLE_

// various constants used in the core BlackScholes computations

#define ZERO		0.0f
#define ONE		1.0f
#define HALF		0.5f
#define A1		0.319381530f
#define A2		-0.356563782f
#define A3		1.781477937f
#define A4		-1.821255978f
#define A5		1.330274429f
#define INV_ROOT2PI	0.39894228f
#define NCDF		0.2316419f

// You can specify a vector width of 1 (scalar), 2, 4, 8, or 16.
// Character strings appropriate to each width are defined here.

#if SIZE==1
#define FLOAT float
#define FIXED uint
#define SFIXED int
#define STRIDESHIFT 2
#define SELECT(_a, _b, _c) (_c ? _b : _a)
#endif

#if SIZE==2
#define FLOAT float2
#define FIXED uint2
#define SFIXED int2
#define STRIDESHIFT 3
#define SELECT(_a, _b, _c) bitselect(_a, _b, as_float2(_c))
#endif

#if SIZE==4
#define FLOAT float4
#define FIXED uint4
#define SFIXED int4
#define STRIDESHIFT 4
#define SELECT(_a, _b, _c) bitselect(_a, _b, as_float4(_c))
#endif

#if SIZE==8
#define FLOAT float8
#define FIXED uint8
#define SFIXED int8
#define STRIDESHIFT 5
#define SELECT(_a, _b, _c) bitselect(_a, _b, as_float8(_c))
#endif

#if SIZE==16
#define FLOAT float16
#define FIXED uint16
#define SFIXED int16
#define STRIDESHIFT 6
#define SELECT(_a, _b, _c) bitselect(_a, _b, as_float16(_c))
#endif

#endif // SINGLE

#ifdef _DOUBLE_
#pragma OPENCL EXTENSION cl_khr_fp64: enable
//#pragma OPENCL EXTENSION cl_amd_fp64: enable

// various constants used in the core BlackScholes computations

#define ZERO		0.0
#define ONE		1.0
#define HALF		0.5
#define A1		0.319381530
#define A2		-0.356563782
#define A3		1.781477937
#define A4		-1.821255978
#define A5		1.330274429
#define INV_ROOT2PI 	0.39894228
#define NCDF		0.2316419

// You can specify a vector width of 1 (scalar), 2, 4, 8, or 16.
// Character strings appropriate to each width are defined here.

#if SIZE==1
#define FLOAT double
#define FIXED ulong
#define SFIXED long
#define STRIDESHIFT 3
#define SELECT(_a, _b, _c) _c ? _b : _a
#endif

#if SIZE==2
#define FLOAT double2
#define FIXED ulong2
#define SFIXED long2
#define STRIDESHIFT 4
#define SELECT(_a, _b, _c) bitselect(_a, _b, as_double2(_c))
#endif

#if SIZE==4
#define FLOAT double4
#define FIXED ulong4
#define SFIXED long4
#define STRIDESHIFT 5
#define SELECT(_a, _b, _c) bitselect(_a, _b, as_double4(_c))
#endif

#if SIZE==8
#define FLOAT double8
#define FIXED ulong8
#define SFIXED long8
#define STRIDESHIFT 6
#define SELECT(_a, _b, _c) bitselect(_a, _b, as_double8(_c))
#endif

#if SIZE==16
#define FLOAT double16
#define FIXED ulong16
#define SFIXED long16
#define STRIDESHIFT 7
#define SELECT(_a, _b, _c) bitselect(_a, _b, as_double16(_c))
#endif

#endif // _DOUBLE_

/* If fast_math is enabled then use native functions when appropriate 
 * for each of the supported platforms. This "fast" optimization
 * is okay for this code example's error tolerance and input set.
 */
#define SQRT(_x) sqrt(_x)
#define LOG(_x) log(_x)
#define EXP(_x) exp(_x)
#define RECIP(_x) (1.0f/(_x))
#define DIVIDE(_x,_y) (_x/_y)

#define XN(_x)(native_recip (_x))
#define XN1(_x)((XN(_x)) * (2.0f - ((_x) * (XN(_x)))))


//===================================================================================================
// First kernel: this "NDRange" kernel is designed to be instantiated many times in parallel.
// It uses the simplest form of memory movement: basic load/store.	("dm" stands for "device memory")
//===================================================================================================
#ifdef INSIEME
#pragma insieme mark
#endif
__kernel void bsop_kernel(__global FIXED * restrict dm_cpflag,
						 __global FLOAT * restrict dm_S0,
						 __global FLOAT * restrict dm_K,
						 __global FLOAT * restrict dm_r,
						 __global FLOAT * restrict dm_sigma,
						 __global FLOAT * restrict dm_T, 
						 __global FLOAT * restrict dm_answer,
						int size)
{
	uint tid = get_global_id(0);
	if(tid >= size)
		return;

	FIXED cpflag = dm_cpflag[tid];
	FLOAT S0 = dm_S0[tid];
	FLOAT K = dm_K[tid];
	FLOAT r = dm_r[tid];
	FLOAT sigma = dm_sigma[tid];
	FLOAT T = dm_T[tid];

	FLOAT d1, d2, Nd1, Nd2, expval;
	FLOAT k1, n1, k2, n2;
	FLOAT accum1, accum2;
	FLOAT candidate_answer1, candidate_answer2;
	FLOAT call, put;
	SFIXED flag1, flag2;
	d1 = LOG(DIVIDE(S0,K)) + (r + HALF * sigma * sigma) * T;
	d1 = DIVIDE (d1, (sigma * SQRT(T)));
	expval = EXP(ZERO - r * T);
	d2 = d1 - sigma * SQRT(T);
	flag1 = (d1 < ZERO);
	flag2 = (d2 < ZERO);
	d1 = fabs(d1);
	d2 = fabs(d2);
	k1 = RECIP(ONE + NCDF * d1);
	k2 = RECIP(ONE + NCDF * d2);
	accum1 = A4 + A5 * k1;
	accum2 = A4 + A5 * k2;
	accum1 = k1 * accum1 + A3;
	accum2 = k2 * accum2 + A3;
	accum1 = k1 * accum1 + A2;
	accum2 = k2 * accum2 + A2;
	accum1 = k1 * accum1 + A1;
	accum2 = k2 * accum2 + A1;
	accum1 = k1 * accum1;
	accum2 = k2 * accum2;
	n1 = EXP(ZERO - HALF * d1 * d1);
	n2 = EXP(ZERO - HALF * d2 * d2);
	n1 *= INV_ROOT2PI;
	n2 *= INV_ROOT2PI;
	candidate_answer1 = ONE - n1 * accum1;
	candidate_answer2 = ONE - n2 * accum2;
	Nd1 = SELECT(candidate_answer1, (ONE - candidate_answer1), flag1);
	Nd2 = SELECT(candidate_answer2, (ONE - candidate_answer2), flag2);
	call = S0 * Nd1 - K * expval * Nd2;
	put = K * expval * (ONE - Nd2) - S0 * (ONE - Nd1);
	dm_answer[tid] = SELECT(put, call, cpflag);
}
