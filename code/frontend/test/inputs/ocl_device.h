/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

//OpenCL definitions

//define all pragmas per default
#define cl_khr_fp64
#define cl_khr_base_atomics
#define  cl_khr_int64_extended_atomics
//#define cl_khr_3d_image_writes not supported at the moment
#define cl_khr_fp16
#define CL_APPLE_gl_sharing
#define CL_KHR_gl_sharing
#define cl_khr_gl_event
//#define cl_khr_d3d10_sharing

//OpenCL constants
#define FLT_DIG        6
#define FLT_MANT_DIG   24
#define FLT_MAX_10_EXP +38
#define FLT_MAX_EXP    +128
#define FLT_MIN_10_EXP -37
#define FLT_MIN_EXP    -125
#define FLT_RADIX      2
#define FLT_MAX        0x1.fffffep127f
#define FLT_MIN        0x1.0p-126f
#define FLT_EPSILON    0x1.0p-23f

//Symbolic Math Constants
#define MAXFLOAT FLT_MAX
#define HUGE_VALF ((float)0x7f800000)
#define HUGE_VAL ((double)0x7ff0000000000000)
#define INFINITY ((float)0x7ff0000000000000)
#define NAN ((float)0x7fffffff)

//TODO change to enums once they are supported
//enum cl_mem_fence_flags {CLK_LOCAL_MEM_FENCE, CLK_GLOBAL_MEM_FENCE};
#define CLK_LOCAL_MEM_FENCE 0
#define CLK_GLOBAL_MEM_FENCE 1
// avoid naming conflicts with insieme barriers
#define barrier(mem_fence) ocl_barrier(mem_fence)

//images are not supported
#define __IMAGE_SUPPORT__ 0

//define address spaces
#define __private __attribute__((annotate("__private"))) //default value
#define private __attribute__((annotate("__private")))
#define __local __attribute__((annotate("__local")))
#define local __attribute__((annotate("__local")))
#define __global __attribute__((annotate("__global")))
#define global __attribute__((annotate("__global")))
#define __constant __attribute__((annotate("__constant")))
#define constant __attribute__((annotate("__constant")))

#define __kernel __attribute__((annotate("__kernel")))
#define kernel __attribute__((annotate("__kernel")))

//define built-in data types
#define ulong unsigned long
#define uint unsigned int
#define size_t unsigned int
#define ushort unsigned short
#define uchar unsigned char

//half workaround
#ifdef cl_khr_fp16
#define half float
#endif

//define built-in vector types
#define ivec(T,v) typedef __attribute__((ext_vector_type(v))) T T##v; typedef __attribute__((ext_vector_type(v))) unsigned T u##T##v;
#define fvec(T,v) typedef __attribute__((ext_vector_type(v))) T T##v;

typedef __attribute__((ext_vector_type(2))) bool bool2;
typedef __attribute__((ext_vector_type(3))) bool bool3;
typedef __attribute__((ext_vector_type(4))) bool bool4;
typedef __attribute__((ext_vector_type(8))) bool bool8;
typedef __attribute__((ext_vector_type(16))) bool bool16;

ivec(char,2)
ivec(char,3)
ivec(char,4)
ivec(char,8)
ivec(char,16)

ivec(short,2)
ivec(short,3)
ivec(short,4)
ivec(short,8)
ivec(short,16)

ivec(int,2)
ivec(int,3)
ivec(int,4)
ivec(int,8)
ivec(int,16)

//assuming that long long is 64 bit wide on all architectures
typedef __attribute__((ext_vector_type(2))) long long long2; typedef __attribute__((ext_vector_type(2))) unsigned long long ulong2;
typedef __attribute__((ext_vector_type(3))) long long long3; typedef __attribute__((ext_vector_type(3))) unsigned long long ulong3;
typedef __attribute__((ext_vector_type(4))) long long long4; typedef __attribute__((ext_vector_type(4))) unsigned long long ulong4;
typedef __attribute__((ext_vector_type(8))) long long long8; typedef __attribute__((ext_vector_type(8))) unsigned long long ulong8;
typedef __attribute__((ext_vector_type(16))) long long long16; typedef __attribute__((ext_vector_type(16))) unsigned long long ulong16;

fvec(float,2)
fvec(float,3)
fvec(float,4)
fvec(float,8)
fvec(float,16)

#ifdef cl_khr_fp64
fvec(double,2)
fvec(double,3)
fvec(double,4)
fvec(double,8)
fvec(double,16)
#endif

//define build-in functions
unsigned long get_global_id(unsigned int dmindx);
unsigned long get_global_size(unsigned int dmindx);
// unsupported at the moment: unsigned int get_global_offset(unsigned int dmindx);
unsigned long get_group_id(unsigned int dmindx);
unsigned long get_num_groups(unsigned int dmindx);
unsigned long get_local_id(unsigned int dmindx);
unsigned long get_local_size(unsigned int dmindx);

void barrier(int flags); //TODO change to enum

// math functions
#ifdef cl_khr_fp64
#define dtypefun(fct) double __attribute__((overloadable)) fct(double); double2 __attribute__((overloadable)) fct(double2);
    double3 __attribute__((overloadable)) fct(double3); double4 __attribute__((overloadable)) fct(double4); \
    double8 __attribute__((overloadable)) fct(double8); double16 __attribute__((overloadable)) fct(double16);

#define dtypefun2(fct) double __attribute__((overloadable)) fct(double, double); double2 __attribute__((overloadable)) fct(double2, double2); \
	double3 __attribute__((overloadable)) fct(double3, double3); double4 __attribute__((overloadable)) fct(double4, double4); \
	double8 __attribute__((overloadable)) fct(double8, double8); double16 __attribute__((overloadable)) fct(double16, double16);

#define dtypefun3(fct) double __attribute__((overloadable)) fct(double, double, double); double2 __attribute__((overloadable)) fct(double2, double2, double2); \
	double3 __attribute__((overloadable)) fct(double3, double3, double3); double4 __attribute__((overloadable)) fct(double4, double4, double4); \
	double8 __attribute__((overloadable)) fct(double8, double8, double8); double16 __attribute__((overloadable)) fct(double16, double16, double16);

#define dtypefuni(fct) double __attribute__((overloadable)) fct(double, int); double2 __attribute__((overloadable)) fct(double2, int2); \
	double3 __attribute__((overloadable)) fct(double3, int3); double4 __attribute__((overloadable)) fct(double4, int4); \
	double8 __attribute__((overloadable)) fct(double8, int8); double16 __attribute__((overloadable)) fct(double16, int16);

#define dtypefunptr(fct) double __attribute__((overloadable)) fct(double, double*); double2 __attribute__((overloadable)) fct(double2, double2*); \
	double3 __attribute__((overloadable)) fct(double3, double3*); double4 __attribute__((overloadable)) fct(double4, double4*); \
	double8 __attribute__((overloadable)) fct(double8, double8*); double16 __attribute__((overloadable)) fct(double16, double16*);

#define dtypefuniptr(fct) double __attribute__((overloadable)) fct(double, int*); double2 __attribute__((overloadable)) fct(double2, int2*); \
	double3 __attribute__((overloadable)) fct(double3, int3*); double4 __attribute__((overloadable)) fct(double4, int4*); \
	double8 __attribute__((overloadable)) fct(double8, int8*); double16 __attribute__((overloadable)) fct(double16, int16*);

#define dtypefun2iptr(fct) double __attribute__((overloadable)) fct(double, double, int*); double2 __attribute__((overloadable)) fct(double2, double2, int2*); \
	double3 __attribute__((overloadable)) fct(double3, double3, int3*); double4 __attribute__((overloadable)) fct(double4, double4, int4*); \
	double8 __attribute__((overloadable)) fct(double8, double8, int8*); double16 __attribute__((overloadable)) fct(double16, double16, int16*);

#define dtypefundbl(fct) double __attribute__((overloadable)) fct(double, double); double2 __attribute__((overloadable)) fct(double2, double); \
	double3 __attribute__((overloadable)) fct(double3, double); double4 __attribute__((overloadable)) fct(double4, double); \
	double8 __attribute__((overloadable)) fct(double8, double); double16 __attribute__((overloadable)) fct(double16, double);

#else
#define dtypefun(fct)
#define dtypefun2(fct)
#define dtypefun3(fct)
#define dtypefuni(fct)
#define dtypefunptr(fct)
#define dtypefuniptr(fct)
#define dtypefun2iptr(fct)
#define dtypefundbl(fct)
#endif

#define genfun(fct) float __attribute__((overloadable)) fct(float); float2 __attribute__((overloadable)) fct(float2);  \
	float3 __attribute__((overloadable)) fct(float3); float4 __attribute__((overloadable)) fct(float4); \
	float8 __attribute__((overloadable)) fct(float8); float16 __attribute__((overloadable)) fct(float16); dtypefun(fct);

#define genfun2(fct) float __attribute__((overloadable)) fct(float, float); int __attribute__((overloadable)) fct(int, uint); float2 __attribute__((overloadable)) fct(float2, float2); \
	float3 __attribute__((overloadable)) fct(float3, float3); float4 __attribute__((overloadable)) fct(float4, float4); \
	float8 __attribute__((overloadable)) fct(float8, float8); float16 __attribute__((overloadable)) fct(float16, float16); dtypefun2(fct)

#define genfun3(fct) float __attribute__((overloadable)) fct(float, float, float); float2 __attribute__((overloadable)) fct(float2, float2, float2); \
	float3 __attribute__((overloadable)) fct(float3, float3, float3); float4 __attribute__((overloadable)) fct(float4, float4, float4); \
	float8 __attribute__((overloadable)) fct(float8, float8, float8); float16 __attribute__((overloadable)) fct(float16, float16, float16); dtypefun3(fct)

#define genfuni(fct) float __attribute__((overloadable)) fct(float, int); float2 __attribute__((overloadable)) fct(float2, int2); \
	float3 __attribute__((overloadable)) fct(float3, int3); float4 __attribute__((overloadable)) fct(float4, int4); \
	float8 __attribute__((overloadable)) fct(float8, int8); float16 __attribute__((overloadable)) fct(float16, int16); dtypefuni(fct)

#define genfunptr(fct) float __attribute__((overloadable)) fct(float, float*); float2 __attribute__((overloadable)) fct(float2, float2*); \
	float3 __attribute__((overloadable)) fct(float3, float3*); float4 __attribute__((overloadable)) fct(float4, float4*); \
	float8 __attribute__((overloadable)) fct(float8, float8*); float16 __attribute__((overloadable)) fct(float16, float16*); dtypefunptr(fct)

#define genfuniptr(fct) float __attribute__((overloadable)) fct(float, int*); float2 __attribute__((overloadable)) fct(float2, int2*); \
	float3 __attribute__((overloadable)) fct(float3, int3*); float4 __attribute__((overloadable)) fct(float4, int4*); \
	float8 __attribute__((overloadable)) fct(float8, int8*); float16 __attribute__((overloadable)) fct(float16, int16*); dtypefuniptr(fct)

#define genfun2iptr(fct) float __attribute__((overloadable)) fct(float, float, int*); float2 __attribute__((overloadable)) fct(float2, float2, int2*); \
	float3 __attribute__((overloadable)) fct(float3, float3, int3*); float4 __attribute__((overloadable)) fct(float4, float4, int4*); \
	float8 __attribute__((overloadable)) fct(float8, float8, int8*); float16 __attribute__((overloadable)) fct(float16, float16, int16*); dtypefun2iptr(fct)

#define genfunflt(fct) float __attribute__((overloadable)) fct(float, float); float2 __attribute__((overloadable)) fct(float2, float); \
	float3 __attribute__((overloadable)) fct(float3, float); float4 __attribute__((overloadable)) fct(float4, float); \
	float8 __attribute__((overloadable)) fct(float8, float); float16 __attribute__((overloadable)) fct(float16, float); dtypefundbl(fct)

#define genfun_native(fct) float __attribute__((overloadable)) native_##fct(float); float2 __attribute__((overloadable)) native_##fct(float2); \
	float3 __attribute__((overloadable)) native_##fct(float3); float4 __attribute__((overloadable)) native_##fct(float4); \
	float8 __attribute__((overloadable)) native_##fct(float8); float16 __attribute__((overloadable)) native_##fct(float16); genfun(fct)

#define genfun2_native(fct) float __attribute__((overloadable)) native_##fct(float, float); float2 __attribute__((overloadable)) native_##fct(float2, float2); \
	float3 __attribute__((overloadable)) native_##fct(float3, float3); float4 __attribute__((overloadable)) native_##fct(float4, float4); \
	float8 __attribute__((overloadable)) native_##fct(float8, float8); float16 __attribute__((overloadable)) native_##fct(float16, float16); genfun2(fct)

// opencl math functions
genfun(acos)
genfun(acosh)
genfun(acospi)
genfun(asin)
genfun(asinh)
genfun(asinpi)
genfun(atan)
genfun2(atan2)
genfun(atanh)
genfun(atanpi)
genfun2(atan2pi)
genfun(cbrt)
genfun(ceil)
genfun2(copysign)
genfun_native(cos)
genfun(cosh)
genfun(cospi)
genfun(erfc)
genfun(erf)
genfun_native(exp)
genfun_native(exp2)
genfun_native(exp10)
genfun(expm1)
genfun(fabs)
genfun2(fdim)
genfun(floor)
genfun3(fma)
genfun2(fmax) genfunflt(fmax)
genfun2(fmin) genfunflt(fmin)
genfun2(fmod)
genfunptr(fract)
genfuniptr(frexp)
genfun2(hypot)
genfun(ilogb)
genfuni(ldexp)
genfun(lgamma)
genfuniptr(lgamma_r)
genfun_native(log)
genfun_native(log2)
genfun_native(log10)
genfun(log1p)
genfun(logb)
//genfun3(mad)
genfun2(max) genfunflt(max)
genfun2(min) genfunflt(min)
genfun2(maxmag)
genfun2(minmag)
genfunptr(modf)
genfun2(nextafter)
genfun2(pow)
genfuni(pown)
genfun2_native(powr)
genfun2(remainder)
genfun2iptr(remquo)
genfun(rint)
genfuni(rootn)
genfun(round)
genfun_native(rsqrt)
genfun_native(sin)
genfunptr(sincos)
genfun(sinh)
genfun(sinpi)
genfun_native(sqrt)
genfun_native(tan)
genfun(tanh)
genfun(tanpi)
genfun(tgamma)
genfun(trunc)

// additional opencl built-in functions
genfun(normalize)

#define mad(a, b, c) (a * b) + c

#define clamp(x, minval, maxval) min(max(x, minval), maxval)

#define genfunVec(fct, type) type __attribute__((overloadable)) fct(type, type); type##2 __attribute__((overloadable)) fct(type##2, type##2); \
    type##3 __attribute__((overloadable)) fct(type##3, type##3);  type##4 __attribute__((overloadable)) fct(type##4, type##4); \
	type##8 __attribute__((overloadable)) fct(type##8, type##8);  type##16 __attribute__((overloadable)) fct(type##16, type##16);
#define genfunSign(fct, type) genfunVec(fct, type) genfunVec(fct, u##type)
#define genfunInt(fct) genfunSign(fct, char) genfunSign(fct, short) genfunSign(fct, int) genfunSign(fct, long)

genfunInt(mul_hi)
genfunSign(mul24, int)

#define genfuniVec(fct, type) int __attribute__((overloadable)) fct(type); int __attribute__((overloadable)) fct(type##2); \
		int __attribute__((overloadable)) fct(type##3);  int __attribute__((overloadable)) fct(type##4); \
		int __attribute__((overloadable)) fct(type##8);  int __attribute__((overloadable)) fct(type##16);
#define genfuniSign(fct, type) genfuniVec(fct, type) genfuniVec(fct, u##type)
#define genfuniInt(fct) genfuniSign(fct, char) genfuniSign(fct, short) genfuniSign(fct, int) genfuniSign(fct, long)

genfuniInt(any)
genfuniInt(all)

float __attribute__((overloadable)) ldexp(float, int); float2 __attribute__((overloadable)) ldexp(float2, int2); \
    float3 __attribute__((overloadable)) ldexp(float3, int3); float4 __attribute__((overloadable)) ldexp(float4, int4); \
    float8 __attribute__((overloadable)) ldexp(float8, int8); float16 __attribute__((overloadable)) ldexp(float16, int16);
#ifdef cl_khr_fp64
double __attribute__((overloadable)) ldexp(double, int); double2 __attribute__((overloadable)) ldexp(double2, int2); \
    double3 __attribute__((overloadable)) ldexp(double3, int3); double4 __attribute__((overloadable)) ldexp(double4, int4); \
    double8 __attribute__((overloadable)) ldexp(double8, int8); double16 __attribute__((overloadable)) ldexp(double16, int16);
#endif

float __attribute__((overloadable)) nan(uint); float2 __attribute__((overloadable)) nan(uint2); float3 __attribute__((overloadable)) nan(uint3);
    float4 __attribute__((overloadable)) nan(uint4); float8 __attribute__((overloadable)) nan(uint8); float16 __attribute__((overloadable)) nan(uint16);

float __attribute__((overloadable)) native_divide(float, float); float2 __attribute__((overloadable)) native_divide(float2, float2); \
    float3 __attribute__((overloadable)) native_divide(float3, float3); float4 __attribute__((overloadable)) native_divide(float4, float4); \
    float8 __attribute__((overloadable)) native_divide(float8, float8); float16 __attribute__((overloadable)) native_divide(float16, float16);


// atomic operations
#define atom_fct(op) long __attribute__((overloadable)) atom_##op(long* p, long val); ulong __attribute__((overloadable)) atom_##op(ulong* p, ulong val); \
    /* only to ship around clang problems */ \
    int __attribute__((overloadable)) atom_##op(int* p, int val); uint __attribute__((overloadable)) atom_##op(uint* p, uint val); \

atom_fct(add)
atom_fct(sub)
atom_fct(xchg)
atom_fct(xchg)

#define atom_inc(p) atom_add(p, 1)
#define atom_dec(p) atom_sub(p, 1)
long __attribute__((overloadable)) atom_cmpxchg(long *p, long cmp, long val); ulong __attribute__((overloadable)) atom_cmpxchg(ulong *p, ulong cmp, ulong val);

// convert
//* too slow...
#define iconv_round(dest, src, fct) dest __attribute__((overloadable)) fct(src); dest __attribute__((overloadable)) fct##_rte(src); \
    dest __attribute__((overloadable)) fct##_rtz(src); dest __attribute__((overloadable)) fct##_rtp(src); dest __attribute__((overloadable)) fct##_rtn(src);
#define iconv_sat(dest, src) iconv_round(dest, src, convert_##dest) iconv_round(dest, src, convert_##dest##_sat) \
    iconv_round(u##dest, src, convert_u##dest) iconv_round(u##dest, src, convert_u##dest##_sat)
#define iconv_vec(dest, src) iconv_sat(dest, src) iconv_sat(dest##2, src##2) iconv_sat(dest##3, src##3) iconv_sat(dest##4, src##4) iconv_sat(dest##8, src##8) \
    iconv_sat(dest##16, src##16)
#define iconv(type) iconv_vec(type, char) iconv_vec(type, uchar) iconv_vec(type, short) iconv_vec(type, ushort) iconv_vec(type, int) iconv_vec(type, uint) \
    iconv_vec(type, long) iconv_vec(type, ulong) iconv_vec(type, float) iconv_vec(type, double)

iconv(char)
iconv(short)
iconv(int)
iconv(long)

#define fconv_round(dest, src, fct) dest __attribute__((overloadable)) fct(src); dest __attribute__((overloadable)) fct##_rte(src); \
    dest __attribute__((overloadable)) fct##_rtz(src); dest __attribute__((overloadable)) fct##_rtp(src); dest __attribute__((overloadable)) fct##_rtn(src);
#define fconv_sat(dest, src) fconv_round(dest, src, convert_##dest) fconv_round(dest, src, convert_##dest##_sat)
#define fconv_vec(dest, src) fconv_sat(dest, src) fconv_sat(dest##2, src##2) fconv_sat(dest##3, src##3) fconv_sat(dest##4, src##4) fconv_sat(dest##8, src##8) \
    fconv_sat(dest##16, src##16)
#define fconv(type) fconv_vec(type, char) fconv_vec(type, uchar) fconv_vec(type, short) fconv_vec(type, ushort) fconv_vec(type, int) fconv_vec(type, uint) \
    fconv_vec(type, long) fconv_vec(type, ulong) fconv_vec(type, float) fconv_vec(type, double)

fconv(float)
fconv(double)

// these variables should only be used in the insieme datarange pragma to describe the data elements one thread accesses
int __insieme_ocl_globalId, __insieme_ocl_groupId, __insieme_ocl_localId;
int __insieme_ocl_globalSize, __insieme_ocl_numGroups, __insieme_ocl_localSize;

