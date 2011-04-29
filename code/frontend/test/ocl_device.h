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
unsigned int get_global_id(unsigned int dmindx);
unsigned int get_global_size(unsigned int dmindx);
// unsupported at the moment: unsigned int get_global_offset(unsigned int dmindx);
unsigned int get_group_id(unsigned int dmindx);
unsigned int get_num_groups(unsigned int dmindx);
unsigned int get_local_id(unsigned int dmindx);
unsigned int get_local_size(unsigned int dmindx);

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
	float8 __attribute__((overloadable)) fct(float8, float8, float8); float16 __attri