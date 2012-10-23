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

#include "impl/client_app.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/irt_mqueue.impl.h"
#include "impl/data_item.impl.h"
#include "irt_types.h"
#include "wi_implementation.h"
#include "utils/timing.h"
#include "impl/work_group.impl.h"

#ifdef USE_OPENCL
#include <CL/cl.h>
#include "impl/irt_ocl.impl.h"
#endif

#define N 1000

#define INSIEME_BOOL_T_INDEX 0
#define INSIEME_DOUBLE_T_INDEX 1
#define INSIEME_DATA_ITEM_ID_T_INDEX 2
#define INSIEME_TYPE_ID_T_INDEX 3
#define INSIEME_WI_INIT_PARAM_T_INDEX 4
#define INSIEME_WI_MUL_PARAM_T_INDEX 5

typedef struct _insieme_wi_init_params {
	irt_type_id type;
	irt_data_item_id A;
	irt_data_item_id B;
} insieme_wi_init_params;

typedef struct _insieme_wi_mul_params {
	irt_type_id type;
	irt_data_item_id A;
	irt_data_item_id B;
	irt_data_item_id C;
} insieme_wi_mul_params;

// type table

irt_type_id g_insieme_init_params_subtypes[] = {
	INSIEME_TYPE_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX // struct including 2 data item ids
};

irt_type_id g_insieme_mul_params_subtypes[] = {
	INSIEME_TYPE_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX // struct including 3 data item ids
};

// type table:
// # kind, size, number of sub-elements, array of sub-elements
irt_type g_insieme_type_table[] = {
	{ IRT_T_BOOL, sizeof(int), 0, 0 },
	{ IRT_T_REAL64, sizeof(double), 0, 0 },
	{ IRT_T_BASIC,  sizeof(irt_data_item_id), 0, 0 },
	{ IRT_T_BASIC,  sizeof(irt_type_id), 0, 0 },
	{ IRT_T_STRUCT, sizeof(insieme_wi_init_params), 2, g_insieme_init_params_subtypes },
	{ IRT_T_STRUCT, sizeof(insieme_wi_mul_params), 3, g_insieme_mul_params_subtypes },
};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);

void insieme_wi_init_implementation(irt_work_item* wi);
void insieme_wi_init_datareq(irt_work_item* wi, irt_wi_di_requirement* requirements);

void insieme_wi_mul_implementation1(irt_work_item* wi);
void insieme_wi_mul_implementation2(irt_work_item* wi);
void insieme_wi_mul_implementation3(irt_work_item* wi);
void insieme_wi_mul_datareq(irt_work_item* wi, irt_wi_di_requirement* requirements);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_init_variants[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_init_implementation, NULL, 4, &insieme_wi_init_datareq, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_mul_variants[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_mul_implementation1, NULL, 6, &insieme_wi_mul_datareq, 0, NULL },
	{ IRT_WI_IMPL_OPENCL, &insieme_wi_mul_implementation2, NULL, 6, &insieme_wi_mul_datareq, 0, NULL },
	{ IRT_WI_IMPL_OPENCL, &insieme_wi_mul_implementation3, NULL, 6, &insieme_wi_mul_datareq, 0, NULL }
};

#define INSIEME_WI_INIT_INDEX 1
#define INSIEME_WI_MUL_INDEX 2

// The implementation table:
// # of variants, array of variants
irt_wi_implementation g_insieme_impl_table[] = {
	{ 1, g_insieme_wi_startup_variants },
	{ 1, g_insieme_wi_init_variants },
	{ 2, g_insieme_wi_mul_variants }
};

// OpenCL Kernel table
#ifdef USE_OPENCL
unsigned g_kernel_code_table_size = 1;
irt_ocl_kernel_code g_kernel_code_table[] = {
	{
		"matrix_mul",
		"#ifdef cl_amd_fp64 \n"
		"#	pragma OPENCL EXTENSION cl_amd_fp64 : enable // AMD GPU PRAGMA \n"
		"#endif \n"
		"#ifdef cl_khr_fp64 \n"
		"#	pragma OPENCL EXTENSION cl_khr_fp64 : enable \n"
		"#endif \n"
		"__kernel void matrix_mul(__global const double* A, __global const double* B, __global double* C, long hA, long wA, long wB)\n"
		"{\n"
		"	int tx = get_global_id(0);\n"
		"	int ty = get_global_id(1);\n"
		"	if (tx >= hA || ty >= wA ) return;\n"
		"	double sum = 0;\n"
		"	for (int k = 0; k < wA; ++k) { sum += A[tx * wA + k] * B[k * wB + ty]; } \n"
		"	C[tx * wB + ty] = sum;\n"
		"}"
	}
};
#endif

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 6;
	context->impl_table_size = 3;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
	#ifdef USE_OPENCL
	irt_ocl_rt_create_all_kernels(context, g_kernel_code_table, g_kernel_code_table_size);
	#endif
}

void insieme_cleanup_context(irt_context* context) {
	#ifdef USE_OPENCL
	irt_ocl_rt_release_all_kernels(context, g_kernel_code_table_size);
	#endif
	// nothing
	printf("Cleaning up manual IRT test matrix mul\n");
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {

	// create data arrays
	irt_data_range range[] = {{0,N,1},{0,N,1}};
	irt_data_item* A = irt_di_create(INSIEME_DOUBLE_T_INDEX, 2, range);
	irt_data_item* B = irt_di_create(INSIEME_DOUBLE_T_INDEX, 2, range);
	irt_data_item* C = irt_di_create(INSIEME_DOUBLE_T_INDEX, 2, range);

	// measure the time
	uint64 start_time = irt_time_ms();

	// create and run initialization job
	insieme_wi_init_params init_params = {INSIEME_WI_INIT_PARAM_T_INDEX, A->id, B->id};
	irt_work_item* init_wi = irt_wi_create((irt_work_item_range){0,N,1}, INSIEME_WI_INIT_INDEX, (irt_lw_data_item*)&init_params);
	irt_scheduling_assign_wi(irt_worker_get_current(), init_wi);

	// wait until finished
	irt_wi_join(init_wi);

	// conduct the multiplication
	insieme_wi_mul_params mul_params = {INSIEME_WI_MUL_PARAM_T_INDEX, A->id, B->id, C->id};
	irt_work_item* mul_wi = irt_wi_create((irt_work_item_range){0,N,1}, INSIEME_WI_MUL_INDEX, (irt_lw_data_item*)&mul_params);
	irt_scheduling_assign_wi(irt_worker_get_current(), mul_wi);

	// wait until finished
	irt_wi_join(mul_wi);

	// stop the time
	uint64 end_time = irt_time_ms();


	// check correctness

	irt_data_range subrange[] = {{0,N,1},{0,N,1}};
	irt_data_item* itemR = irt_di_create_sub(irt_data_item_table_lookup(C->id), subrange);
	irt_data_block* blockR = irt_di_acquire(itemR, IRT_DMODE_READ_ONLY);
	double** R = (double**)blockR->data;

	printf("======================\n= manual irt test matrix multiplication\n");
	printf("= time taken: %lu\n", end_time - start_time);
	bool check = true;
	for (int i=0; i<N; i++) {
		for (int j=0; j<N; j++) {
			if (R[i][j] != i*j) {
				check = false;
				//printf("= fail at (%d,%d) - expected %d / actual %f\n", i, j, i*j, R[i][j]);
			}
		}
	}
	printf("= result check: %s\n======================\n", check ? "OK" : "FAIL");

	irt_di_free(blockR);
	irt_di_destroy(itemR);

	// cleanup
	irt_di_destroy(A);
	irt_di_destroy(B);
	irt_di_destroy(C);
}

void insieme_wi_mul_implementation1(irt_work_item* wi) {
	// get parameters
	insieme_wi_mul_params *params = (insieme_wi_mul_params*)wi->parameters;

	irt_work_item_range range = wi->range;
	IRT_DEBUG("MMUL WI Range: ");
	IRT_VERBOSE_ONLY(_irt_print_work_item_range(&range));

	irt_data_range subrange[] = {{range.begin, range.end, range.step}, {0,N,1}};
	irt_data_range fullrange[] = {{0,N,1}, {0,N,1}};

	irt_data_item* itemA = irt_di_create_sub(irt_data_item_table_lookup(params->A), subrange);
	irt_data_item* itemB = irt_di_create_sub(irt_data_item_table_lookup(params->B), fullrange);
	irt_data_item* itemC = irt_di_create_sub(irt_data_item_table_lookup(params->C), subrange);

	irt_data_block* blockA = irt_di_acquire(itemA, IRT_DMODE_READ_ONLY);
	irt_data_block* blockB = irt_di_acquire(itemB, IRT_DMODE_READ_ONLY);
	irt_data_block* blockC = irt_di_acquire(itemC, IRT_DMODE_WRITE_FIRST);

	double** A = (double**)blockA->data;
	double** B = (double**)blockB->data;
	double** C = (double**)blockC->data;

	for (uint64 i = range.begin; i < range.end; i+=range.step) {
		for (uint64 j = 0; j < N; ++j) {
			double sum = 0;
			for (uint64 k =0; k<N; ++k) {
				sum += A[i][k] * B[k][j];
			}
			C[i][j] = sum;
		}
	}

	irt_di_free(blockA);
	irt_di_free(blockB);
	irt_di_free(blockC);
	irt_di_destroy(itemA);
	irt_di_destroy(itemB);
	irt_di_destroy(itemC);
}

void insieme_wi_mul_implementation2(irt_work_item* wi) {
	#ifdef USE_OPENCL
	// get parameters
	insieme_wi_mul_params *params = (insieme_wi_mul_params*)wi->parameters;

	irt_work_item_range range = wi->range;
	IRT_DEBUG("MMUL WI Range: ");
	IRT_VERBOSE_ONLY(_irt_print_work_item_range(&range));

	irt_data_range subrange[] = {{range.begin, range.end, range.step}, {0,N,1}};
	irt_data_range fullrange[] = {{0,N,1}, {0,N,1}};

	irt_data_item* itemA = irt_di_create_sub(irt_data_item_table_lookup(params->A), subrange);
	irt_data_item* itemB = irt_di_create_sub(irt_data_item_table_lookup(params->B), fullrange);
	irt_data_item* itemC = irt_di_create_sub(irt_data_item_table_lookup(params->C), subrange);

	irt_data_block* blockA = irt_di_acquire(itemA, IRT_DMODE_READ_ONLY);
	irt_data_block* blockB = irt_di_acquire(itemB, IRT_DMODE_READ_ONLY);
	irt_data_block* blockC = irt_di_acquire(itemC, IRT_DMODE_WRITE_FIRST);

	double** A = (double**)blockA->data; 
	double** B = (double**)blockB->data;
	double** C = (double**)blockC->data;
	
	cl_long hA = (subrange[0].end-subrange[0].begin);
	cl_long wA = (subrange[1].end-subrange[1].begin);
	cl_long hB = (fullrange[0].end-fullrange[0].begin);
	cl_long wB = (fullrange[1].end-fullrange[1].begin);
	cl_long hC = (subrange[0].end-subrange[0].begin);
	cl_long wC = (subrange[1].end-subrange[1].begin);

	unsigned int len_A = hA * wA;
	unsigned int len_B = hB * wB;
	unsigned int len_C = hC * wC;

	unsigned int mem_size_A = sizeof(double) * len_A;
	unsigned int mem_size_B = sizeof(double) * len_B;
	unsigned int mem_size_C = sizeof(double) * len_C;

	irt_ocl_buffer* buff_A = irt_ocl_rt_create_buffer(CL_MEM_READ_ONLY, mem_size_A);
	irt_ocl_buffer* buff_B = irt_ocl_rt_create_buffer(CL_MEM_READ_ONLY, mem_size_B);
	irt_ocl_buffer* buff_C = irt_ocl_rt_create_buffer(CL_MEM_WRITE_ONLY, mem_size_C);

	irt_ocl_write_buffer(buff_A, CL_FALSE, 0, mem_size_A, &A[subrange[0].begin][0]);
	irt_ocl_write_buffer(buff_B, CL_FALSE, 0, mem_size_B, &B[0][0]);

	size_t localWS = 16;
	float multiplier = hA/(float)localWS;
	if(multiplier > (int)multiplier){
		multiplier += 1;
	}
	size_t globalh = (int)multiplier * localWS;

	multiplier = wA/(float)localWS;
	if(multiplier > (int)multiplier){
		multiplier += 1;
	}
	size_t globalw = (int)multiplier * localWS;

	size_t szLocalWorkSize[2] = {localWS, localWS};
	size_t szGlobalWorkSize[2] = {globalh, globalw};

	irt_ocl_rt_run_kernel(	0,
							2,0,szGlobalWorkSize, szLocalWorkSize,
							6,	(size_t)0, (void *)buff_A,
								(size_t)0, (void *)buff_B,
								(size_t)0, (void *)buff_C,
								sizeof(cl_long), (void *)&hA,
								sizeof(cl_long), (void *)&wA,
								sizeof(cl_long), (void *)&wB);

	irt_ocl_read_buffer(buff_C, CL_TRUE, 0, mem_size_C, &C[subrange[0].begin][0]);

	irt_ocl_release_buffer(buff_A);
	irt_ocl_release_buffer(buff_B);
	irt_ocl_release_buffer(buff_C);

#ifdef IRT_OCL_INSTR // remove this when cleanup context will work.
	irt_ocl_print_events();
#endif
	irt_di_free(blockA);
	irt_di_free(blockB);
	irt_di_free(blockC);
	irt_di_destroy(itemA);
	irt_di_destroy(itemB);
	irt_di_destroy(itemC);
	#endif
}

void insieme_wi_mul_implementation3(irt_work_item* wi) {
	/*#ifdef USE_OPENCL
	// get parameters
	insieme_wi_mul_params *params = (insieme_wi_mul_params*)wi->parameters;

	irt_work_item_range range = wi->range;
	IRT_DEBUG("MMUL WI Range: ");
	IRT_VERBOSE_ONLY(_irt_print_work_item_range(&range));

	irt_data_range subrange[] = {{range.begin, range.end, range.step}, {0,N,1}};
	irt_data_range fullrange[] = {{0,N,1}, {0,N,1}};

	irt_data_item* itemA = irt_di_create_sub(irt_data_item_table_lookup(params->A), subrange);
	irt_data_item* itemB = irt_di_create_sub(irt_data_item_table_lookup(params->B), fullrange);
	irt_data_item* itemC = irt_di_create_sub(irt_data_item_table_lookup(params->C), subrange);

	irt_data_block* blockA = irt_di_acquire(itemA, IRT_DMODE_READ_ONLY);
	irt_data_block* blockB = irt_di_acquire(itemB, IRT_DMODE_READ_ONLY);
	irt_data_block* blockC = irt_di_acquire(itemC, IRT_DMODE_WRITE_FIRST);

	double** A = (double**)blockA->data; 
	double** B = (double**)blockB->data;
	double** C = (double**)blockC->data;

	cl_long hA = (subrange[0].end-subrange[0].begin);
	cl_long wA = (subrange[1].end-subrange[1].begin);
	cl_long hB = (fullrange[0].end-fullrange[0].begin);
	cl_long wB = (fullrange[1].end-fullrange[1].begin);
	cl_long hC = (subrange[0].end-subrange[0].begin);
	cl_long wC = (subrange[1].end-subrange[1].begin);

	unsigned int len_A = hA * wA;
	unsigned int len_B = hB * wB;
	unsigned int len_C = hC * wC;

	unsigned int mem_size_A = sizeof(double) * len_A;
	unsigned int mem_size_B = sizeof(double) * len_B;
	unsigned int mem_size_C = sizeof(double) * len_C;

	irt_ocl_buffer* buff_Ad = irt_ocl_rt_create_buffer(CL_MEM_READ_ONLY, mem_size_A);
	irt_ocl_buffer* buff_Bd = irt_ocl_rt_create_buffer(CL_MEM_READ_ONLY, mem_size_B);
	irt_ocl_buffer* buff_Cd = irt_ocl_rt_create_buffer(CL_MEM_WRITE_ONLY, mem_size_C);
	
	irt_ocl_buffer* buff_Ah = irt_ocl_rt_create_buffer(CL_MEM_ALLOC_HOST_PTR | CL_MEM_READ_ONLY, mem_size_A);
	irt_ocl_buffer* buff_Bh = irt_ocl_rt_create_buffer(CL_MEM_ALLOC_HOST_PTR | CL_MEM_READ_ONLY, mem_size_B);
	irt_ocl_buffer* buff_Ch = irt_ocl_rt_create_buffer(CL_MEM_ALLOC_HOST_PTR | CL_MEM_WRITE_ONLY, mem_size_C);
			
	double* Data_A = irt_ocl_map_buffer(buff_Ah, CL_TRUE, CL_MAP_WRITE, mem_size_A);
	double* Data_B = irt_ocl_map_buffer(buff_Bh, CL_TRUE, CL_MAP_WRITE, mem_size_B);
	
	memcpy(&Data_A[0], &A[subrange[0].begin][0], mem_size_A);
	memcpy(&Data_B[0], &B[0][0], mem_size_B);
	
	//irt_ocl_write_buffer(buff_Ad, CL_FALSE, mem_size_A, &Data_A[0]);
	//irt_ocl_write_buffer(buff_Bd, CL_FALSE, mem_size_B, &Data_B[0]);

	irt_ocl_unmap_buffer(buff_Ah, Data_A);
	irt_ocl_unmap_buffer(buff_Bh, Data_B);

	irt_ocl_copy_buffer(buff_Ah, buff_Ad, mem_size_A);
	irt_ocl_copy_buffer(buff_Bh, buff_Bd, mem_size_B);

	size_t localWS = 16;
	float multiplier = hA/(float)localWS;
	if(multiplier > (int)multiplier){
		multiplier += 1;
	}
	size_t globalh = (int)multiplier * localWS;

	multiplier = wA/(float)localWS;
	if(multiplier > (int)multiplier){
		multiplier += 1;
	}
	size_t globalw = (int)multiplier * localWS;

	size_t szLocalWorkSize[2] = {localWS, localWS};
	size_t szGlobalWorkSize[2] = {globalh, globalw};
	
	irt_ocl_rt_run_kernel(	0,
							2,0,szGlobalWorkSize, szLocalWorkSize,
							6,	(size_t)0, (void *)buff_Ad,
								(size_t)0, (void *)buff_Bd,
								(size_t)0, (void *)buff_Cd,
								sizeof(cl_long), (void *)&hA,
								sizeof(cl_long), (void *)&wA,
								sizeof(cl_long), (void *)&wB);

	irt_ocl_copy_buffer(buff_Cd, buff_Ch, mem_size_C);
	
	double* Data_C = irt_ocl_map_buffer(buff_Ch, CL_TRUE, CL_MAP_READ, mem_size_C);

	//irt_ocl_read_buffer(buff_Cd, CL_TRUE, mem_size_C, &Data_C[0]);

	memcpy(&C[subrange[0].begin][0], &Data_C[0], mem_size_C);

	irt_ocl_unmap_buffer(buff_Ch, Data_C);

	irt_ocl_release_buffer(buff_Ad);
	irt_ocl_release_buffer(buff_Bd);
	irt_ocl_release_buffer(buff_Cd);
	irt_ocl_release_buffer(buff_Ah);
	irt_ocl_release_buffer(buff_Bh);
	irt_ocl_release_buffer(buff_Ch);

#ifdef IRT_OCL_INSTR // remove this when cleanup context will work.
	irt_ocl_print_events();
#endif
	
	irt_di_free(blockA);
	irt_di_free(blockB);
	irt_di_free(blockC);
	irt_di_destroy(itemA);
	irt_di_destroy(itemB);
	irt_di_destroy(itemC);
	#endif
	*/
}

void insieme_wi_mul_datareq(irt_work_item* wi, irt_wi_di_requirement* requirements) {

	irt_work_item_range range = wi->range;
	insieme_wi_mul_params* params = ((insieme_wi_mul_params*)(wi->parameters));

	int i =0;

	// dependency A (just a few rows)
	// dim = 1
	requirements[i].di_id = params->A;
	requirements[i].range = (irt_data_range){range.begin, range.end, range.step};
	i++;
	// dim = 2
	requirements[i].di_id = params->A;
	requirements[i].range = (irt_data_range){0,N,1};
	i++;


	// dependency B (all of B)
	// dim = 1
	requirements[i].di_id = params->B;
	requirements[i].range = (irt_data_range){0,N,1};
	i++;
	// dim = 2
	requirements[i].di_id = params->B;
	requirements[i].range = (irt_data_range){0,N,1};
	i++;

	// dependency C (just a few rows)
	// dim = 1
	requirements[i].di_id = params->C;
	requirements[i].range = (irt_data_range){range.begin, range.end, range.step};
	i++;
	// dim = 2
	requirements[i].di_id = params->C;
	requirements[i].range = (irt_data_range){0,N,1};
	i++;
}

void insieme_wi_init_implementation(irt_work_item* wi) {

	// get parameters
	insieme_wi_mul_params *params = (insieme_wi_mul_params*)wi->parameters;

	irt_work_item_range range = wi->range;
	irt_data_range subrange[] = {{range.begin, range.end, range.step}, {0,N,1}};

	irt_data_item* itemA = irt_di_create_sub(irt_data_item_table_lookup(params->A), subrange);
	irt_data_item* itemB = irt_di_create_sub(irt_data_item_table_lookup(params->B), subrange);

	irt_data_block* blockA = irt_di_acquire(itemA, IRT_DMODE_WRITE_FIRST);
	irt_data_block* blockB = irt_di_acquire(itemB, IRT_DMODE_WRITE_FIRST);

	double** A = (double**)blockA->data;
	double** B = (double**)blockB->data;

	for (uint64 i = range.begin; i < range.end; i+=range.step) {
		for (uint64 j = 0; j < N; ++j) {
			A[i][j] = i*j;
			B[i][j] = (i==j)?1:0;
		}
	}

	irt_di_free(blockA);
	irt_di_free(blockB);
	irt_di_destroy(itemA);
	irt_di_destroy(itemB);
}

void insieme_wi_init_datareq(irt_work_item* wi, irt_wi_di_requirement* requirements) {

	irt_work_item_range range = wi->range;
	insieme_wi_init_params* params = ((insieme_wi_init_params*)(wi->parameters));

	int i =0;

	// dependency A (just a few rows)
	// dim = 1
	requirements[i].di_id = params->A;
	requirements[i].range = (irt_data_range){range.begin, range.end, range.step};
	i++;
	// dim = 2
	requirements[i].di_id = params->A;
	requirements[i].range = (irt_data_range){0,N,1};
	i++;


	// dependency B (all of B)
	// dim = 1
	requirements[i].di_id = params->B;
	requirements[i].range = (irt_data_range){range.begin, range.end, range.step};
	i++;
	// dim = 2
	requirements[i].di_id = params->B;
	requirements[i].range = (irt_data_range){0,N,1};
	i++;

}


