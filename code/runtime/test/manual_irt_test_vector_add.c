/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#include "impl/irt_loop_sched.impl.h"
#include "irt_types.h"
#include "wi_implementation.h"
#include "utils/timing.h"
#include "impl/work_group.impl.h"
#include "utils/impl/frequency.impl.h"

#ifdef USE_OPENCL
#include <CL/cl.h>
#include "impl/irt_ocl.impl.h"
#endif

#define N 200

#define INSIEME_BOOL_T_INDEX 0
#define INSIEME_DOUBLE_T_INDEX 1
#define INSIEME_DATA_ITEM_ID_T_INDEX 2
#define INSIEME_TYPE_ID_T_INDEX 3
#define INSIEME_WI_INIT_PARAM_T_INDEX 4
#define INSIEME_WI_ADD_PARAM_T_INDEX 5
#define INSIEME_WI_CHECK_PARAM_T_INDEX 6

typedef struct _insieme_wi_init_params {
	irt_type_id type;
	irt_data_item_id A;
	irt_data_item_id B;
} insieme_wi_init_params;

typedef struct _insieme_wi_add_params {
	irt_type_id type;
	irt_data_item_id A;
	irt_data_item_id B;
	irt_data_item_id C;
} insieme_wi_add_params;

typedef struct _insieme_wi_check_params {
	irt_type_id type;
	irt_data_item_id C;
} insieme_wi_check_params;

// type table
irt_type_id g_insieme_init_params_subtypes[] = {
    INSIEME_TYPE_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX // struct including 2 data item ids
};

irt_type_id g_insieme_add_params_subtypes[] = {
    INSIEME_TYPE_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX // struct including 3 data item ids
};

irt_type_id g_insieme_check_params_subtypes[] = {
    INSIEME_TYPE_ID_T_INDEX, INSIEME_DATA_ITEM_ID_T_INDEX, // struct including 1 data item ids
};

// type table:
// # kind, size, number of sub-elements, array of sub-elements
irt_type g_insieme_type_table[] = {
    {IRT_T_BOOL, sizeof(int), 0, 0},
    {IRT_T_REAL64, sizeof(double), 0, 0},
    {IRT_T_BASIC, sizeof(irt_data_item_id), 0, 0},
    {IRT_T_BASIC, sizeof(irt_type_id), 0, 0},
    {IRT_T_STRUCT, sizeof(insieme_wi_init_params), 2, g_insieme_init_params_subtypes},
    {IRT_T_STRUCT, sizeof(insieme_wi_add_params), 2, g_insieme_add_params_subtypes},     // FIXME: CHANGE
    {IRT_T_STRUCT, sizeof(insieme_wi_check_params), 1, g_insieme_check_params_subtypes}, // FIXME: CHECK the 2 number of components
};

// work item table
void insieme_wi_startup_implementation(irt_work_item* wi);
void insieme_wi_init_implementation(irt_work_item* wi);
void insieme_wi_add_implementation1(irt_work_item* wi);
void insieme_wi_add_implementation2(irt_work_item* wi);
void insieme_wi_check_implementation(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {{&insieme_wi_startup_implementation, 0, NULL, 0, NULL, 0, {0}}};

irt_wi_implementation_variant g_insieme_wi_init_variants[] = {{&insieme_wi_init_implementation, 0, NULL, 0, NULL, 0, {0}}};

irt_wi_implementation_variant g_insieme_wi_add_variants[] = {{&insieme_wi_add_implementation1, 0, NULL, 0, NULL, 0, {0}},
                                                             {&insieme_wi_add_implementation2, 0, NULL, 0, NULL, 0, {0}}};

irt_wi_implementation_variant g_insieme_wi_check_variants[] = {{&insieme_wi_check_implementation, 0, NULL, 0, NULL, 0, {0}}};

#define INSIEME_WI_INIT_INDEX 1
#define INSIEME_WI_ADD_INDEX 2
#define INSIEME_WI_CHECK_INDEX 3

// The implementation table:code/runtime/test/manual_irt_test_vector_add.c
// # of variants, array of variants
irt_wi_implementation g_insieme_impl_table[] = {
    {1, 1, g_insieme_wi_startup_variants}, {2, 1, g_insieme_wi_init_variants}, {3, 2, g_insieme_wi_add_variants}, {4, 1, g_insieme_wi_check_variants}};

// OpenCL Kernel table
#ifdef USE_OPENCL
unsigned g_kernel_code_table_size = 1;
irt_ocl_kernel_code g_kernel_code_table[] = {
    {"vector_add", "#ifdef cl_amd_fp64 \n"
                   "#	pragma OPENCL EXTENSION cl_amd_fp64 : enable // AMD GPU PRAGMA \n"
                   "#endif \n"
                   "#ifdef cl_khr_fp64 \n"
                   "#	pragma OPENCL EXTENSION cl_khr_fp64 : enable \n"
                   "#endif \n"
                   "__kernel void vector_add(__global const double* A, __global const double* B, __global double* C, long lA, long lB)\n"
                   "{\n"
                   "	int tx = get_global_id(0);\n"
                   "	if (tx >= lA) return;\n"
                   "	double sum = A[tx];\n"
                   "	for (int i = 0; i < lB; ++i) { sum += B[i]; } \n"
                   "	C[tx] = sum;\n"
                   "}"}};
#endif

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 7;
	context->impl_table_size = 4;
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
	printf("Cleaning up manual IRT vector add\n");
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {
	// create data arrays
	irt_data_range range = {0, N, 1};
	irt_data_item* A = irt_di_create(INSIEME_DOUBLE_T_INDEX, 1, &range);
	irt_data_item* B = irt_di_create(INSIEME_DOUBLE_T_INDEX, 1, &range);
	irt_data_item* C = irt_di_create(INSIEME_DOUBLE_T_INDEX, 1, &range);

	// create and run initialization job
	insieme_wi_init_params init_params = {INSIEME_WI_INIT_PARAM_T_INDEX, A->id, B->id};
	irt_work_item* init_wi = irt_wi_create((irt_work_item_range){0, N, 1}, &g_insieme_impl_table[INSIEME_WI_INIT_INDEX], (irt_lw_data_item*)&init_params);
	irt_work_item_id init_wi_id = init_wi->id;
	irt_scheduling_assign_wi(irt_worker_get_current(), init_wi);

	// wait until finished
	irt_wi_join(init_wi_id);

	// conduct the addition
	insieme_wi_add_params add_params = {INSIEME_WI_ADD_PARAM_T_INDEX, A->id, B->id, C->id};
	irt_work_item* add_wi = irt_wi_create((irt_work_item_range){0, N, 1}, &g_insieme_impl_table[INSIEME_WI_ADD_INDEX], (irt_lw_data_item*)&add_params);
	irt_work_item_id add_wi_id = add_wi->id;
	irt_scheduling_assign_wi(irt_worker_get_current(), add_wi);

	// wait until finished
	irt_wi_join(add_wi_id);

	// conduct the check
	insieme_wi_check_params check_params = {INSIEME_WI_CHECK_PARAM_T_INDEX, C->id};
	irt_work_item* check_wi = irt_wi_create((irt_work_item_range){0, N, 1}, &g_insieme_impl_table[INSIEME_WI_CHECK_INDEX], (irt_lw_data_item*)&check_params);
	irt_work_item_id check_wi_id = check_wi->id;
	irt_scheduling_assign_wi(irt_worker_get_current(), check_wi);

	irt_wi_join(check_wi_id);

	// cleanup
	irt_di_destroy(A);
	irt_di_destroy(B);
	irt_di_destroy(C);
}

void insieme_wi_init_implementation(irt_work_item* wi) {
	// get parameters
	insieme_wi_add_params* params = (insieme_wi_add_params*)wi->parameters;

	irt_work_item_range range = wi->range;
	irt_data_range subrange = {range.begin, range.end, range.step};

	irt_data_item* itemA = irt_di_create_sub(irt_data_item_table_lookup(params->A), &subrange);
	irt_data_item* itemB = irt_di_create_sub(irt_data_item_table_lookup(params->B), &subrange);

	irt_data_block* blockA = irt_di_acquire(itemA, IRT_DMODE_WRITE_FIRST);
	irt_data_block* blockB = irt_di_acquire(itemB, IRT_DMODE_WRITE_FIRST);

	double* A = (double*)blockA->data;
	double* B = (double*)blockB->data;

	for(uint64 i = range.begin; i < range.end; i += range.step) {
		A[i] = i;
		B[i] = i;
	}

	irt_di_free(blockA);
	irt_di_free(blockB);
	irt_di_destroy(itemA);
	irt_di_destroy(itemB);
}

void insieme_wi_add_implementation1(irt_work_item* wi) {
	// get parameters
	insieme_wi_add_params* params = (insieme_wi_add_params*)wi->parameters;

	irt_work_item_range range = wi->range;
	IRT_DEBUG("VECADD WI Range: ");
	IRT_VERBOSE_ONLY(_irt_print_work_item_range(&range));

	irt_data_range subrange = {range.begin, range.end, range.step};
	irt_data_range fullrange = {0, N, 1};

	irt_data_item* itemA = irt_di_create_sub(irt_data_item_table_lookup(params->A), &subrange);
	irt_data_item* itemB = irt_di_create_sub(irt_data_item_table_lookup(params->B), &fullrange);
	irt_data_item* itemC = irt_di_create_sub(irt_data_item_table_lookup(params->C), &subrange);

	irt_data_block* blockA = irt_di_acquire(itemA, IRT_DMODE_READ_ONLY);
	irt_data_block* blockB = irt_di_acquire(itemB, IRT_DMODE_READ_ONLY);
	irt_data_block* blockC = irt_di_acquire(itemC, IRT_DMODE_WRITE_FIRST);

	double* A = (double*)blockA->data;
	double* B = (double*)blockB->data;
	double* C = (double*)blockC->data;

	for(uint64 i = range.begin; i < range.end; i += range.step) {
		double sum = A[i];
		for(uint64 j = fullrange.begin; j < fullrange.end; j += fullrange.step) { // change in fullRange if it works
			sum += B[j];
		}
		C[i] = sum;
	}

	irt_di_free(blockA);
	irt_di_free(blockB);
	irt_di_free(blockC);
	irt_di_destroy(itemA);
	irt_di_destroy(itemB);
	irt_di_destroy(itemC);
}

void insieme_wi_add_implementation2(irt_work_item* wi) {
#ifdef USE_OPENCL
	// get parameters
	insieme_wi_add_params* params = (insieme_wi_add_params*)wi->parameters;

	irt_work_item_range range = wi->range;
	IRT_DEBUG("VECADD WI Range: ");
	IRT_VERBOSE_ONLY(_irt_print_work_item_range(&range));

	irt_data_range subrange = {range.begin, range.end, range.step};
	irt_data_range fullrange = {0, N, 1};

	irt_data_item* itemA = irt_di_create_sub(irt_data_item_table_lookup(params->A), &subrange);
	irt_data_item* itemB = irt_di_create_sub(irt_data_item_table_lookup(params->B), &fullrange);
	irt_data_item* itemC = irt_di_create_sub(irt_data_item_table_lookup(params->C), &subrange);

	irt_data_block* blockA = irt_di_acquire(itemA, IRT_DMODE_READ_ONLY);
	irt_data_block* blockB = irt_di_acquire(itemB, IRT_DMODE_READ_ONLY);
	irt_data_block* blockC = irt_di_acquire(itemC, IRT_DMODE_WRITE_FIRST);

	double* A = (double*)blockA->data;
	double* B = (double*)blockB->data;
	double* C = (double*)blockC->data;

	cl_long lA = (subrange.end - subrange.begin);
	cl_long lB = (fullrange.end - fullrange.begin);
	cl_long lC = (subrange.end - subrange.begin);

	unsigned int mem_size_A = sizeof(double) * lA;
	unsigned int mem_size_B = sizeof(double) * lB;
	unsigned int mem_size_C = sizeof(double) * lC;

	irt_ocl_buffer* buff_A = irt_ocl_rt_create_buffer(CL_MEM_READ_ONLY, mem_size_A);
	irt_ocl_buffer* buff_B = irt_ocl_rt_create_buffer(CL_MEM_READ_ONLY, mem_size_B);
	irt_ocl_buffer* buff_C = irt_ocl_rt_create_buffer(CL_MEM_WRITE_ONLY, mem_size_C);

	irt_ocl_write_buffer(buff_A, CL_FALSE, 0, mem_size_A, &A[subrange.begin]);
	irt_ocl_write_buffer(buff_B, CL_FALSE, 0, mem_size_B, &B[fullrange.begin]);

	size_t localWS = 16;
	float multiplier = lA / (float)localWS;
	if(multiplier > (int)multiplier) { multiplier += 1; }
	size_t globalWS = (int)multiplier * localWS;

	size_t szLocalWorkSize = localWS;
	size_t szGlobalWorkSize = globalWS;

	irt_ocl_rt_run_kernel(0, 1, 0, &szGlobalWorkSize, &szLocalWorkSize, 5, (size_t)0, (void*)buff_A, (size_t)0, (void*)buff_B, (size_t)0, (void*)buff_C,
	                      sizeof(cl_long), (void*)&lA, sizeof(cl_long), (void*)&lB);

	irt_ocl_read_buffer(buff_C, CL_TRUE, 0, mem_size_C, &C[subrange.begin]);

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

void insieme_wi_check_implementation(irt_work_item* wi) {
	// get parameters
	insieme_wi_check_params* params = (insieme_wi_check_params*)wi->parameters;
	irt_work_item_range range = wi->range;
	irt_data_range subrange = {range.begin, range.end, range.step};
	irt_data_item* itemC = irt_di_create_sub(irt_data_item_table_lookup(params->C), &subrange);
	irt_data_block* blockC = irt_di_acquire(itemC, IRT_DMODE_READ_ONLY);

	double* C = (double*)blockC->data;

	printf("======================\n= manual irt test vector addition\n");
	bool check = true;
	int sum = 0;
	for(int j = 0; j < N; j++) {
		sum += j;
	}
	for(int i = 0; i < N; i++) {
		if(C[i] != i + sum) {
			check = false;
			// printf("= fail at (%d,%d) - expected %d / actual %f\n", i, j, i*j, R[i][j]);
		}
	}
	printf("= result check: %s\n======================\n", check ? "OK" : "FAIL");

	irt_di_free(blockC);
	irt_di_destroy(itemC);
}
