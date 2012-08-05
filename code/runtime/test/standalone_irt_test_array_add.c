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

#include "declarations.h"
#include "runtime.h"

#include "irt_all_impls.h"
#include "standalone.h"

#define NUM_ELEMENTS 10000000

#define INSIEME_BOOL_T_INDEX 0
#define INSIEME_INT_T_INDEX 1
#define INSIEME_STRUCT1_T_INDEX 2
#define INSIEME_ADD_WI_PARAM_T_INDEX 3

#define INSIEME_ADD_WI_INDEX 1

typedef struct _insieme_wi_add_params {
	irt_type_id type_id;
	irt_data_item_id input;
	irt_data_item_id output;
} insieme_wi_add_params;

typedef struct _insieme_struct1 {
	bool do_add;
	uint64 v1;
	uint64 v2;
} insieme_struct1;

// type table

irt_type_id g_insieme_struct1_subtypes[] = {
	INSIEME_BOOL_T_INDEX, INSIEME_INT_T_INDEX, INSIEME_INT_T_INDEX // struct with a bool and 2 32 bit integers
};

irt_type g_insieme_type_table[] = {
	{ IRT_T_BOOL, 4, 0, 0 },
	{ IRT_T_INT64, 8, 0, 0 },
	{ IRT_T_STRUCT, sizeof(insieme_struct1), 3, g_insieme_struct1_subtypes },
	{ IRT_T_STRUCT, sizeof(insieme_wi_add_params), 0, 0 }
};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);

void insieme_wi_add_implementation1(irt_work_item* wi);
void insieme_wi_add_implementation2(irt_work_item* wi);
void insieme_wi_add_datareq(irt_work_item* wi, irt_wi_di_requirement* requirements);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_add_variants[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_add_implementation1, NULL, 2, &insieme_wi_add_datareq, 0, NULL },
	{ IRT_WI_IMPL_OPENCL, &insieme_wi_add_implementation2, NULL, 2, &insieme_wi_add_datareq, 0, NULL }
};

irt_wi_implementation g_insieme_impl_table[] = {
	{ 1, g_insieme_wi_startup_variants },
	{ 2, g_insieme_wi_add_variants }
};

// OpenCL Kernel table
#ifdef USE_OPENCL
unsigned g_kernel_code_table_size = 1;
irt_ocl_kernel_code g_kernel_code_table[] = {
	{
		"vector_add",
		"typedef struct _insieme_struct1 { \n"
		"	int do_add; \n"
		"	ulong v1; \n"
		"	ulong v2; \n"
		"} insieme_struct1; \n"
		"__kernel void vector_add(__global const insieme_struct1* input, __global ulong* output, long num_elements) \n"
		"{\n"
		"	int gid = get_global_id(0);\n"
		"	if (gid >= num_elements) return;\n"
		"	if(input[gid].do_add) { output[gid] = (input[gid].v1 + input[gid].v2)/2; }\n"
		"}"
	}
};
#endif

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 4;
	context->impl_table_size = 2;
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
	printf("Cleaning up standalone irt test array add\n");
}


int main(int argc, char **argv) {
	uint32 wcount = irt_get_default_worker_count();
	if(argc>=2) wcount = atoi(argv[1]);
	irt_runtime_standalone(wcount, &insieme_init_context, &insieme_cleanup_context, 0, NULL);
	return 0;
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {
	irt_data_range fullrange = {0, NUM_ELEMENTS, 1};
	irt_work_item_range fullrange_wi = {0, NUM_ELEMENTS, 1};
	irt_data_item* inputdata = irt_di_create(INSIEME_STRUCT1_T_INDEX, 1, &fullrange);
	irt_data_item* outputdata = irt_di_create(INSIEME_INT_T_INDEX, 1, &fullrange);

	// fill input data
	irt_data_block* inputblock = irt_di_acquire(inputdata, IRT_DMODE_WRITE_ONLY);
	insieme_struct1* input = (insieme_struct1*)inputblock->data;
	for(int i=0; i<NUM_ELEMENTS; ++i) {
		input[i].v1 = i;
		input[i].v2 = i*2;
		input[i].do_add = true;
	}

	// pre-acquire output block
	irt_data_block* outputblock = irt_di_acquire(outputdata, IRT_DMODE_READ_ONLY);
	uint64* output = (uint64*)outputblock->data;

	uint64 start_time = irt_time_ms();
	uint64 start_ticks = irt_time_ticks();

	insieme_wi_add_params addition_params = {INSIEME_ADD_WI_PARAM_T_INDEX, inputdata->id, outputdata->id };
	irt_work_item* addition_wi = irt_wi_create(fullrange_wi, INSIEME_ADD_WI_INDEX, (irt_lw_data_item*)&addition_params);
	irt_scheduling_assign_wi(irt_worker_get_current(), addition_wi);

	irt_wi_join(addition_wi);

	uint64 end_ticks = irt_time_ticks();
	uint64 end_time = irt_time_ms();

	printf("======================\n= manual irt test array add done\n");
	printf("= time taken: %lu ms, %lu clock ticks\n", end_time - start_time, end_ticks - start_ticks);
	bool check = true;
	for(uint64 i=0; i<NUM_ELEMENTS; ++i) {
		if(output[i] != i*3/2) {
			check = false;
			printf("= fail at %lu, expected %lu / actual %lu", i, i*3/2, output[i]);
			break;
		}
	}
	printf("= result check: %s\n======================\n", check ? "OK" : "FAIL");

	irt_di_free(inputblock);
	irt_di_free(outputblock);
	irt_di_destroy(inputdata);
	irt_di_destroy(outputdata);
}

void insieme_wi_add_implementation1(irt_work_item* wi) {
	insieme_wi_add_params *params = (insieme_wi_add_params*)wi->parameters;
	irt_data_item* inputdata = irt_di_create_sub(irt_data_item_table_lookup(params->input), (irt_data_range*)(&wi->range));
	irt_data_item* outputdata = irt_di_create_sub(irt_data_item_table_lookup(params->output), (irt_data_range*)(&wi->range));
	irt_data_block* inputblock = irt_di_acquire(inputdata, IRT_DMODE_READ_ONLY);
	irt_data_block* outputblock = irt_di_acquire(outputdata, IRT_DMODE_WRITE_ONLY);
	insieme_struct1* input = (insieme_struct1*)inputblock->data;
	uint64* output = (uint64*)outputblock->data;

	for(uint64 i = wi->range.begin; i < wi->range.end; ++i) {
		if(input[i].do_add) {
			output[i] = (input[i].v1 + input[i].v2) / 2;
		}
	}

	irt_di_free(inputblock);
	irt_di_free(outputblock);
	irt_di_destroy(inputdata);
	irt_di_destroy(outputdata);
}

void insieme_wi_add_implementation2(irt_work_item* wi) {
	#ifdef USE_OPENCL
	insieme_wi_add_params *params = (insieme_wi_add_params*)wi->parameters;
	irt_data_item* inputdata = irt_di_create_sub(irt_data_item_table_lookup(params->input), (irt_data_range*)(&wi->range));
	irt_data_item* outputdata = irt_di_create_sub(irt_data_item_table_lookup(params->output), (irt_data_range*)(&wi->range));
	irt_data_block* inputblock = irt_di_acquire(inputdata, IRT_DMODE_READ_ONLY);
	irt_data_block* outputblock = irt_di_acquire(outputdata, IRT_DMODE_WRITE_ONLY);
	insieme_struct1* input = (insieme_struct1*)inputblock->data;
	uint64* output = (uint64*)outputblock->data;

	cl_long len_input = (wi->range.end - wi->range.begin);
	cl_long len_output = (wi->range.end - wi->range.begin);

	unsigned int mem_size_input = sizeof(insieme_struct1) * len_input;
	unsigned int mem_size_output = sizeof(uint64) * len_output;

	irt_ocl_buffer* buf_input = irt_ocl_rt_create_buffer(CL_MEM_READ_ONLY, mem_size_input);
	irt_ocl_buffer* buf_output = irt_ocl_rt_create_buffer(CL_MEM_WRITE_ONLY, mem_size_output);

	irt_ocl_write_buffer(buf_input, CL_FALSE, 0, mem_size_input, &input[wi->range.begin]);

	size_t szLocalWorkSize = 256;
	float multiplier = NUM_ELEMENTS/(float)szLocalWorkSize;
	if(multiplier > (int)multiplier){
		multiplier += 1;
	}
	size_t szGlobalWorkSize = (int)multiplier * szLocalWorkSize;

	irt_ocl_rt_run_kernel(0,	1,0,&szGlobalWorkSize, &szLocalWorkSize,
								3,	(size_t)0, buf_input,
									(size_t)0, buf_output,
									sizeof(cl_long), &len_input);

	irt_ocl_read_buffer(buf_output, CL_TRUE, 0, mem_size_output, &output[wi->range.begin]);
	//clFinish(dev->cl_queue); // ??

	irt_ocl_release_buffer(buf_input);
	irt_ocl_release_buffer(buf_output);

#ifdef IRT_OCL_INSTR // remove this when cleanup context will work.
	irt_ocl_print_events();
#endif
	irt_di_free(inputblock);
	irt_di_free(outputblock);
	irt_di_destroy(inputdata);
	irt_di_destroy(outputdata);
	#endif
}

void insieme_wi_add_datareq(irt_work_item* wi, irt_wi_di_requirement* requirements) {
	insieme_wi_add_params* params = ((insieme_wi_add_params*)(wi->parameters));
	requirements[0].di_id = params->input;
	requirements[0].range.begin = wi->range.begin;
	requirements[0].range.end = wi->range.end;
	requirements[0].range.step = wi->range.step;
	requirements[1].di_id = params->output;
	requirements[1].range.begin = wi->range.begin;
	requirements[1].range.end = wi->range.end;
	requirements[1].range.step = wi->range.step;
}

