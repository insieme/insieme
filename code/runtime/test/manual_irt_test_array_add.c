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
#include "impl/irt_mqueue.impl.h"
#include "impl/data_item.impl.h"
#include "irt_types.h"
#include "wi_implementation.h"
#include "utils/timing.h"

#define NUM_ELEMENTS 100000000

#define INSIEME_BOOL_T_INDEX 0
#define INSIEME_INT_T_INDEX 1
#define INSIEME_STRUCT1_T_INDEX 2
#define INSIEME_ADD_WI_PARAM_T_INDEX 2

#define INSIEME_ADD_WI_INDEX 1

typedef struct _insieme_wi_add_params {
	irt_type_id type_id;
	irt_data_item_id input;
	irt_data_item_id output;
} insieme_wi_add_params;

typedef struct _insieme_struct1 {
	bool do_add;
	int v1;
	int v2;
} insieme_struct1;

// type table

irt_type_id g_insieme_struct1_subtypes[] = {
	INSIEME_BOOL_T_INDEX, INSIEME_INT_T_INDEX, INSIEME_INT_T_INDEX // struct with a bool and 2 32 bit integers
};

irt_type g_insieme_type_table[] = {
	{ IRT_T_BOOL, 4, 0, 0 },
	{ IRT_T_INT32, 4, 0, 0 },
	{ IRT_T_STRUCT, sizeof(insieme_struct1), 3, g_insieme_struct1_subtypes },
	{ IRT_T_STRUCT, sizeof(insieme_wi_add_params), 0, 0 }
};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);

void insieme_wi_add_implementation1(irt_work_item* wi);
void insieme_wi_add_implementation2(irt_work_item* wi);
void insieme_wi_add_datareq(irt_work_item* wi, irt_wi_di_requirement* requirements);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {
	{ &insieme_wi_startup_implementation, 0, NULL, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_add_variants[] = {
	{ &insieme_wi_add_implementation1, 2, &insieme_wi_add_datareq, 0, NULL },
	{ &insieme_wi_add_implementation2, 2, &insieme_wi_add_datareq, 0, NULL }
};

irt_wi_implementation g_insieme_impl_table[] = {
	{ 1, g_insieme_wi_startup_variants },
	{ 2, g_insieme_wi_add_variants }
};

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
	printf("Cleaning up manual irt test array add\n");
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {
	irt_data_range fullrange = {0, NUM_ELEMENTS, 1};
	irt_data_item* inputdata = irt_di_create(INSIEME_STRUCT1_T_INDEX, 1, &fullrange);
	irt_data_item* outputdata = irt_di_create(INSIEME_INT_T_INDEX, 1, &fullrange);

	// fill input data
	irt_data_block* inputblock = irt_di_aquire(inputdata, IRT_DMODE_WRITE_ONLY);
	insieme_struct1* input = (insieme_struct1*)inputblock->data;
	for(int i=0; i<NUM_ELEMENTS; ++i) {
		input[i].v1 = i;
		input[i].v2 = i*2;
		input[i].do_add = true;
	}

	// pre-aquire output block
	irt_data_block* outputblock = irt_di_aquire(outputdata, IRT_DMODE_READ_ONLY);
	int* output = (int*)outputblock->data;

	printf("out_a\n");
	uint64 start_time = irt_time_ms();

	insieme_wi_add_params addition_params = {INSIEME_ADD_WI_PARAM_T_INDEX, inputdata->id, outputdata->id };
	irt_work_item* addition_wi = irt_wi_create(*(irt_work_item_range*)&fullrange, INSIEME_ADD_WI_INDEX, (irt_lw_data_item*)&addition_params);
	irt_worker_enqueue(irt_worker_get_current(), addition_wi);

	printf("out_b\n");
	irt_wi_join(addition_wi);
	printf("out_c\n");

	uint64 end_time = irt_time_ms();
	printf("out_d\n");

	printf("======================\n= manual irt test array add done\n");
	printf("= time taken: %lu\n", end_time - start_time);
	bool check = true;
	for(int i=0; i<NUM_ELEMENTS; ++i) {
		if(output[i] != i*3) {
			check = false;
			break;
		}
	}
	printf("= result check: %s\n======================\n", check ? "OK" : "FAIL");

	irt_di_free(inputblock);
	irt_di_free(outputblock);
	irt_di_destroy(inputdata);
	irt_di_destroy(outputdata);
	irt_wi_end(wi);
}

void insieme_wi_add_implementation1(irt_work_item* wi) {
	printf("a\n");
	insieme_wi_add_params *params = (insieme_wi_add_params*)wi->parameters;
	printf("a0\n");
	irt_data_item* inputdata = irt_di_create_sub(irt_data_item_table_lookup(params->input), (irt_data_range*)(&wi->range));
	irt_data_item* outputdata = irt_di_create_sub(irt_data_item_table_lookup(params->output), (irt_data_range*)(&wi->range));
	printf("a1\n");
	irt_data_block* inputblock = irt_di_aquire(inputdata, IRT_DMODE_READ_ONLY);
	irt_data_block* outputblock = irt_di_aquire(outputdata, IRT_DMODE_WRITE_ONLY);
	printf("a2\n");
	insieme_struct1* input = (insieme_struct1*)inputblock->data;
	int* output = (int*)outputblock->data;
	printf("b\n");

	for(int i = wi->range.begin; i < wi->range.end; ++i) {
		if(input[i].do_add) output[i] = input[i].v1 + input[i].v2;
	}
	printf("c\n");

	irt_di_free(inputblock);
	irt_di_free(outputblock);
	printf("d\n");
	irt_di_destroy(inputdata);
	irt_di_destroy(outputdata);
	printf("e\n");
	irt_wi_end(wi);
	printf("f\n");
}

void insieme_wi_add_implementation2(irt_work_item* wi) {

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

