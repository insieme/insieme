/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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

#define NUM_ELEMENTS 10000000

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
	uint64 v1;
	uint64 v2;
} insieme_struct1;

// type table

irt_type_id g_insieme_struct1_subtypes[] = {
    INSIEME_BOOL_T_INDEX, INSIEME_INT_T_INDEX, INSIEME_INT_T_INDEX // struct with a bool and 2 32 bit integers
};

irt_type g_insieme_type_table[] = {{IRT_T_BOOL, 4, 0, 0},
                                   {IRT_T_INT64, 8, 0, 0},
                                   {IRT_T_STRUCT, sizeof(insieme_struct1), 3, g_insieme_struct1_subtypes},
                                   {IRT_T_STRUCT, sizeof(insieme_wi_add_params), 0, 0}};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);

void insieme_wi_add_implementation1(irt_work_item* wi);
void insieme_wi_add_implementation2(irt_work_item* wi);
void insieme_wi_add_datareq(irt_work_item* wi, irt_wi_di_requirement* requirements);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {{&insieme_wi_startup_implementation, 0, NULL, 0, NULL}};

irt_wi_implementation_variant g_insieme_wi_add_variants[] = {{&insieme_wi_add_implementation1, 2, &insieme_wi_add_datareq}};

irt_wi_implementation g_insieme_impl_table[] = {{1, 1, g_insieme_wi_startup_variants}, {2, 1, g_insieme_wi_add_variants}};

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 4;
	context->impl_table_size = 2;
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
	irt_work_item_range fullrange_wi = {0, NUM_ELEMENTS, 1};
	irt_data_item* inputdata = irt_di_create(INSIEME_STRUCT1_T_INDEX, 1, &fullrange);
	irt_data_item* outputdata = irt_di_create(INSIEME_INT_T_INDEX, 1, &fullrange);

	// fill input data
	irt_data_block* inputblock = irt_di_acquire(inputdata, IRT_DMODE_WRITE_ONLY);
	insieme_struct1* input = (insieme_struct1*)inputblock->data;
	for(int i = 0; i < NUM_ELEMENTS; ++i) {
		input[i].v1 = i;
		input[i].v2 = i * 2;
		input[i].do_add = true;
	}

	// pre-acquire output block
	irt_data_block* outputblock = irt_di_acquire(outputdata, IRT_DMODE_READ_ONLY);
	uint64* output = (uint64*)outputblock->data;

	uint64 start_time = irt_time_ms();

	insieme_wi_add_params addition_params = {INSIEME_ADD_WI_PARAM_T_INDEX, inputdata->id, outputdata->id};
	irt_work_item* addition_wi = irt_wi_create(fullrange_wi, &g_insieme_impl_table[INSIEME_ADD_WI_INDEX], (irt_lw_data_item*)&addition_params);
	irt_work_item_id add_id = addition_wi->id;
	irt_scheduling_assign_wi(irt_worker_get_current(), addition_wi);

	irt_wi_join(add_id);

	uint64 end_time = irt_time_ms();

	printf("======================\n= manual irt test array add done\n");
	printf("= time taken: %lu\n", end_time - start_time);
	bool check = true;
	for(uint64 i = 0; i < NUM_ELEMENTS; ++i) {
		if(output[i] != i * 3 / 2) {
			check = false;
			printf("= fail at %lu, expected %lu / actual %lu", i, i * 3 / 2, output[i]);
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
	insieme_wi_add_params* params = (insieme_wi_add_params*)wi->parameters;
	irt_data_item* inputdata = irt_di_create_sub(irt_data_item_table_lookup(params->input), (irt_data_range*)(&wi->range));
	irt_data_item* outputdata = irt_di_create_sub(irt_data_item_table_lookup(params->output), (irt_data_range*)(&wi->range));
	irt_data_block* inputblock = irt_di_acquire(inputdata, IRT_DMODE_READ_ONLY);
	irt_data_block* outputblock = irt_di_acquire(outputdata, IRT_DMODE_WRITE_ONLY);
	insieme_struct1* input = (insieme_struct1*)inputblock->data;
	uint64* output = (uint64*)outputblock->data;

	for(uint64 i = wi->range.begin; i < wi->range.end; ++i) {
		if(input[i].do_add) { output[i] = (input[i].v1 + input[i].v2) / 2; }
	}

	irt_di_free(inputblock);
	irt_di_free(outputblock);
	irt_di_destroy(inputdata);
	irt_di_destroy(outputdata);
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
