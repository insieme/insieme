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
#include "impl/work_group.impl.h"
#include "impl/irt_loop_sched.impl.h"
#include "irt_types.h"
#include "wi_implementation.h"
#include "utils/timing.h"
#include "utils/impl/frequency.impl.h"

#define NUM_WIS 100

typedef struct _insieme_wi_test_params {
	irt_type_id type_id;
	irt_work_group* wg;
	uint64 vals[NUM_WIS];
} insieme_wi_test_params;

irt_type g_insieme_type_table[] = {{IRT_T_INT64, 8, 0, 0}, {IRT_T_STRUCT, sizeof(insieme_wi_test_params), 0, 0}};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);

void insieme_wi_test_implementation(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {{&insieme_wi_startup_implementation}};

irt_wi_implementation_variant g_insieme_wi_test_variants[] = {{&insieme_wi_test_implementation}};

irt_wi_implementation g_insieme_impl_table[] = {{1, 1, g_insieme_wi_startup_variants}, {2, 1, g_insieme_wi_test_variants}};

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 2;
	context->impl_table_size = 2;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
	printf("Cleaning up manual irt test redistribute\n");
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {
	uint64 start_time = irt_time_ms();
	irt_work_group* wg1 = irt_wg_create();

	insieme_wi_test_params* test_params = (insieme_wi_test_params*)malloc(sizeof(insieme_wi_test_params));
	test_params->type_id = 1;
	test_params->wg = wg1;
	irt_work_item** test_wis = (irt_work_item**)malloc(NUM_WIS * sizeof(irt_work_item*));
	irt_work_item_id* test_wi_ids = (irt_work_item_id*)malloc(NUM_WIS * sizeof(irt_work_item_id));
	for(int i = 0; i < NUM_WIS; ++i) {
		test_wis[i] = irt_wi_create(irt_g_wi_range_one_elem, &g_insieme_impl_table[1], (irt_lw_data_item*)test_params);
		test_wi_ids[i] = test_wis[i]->id;
		irt_wg_insert(wg1, test_wis[i]);
		test_params->vals[i] = 0;
	}
	for(int i = 0; i < NUM_WIS; ++i) {
		irt_scheduling_assign_wi(irt_g_workers[i % irt_g_worker_count], test_wis[i]);
	}

	for(int i = 0; i < NUM_WIS; ++i) {
		irt_wi_join(test_wi_ids[i]);
	}

	uint64 end_time = irt_time_ms();

	printf("======================\n= manual irt test redistribute done\n");
	printf("= time taken: %lu\n", end_time - start_time);
	bool check = true;
	for(uint64 i = 0; i < NUM_WIS; ++i) {
		if(test_params->vals[i] != NUM_WIS - 1) {
			check = false;
			printf("= fail at %lu, expected %d / actual %lu\n", i, NUM_WIS - 1, (test_params->vals[i]));
			break;
		}
	}
	printf("= result check: %s\n======================\n", check ? "OK" : "FAIL");

	free(test_wis);
	free(test_wi_ids);
	free(test_params);
}

void test_redist_function(void** collected, uint32 local_id, uint32 num_participants, void* out_result) {
	uint64** collected_uint = (uint64**)collected;
	*(uint64*)out_result = *(collected_uint[local_id]) + *(collected_uint[num_participants - 1 - local_id]);
}

void insieme_wi_test_implementation(irt_work_item* wi) {
	insieme_wi_test_params* params = (insieme_wi_test_params*)wi->parameters;
	uint64 thread_num = irt_wg_get_wi_num(params->wg, wi);
	params->vals[thread_num] = thread_num;
	uint64 output;
	irt_wg_redistribute(params->wg, wi, &params->vals[thread_num], &output, &test_redist_function);
	params->vals[thread_num] = output;
}
