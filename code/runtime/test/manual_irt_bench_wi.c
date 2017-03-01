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
 *
 */
#include <math.h>

#include "impl/client_app.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"
#include "impl/irt_mqueue.impl.h"
#include "impl/data_item.impl.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/irt_loop_sched.impl.h"
#include "irt_types.h"
#include "wi_implementation.h"
#include "utils/timing.h"
#include "impl/work_group.impl.h"
#include "utils/impl/frequency.impl.h"

#define NUM_REPEATS 10
#define NUM_ITER 10000
#define NUM_LEVELS 1

typedef struct _insieme_wi_bench_params {
	irt_type_id type_id;
	uint64 count;
	uint64* check;
} insieme_wi_bench_params;

irt_type g_insieme_type_table[] = {{IRT_T_INT64, 8, 0, 0}, {IRT_T_STRUCT, sizeof(insieme_wi_bench_params), 0, 0}};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);
void insieme_wi_bench_implementation(irt_work_item* wi);
void insieme_wi_opt_bench_implementation(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {{&insieme_wi_startup_implementation}};

irt_wi_implementation_variant g_insieme_wi_bench_variants[] = {{&insieme_wi_bench_implementation}};
irt_wi_implementation_variant g_insieme_wi_opt_bench_variants[] = {{&insieme_wi_opt_bench_implementation}};

irt_wi_implementation g_insieme_impl_table[] = {
    {1, 1, g_insieme_wi_startup_variants}, {2, 1, g_insieme_wi_bench_variants}, {3, 1, g_insieme_wi_opt_bench_variants}};

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 2;
	context->impl_table_size = 3;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {
	{
		uint64 start_time = irt_time_ms();
		uint64 check_val = 0;
		insieme_wi_bench_params bench_params = {1, NUM_LEVELS, &check_val};
		for(int i = 0; i < NUM_REPEATS; ++i) {
			irt_work_item* bench_wi = irt_wi_create(irt_g_wi_range_one_elem, &g_insieme_impl_table[1], (irt_lw_data_item*)&bench_params);
			irt_work_item_id wi_id = bench_wi->id;
			irt_scheduling_assign_wi(irt_worker_get_current(), bench_wi);
			irt_wi_join(wi_id);
		}
		uint64 total_time = irt_time_ms() - start_time;
		uint64 total_wis = (uint64)pow((double)NUM_ITER, (double)NUM_LEVELS);
		uint64 wis_per_sec = (uint64)(total_wis / ((double)total_time / 1000.0));
		printf("======================\n= manual irt wi benchmark done\n");
		printf("= number of wis executed: %lu\n", check_val);
		printf("= time taken: %lu\n", total_time);
		printf("= wis/s: %lu\n======================\n", wis_per_sec);
	}

	//{
	//	uint64 start_time = irt_time_ms();
	//	uint64 check_val = 0;
	//	insieme_wi_bench_params bench_params = { 1, NUM_LEVELS, &check_val };
	//	irt_work_item* bench_wi = irt_wi_create(irt_g_wi_range_one_elem, 2, (irt_lw_data_item*)&bench_params);
	//	irt_scheduling_assign_wi(irt_worker_get_current(), bench_wi);
	//	irt_wi_join(bench_wi);
	//	uint64 total_time = irt_time_ms() - start_time;
	//	uint64 total_wis = pow(NUM_ITER, NUM_LEVELS);
	//	uint64 wis_per_sec = (uint64)(total_wis/((double)total_time/1000.0));
	//	printf("======================\n= manual irt optional wi benchmark done\n");
	//	printf("= number of wis executed: %lu\n", check_val);
	//	printf("= time taken: %lu\n", total_time);
	//	printf("= optional wis/s: %lu\n======================\n", wis_per_sec);
	//}
}

void insieme_wi_bench_implementation(irt_work_item* wi) {
	insieme_wi_bench_params* params = (insieme_wi_bench_params*)wi->parameters;

	if(params->count > 0) {
		insieme_wi_bench_params bench_params = {1, params->count - 1, params->check};
		irt_work_item_id* bench_wi_ids = (irt_work_item_id*)malloc(NUM_ITER * sizeof(irt_work_item_id));
		for(int i = 0; i < NUM_ITER; ++i) {
			irt_work_item* wi = irt_wi_create(irt_g_wi_range_one_elem, &g_insieme_impl_table[1], (irt_lw_data_item*)&bench_params);
			bench_wi_ids[i] = wi->id;
			irt_scheduling_assign_wi(irt_worker_get_current(), wi);
		}

		// irt_wi_multi_join(NUM_ITER, bench_wis);
		for(int i = 0; i < NUM_ITER; ++i) {
			irt_wi_join(bench_wi_ids[i]);
		}

		free(bench_wi_ids);
	}
	irt_atomic_inc(params->check, uint64);
}

void insieme_wi_opt_bench_implementation(irt_work_item* wi) {
	insieme_wi_bench_params* params = (insieme_wi_bench_params*)wi->parameters;

	if(params->count > 0) {
		insieme_wi_bench_params bench_params = {1, params->count - 1, params->check};
		irt_work_item_id* bench_wi_ids = (irt_work_item_id*)malloc(NUM_ITER * sizeof(irt_work_item_id));
		for(int i = 0; i < NUM_ITER; ++i) {
			bench_wi_ids[i] = irt_wi_run_optional(irt_g_wi_range_one_elem, &g_insieme_impl_table[2], (irt_lw_data_item*)&bench_params).wi_id;
		}

		// irt_wi_multi_join(NUM_ITER, bench_wis);
		for(int i = 0; i < NUM_ITER; ++i) {
			irt_wi_join(bench_wi_ids[i]);
		}

		free(bench_wi_ids);
	}
	irt_atomic_inc(params->check, uint64);
}
