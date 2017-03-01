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
#include "irt_all_impls.h"
#include "standalone.h"

#define INSIEME_BOOL_T_INDEX 0
#define INSIEME_INT_T_INDEX 1

#define INSIEME_TEST_WI_INDEX 1
#define INSIEME_LOOP_WI_INDEX 2

typedef struct _insieme_wi_add_params {
	irt_type_id type_id;
	irt_data_item_id input;
	irt_data_item_id output;
} insieme_wi_add_params;

// type table

irt_type g_insieme_type_table[] = {
    {IRT_T_BOOL, 4, 0, 0}, {IRT_T_INT64, 8, 0, 0},
};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);
void insieme_wi_test_implementation(irt_work_item* wi);
void insieme_wi_loop_implementation(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {{&insieme_wi_startup_implementation, 0, NULL, 0, NULL, 0, {0}}};

irt_wi_implementation_variant g_insieme_wi_test_variants[] = {{&insieme_wi_test_implementation, 0, NULL, 0, NULL, 0, {0}}};

irt_wi_implementation_variant g_insieme_wi_loop_variants[] = {{&insieme_wi_loop_implementation, 0, NULL, 0, NULL, 0, {0}}};

irt_wi_implementation g_insieme_impl_table[] = {{1, 1, g_insieme_wi_startup_variants}, {2, 1, g_insieme_wi_test_variants}, {3, 1, g_insieme_wi_loop_variants}};

uint32 g_memcount = 8;

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 2;
	context->impl_table_size = 3;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
	printf("Cleaning up standalone irt test ir interface\n");
}


int main(int argc, char** argv) {
	uint32 wcount = irt_get_default_worker_count();
	if(argc >= 2) { wcount = atoi(argv[1]); }
	if(argc >= 3) { g_memcount = atoi(argv[2]); }
	irt_runtime_standalone(wcount, &insieme_init_context, &insieme_cleanup_context, &g_insieme_impl_table[0], NULL);
	return 0;
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {
	irt_parallel_job job = {g_memcount, g_memcount, 1, &g_insieme_impl_table[INSIEME_TEST_WI_INDEX], NULL};
	irt_joinable joinable = irt_parallel(&job);
	irt_merge(joinable);
}

void insieme_wi_test_implementation(irt_work_item* wi) {
	uint32 id = wi->wg_memberships[0].num;
	printf("WI %d here\n", id);
	irt_work_group* wg = wi->wg_memberships[0].wg_id.cached;
	irt_wg_barrier(wg);

	irt_work_item_range loop_range = {0, 100, 3};
	irt_schedule_loop(wi, wg, loop_range, &g_insieme_impl_table[INSIEME_LOOP_WI_INDEX], NULL);
	irt_wg_barrier(wg);
	if(id == 0) { printf("---\n"); }
	irt_wg_barrier(wg);

	irt_loop_sched_policy static10 = {IRT_STATIC_CHUNKED, 1024, .param.chunk_size = 10};
	irt_wg_set_loop_scheduling_policy(wg, &static10);
	irt_schedule_loop(wi, wg, loop_range, &g_insieme_impl_table[INSIEME_LOOP_WI_INDEX], NULL);
	irt_wg_barrier(wg);
	if(id == 0) { printf("---\n"); }
	irt_wg_barrier(wg);

	irt_loop_sched_policy dynamic10 = {IRT_DYNAMIC_CHUNKED, 1024, .param.chunk_size = 2};
	irt_wg_set_loop_scheduling_policy(wg, &dynamic10);
	irt_schedule_loop(wi, wg, loop_range, &g_insieme_impl_table[INSIEME_LOOP_WI_INDEX], NULL);
	irt_wg_barrier(wg);
	if(id == 0) { printf("---\n"); }
	irt_wg_barrier(wg);

	irt_loop_sched_policy guided10 = {IRT_GUIDED_CHUNKED, 1024, .param.chunk_size = 10};
	irt_wg_set_loop_scheduling_policy(wg, &guided10);
	irt_schedule_loop(wi, wg, loop_range, &g_insieme_impl_table[INSIEME_LOOP_WI_INDEX], NULL);
	irt_wg_barrier(wg);
	if(id == 0) { printf("---\n"); }
	irt_wg_barrier(wg);

	uint64 boundaries[] = {2, 4, 8, 16, 32, 64};
	irt_loop_sched_policy fixed_policy;
	fixed_policy.type = IRT_FIXED;
	fixed_policy.participants = 7;
	fixed_policy.param.boundaries = boundaries;
	irt_wg_set_loop_scheduling_policy(wg, &fixed_policy);
	irt_schedule_loop(wi, wg, loop_range, &g_insieme_impl_table[INSIEME_LOOP_WI_INDEX], NULL);
	irt_wg_barrier(wg);
	if(id == 0) { printf("---\n"); }
	irt_wg_barrier(wg);
}

void insieme_wi_loop_implementation(irt_work_item* wi) {
	printf("Loop WI % 2d here, from % 3ld to % 3ld\n", wi->wg_memberships[0].num, wi->range.begin, wi->range.end);
}
