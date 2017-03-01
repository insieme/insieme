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

#define INSIEME_INT64_T_INDEX 0

#define INSIEME_HW_WI_INDEX 1

int g_dummy_test = 0;

// type table

irt_type g_insieme_type_table[] = {
    {IRT_T_INT64, 8, 0, 0},
};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);
void insieme_wi_hw_implementation(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {{&insieme_wi_startup_implementation, 0, NULL, 0, NULL, 0, {0}}};

irt_wi_implementation_variant g_insieme_wi_hw_variants[] = {{&insieme_wi_hw_implementation, 0, NULL, 0, NULL, 0, {0}}};

irt_wi_implementation g_insieme_impl_table[] = {{1, 1, g_insieme_wi_startup_variants}, {2, 1, g_insieme_wi_hw_variants}};

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 1;
	context->impl_table_size = 2;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
	printf("Cleaning up standalone irt test hello world\n");
}


int main(int argc, char** argv) {
	uint32 wcount = irt_get_default_worker_count();
	if(argc >= 2) { wcount = atoi(argv[1]); }
	irt_runtime_standalone(wcount, &insieme_init_context, &insieme_cleanup_context, &g_insieme_impl_table[0], NULL);
	return 0;
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {
	printf("Startup!\n");

	irt_time_set_ticks_per_sec();
	bool ticks_available = irt_time_ticks_available();
	if(!ticks_available) { printf("No time ticks available!\n"); }

	printf("tsc constant: %d\n", irt_time_ticks_constant());

	uint64 starttsc, endtsc;
	if(ticks_available) { starttsc = irt_time_ticks(); }

	uint64 startms = irt_time_ms();

	irt_work_item_range range;
	range.begin = 0;
	range.end = irt_g_worker_count;
	range.step = 1;
	irt_work_item* child = irt_wi_create(range, &g_insieme_impl_table[INSIEME_HW_WI_INDEX], NULL);
	irt_work_item_id child_id = child->id;
	irt_scheduling_assign_wi(irt_worker_get_current(), child);
	irt_wi_join(child_id);

	uint64 endms = irt_time_ms();

	// we have a couple of ticks more now because of the irt_time_ms() call
	if(ticks_available) {
		endtsc = irt_time_ticks();
		printf("tickdiff: %16lu\n", endtsc - starttsc);
		printf("tickdiff: %16lu ns\n", irt_time_convert_ticks_to_ns(endtsc - starttsc));
	}

	printf("tickdiff: %16lu ms\n", endms - startms);
}

void insieme_wi_hw_implementation(irt_work_item* wi) {
	printf("Hello world from worker %d\n", irt_worker_get_current()->id.thread);
}
