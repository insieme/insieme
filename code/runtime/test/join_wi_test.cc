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

#include <gtest/gtest.h>
#include "standalone.h"

// type table

irt_type g_insieme_type_table[] = {
    {IRT_T_INT64, 8, 0, 0},
};

// work item table

void insieme_wi_startup_implementation_simple(irt_work_item* wi);

void insieme_wi_startup_implementation_simple_child(irt_work_item* wi);

void insieme_wi_startup_implementation_complex(irt_work_item* wi);

void insieme_wi_startup_implementation_complex_child(irt_work_item* wi);

void insieme_wi_startup_implementation_recursive(irt_work_item* wi);

void insieme_wi_startup_implementation_recursive_child(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants_simple[] = {{&insieme_wi_startup_implementation_simple}};

irt_wi_implementation_variant g_insieme_wi_startup_variants_simple_child[] = {{&insieme_wi_startup_implementation_simple_child}};

irt_wi_implementation_variant g_insieme_wi_startup_variants_complex[] = {{&insieme_wi_startup_implementation_complex}};

irt_wi_implementation_variant g_insieme_wi_startup_variants_complex_child[] = {{&insieme_wi_startup_implementation_complex_child}};

irt_wi_implementation_variant g_insieme_wi_startup_variants_recursive[] = {{&insieme_wi_startup_implementation_recursive}};

irt_wi_implementation_variant g_insieme_wi_startup_variants_recursive_child[] = {{&insieme_wi_startup_implementation_recursive_child}};

irt_wi_implementation g_insieme_impl_table[] = {
    {1, 1, g_insieme_wi_startup_variants_simple},    {1, 1, g_insieme_wi_startup_variants_simple_child},
    {1, 1, g_insieme_wi_startup_variants_complex},   {1, 1, g_insieme_wi_startup_variants_complex_child},
    {1, 1, g_insieme_wi_startup_variants_recursive}, {1, 1, g_insieme_wi_startup_variants_recursive_child},
};

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 1;
	context->impl_table_size = 1;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
	context->num_regions = 0;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
}

// work item function definitions

void insieme_wi_startup_implementation_simple(irt_work_item* wi) {
	for(int i = 0; i < 10; i++) {
		irt_parallel_job job;
		job.max = 1;
		job.impl = &g_insieme_impl_table[1];

		irt_joinable child = irt_task(&job);

		irt_merge(child);
	}

	// not really much to check here - we just want to make sure we end up here eventually
	EXPECT_TRUE(true);
}

void insieme_wi_startup_implementation_simple_child(irt_work_item* wi) {
	usleep(1000);
}


void insieme_wi_startup_implementation_complex(irt_work_item* wi) {
	const uint32 runs = 100;
	irt_joinable child[runs];
	for(int i = 0; i < runs; i++) {
		irt_parallel_job job;
		job.max = 1;
		job.impl = &g_insieme_impl_table[3];

		child[i] = irt_task(&job);
	}

	for(int i = 0; i < runs; i++) {
		irt_merge(child[i]);
	}

	// not really much to check here - we just want to make sure we end up here eventually
	EXPECT_TRUE(true);
}

void insieme_wi_startup_implementation_complex_child(irt_work_item* wi) {
	usleep(1000);
}


typedef struct _data_struct {
	irt_type_id type_id;
	uint32 param;
	uint32* result;
	uint32 result_field;
} data_struct;

uint32 fib(const uint32 param);

void insieme_wi_startup_implementation_recursive(irt_work_item* wi) {
	uint32 param = 30;
	uint32 result = fib(param);
	EXPECT_EQ(832040, result);
}

void insieme_wi_startup_implementation_recursive_child(irt_work_item* wi) {
	*((data_struct*)wi->parameters)->result = fib(((data_struct*)wi->parameters)->param);
}

uint32 fib(const uint32 param) {
	if(param == 1 || param == 2) { return 1; }

	data_struct args1;
	args1.type_id = -((int32)sizeof(data_struct));
	args1.param = param - 1;
	args1.result = &args1.result_field;
	irt_parallel_job job1;
	job1.max = 1;
	job1.impl = &g_insieme_impl_table[5];
	job1.args = (irt_lw_data_item*)&args1;
	irt_joinable task1 = irt_task(&job1);

	data_struct args2;
	args2.type_id = -((int32)sizeof(data_struct));
	args2.param = param - 2;
	args2.result = &args2.result_field;
	irt_parallel_job job2;
	job2.max = 1;
	job2.impl = &g_insieme_impl_table[5];
	job2.args = (irt_lw_data_item*)&args2;
	irt_joinable task2 = irt_task(&job2);

	irt_merge(task1);
	irt_merge(task2);
	// irt_wi_join_all(irt_wi_get_current());

	return *args1.result + *args2.result;
}


TEST(join_wi, simple) {
	irt_runtime_standalone(irt_get_default_worker_count(), &insieme_init_context, &insieme_cleanup_context, &g_insieme_impl_table[0], NULL);
}

TEST(join_wi, complex) {
	irt_runtime_standalone(irt_get_default_worker_count(), &insieme_init_context, &insieme_cleanup_context, &g_insieme_impl_table[2], NULL);
}

TEST(join_wi, recursive) {
	irt_runtime_standalone(irt_get_default_worker_count(), &insieme_init_context, &insieme_cleanup_context, &g_insieme_impl_table[4], NULL);
}
