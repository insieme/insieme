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

#define IRT_ENABLE_REGION_INSTRUMENTATION
#define IRT_USE_PAPI
#define IRT_SCHED_POLICY IRT_SCHED_POLICY_STATIC
#define IRT_RUNTIME_TUNING

#include <gtest/gtest.h>
#include "standalone.h"

// type table

irt_type g_insieme_type_table[] = {
	{ IRT_T_INT64, 8, 0, 0 },
};

// work item table

void insieme_wi_startup_implementation_simple(irt_work_item* wi);
void insieme_wi_startup_implementation_nested(irt_work_item* wi);
void insieme_wi_startup_implementation_repeated_execution(irt_work_item* wi);
void insieme_wi_startup_implementation_rapl(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants_simple[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation_simple, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_startup_variants_nested[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation_nested, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_startup_variants_repeated_execution[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation_repeated_execution, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_startup_variants_rapl[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation_rapl, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation g_insieme_impl_table[] = {
	{ 1, g_insieme_wi_startup_variants_simple },
	{ 1, g_insieme_wi_startup_variants_nested },
	{ 1, g_insieme_wi_startup_variants_repeated_execution },
	{ 1, g_insieme_wi_startup_variants_rapl }
};

// initialization
void insieme_init_context_common(irt_context* context) {
	context->type_table_size = 1;
	context->impl_table_size = 4;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
}

void insieme_init_context_simple(irt_context* context) {
	insieme_init_context_common(context);
	context->num_regions = 1;
}

void insieme_init_context_nested(irt_context* context) {
	insieme_init_context_common(context);
	context->num_regions = 2;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
}

// work item function definitions

void insieme_wi_startup_implementation_simple(irt_work_item* wi) {
	ir_inst_region_start(0);
	sleep(1);
	ir_inst_region_end(0);

	irt_inst_region_struct* reg0 = &(irt_context_get_current()->inst_region_data[0]);
	EXPECT_GT(reg0->aggregated_cpu_time, 0);
	EXPECT_EQ(reg0->num_executions, 1);
}

void insieme_wi_startup_implementation_nested(irt_work_item* wi) {

	ir_inst_region_start(0);
	ir_inst_region_start(1);
	ir_inst_region_end(1);
	ir_inst_region_end(0);

	irt_inst_region_struct* reg0 = &(irt_context_get_current()->inst_region_data[0]);
	irt_inst_region_struct* reg1 = &(irt_context_get_current()->inst_region_data[1]);
	EXPECT_GT(reg0->aggregated_cpu_time, 0);
	EXPECT_GT(reg1->aggregated_cpu_time, 0);
	EXPECT_GT(reg0->aggregated_cpu_time, reg1->aggregated_cpu_time);
}

void insieme_wi_startup_implementation_repeated_execution(irt_work_item* wi) {
	irt_inst_region_struct* reg0 = &(irt_context_get_current()->inst_region_data[0]);

	EXPECT_EQ(reg0->num_executions, 0);

	ir_inst_region_start(0);
	ir_inst_region_end(0);

	EXPECT_EQ(reg0->num_executions, 1);

	for(uint32 i = 0; i < 1e6; ++i) {
		ir_inst_region_start(0);
		ir_inst_region_end(0);
	}

	EXPECT_GT(reg0->aggregated_cpu_time, 0);
	EXPECT_EQ(reg0->num_executions, 1e6 + 1);
}

void insieme_wi_startup_implementation_rapl(irt_work_item* wi) {

	irt_affinity_policy policy = { IRT_AFFINITY_FIXED, 0 };
	uint32 workerid = 0;

	// init affinity map to 0
	for(uint32 coreid = 0; coreid < IRT_MAX_WORKERS; ++coreid)
		policy.fixed_map[coreid] = 0;

	// set affinity map to use all cores of the current socket
	for(uint32 coreid = 0; coreid < irt_get_num_sockets() * irt_get_num_cores_per_socket(); ++coreid) {
		policy.fixed_map[workerid++] = coreid;
		//printf("%d\n", policy.fixed_map[coreid]);
	}

	// set afinity
	irt_set_global_affinity_policy(policy);


	irt_inst_region_struct* reg0 = &(irt_context_get_current()->inst_region_data[0]);

	ir_inst_region_start(0);
	irt_nanosleep(1e8);
	ir_inst_region_end(0);

	EXPECT_GT(reg0->aggregated_cpu_energy, 0);
	EXPECT_LT(reg0->aggregated_cpu_energy, 100);
	EXPECT_GT(reg0->aggregated_cores_energy, 0);
	EXPECT_LT(reg0->aggregated_cores_energy, 100);
	// mc readings are not present on all CPUs, therefore they can be 0
	EXPECT_LT(reg0->aggregated_memory_controller_energy, 100);
}

TEST(region_instrumentation, simple) {
	uint32 wcount = irt_get_default_worker_count();
	irt_runtime_standalone(wcount, &insieme_init_context_simple, &insieme_cleanup_context, 0, NULL);
}

TEST(region_instrumentation, nested) {
	uint32 wcount = irt_get_default_worker_count();
	irt_runtime_standalone(wcount, &insieme_init_context_nested, &insieme_cleanup_context, 1, NULL);
}

TEST(region_instrumentation, repeated_execution) {
	uint32 wcount = irt_get_default_worker_count();
	irt_runtime_standalone(wcount, &insieme_init_context_simple, &insieme_cleanup_context, 2, NULL);
}

TEST(region_instrumentation, rapl) {
#ifndef IRT_USE_PAPI
	printf("warning: PAPI not available, not testing RAPL\n");
	return;
#endif
	if(!irt_rapl_is_supported()) {
		printf("warning: RAPL not available, not testing it\n");
		return;
	}
	uint32 wcount = irt_get_default_worker_count();
	irt_runtime_standalone(wcount, &insieme_init_context_simple, &insieme_cleanup_context, 3, NULL);
}
