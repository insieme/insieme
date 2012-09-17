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

#include <math.h>

#include "standalone.h"
#include "impl/client_app.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"
#include "impl/data_item.impl.h"
#include "impl/irt_scheduling.impl.h"
#include "irt_types.h"
#include "wi_implementation.h"
#include "utils/timing.h"
#include "impl/work_group.impl.h"

#define NUM_REPEATS 10
#define NUM_ITER 10000
#define NUM_LEVELS 1

typedef struct _insieme_wi_bench_params {
	irt_type_id type_id;
	uint64 count;
	uint64 *check;
} insieme_wi_bench_params;

irt_type g_insieme_type_table[] = {
	{ IRT_T_INT64, 8, 0, 0 },
	{ IRT_T_STRUCT, sizeof(insieme_wi_bench_params), 0, 0 }
};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);
void insieme_wi_bench_implementation(irt_work_item* wi);
void insieme_wi_opt_bench_implementation(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = { { IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation, NULL, 0, NULL, 0, NULL } };

irt_wi_implementation_variant g_insieme_wi_bench_variants[] = { { IRT_WI_IMPL_SHARED_MEM, &insieme_wi_bench_implementation, NULL, 0, NULL, 0, NULL } };
irt_wi_implementation_variant g_insieme_wi_opt_bench_variants[] = { { IRT_WI_IMPL_SHARED_MEM, &insieme_wi_opt_bench_implementation, NULL, 0, NULL, 0, NULL } };

irt_wi_implementation g_insieme_impl_table[] = {
	{ 1, g_insieme_wi_startup_variants },
	{ 1, g_insieme_wi_bench_variants },
	{ 1, g_insieme_wi_opt_bench_variants }
};

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
		insieme_wi_bench_params bench_params = { 1, NUM_LEVELS, &check_val };
		for(int i=0; i<NUM_REPEATS; ++i) {
			irt_work_item* bench_wi = irt_wi_create(irt_g_wi_range_one_elem, 1, (irt_lw_data_item*)&bench_params);
			irt_scheduling_assign_wi(irt_worker_get_current(), bench_wi);
			irt_wi_join(bench_wi);
		}
		uint64 total_time = irt_time_ms() - start_time;
		uint64 total_wis = (uint64)pow((double)NUM_ITER, (double)NUM_LEVELS);
		uint64 wis_per_sec = (uint64)(total_wis/((double)total_time/1000.0));
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
	insieme_wi_bench_params *params = (insieme_wi_bench_params*)wi->parameters;

	if(params->count>0) {
		insieme_wi_bench_params bench_params = { 1, params->count-1, params->check };
		irt_work_item **bench_wis = (irt_work_item**)malloc(NUM_ITER*sizeof(irt_work_item*));
		for(int i=0; i<NUM_ITER; ++i) {
			bench_wis[i] = irt_wi_create(irt_g_wi_range_one_elem, 1, (irt_lw_data_item*)&bench_params);
			irt_scheduling_assign_wi(irt_worker_get_current(), bench_wis[i]);
		}

		//irt_wi_multi_join(NUM_ITER, bench_wis);
		for(int i=0; i<NUM_ITER; ++i) {
			irt_wi_join(bench_wis[i]);
		}

		free(bench_wis);
	}
	irt_atomic_inc(params->check);
}

void insieme_wi_opt_bench_implementation(irt_work_item* wi) {
	insieme_wi_bench_params *params = (insieme_wi_bench_params*)wi->parameters;

	if(params->count>0) {
		insieme_wi_bench_params bench_params = { 1, params->count-1, params->check };
		irt_work_item **bench_wis = (irt_work_item**)malloc(NUM_ITER*sizeof(irt_work_item*));
		for(int i=0; i<NUM_ITER; ++i) {
			bench_wis[i] = irt_wi_run_optional(irt_g_wi_range_one_elem, 2, (irt_lw_data_item*)&bench_params);
		}

		//irt_wi_multi_join(NUM_ITER, bench_wis);
		for(int i=0; i<NUM_ITER; ++i) {
			irt_wi_join(bench_wis[i]);
		}

		free(bench_wis);
	}
	irt_atomic_inc(params->check);
}


int main(int argc, char **argv) {
	uint32 wcount = irt_get_default_worker_count();
	if(argc>=2) wcount = atoi(argv[1]);
	irt_runtime_standalone(wcount, &insieme_init_context, &insieme_cleanup_context, 0, NULL);
	return 0;
}



