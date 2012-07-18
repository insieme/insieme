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

#define ERR(blabla) //fprintf(stderr, blabla "\n") 
#include "impl/client_app.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/irt_mqueue.impl.h"
#include "impl/data_item.impl.h"
#include "impl/work_group.impl.h"
#include "irt_types.h"
#include "wi_implementation.h"
#include "utils/timing.h"

#define NUM_WIS 100
#define WI_RANGE 100

typedef struct _insieme_wi_test_params {
	irt_type_id type_id;
	irt_work_group *wg1;
	irt_work_group *wg2;
	uint64 val1;
	uint64 val2;
} insieme_wi_test_params;

irt_type g_insieme_type_table[] = {
	{ IRT_T_INT64, 8, 0, 0 },
	{ IRT_T_STRUCT, sizeof(insieme_wi_test_params), 0, 0 }
};

// work item table

void insieme_wi_startup_implementation(irt_work_item* wi);

void insieme_wi_test_implementation(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_startup_implementation, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation_variant g_insieme_wi_test_variants[] = {
	{ IRT_WI_IMPL_SHARED_MEM, &insieme_wi_test_implementation, NULL, 0, NULL, 0, NULL }
};

irt_wi_implementation g_insieme_impl_table[] = {
	{ 1, g_insieme_wi_startup_variants },
	{ 1, g_insieme_wi_test_variants }
};

// initialization
void insieme_init_context(irt_context* context) {
	context->type_table_size = 2;
	context->impl_table_size = 2;
	context->type_table = g_insieme_type_table;
	context->impl_table = g_insieme_impl_table;
}

void insieme_cleanup_context(irt_context* context) {
	// nothing
	printf("Cleaning up manual irt test barrier add\n");
}

// work item function definitions

void insieme_wi_startup_implementation(irt_work_item* wi) {
	irt_work_item_range fullrange_wi = {0, WI_RANGE, 1};

	uint64 start_time = irt_time_ms();

	irt_work_group *wg1 = irt_wg_create(), *wg2 = irt_wg_create();

	insieme_wi_test_params test_params = { 1, wg1, wg2, 0, 0 };
	irt_work_item **test_wis = (irt_work_item **)malloc(NUM_WIS*sizeof(irt_work_item*));
	for(int i=0; i<NUM_WIS; ++i) {
		test_wis[i] = irt_wi_create(fullrange_wi, 1, (irt_lw_data_item*)&test_params);
		irt_wg_insert(wg1, test_wis[i]);
	}
	for(int i=0; i<NUM_WIS; ++i) {
		ERR("Z1");
		irt_scheduling_assign_wi(irt_g_workers[i%irt_g_worker_count], test_wis[i]);
		ERR("Z2");
	}
	ERR("Z3 ---");
	//for(int i=0; i<NUM_WIS; ++i) {
	//	ERR("Z3.5");
	//	irt_wi_join(test_wis[i]);
	//	ERR("Z4");
	//}
	irt_wg_join(wg1);
	ERR("Z5 ---");

	uint64 end_time = irt_time_ms();

	printf("======================\n= manual irt test barrier done\n");
	printf("= time taken: %lu\n", end_time - start_time);
	printf("======================\n");

	free(test_wis);
}

void insieme_wi_test_implementation(irt_work_item* wi) {
	insieme_wi_test_params *params = (insieme_wi_test_params*)wi->parameters;
	irt_atomic_add_and_fetch(&(params->val1), irt_wi_range_get_size(&wi->range));

	ERR("A1");
	irt_wg_barrier(params->wg1);
	ERR("A2");
	if(params->val1 != NUM_WIS*WI_RANGE) printf("Barrier error!\n");
	ERR("A3");
	irt_atomic_add_and_fetch(&(params->val2), irt_wi_range_get_size(&wi->range));
	ERR("A4");
	irt_wg_barrier(params->wg1);
	ERR("A5");
	if(params->val2 != NUM_WIS*WI_RANGE) printf("Barrier error!\n");
	ERR("A6");
}

