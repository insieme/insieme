/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#define IRT_ENABLE_INSTRUMENTATION
#define IRT_RUNTIME_TUNING

#include <gtest/gtest.h>
#include "standalone.h"

// type table

irt_type g_insieme_type_table[] = {
    {IRT_T_INT64, 8, 0, 0},
};

// work item table

void insieme_wi_startup_implementation_simple(irt_work_item* wi);

irt_wi_implementation_variant g_insieme_wi_startup_variants_simple[] = {{&insieme_wi_startup_implementation_simple, 0, NULL, 0, NULL, 0, NULL}};

irt_wi_implementation g_insieme_impl_table[] = {
    {1, 1, g_insieme_wi_startup_variants_simple},
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
	irt_inst_set_wi_instrumentation(true);

	irt_worker* worker = irt_worker_get_current();
	irt_instrumentation_event_data_table* table = worker->instrumentation_event_data;

	EXPECT_EQ(table->number_of_elements, 0);

	irt_work_item_id id = {(uint64)0};

	irt_inst_insert_wi_event(worker, IRT_INST_WORK_ITEM_CREATED, id);
	EXPECT_EQ(table->number_of_elements, 1);
	EXPECT_EQ(table->data[0].event_id, IRT_INST_WORK_ITEM_CREATED);
	EXPECT_NE(table->data[0].timestamp, 0);
	irt_inst_insert_wi_event(worker, IRT_INST_WORK_ITEM_STARTED, id);
	EXPECT_EQ(table->number_of_elements, 2);
	EXPECT_EQ(table->data[1].event_id, IRT_INST_WORK_ITEM_STARTED);
	EXPECT_NE(table->data[1].timestamp, 0);
	EXPECT_GE(table->data[1].timestamp, table->data[0].timestamp);

	irt_inst_set_wi_instrumentation(false);

	irt_inst_insert_wi_event(worker, IRT_INST_WORK_ITEM_FINALIZED, id);
	EXPECT_EQ(table->number_of_elements, 2);
}

TEST(event_instrumentation, simple) {
	uint32 wcount = irt_get_default_worker_count();
	irt_runtime_standalone(wcount, &insieme_init_context, &insieme_cleanup_context, &g_insieme_impl_table[0], NULL);
}
