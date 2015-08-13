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

#pragma once
#ifndef __GUARD_IMPL_IRT_CONTEXT_IMPL_H
#define __GUARD_IMPL_IRT_CONTEXT_IMPL_H

#include "irt_context.h"

#include "worker.h"
#include "irt_optimizer.h"
#include "irt_logging.h"
#include "instrumentation_regions.h"
#include "instrumentation_events.h"
#include "wi_implementation.h"

#include "utils/lookup_tables.h"

IRT_DEFINE_LOCKED_LOOKUP_TABLE(context, lookup_table_next, IRT_ID_HASH, IRT_CONTEXT_LT_BUCKETS)

static inline irt_context* irt_context_get_current() {
	return irt_context_table_lookup(irt_worker_get_current()->cur_context);
}

irt_context* irt_context_create_standalone(init_context_fun* init_fun, cleanup_context_fun* cleanup_fun) {
	irt_context* context = (irt_context*)malloc(sizeof(irt_context));
	context->id = irt_generate_context_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	context->id.cached = context;
	context->client_app = NULL;
	context->init_fun = init_fun;
	context->cleanup_fun = cleanup_fun;
	irt_context_table_insert(context);
	return context;
}

void irt_context_initialize(irt_context* context) {
	if(context->init_fun) { context->init_fun(context); }
	irt_optimizer_context_startup(context);
	irt_inst_region_init(context);
}

irt_context* irt_context_create(irt_client_app* app, init_context_fun* init_fun, cleanup_context_fun* cleanup_fun) {
	irt_context* context = irt_context_create_standalone(init_fun, cleanup_fun);
	context->client_app = app;
	irt_log_comment("starting new context");
	irt_context_initialize(context);
	return context;
}

void irt_context_destroy(irt_context* context) {
	irt_inst_region_finalize(context);
	#ifdef IRT_ENABLE_INSTRUMENTATION
	if(irt_g_instrumentation_event_output_is_enabled) { irt_inst_event_data_output_all(irt_g_instrumentation_event_output_is_binary); }
	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		irt_inst_destroy_event_data_table(irt_g_workers[i]->instrumentation_event_data);
	}
	#endif

	irt_optimizer_context_destroy(context);

	if(context->cleanup_fun) { context->cleanup_fun(context); }
	irt_context_table_remove(context->id);
	free(context);
}


#endif // ifndef __GUARD_IMPL_IRT_CONTEXT_IMPL_H
