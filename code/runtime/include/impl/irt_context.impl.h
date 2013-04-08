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

#pragma once

#include "irt_context.h"

#include "irt_optimizer.h"
#include "irt_logging.h"
#include "instrumentation.h"

#include "utils/lookup_tables.h"
#include "impl/worker.impl.h"

IRT_DEFINE_LOOKUP_TABLE(context, lookup_table_next, IRT_ID_HASH, IRT_CONTEXT_LT_BUCKETS);

static inline irt_context* irt_context_get_current() {
	return irt_context_table_lookup(irt_worker_get_current()->cur_context);
}


irt_context* irt_context_create_standalone(init_context_fun* init_fun, cleanup_context_fun* cleanup_fun) {
	irt_context *context = (irt_context*)malloc(sizeof(irt_context));
	context->id = irt_generate_context_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	context->id.cached = context;
	context->client_app = NULL;
	init_fun(context);
	irt_optimizer_context_startup(context);
	irt_inst_init(context);
	irt_context_table_insert(context);
	return context;
}

irt_context* irt_context_create(irt_client_app* app) {
	irt_context *context = (irt_context*)malloc(sizeof(irt_context));
	context->id = irt_generate_context_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	context->client_app = app;
	context->client_app->init_context(context);
	irt_log_comment("starting new context");
	irt_optimizer_context_startup(context);
	irt_inst_init(context);
	irt_context_table_insert(context);
	return context;
}

void irt_context_destroy(irt_context* context) {
	irt_inst_finalize(context);
	if (context->client_app && context->client_app->cleanup_context) {
		context->client_app->cleanup_context(context);
	}
	free(context);
}
