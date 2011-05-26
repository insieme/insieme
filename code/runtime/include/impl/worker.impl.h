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

#include "worker.h"

#include <stdlib.h>

#include "globals.h"
#include "impl/irt_mqueue.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/work_item.impl.h"
#include "utils/impl/minlwt.impl.h"
#include "utils/affinity.h"
#include "impl/error_handling.impl.h"

#ifdef IRT_VERBOSE
void _irt_worker_print_debug_info(irt_worker* self) {
	IRT_INFO("======== Worker %p debug info:\n", (void*)self);
	IRT_INFO("== Base ptr: %p\n", (void*)self->basestack);
	IRT_INFO("== Current wi: %p\n", (void*)self->cur_wi);
	//IRT_INFO("==== Pool:\n");
	//irt_work_item* next_wi = self->pool.start;
	//while(next_wi != NULL) {
	//	IRT_INFO("--- Work item %p:\n", (void*)next_wi);
	//	IRT_INFO("- stack ptr: %p\n", (void*)next_wi->stack_ptr);
	//	IRT_INFO("- start ptr: %p\n", (void*)next_wi->stack_start);
	//	next_wi = next_wi->work_deque_next;
	//}
	//IRT_INFO("==== Queue:\n");
	//next_wi = self->queue.start;
	//while(next_wi != NULL) {
	//	IRT_INFO("--- Work item %p:\n", (void*)next_wi);
	//	IRT_INFO("- stack ptr: %p\n", (void*)next_wi->stack_ptr);
	//	IRT_INFO("- start ptr: %p\n", (void*)next_wi->stack_start);
	//	next_wi = next_wi->work_deque_next;
	//}
	IRT_INFO("========\n");
}
#endif

typedef struct __irt_worker_func_arg {
	irt_worker *generated;
	volatile bool ready;
	irt_affinity_mask affinity;
	uint16 index;
} _irt_worker_func_arg;

void* _irt_worker_func(void *argvp) {
	_irt_worker_func_arg *arg = (_irt_worker_func_arg*)argvp;
	irt_set_affinity(arg->affinity);
	arg->generated = (irt_worker*)calloc(1, sizeof(irt_worker));
	irt_worker* self = arg->generated;
	self->pthread = pthread_self();
	self->id.value.components.index = 1;
	self->id.value.components.thread = arg->index;
	self->id.value.components.node = 0; // TODO correct node id
	self->id.cached = self;
	self->generator_id = self->id.value.full;
	self->affinity = arg->affinity;
	self->cur_context = irt_context_null_id();
	self->cur_wi = NULL;
	self->state = IRT_WORKER_STATE_CREATED;
	irt_scheduling_init_worker(self);
	IRT_ASSERT(pthread_setspecific(irt_g_worker_key, arg->generated) == 0, IRT_ERR_INTERNAL, "Could not set worker threadprivate data");
	arg->ready = true;

	while(!self->state == IRT_WORKER_STATE_START) { pthread_yield(); } // MARK busy wait
	self->state = IRT_WORKER_STATE_RUNNING;
	irt_scheduling_loop(self);

	return NULL;
}

void _irt_worker_switch_to_wi(irt_worker* self, irt_work_item *wi) {
	IRT_ASSERT(self->cur_wi == NULL, IRT_ERR_INTERNAL, "Worker %p _irt_worker_switch_to_wi with non-null current WI", self);
	self->cur_context = wi->context_id;
	if(wi->state == IRT_WI_STATE_NEW) {
		// start WI from scratch
		wi->state = IRT_WI_STATE_STARTED;
		lwt_prepare(wi, &self->basestack);

		self->cur_wi = wi;
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 1A, new stack ptr: %p.", self, (void*)wi->stack_ptr);
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
		lwt_start(wi, &self->basestack, (irt_context_table_lookup(self->cur_context)->impl_table[wi->impl_id].variants[0].implementation));
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 1B.", self);
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
	} else { 
		// resume WI
		self->cur_wi = wi;
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 2A, new stack ptr: %p.", self, (void*)wi->stack_ptr);
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
		lwt_continue(&wi->stack_ptr, &self->basestack);
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 2B.", self);
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
	}
}

irt_worker* irt_worker_create(uint16 index, irt_affinity_mask affinity) {
	_irt_worker_func_arg arg;
	arg.affinity = affinity;
	arg.index = index;
	arg.ready = false;

	pthread_t thread;

	IRT_ASSERT(pthread_create(&thread, NULL, &_irt_worker_func, &arg) == 0, IRT_ERR_INTERNAL, "Could not create worker thread");

	while(!arg.ready) { } // MARK busy wait

	return arg.generated;
}
