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
#include "impl/error_handling.impl.h"
#include "impl/irt_mqueue.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/work_item.impl.h"
#include "utils/minlwt.h"

typedef struct __irt_worker_func_arg {
	irt_worker *generated;
	volatile bool ready;
	irt_affinity_mask affinity;
	uint16 index;
} _irt_worker_func_arg;

void* _irt_worker_func(void *argvp) {
	_irt_worker_func_arg *arg = (_irt_worker_func_arg*)argvp;
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
	irt_work_item_deque_init(&self->queue);
	irt_work_item_deque_init(&self->pool);
	IRT_ASSERT(pthread_setspecific(irt_g_worker_key, arg->generated) == 0, IRT_ERR_INTERNAL, "Could not set worker threadprivate data");
	arg->ready = true;

	for(;;) {
		irt_worker_schedule(self);
	}

	return NULL;
}

void _irt_worker_switch_to_wi(irt_worker* self, irt_work_item *wi) {
	if(self->cur_wi == wi) return;
	// TODO refactor
	if(wi->state == IRT_WI_STATE_NEW) {
		// start WI from scratch
		wi->stack_start = (intptr_t)malloc(IRT_WI_STACK_SIZE);
		wi->stack_ptr = wi->stack_start + IRT_WI_STACK_SIZE;
		wi->state = IRT_WI_STATE_STARTED;

		if(self->cur_wi) {
			irt_work_item* old_wi = self->cur_wi;
			self->cur_wi = wi;
			lwt_start(wi, &old_wi->stack_ptr, (irt_context_table_lookup(self->cur_context)->impl_table[wi->impl_id].variants[0].implementation));
		} else {
			self->cur_wi = wi;
			lwt_start(wi, &self->basestack, (irt_context_table_lookup(self->cur_context)->impl_table[wi->impl_id].variants[0].implementation));
		}
	} else { 
		// resume WI
		if(self->cur_wi) {
			irt_work_item* old_wi = self->cur_wi;
			self->cur_wi = wi;
			lwt_continue(wi, &old_wi->stack_ptr);
		} else {
			self->cur_wi = wi;
			lwt_continue(wi, &self->basestack);
		}
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

void irt_worker_schedule(irt_worker* self) {

	IRT_INFO("Worker %p scheduling - A.", self);

	// try to take a ready WI from the pool
	// I'm not yet 100% convinced this is thread safe
	irt_work_item* next_wi = self->pool.start;
	while(next_wi != NULL) {
		if(next_wi->ready_check.fun(next_wi)) {
			if(next_wi == irt_work_item_deque_take_elem(&self->pool, next_wi)) break;
		}
		next_wi = next_wi->work_deque_next;
	}
	if(next_wi != NULL) {
		_irt_worker_switch_to_wi(self, next_wi);
		return;
	}

	IRT_INFO("Worker %p scheduling - B.", self);

	// if that failed, try to take a work item from the queue
	// TODO split
	irt_work_item* new_wi = irt_work_item_deque_pop_front(&self->queue);
	if(new_wi != NULL) {
		_irt_worker_switch_to_wi(self, new_wi);
		return;
	}

	IRT_INFO("Worker %p scheduling - C.", self);

	// if that failed as well, look in the IPC message queue
	irt_mqueue_msg* received = irt_mqueue_receive();
	if(received) {
		if(received->type == IRT_MQ_NEW_APP) {
			irt_mqueue_msg_new_app* appmsg = (irt_mqueue_msg_new_app*)received;
			irt_client_app* client_app = irt_client_app_create(appmsg->app_name);
			irt_context* prog_context = irt_context_create(client_app);
			self->cur_context = prog_context->id;
			irt_context_table_insert(prog_context);
			_irt_worker_switch_to_wi(self, irt_wi_create(irt_g_wi_range_one_elem, 0, NULL));
		}
		free(received);
	}

	IRT_INFO("Worker %p scheduling - D.", self);
}

void irt_worker_enqueue(irt_worker* self, irt_work_item* wi) {
	irt_work_item_deque_insert_front(&self->queue, wi);
}

void irt_worker_yield(irt_worker* self, irt_work_item* wi) {
	irt_work_item_deque_insert_back(&self->pool, wi);
	irt_worker_schedule(self);
}
