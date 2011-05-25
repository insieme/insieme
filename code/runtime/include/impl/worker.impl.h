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
	IRT_INFO("==== Pool:\n");
	irt_work_item* next_wi = self->pool.start;
	while(next_wi != NULL) {
		IRT_INFO("--- Work item %p:\n", (void*)next_wi);
		IRT_INFO("- stack ptr: %p\n", (void*)next_wi->stack_ptr);
		IRT_INFO("- start ptr: %p\n", (void*)next_wi->stack_start);
		next_wi = next_wi->work_deque_next;
	}
	IRT_INFO("==== Queue:\n");
	next_wi = self->queue.start;
	while(next_wi != NULL) {
		IRT_INFO("--- Work item %p:\n", (void*)next_wi);
		IRT_INFO("- stack ptr: %p\n", (void*)next_wi->stack_ptr);
		IRT_INFO("- start ptr: %p\n", (void*)next_wi->stack_start);
		next_wi = next_wi->work_deque_next;
	}
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
	self->start = false;
	irt_work_item_cdeque_init(&self->queue);
	irt_work_item_deque_init(&self->pool);
	IRT_ASSERT(pthread_setspecific(irt_g_worker_key, arg->generated) == 0, IRT_ERR_INTERNAL, "Could not set worker threadprivate data");
	arg->ready = true;

	while(!self->start) { } // MARK busy wait

	for(;;) {
		irt_worker_schedule(self);
	}

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

static inline irt_work_item* _irt_get_ready_wi_from_pool(irt_work_item_deque* pool) {
	irt_work_item* next_wi = pool->start;
	while(next_wi != NULL) {
		if(next_wi->ready_check.fun(next_wi)) {
			next_wi = irt_work_item_deque_take_elem(pool, next_wi); 
			if(next_wi) break;
			else return _irt_get_ready_wi_from_pool(pool); // wi was stolen, retry
		} else {
			next_wi = next_wi->work_deque_next;
		}
	}
	return next_wi;
}

static inline bool _irt_sched_split_decision_min_size(irt_work_item* wi, const uint32 min_size) {
	return irt_wi_range_get_size(&wi->range) > min_size;
}

static inline bool _irt_sched_split_decision_max_queued_min_size(irt_work_item* wi, irt_worker* self, const uint32 max_queued, const uint32 min_size) {
	return irt_wi_range_get_size(&wi->range) > min_size && self->queue.size < max_queued;
}

static inline void _irt_sched_split_work_item_binary(irt_work_item* wi, irt_worker* self) {
	irt_work_item *split_wis[2];
	irt_wi_split_binary(wi, split_wis);
	irt_work_item_cdeque_insert_front(&self->queue, split_wis[1]);
	irt_work_item_cdeque_insert_front(&self->queue, split_wis[0]);
}

static inline void _irt_sched_check_ipc_queue(irt_worker* self) {
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
}

static inline irt_work_item* _irt_sched_steal_from_prev_thread(irt_worker* self) {
	int32 neighbour_index = self->id.value.components.thread-1;
	if(neighbour_index<0) neighbour_index = irt_g_worker_count-1;
	return irt_work_item_cdeque_pop_back(&irt_g_workers[neighbour_index]->queue);
}

#if 0
void irt_worker_schedule(irt_worker* self) {

	// try to take a ready WI from the pool
	irt_work_item* next_wi = _irt_get_ready_wi_from_pool(&self->pool);
	if(next_wi != NULL) {
		_irt_worker_switch_to_wi(self, next_wi);
		return;
	}

	// if that failed, try to take a work item from the queue
	irt_work_item* new_wi = irt_work_item_cdeque_pop_front(&self->queue);
	// if none available, try to steal from another thread
	if(new_wi == NULL) new_wi = _irt_sched_steal_from_prev_thread(self);
	if(new_wi != NULL) {
		if(_irt_sched_split_decision_max_queued_min_size(new_wi, self, 4, 50000)) {
			_irt_sched_split_work_item_binary(new_wi, self);
			return;
		}
		_irt_worker_switch_to_wi(self, new_wi);
		return;
	}

	// if that failed as well, look in the IPC message queue
	_irt_sched_check_ipc_queue(self);

	pthread_yield();
}
#else
void irt_worker_schedule(irt_worker* self) {
	// try to take a ready WI from the pool
	irt_work_item* next_wi = _irt_get_ready_wi_from_pool(&self->pool);
	if(next_wi != NULL) {
		_irt_worker_switch_to_wi(self, next_wi);
		return;
	}

	// if that failed, try to take a work item from the queue
	irt_work_item* new_wi = irt_work_item_cdeque_pop_front(&self->queue);
	if(new_wi != NULL) {
		if(irt_wi_is_fragment(new_wi)) {
			_irt_worker_switch_to_wi(self, new_wi);
			return;
		} else { // split unsplit WI statically
			irt_work_item **split_wis = (irt_work_item**)alloca(irt_g_worker_count * sizeof(irt_work_item*));
			irt_wi_split_uniform(new_wi, irt_g_worker_count, split_wis);
			for(int i=0; i<irt_g_worker_count; ++i) {
				irt_work_item_cdeque_insert_back(&irt_g_workers[i]->queue, split_wis[i]);
			}
			return;
		}
	}

	// if that failed as well, look in the IPC message queue
	_irt_sched_check_ipc_queue(self);

	pthread_yield();
}
#endif

void irt_worker_enqueue(irt_worker* self, irt_work_item* wi) {
	irt_work_item_cdeque_insert_front(&self->queue, wi);
}

void irt_worker_yield(irt_worker* self, irt_work_item* wi) {
	IRT_DEBUG("Worker yield, worker: %p,  wi: %p", self, wi);
	irt_work_item_deque_insert_back(&self->pool, wi);
	self->cur_wi = NULL;
	lwt_continue(&self->basestack, &wi->stack_ptr);
}
