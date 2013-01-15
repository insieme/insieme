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

#include "sched_policies/utils/impl/irt_sched_ipc_base.impl.h"
#include "sched_policies/irt_sched_stealing_circular.h"
#include "impl/worker.impl.h"

// ============================================================================ Scheduling (general)

void irt_scheduling_init_worker(irt_worker* self) {
	irt_cwb_init(&self->sched_data.pool);
	irt_cwb_init(&self->sched_data.queue);
#ifdef IRT_TASK_OPT
	self->sched_data.demand = IRT_CWBUFFER_LENGTH;
#endif //IRT_TASK_OPT
}

void irt_scheduling_yield(irt_worker* self, irt_work_item* yielding_wi) {
	IRT_DEBUG("Worker yield, worker: %p,  wi: %p", self, yielding_wi);
	irt_cwb_push_back(&self->sched_data.pool, yielding_wi);
	self->cur_wi = NULL;
	lwt_continue(&self->basestack, &yielding_wi->stack_ptr);
}

static inline void irt_scheduling_continue_wi(irt_worker* target, irt_work_item* wi) {
	irt_cwb_push_front(&target->sched_data.pool, wi);
	irt_signal_worker(target);
}

irt_work_item* irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi) {
	return irt_scheduling_optional(target, &wi->range, wi->impl_id, wi->parameters);
}

irt_work_item* irt_scheduling_optional(irt_worker* target, const irt_work_item_range* range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
#ifndef IRT_TASK_OPT
	irt_circular_work_buffer *queue = &target->sched_data.queue;
	if(irt_cwb_size(queue) >= IRT_CWBUFFER_LENGTH-2) {
		irt_worker_run_immediate(target, range, impl_id, args);
		return NULL;
	}
	else {
		irt_work_item *real_wi = _irt_wi_create(target, range, impl_id, args);
		irt_scheduling_assign_wi(target, real_wi);
		return real_wi;
	}
#else //!IRT_TASK_OPT
	int64 demand = --target->sched_data.demand;
	if(demand > IRT_CWBUFFER_LENGTH/2) {
		irt_work_item *real_wi = _irt_wi_create(target, range, impl_id, args);
		irt_scheduling_assign_wi(target, real_wi);
		return real_wi;
	} else {
		irt_worker_run_immediate(target, range, impl_id, args);
		return NULL;
	}
#endif //!IRT_TASK_OPT
}

#ifdef IRT_TASK_OPT
uint32 irt_scheduling_select_taskopt_variant(irt_work_item* wi, irt_worker* wo) {
	int64 demand = wo->sched_data.demand;
	if(demand > IRT_CWBUFFER_LENGTH/2) {
		return 0;
	} else if(demand > IRT_CWBUFFER_LENGTH/4) {
		return 1;
	} else if(demand > 0) {
		return 2;
	}
	return 3;
}
#endif //IRT_TASK_OPT


// ============================================================================ Scheduling (LINEAR STEALING)
#if 0

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	//if(irt_cwb_size(&target->sched_data.queue) == 0) {
	//	irt_cwb_push_front(&target->sched_data.queue, wi);
	//	irt_signal_worker(target);
	//	// signal successor
	//	int succ = (target->id.thread+1)%irt_g_worker_count;
	//	irt_signal_worker(irt_g_workers[succ]);
	//} else {
		irt_cwb_push_front(&target->sched_data.queue, wi);
	//}
}

int irt_scheduling_iteration(irt_worker* self) {
	irt_work_item* wi = NULL;

	// try to take a WI from the pool
	if(wi = irt_cwb_pop_front(&self->sched_data.pool)) {
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}
	
	// if that failed, try to take a work item from the queue
	if(wi = irt_cwb_pop_front(&self->sched_data.queue)) {
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}

	// try to steal a work item from predecessor
	irt_worker_instrumentation_event(self, WORKER_STEAL_TRY, self->id);
	int pred = self->id.thread-1;
	if(pred < 0) pred = irt_g_worker_count-1;
	if(wi = irt_cwb_pop_back(&irt_g_workers[pred]->sched_data.queue)) {
		irt_worker_instrumentation_event(self, WORKER_STEAL_SUCCESS, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}

	// if that failed as well, look in the IPC message queue
	if(_irt_sched_check_ipc_queue(self)) return 1;

	// didn't find any work
	return 0;
}

#endif // Scheduling (LINEAR STEALING)

// ============================================================================ Scheduling (RANDOM STEALING)
#if 1

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	irt_cwb_push_front(&target->sched_data.queue, wi);
}

int irt_scheduling_iteration(irt_worker* self) {
	irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP, self->id);
	irt_work_item* wi = NULL;
	
	// try to take a WI from the pool
	if((wi = irt_cwb_pop_front(&self->sched_data.pool))) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}
	
	// if that failed, try to take a work item from the queue
	//if((wi = irt_cwb_pop_front(&self->sched_data.queue))) {
	if((wi = irt_cwb_pop_back(&self->sched_data.queue))) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}

	// try to steal a work item from random
	irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_TRY, self->id);
	irt_worker *wo = irt_g_workers[rand_r(&self->rand_seed)%irt_g_worker_count];
	if((wi = irt_cwb_pop_back(&wo->sched_data.queue))) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_SUCCESS, self->id);
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	} else {
#ifdef IRT_TASK_OPT
		wo->sched_data.demand = IRT_CWBUFFER_LENGTH;
#endif //IRT_TASK_OPT
	}

	// if that failed as well, look in the IPC message queue
	//if(_irt_sched_check_ipc_queue(self)) return 1;

	// didn't find any work
	return 0;
}

#endif // Scheduling (RANDOM STEALING)



#if 0
///////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// 64 bit triplet implementation ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////

static inline void irt_cwb_init(irt_circular_work_buffer* wb) {
	wb->front_free = 5;
	wb->back_free = 4;
	wb->size = 0;
}

static inline void irt_cwb_push_front(irt_circular_work_buffer* wb, irt_work_item* wi) {
	for(;;) {
		// try to increase size, see if still fits
		if(irt_atomic_add_and_fetch(&wb->size, 1) >= IRT_CWBUFFER_LENGTH) {
			irt_atomic_dec(&wb->size);
			//pthread_yield();
			continue;
		}
		// select slot, try to push item
		uint64 pos = wb->front_free;
		if(irt_atomic_bool_compare_and_swap((intptr_t*)&wb->items[pos%IRT_CWBUFFER_LENGTH], (intptr_t)NULL, (intptr_t)wi)) {
			irt_atomic_inc(&wb->front_free);
			return;
		}
		// failed to put item, undo size change
		irt_atomic_dec(&wb->size);
		//pthread_yield();
	}
}

static inline void irt_cwb_push_back(irt_circular_work_buffer* wb, irt_work_item* wi) {
	for(;;) {
		// try to increase size, see if still fits
		if(irt_atomic_add_and_fetch(&wb->size, 1) >= IRT_CWBUFFER_LENGTH) {
			irt_atomic_dec(&wb->size);
			//pthread_yield();
			continue;
		}
		// select slot, try to push item
		uint64 pos = wb->back_free;
		if(irt_atomic_bool_compare_and_swap((intptr_t*)&wb->items[pos%IRT_CWBUFFER_LENGTH], (intptr_t)NULL, (intptr_t)wi)) {
			irt_atomic_dec(&wb->back_free);
			return;
		}
		// failed to put item, undo size change
		irt_atomic_dec(&wb->size);
		//pthread_yield();
	}
}

static inline irt_work_item* irt_cwb_pop_front(irt_circular_work_buffer* wb) {
	// try to decrease size, see if there is something left
	int64 post_size = irt_atomic_sub_and_fetch(&wb->size, 1);
	if(post_size < 0) {
		irt_atomic_inc(&wb->size);
		return NULL;
	}
	// pop item from front
	uint64 pop_index = irt_atomic_sub_and_fetch(&wb->front_free, 1);
	irt_work_item* wi = wb->items[pop_index%IRT_CWBUFFER_LENGTH];
	if(!irt_atomic_bool_compare_and_swap((intptr_t*)&wb->items[pop_index%IRT_CWBUFFER_LENGTH], (intptr_t)wi, (intptr_t)NULL)) {
		irt_throw_string_error(IRT_ERR_INTERNAL, "Fucking shithog A");
	}
	printf("Popped WI %p from front position %lu, post size %ld\n", wi, pop_index, post_size);
	return wi;	
}

static inline irt_work_item* irt_cwb_pop_back(irt_circular_work_buffer* wb) {
	// try to decrease size, see if there is something left
	int64 post_size = irt_atomic_sub_and_fetch(&wb->size, 1);
	if(post_size < 0) {
		irt_atomic_inc(&wb->size);
		return NULL;
	}
	// pop item from back
	uint64 pop_index = irt_atomic_add_and_fetch(&wb->back_free, 1);
	irt_work_item* wi = wb->items[pop_index%IRT_CWBUFFER_LENGTH];
	if(!irt_atomic_bool_compare_and_swap((intptr_t*)&wb->items[pop_index%IRT_CWBUFFER_LENGTH], (intptr_t)wi, (intptr_t)NULL)) {
		irt_throw_string_error(IRT_ERR_INTERNAL, "Fucking shithog B");
	}
	printf("Popped WI %p from back position %lu, post size %ld\n", wi, pop_index, post_size);
	return wi;	
}

#endif // 64 bit triplet implementation
