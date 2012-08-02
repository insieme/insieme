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


// ============================================================================ Circular work buffers
// front = top, back = bottom
// top INCLUSIVE, bottom EXCLUSIVE
// Length needs to be a power of 2!
//
//  8 |       |
//    |-------|
//  7 |       |  <- top_update
//  6 |       |
//    |-------|
//  5 |#######|  <- top_val
//  4 |#######|
//  3 |#######|
//    |-------|
//  2 |       |  <- bot_val
//  1 |       |
//    |-------|
//  0 |       |  <- bot_update

static inline void irt_cwb_init(irt_circular_work_buffer* wb) {
	wb->state.top_val = IRT_CWBUFFER_LENGTH/2;
	wb->state.top_update = IRT_CWBUFFER_LENGTH/2;
	wb->state.bot_val = IRT_CWBUFFER_LENGTH/2;
	wb->state.bot_update = IRT_CWBUFFER_LENGTH/2;
}

static inline uint32 irt_cwb_size(irt_circular_work_buffer* wb) {
	return (wb->state.top_val - wb->state.bot_val) & IRT_CWBUFFER_MASK;
}

static inline void irt_cwb_push_front(irt_circular_work_buffer* wb, irt_work_item* wi) {
	// check feasibility
	irt_cwb_state state, newstate;
	for(;;) {
		state.all = wb->state.all;
		if(state.top_update != state.top_val) continue; // operation in progress on top
		// check for space
		newstate.all = state.all;
		newstate.top_update = (newstate.top_update+1) & IRT_CWBUFFER_MASK;
		if(newstate.top_update == state.bot_update 
			|| newstate.top_update == state.bot_val) continue; // not enough space in buffer, would be full after op
		// if we reach this point and no changes happened, we can perform our op
		if(irt_atomic_bool_compare_and_swap(&wb->state.all, state.all, newstate.all)) break; // repeat if state change since check
	} 
	//printf("%p push front %3hu,%3hu,%3hu,%3hu -> %3hu,%3hu,%3hu,%3hu \n", wb,  
	//	state.top_update, state.top_val, state.bot_val, state.bot_update, 
	//	newstate.top_update, newstate.top_val, newstate.bot_val, newstate.bot_update);

	// write actual data to buffer
	wb->items[newstate.top_update] = wi;

	// finish operation
	wb->state.top_val = newstate.top_update;
}

static inline void irt_cwb_push_back(irt_circular_work_buffer* wb, irt_work_item* wi) {
	// check feasibility
	irt_cwb_state state, newstate;
	for(;;) {
		state.all = wb->state.all;
		if(state.bot_update != state.bot_val) continue; // operation in progress on bot
		// check for space
		newstate.all = state.all;
		newstate.bot_update = (newstate.bot_update-1) & IRT_CWBUFFER_MASK;
		if(newstate.bot_update == state.top_update 
			|| newstate.bot_update == state.top_val) continue; // not enough space in buffer, would be full after op
		// if we reach this point and no changes happened, we can perform our op
		if(irt_atomic_bool_compare_and_swap(&wb->state.all, state.all, newstate.all)) break; // repeat if state change since check
	} 
	//printf("%p push back  %3hu,%3hu,%3hu,%3hu -> %3hu,%3hu,%3hu,%3hu \n", wb,  
	//	state.top_update, state.top_val, state.bot_val, state.bot_update, 
	//	newstate.top_update, newstate.top_val, newstate.bot_val, newstate.bot_update);
	
	// write actual data to buffer
	wb->items[newstate.bot_val] = wi;

	// finish operation
	wb->state.bot_val = newstate.bot_update;
}

static inline irt_work_item* irt_cwb_pop_front(irt_circular_work_buffer* wb) {
	// check feasibility
	irt_cwb_state state, newstate;
	for(;;) {
		state.all = wb->state.all;
		if(state.top_val == state.bot_val) return NULL; // empty buffer
		if(state.top_update != state.top_val) continue; // operation in progress on top
		if(state.top_val == state.bot_update) continue; // conflicting op in progress on bot
		
		// decrement update pointer if feasible
		newstate.all = state.all;
		newstate.top_update = (newstate.top_update-1) & IRT_CWBUFFER_MASK;
		
		if(irt_atomic_bool_compare_and_swap(&wb->state.all, state.all, newstate.all)) break; // state change since check
	}
	//printf("%p pop front  %3hu,%3hu,%3hu,%3hu -> %3hu,%3hu,%3hu,%3hu \n", wb,  
	//	state.top_update, state.top_val, state.bot_val, state.bot_update, 
	//	newstate.top_update, newstate.top_val, newstate.bot_val, newstate.bot_update);

	// read actual data from buffer
	irt_work_item *ret = wb->items[newstate.top_val];
	
	// finish operation
	wb->state.top_val = newstate.top_update;
	return ret;
}
	
static inline irt_work_item* irt_cwb_pop_back(irt_circular_work_buffer* wb) {
	// check feasibility
	irt_cwb_state state, newstate;
	for(;;) {
		state.all = wb->state.all;
		if(state.top_val == state.bot_val) return NULL; // empty buffer
		if(state.bot_update != state.bot_val) continue; // operation in progress on bot
		if(state.bot_val == state.top_update) continue; // conflicting op in progress on top

		// decrement update pointer if feasible
		newstate.all = state.all;
		newstate.bot_update = (newstate.bot_update+1) & IRT_CWBUFFER_MASK;
		if(irt_atomic_bool_compare_and_swap(&wb->state.all, state.all, newstate.all)) break; // state change since check
	} 
	//printf("%p pop back   %3hu,%3hu,%3hu,%3hu -> %3hu,%3hu,%3hu,%3hu \n", wb,  
	//	state.top_update, state.top_val, state.bot_val, state.bot_update, 
	//	newstate.top_update, newstate.top_val, newstate.bot_val, newstate.bot_update);

	// read actual data from buffer
	irt_work_item *ret = wb->items[newstate.bot_update];
	
	// finish operation
	wb->state.bot_val = newstate.bot_update;
	return ret;
}

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


// ============================================================================ Scheduling (general)

void irt_scheduling_init_worker(irt_worker* self) {
	irt_cwb_init(&self->sched_data.pool);
	irt_cwb_init(&self->sched_data.queue);
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
	irt_circular_work_buffer *queue = &target->sched_data.queue;
	if(irt_g_worker_count == 1 || irt_cwb_size(queue) >= IRT_CWBUFFER_LENGTH-2) {
		irt_worker_run_immediate(target, range, impl_id, args);
		return NULL;
	}
	else {
		irt_work_item *real_wi = _irt_wi_create(target, range, impl_id, args);
		irt_scheduling_assign_wi(target, real_wi);
		return real_wi;
	}
}


// ============================================================================ Scheduling (LINEAR STEALING)
#if 0

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	//if(irt_cwb_size(&target->sched_data.queue) == 0) {
	//	irt_cwb_push_front(&target->sched_data.queue, wi);
	//	irt_signal_worker(target);
	//	// signal successor
	//	int succ = (target->id.value.components.thread+1)%irt_g_worker_count;
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
	int pred = self->id.value.components.thread-1;
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
	//if(irt_cwb_size(&target->sched_data.queue) == 0) {
	//	irt_cwb_push_front(&target->sched_data.queue, wi);
	//	irt_signal_worker(target);
	//	// signal successor
	//	int succ = (target->id.value.components.thread+1)%irt_g_worker_count;
	//	irt_signal_worker(irt_g_workers[succ]);
	//} else {
		irt_cwb_push_front(&target->sched_data.queue, wi);
	//}
}

int irt_scheduling_iteration(irt_worker* self) {
	irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP, self->id);
	irt_work_item* wi = NULL;

	// try to take a WI from the pool
	if(wi = irt_cwb_pop_front(&self->sched_data.pool)) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}
	
	// if that failed, try to take a work item from the queue
	if(wi = irt_cwb_pop_front(&self->sched_data.queue)) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}

	// try to steal a work item from random
	irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_TRY, self->id);
	for(int i=0; i<irt_g_worker_count; ++i) {
		if(wi = irt_cwb_pop_back(&irt_g_workers[rand()%irt_g_worker_count]->sched_data.queue)) {
			irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_SUCCESS, self->id);
			irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
			_irt_worker_switch_to_wi(self, wi);
			return 1;
		}
	}

	// if that failed as well, look in the IPC message queue
	//if(_irt_sched_check_ipc_queue(self)) return 1;

	// didn't find any work
	return 0;
}

#endif // Scheduling (RANDOM STEALING)