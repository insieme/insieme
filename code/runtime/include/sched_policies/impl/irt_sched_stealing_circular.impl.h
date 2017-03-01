/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once
#ifndef __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_STEALING_CIRCULAR_IMPL_H
#define __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_STEALING_CIRCULAR_IMPL_H

#include "sched_policies/utils/impl/irt_sched_ipc_base.impl.h"
#include "sched_policies/irt_sched_stealing_circular.h"
#include "impl/worker.impl.h"

#include "ir_interface.h"

#ifdef _WIN32
#include "../../include_win32/rand_r.h"
#elif defined(_GEMS_SIM)
#include "include_gems/rand_r.h"
#endif

// ============================================================================ Scheduling (general)

static inline bool _irt_cwb_try_push_back(irt_worker* target, irt_work_item* wi) {
	// if other full, find random worker
	bool success = false;
	uint32 tries = 8;
	while(!success && tries > 0) {
		success = irt_cwb_push_back(&target->sched_data.queue, wi);
		if(!success) {
			target = irt_g_workers[rand_r(&(irt_worker_get_current()->rand_seed)) % irt_g_worker_count];
		} else {
			irt_signal_worker(target);
		}
		tries--;
	}
	return success;
}
static inline bool _irt_cwb_try_push_front(irt_worker* target, irt_work_item* wi) {
	// if other full, find random worker
	bool success = false;
	uint32 tries = 8;
	while(!success && tries > 0) {
		success = irt_cwb_push_front(&target->sched_data.queue, wi);
		if(!success) {
			target = irt_g_workers[rand_r(&(irt_worker_get_current()->rand_seed)) % irt_g_worker_count];
		} else {
			irt_signal_worker(target);
		}
		tries--;
	}
	return success;
}

void irt_scheduling_init_worker(irt_worker* self) {
	irt_cwb_init(&self->sched_data.queue);
	self->sched_data.overflow_stack = NULL;
	irt_spin_init(&self->sched_data.overflow_stack_lock);
	#ifdef IRT_TASK_OPT
	self->sched_data.demand = IRT_CWBUFFER_LENGTH;
	#endif // IRT_TASK_OPT
}

void irt_scheduling_yield(irt_worker* self, irt_work_item* yielding_wi) {
	IRT_DEBUG("Worker yield, worker: %p,  wi: %p", (void*)self, (void*)yielding_wi);
	irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_YIELD, yielding_wi->id);
	irt_scheduling_assign_wi(self, yielding_wi);
	_irt_worker_switch_from_wi(self, yielding_wi);
}

static inline void irt_scheduling_continue_wi(irt_worker* target, irt_work_item* wi) {
	irt_scheduling_assign_wi(target, wi);
}

irt_joinable irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi) {
	return irt_scheduling_optional(target, &wi->range, wi->impl, wi->parameters);
}

irt_joinable irt_scheduling_optional(irt_worker* target, const irt_work_item_range* range, irt_wi_implementation* impl, irt_lw_data_item* args) {
#ifndef IRT_TASK_OPT
	irt_circular_work_buffer* queue = &target->sched_data.queue;
	if(irt_cwb_size(queue) >= IRT_CWBUFFER_LENGTH - 2) {
		/* Note that we intentionally do not lock the CWBs here mostly to reduce complexity
		 * Locking is actually not needed here, since the current size will only influence
		 * our scheduling decision and not affect correctness in any way.
		 */
		irt_worker_run_immediate(target, range, impl, args);
		return irt_joinable_null();
	} else {
		irt_work_item* real_wi = _irt_wi_create(target, range, impl, args);
		irt_joinable joinable;
		joinable.wi_id = real_wi->id;
		irt_scheduling_assign_wi(target, real_wi);
		return joinable;
	}
	#else  //! IRT_TASK_OPT
	int64 demand = --target->sched_data.demand;
	if(demand > IRT_CWBUFFER_LENGTH / 2) {
		irt_work_item* real_wi = _irt_wi_create(target, range, impl, args);
		irt_joinable joinable;
		joinable.wi_id = real_wi->id;
		irt_scheduling_assign_wi(target, real_wi);
		return joinable;
	} else {
		irt_worker_run_immediate(target, range, impl, args);
		return irt_joinable_null();
	}
	#endif //! IRT_TASK_OPT
}

#ifdef IRT_TASK_OPT
#define IRT_NUM_TASK_VARIANTS 4
uint32 irt_scheduling_select_taskopt_variant(irt_work_item* wi, irt_worker* wo) {
	int64 demand = wo->sched_data.demand;
	uint64 selection = IRT_NUM_TASK_VARIANTS - 1;
	if(demand > (IRT_CWBUFFER_LENGTH * (IRT_NUM_TASK_VARIANTS - 1)) / (IRT_NUM_TASK_VARIANTS * 2)) {
		selection = 0;
	} else if(demand > (IRT_CWBUFFER_LENGTH * (IRT_NUM_TASK_VARIANTS - 2)) / (IRT_NUM_TASK_VARIANTS * 2)) {
		selection = 1;
	} else if(demand > (IRT_CWBUFFER_LENGTH * (IRT_NUM_TASK_VARIANTS - 3)) / (IRT_NUM_TASK_VARIANTS * 2)) {
		selection = 2;
		//} else if(demand > (IRT_CWBUFFER_LENGTH*(IRT_NUM_TASK_VARIANTS-4))/(IRT_NUM_TASK_VARIANTS*2)) {
		//	return 3;
		//} else if(demand > (IRT_CWBUFFER_LENGTH*(IRT_NUM_TASK_VARIANTS-5))/(IRT_NUM_TASK_VARIANTS*2)) {
		//	return 4;
		//} else if(demand > (IRT_CWBUFFER_LENGTH*(IRT_NUM_TASK_VARIANTS-6))/(IRT_NUM_TASK_VARIANTS*2)) {
		//	return 5;
	}
	irt_inst_insert_db_event(wo, IRT_INST_DBG_TASK_SELECTION, *(irt_worker_id*)(&selection));
	return (uint32)selection;
}
#endif // IRT_TASK_OPT

void irt_scheduling_generate_wi(irt_worker* target, irt_work_item* wi) {
	irt_scheduling_assign_wi(target, wi);
}

// ============================================================================ Scheduling (RANDOM STEALING)

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	irt_inst_insert_wi_event(irt_worker_get_current(), IRT_INST_WORK_ITEM_QUEUED, wi->id);
	bool succeeded = false;
	#ifdef IRT_STEAL_SELF_PUSH_FRONT
	succeeded = _irt_cwb_try_push_front(target, wi);
	#else
	succeeded = _irt_cwb_try_push_back(target, wi);
	#endif
	if(!succeeded) {
		// IRT_ASSERT(false, IRT_ERR_INTERNAL, "IRT circular stealing: ran out of worker queue");
		irt_spin_lock(&target->sched_data.overflow_stack_lock);
		wi->next_reuse = target->sched_data.overflow_stack;
		target->sched_data.overflow_stack = wi;
		irt_spin_unlock(&target->sched_data.overflow_stack_lock);
	}
}

int irt_scheduling_iteration(irt_worker* self) {
	irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP, self->id);
	irt_work_item* wi = NULL;

	// if there are WIs in the overflow stack, re-integrate as many as possible
	if(self->sched_data.overflow_stack != NULL) {
		irt_spin_lock(&self->sched_data.overflow_stack_lock);
		irt_work_item* wi = self->sched_data.overflow_stack;
		while(wi != NULL) {
			irt_work_item* next = wi->next_reuse;
			if(irt_cwb_push_back(&self->sched_data.queue, wi)) {
				// got rid of this one
				self->sched_data.overflow_stack = next;
				wi = next;
			} else {
				wi = NULL;
			}
		};
		irt_spin_unlock(&self->sched_data.overflow_stack_lock);
	}

	// try to take a work item from the queue
	#ifndef IRT_STEAL_SELF_POP_BACK
	if((wi = irt_cwb_pop_front(&self->sched_data.queue))) {
	#else
	if((wi = irt_cwb_pop_back(&self->sched_data.queue))) {
	#endif
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}

	// try to steal a work item from random
	irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_TRY, self->id);
	irt_worker* wo = irt_g_workers[rand_r(&self->rand_seed) % irt_g_worker_count];
	#ifdef IRT_STEAL_OTHER_POP_FRONT
	if((wi = irt_cwb_pop_front(&wo->sched_data.queue))) {
	#else
	if((wi = irt_cwb_pop_back(&wo->sched_data.queue))) {
	#endif
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_SUCCESS, self->id);
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	} else {
	#ifdef IRT_TASK_OPT
		wo->sched_data.demand = IRT_CWBUFFER_LENGTH;
		#endif // IRT_TASK_OPT
	}

	// if that failed as well, look in the IPC message queue
	#ifndef IRT_MIN_MODE
	if(_irt_sched_check_ipc_queue(self)) { return 1; }
	#endif

	// didn't find any work
	return 0;
}

#endif // ifndef __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_STEALING_CIRCULAR_IMPL_H
