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
#ifndef __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_UBER_IMPL_H
#define __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_UBER_IMPL_H

#include "sched_policies/utils/impl/irt_sched_ipc_base.impl.h"
#include "sched_policies/irt_sched_uber.h"
#include "impl/worker.impl.h"
#include "ir_interface.h"

#ifdef _WIN32
#include "../../include_win32/rand_r.h"
#elif defined(_GEMS_SIM)
#include "include_gems/rand_r.h"
#endif


// ============================================================================ Scheduling (general)

void irt_scheduling_init_worker(irt_worker* self) {
	irt_cwb_init(&self->sched_data.queue);
}

void irt_scheduling_generate_wi(irt_worker* target, irt_work_item* wi) {
	irt_scheduling_assign_wi(target, wi);
}

static inline void irt_scheduling_continue_wi(irt_worker* target, irt_work_item* wi) {
	irt_scheduling_assign_wi(target, wi);
}

irt_joinable irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi) {
	return irt_scheduling_optional(target, &wi->range, wi->impl, wi->parameters);
}

irt_joinable irt_scheduling_optional(irt_worker* target, const irt_work_item_range* range, irt_wi_implementation* impl, irt_lw_data_item* args) {
	irt_circular_work_buffer* queue = &target->sched_data.queue;
	if(irt_cwb_size(queue) >= IRT_CWBUFFER_LENGTH - 2) {
		irt_worker_run_immediate(target, range, impl, args);
		return irt_joinable_null();
	} else {
		irt_work_item* real_wi = _irt_wi_create(target, range, impl, args);
		irt_joinable joinable;
		joinable.wi_id = real_wi->id;
		irt_scheduling_generate_wi(target, real_wi);
		return joinable;
	}
}

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	if(irt_cwb_push_front(&target->sched_data.queue, wi)) {
		// pushing to target worked, everything is fine
		irt_signal_worker(target);
		return;
	}
	// target full, try wake target
	target = target->sched_data.wake_target;
	if(target) {
		if(irt_cwb_push_front(&target->sched_data.queue, wi)) {
			// pushed to wake target, everything is fine
			irt_signal_worker(target);
			return;
		}
	}
	// target and wake target failed, try first self and then random until it works
	irt_worker* self = irt_worker_get_current();
	target = self;
	while(!irt_cwb_push_front(&target->sched_data.queue, wi)) {
		target = irt_g_workers[rand_r(&self->rand_seed) % irt_g_worker_count];
	}
	irt_signal_worker(target);
}

bool irt_scheduling_worker_sleep(irt_worker* self) {
	uint32 id = self->id.thread;
	irt_worker* waker = irt_g_workers[(id + 1) % irt_g_worker_count];
	irt_work_item* stolen = irt_cwb_pop_back(&waker->sched_data.queue);
	if(stolen) {
		irt_cwb_push_front(&self->sched_data.queue, stolen);
		return false;
	}
	IRT_DEBUG("Worker %d sleeping. Waker: %d\n", id, waker->id.thread);
	waker->sched_data.wake_target = self;
	irt_atomic_store(&self->state, IRT_WORKER_STATE_SLEEPING);
	return true;
}

// ============================================================================ Scheduling (RANDOM STEALING)

int irt_scheduling_iteration(irt_worker* self) {
	irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP, self->id);
	irt_work_item* wi = NULL;

	// wake sleeper if enough wis
	if(self->sched_data.wake_target && irt_cwb_size(&self->sched_data.queue) > 1) {
		irt_signal_worker(self->sched_data.wake_target);
		self->sched_data.wake_target = NULL;
	}

	// try to take a work item from the queue
	if((wi = irt_cwb_pop_back(&self->sched_data.queue))) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
		_irt_worker_switch_to_wi(self, wi);
		return 1;
	}

	// try to steal a work item from random
	for(int i = 0; i < IRT_SCHED_UBER_STEAL_ATTEMPTS; ++i) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_TRY, self->id);
		irt_worker* wo = irt_g_workers[rand_r(&self->rand_seed) % irt_g_worker_count];
		if(irt_atomic_load(&wo->state) == IRT_WORKER_STATE_SLEEPING) {
			if(irt_cwb_size(&wo->sched_data.queue) > 0) { irt_signal_worker(wo); }
			continue;
		}
		if((wi = irt_cwb_pop_back(&wo->sched_data.queue))) {
			irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_SUCCESS, self->id);
			irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP_END, self->id);
			_irt_worker_switch_to_wi(self, wi);
			return 1;
		}
	}

	// didn't find any work
	return 0;
}

// ------------------------------------------------------------ optional scheduling -----------------------------------

void irt_scheduling_yield(irt_worker* self, irt_work_item* yielding_wi) {
	IRT_DEBUG("Worker yield, worker: %p,  wi: %p", (void*)self, (void*)yielding_wi);
	irt_cwb_push_front(&self->sched_data.queue, yielding_wi);
	lwt_continue(&self->basestack, &yielding_wi->stack_ptr);
}

#endif // ifndef __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_UBER_IMPL_H
