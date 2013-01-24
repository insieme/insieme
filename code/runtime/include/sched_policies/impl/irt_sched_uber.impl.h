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
#include "sched_policies/irt_sched_uber.h"
#include "impl/worker.impl.h"



// ============================================================================ Scheduling (general)

void irt_scheduling_init_worker(irt_worker* self) {
	irt_cwb_init(&self->sched_data.queue);
}

void irt_scheduling_generate_wi(irt_worker* target, irt_work_item* wi) {
	irt_circular_work_buffer *queue = &target->sched_data.queue;
	if(irt_cwb_size(queue) >= 1 && target->sched_data.wake_target) {
		irt_worker *t = target->sched_data.wake_target;
		target->sched_data.wake_target = NULL;
		irt_cwb_push_front(&t->sched_data.queue, wi);
		irt_signal_worker(t);
	} else {
		irt_cwb_push_front(queue, wi);
		if(target->state == IRT_WORKER_STATE_SLEEPING) irt_signal_worker(target);
	}
}

static inline void irt_scheduling_continue_wi(irt_worker* target, irt_work_item* wi) {
	irt_scheduling_assign_wi(target, wi);
}

irt_work_item* irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi) {
	return irt_scheduling_optional(target, &wi->range, wi->impl_id, wi->parameters);
}

irt_work_item* irt_scheduling_optional(irt_worker* target, const irt_work_item_range* range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
	irt_circular_work_buffer *queue = &target->sched_data.queue;
	if(irt_cwb_size(queue) >= IRT_CWBUFFER_LENGTH-2) {
		irt_worker_run_immediate(target, range, impl_id, args);
		return NULL;
	}
	else {
		irt_work_item *real_wi = _irt_wi_create(target, range, impl_id, args);
		irt_scheduling_generate_wi(target, real_wi);
		return real_wi;
	}
}

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	irt_circular_work_buffer *queue = &target->sched_data.queue;
	irt_signal_worker(target);
	if(irt_cwb_size(queue) >= IRT_CWBUFFER_LENGTH/2 && target->sched_data.wake_target) {
		irt_worker *t = target->sched_data.wake_target;
		target->sched_data.wake_target = NULL;
		irt_cwb_push_front(&t->sched_data.queue, wi);
		irt_signal_worker(t);
	} else {
		irt_cwb_push_front(queue, wi);
		irt_signal_worker(target);
	}
}

bool irt_scheduling_worker_sleep(irt_worker *self) {
	uint32 id = self->id.thread;
	irt_worker* waker = irt_g_workers[(id+1)%irt_g_worker_count];
	irt_work_item* stolen = irt_cwb_pop_back(&waker->sched_data.queue);
	if(stolen) {
		irt_cwb_push_front(&self->sched_data.queue, stolen);
		return false;
	}
	IRT_DEBUG("Worker %d sleeping. Waker: %d\n", id, waker->id.thread);
	waker->sched_data.wake_target = self;
	self->state = IRT_WORKER_STATE_SLEEPING;
	return true;
}

// ============================================================================ Scheduling (RANDOM STEALING)

int irt_scheduling_iteration(irt_worker* self) {
	irt_inst_insert_wo_event(self, IRT_INST_WORKER_SCHEDULING_LOOP, self->id);
	irt_work_item* wi = NULL;
	
	// wake sleeper if enough wis
	if(self->sched_data.wake_target && irt_cwb_size(&self->sched_data.queue)>1) {
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
	for(int i=0; i<IRT_SCHED_UBER_STEAL_ATTEMPTS; ++i) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_STEAL_TRY, self->id);
		irt_worker *wo = irt_g_workers[rand_r(&self->rand_seed)%irt_g_worker_count];
		if(wo->state == IRT_WORKER_STATE_SLEEPING) {
			if(irt_cwb_size(&wo->sched_data.queue) > 0) irt_signal_worker(wo);
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
