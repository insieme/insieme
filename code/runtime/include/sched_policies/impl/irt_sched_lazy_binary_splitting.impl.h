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

#include "sched_policies/utils/impl/irt_sched_queue_pool_base.impl.h"
#include "sched_policies/utils/impl/irt_sched_ipc_base.impl.h"
#include "impl/worker.impl.h"

inline bool _irt_sched_split_decision_min_size(irt_work_item* wi, const uint32 min_size) {
	return irt_wi_range_get_size(&wi->range) > min_size;
}

inline bool _irt_sched_split_decision_max_queued_min_size(irt_work_item* wi, irt_worker* self, const uint32 max_queued, const uint32 min_size) {
	return irt_wi_range_get_size(&wi->range) > min_size && self->sched_data.queue.size < max_queued;
}

inline void _irt_sched_split_work_item_binary(irt_work_item* wi, irt_worker* self) {
	irt_work_item *split_wis[2];
	irt_wi_split_binary(wi, split_wis);
	irt_work_item_cdeque_insert_front(&self->sched_data.queue, split_wis[1]);
	irt_work_item_cdeque_insert_front(&self->sched_data.queue, split_wis[0]);
}

inline irt_work_item* _irt_sched_steal_from_prev_thread(irt_worker* self) {
	int32 neighbour_index = self->id.thread-1;
	if(neighbour_index<0) neighbour_index = irt_g_worker_count-1;
	return irt_work_item_cdeque_pop_back(&irt_g_workers[neighbour_index]->sched_data.queue);
}

void irt_scheduling_init_worker(irt_worker* self) {
	irt_work_item_cdeque_init(&self->sched_data.queue);
	irt_work_item_deque_init(&self->sched_data.pool);
}

int irt_scheduling_iteration(irt_worker* self) {
	// try to take a ready WI from the pool
	irt_work_item* next_wi = irt_work_item_deque_pop_front(&self->sched_data.pool);
	if(next_wi != NULL) {
		_irt_worker_switch_to_wi(self, next_wi);
		return 1;
	}

	// if that failed, try to take a work item from the queue
	irt_work_item* new_wi = irt_work_item_cdeque_pop_front(&self->sched_data.queue);
	// if none available, try to steal from another thread
	if(new_wi == NULL) new_wi = _irt_sched_steal_from_prev_thread(self);
	if(new_wi != NULL) {
		if(_irt_sched_split_decision_max_queued_min_size(new_wi, self, 4, 50)) {
			_irt_sched_split_work_item_binary(new_wi, self);
			//irt_signal_worker(self); // TODO signal potential thieves
			return 1;
		}
		_irt_worker_switch_to_wi(self, new_wi);
		return 1;
	}

	// if that failed as well, look in the IPC message queue
	_irt_sched_check_ipc_queue(self);
	return 0;
}

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	irt_work_item_cdeque_insert_front(&target->sched_data.queue, wi);
	irt_signal_worker(target);
}

irt_work_item* irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi) {
	if(target->sched_data.queue.size >= irt_g_worker_count) {
		_irt_worker_run_optional_wi(target, wi);
		return wi;
	}
	else {
		irt_work_item *real_wi = irt_wi_create(wi->range, wi->impl_id, wi->parameters);
		irt_scheduling_assign_wi(target, real_wi);
		return real_wi;
	}
}

void irt_scheduling_yield(irt_worker* self, irt_work_item* yielding_wi) {
	IRT_DEBUG("Worker yield, worker: %p,  wi: %p", self, yielding_wi);
	irt_work_item_deque_insert_back(&self->sched_data.pool, yielding_wi);
	self->cur_wi = NULL;
	lwt_continue(&self->basestack, &yielding_wi->stack_ptr);
}
