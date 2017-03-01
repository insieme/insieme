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
#ifndef __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_STEALING_IMPL_H
#define __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_STEALING_IMPL_H

#include "sched_policies/utils/impl/irt_sched_queue_pool_base.impl.h"
#include "sched_policies/utils/impl/irt_sched_ipc_base.impl.h"
#include "impl/worker.impl.h"

void irt_scheduling_init_worker(irt_worker* self) {
	irt_work_item_cdeque_init(&self->sched_data.queue);
	irt_work_item_deque_init(&self->sched_data.pool);
}

// linear predecessor single stealing

int irt_scheduling_iteration(irt_worker* self) {
	// try to take a WI from the pool
	irt_work_item* next_wi = irt_work_item_deque_pop_back(&self->sched_data.pool);
	if(next_wi != NULL) {
		_irt_worker_switch_to_wi(self, next_wi);
		return 1;
	}
	// if that failed, try to take a work item from the queue
	irt_work_item* new_wi = irt_work_item_cdeque_pop_back(&self->sched_data.queue);
	if(new_wi != NULL) {
		_irt_worker_switch_to_wi(self, new_wi);
		return 1;
	}
	// try to steal a work item from predecessor
	int pred = self->id.thread - 1;
	if(pred < 0) { pred = irt_g_worker_count - 1; }
	irt_work_item* stolen_wi = irt_work_item_cdeque_pop_front(&irt_g_workers[pred]->sched_data.queue);
	if(stolen_wi != NULL) {
		// printf("I'm a dirty thief %d.value\n", self->id.thread);
		_irt_worker_switch_to_wi(self, stolen_wi);
		return 1;
	}
	// if that failed as well, look in the IPC message queue
	if(_irt_sched_check_ipc_queue(self)) { return 1; }
	return 0;
}

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	if(target->sched_data.queue.size == 0) {
		irt_work_item_cdeque_insert_back(&target->sched_data.queue, wi);
		irt_signal_worker(target);
		// signal successor
		int succ = (target->id.thread + 1) % irt_g_worker_count;
		irt_signal_worker(irt_g_workers[succ]);
	} else {
		irt_work_item_cdeque_insert_back(&target->sched_data.queue, wi);
	}
}

// linear predecessor multi stealing
//
// int irt_scheduling_iteration(irt_worker* self) {
//	// try to take a WI from the pool
//	irt_work_item* next_wi = irt_work_item_deque_pop_back(&self->sched_data.pool);
//	if(next_wi != NULL) {
//		_irt_worker_switch_to_wi(self, next_wi);
//		return 1;
//	}
//	// if that failed, try to take a work item from the queue
//	irt_work_item* new_wi = irt_work_item_cdeque_pop_back(&self->sched_data.queue);
//	if(new_wi != NULL) {
//		_irt_worker_switch_to_wi(self, new_wi);
//		return 1;
//	}
//	// try to steal work from predecessor
//	int pred = self->id.thread-1;
//	if(pred < 0) pred = irt_g_worker_count-1;
//	irt_work_item_cdeque *pq = &irt_g_workers[pred]->sched_data.queue;
//	if(pq->size > 4) {
//		irt_work_item_cdeque_move_front_front(pq, &self->sched_data.queue, 4);
//		return irt_scheduling_iteration(self);
//	}
//	// if that failed as well, look in the IPC message queue
//	if(_irt_sched_check_ipc_queue(self))
//		return 1;
//	return 0;
//}
//
// void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
//	if(target->sched_data.queue.size == 0) {
//		irt_work_item_cdeque_insert_back(&target->sched_data.queue, wi);
//		irt_signal_worker(target);
//		// signal successor
//		int succ = (target->id.thread+1)%irt_g_worker_count;
//		irt_signal_worker(irt_g_workers[succ]);
//	} else {
//		irt_work_item_cdeque_insert_back(&target->sched_data.queue, wi);
//	}
//}

//// distance halving
//
// int irt_scheduling_iteration(irt_worker* self) {
//	// try to take a WI from the pool
//	irt_work_item* next_wi = irt_work_item_deque_pop_back(&self->sched_data.pool);
//	if(next_wi != NULL) {
//		_irt_worker_switch_to_wi(self, next_wi);
//		return 1;
//	}
//	// if that failed, try to take a work item from the queue
//	irt_work_item* new_wi = irt_work_item_cdeque_pop_back(&self->sched_data.queue);
//	if(new_wi != NULL) {
//		_irt_worker_switch_to_wi(self, new_wi);
//		return 1;
//	}
//	// try to steal a work item from left thread
//	int tid = self->id.thread;
//	int left_index = tid/2;
//	if(left_index == tid) left_index = irt_g_worker_count-1; // worker 0 overflows to end
//	irt_work_item* stolen_wi = NULL;
//	stolen_wi = irt_work_item_cdeque_pop_front(&irt_g_workers[left_index]->sched_data.queue);
//	if(stolen_wi != NULL) {
//		_irt_worker_switch_to_wi(self, stolen_wi);
//		return 1;
//	}
//	// try to steal a work item from right thread
//	int right_index = irt_g_worker_count/2 + tid/2;
//	if(right_index == tid) right_index = 0; // last worker overflows to start
//	stolen_wi = irt_work_item_cdeque_pop_front(&irt_g_workers[left_index]->sched_data.queue);
//	if(stolen_wi != NULL) {
//		_irt_worker_switch_to_wi(self, stolen_wi);
//		return 1;
//	}
//	// if that failed as well, look in the IPC message queue
//	if(_irt_sched_check_ipc_queue(self))
//		return 1;
//	return 0;
//}
//
// void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
//	// split wis equally among workers
//	int64 size = irt_wi_range_get_size(&wi->range);
//	if(size > 1 && size >= irt_g_worker_count) {
//		irt_work_item **split_wis = (irt_work_item**)alloca(irt_g_worker_count * sizeof(irt_work_item*));
//		irt_wi_split_uniform(wi, irt_g_worker_count, split_wis);
//		for(uint32 i=0; i<irt_g_worker_count; ++i) {
//			irt_work_item_cdeque_insert_back(&irt_g_workers[i]->sched_data.queue, split_wis[i]);
//			irt_signal_worker(irt_g_workers[i]);
//		}
//	} else {
//		if(target->sched_data.queue.size == 0) {
//			irt_work_item_cdeque_insert_back(&target->sched_data.queue, wi);
//			irt_signal_worker(target);
//			// signal left/right
//			int tid = target->id.thread;
//			int left_index = tid/2;
//			if(left_index == tid) left_index = irt_g_worker_count-1; // worker 0 overflows to end
//			irt_signal_worker(irt_g_workers[left_index]);
//			int right_index = irt_g_worker_count/2 + tid/2;
//			if(right_index == tid) right_index = 0; // last worker overflows to start
//			irt_signal_worker(irt_g_workers[right_index]);
//		} else {
//			irt_work_item_cdeque_insert_back(&target->sched_data.queue, wi);
//		}
//	}
//}


// ------------------------------------------------------------ optional scheduling -----------------------------------

irt_joinable irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi) {
	if(target->sched_data.queue.size >= irt_g_worker_count) {
		irt_work_item_id wi_id = wi->id;
		_irt_worker_run_optional_wi(target, wi->id);
		return wi;
	} else {
		irt_work_item* real_wi = irt_wi_create(wi->range, wi->impl, wi->parameters);
		irt_scheduling_assign_wi(target, real_wi);
		return real_wi;
	}
}

void irt_scheduling_yield(irt_worker* self, irt_work_item* yielding_wi) {
	IRT_DEBUG("Worker yield, worker: %p,  wi: %p", (void*)self, (void*)yielding_wi);
	irt_work_item_deque_insert_back(&self->sched_data.pool, yielding_wi);
	_irt_worker_switch_from_wi(self, yielding_wi);
}


#endif // ifndef __GUARD_SCHED_POLICIES_IMPL_IRT_SCHED_STEALING_IMPL_H
