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

void irt_scheduling_init_worker(irt_worker* self) {
	self->sched_data.q_last_valid = 0;
	self->sched_data.q_first_valid = 1;
	self->sched_data.q_last_overwriteable = IRT_CWBUFFER_LENGTH-1;
	self->sched_data.p_last_valid = 0;
	self->sched_data.p_first_valid = 1;
}


void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	irt_circular_work_buffer *sd = &target->sched_data;
	// wait until allowed to add wi
	while(sd->q_last_valid - sd->q_last_overwriteable >= IRT_CWBUFFER_LENGTH) pthread_yield();
	// add wi
	sd->queue[(sd->q_last_valid+1)%IRT_CWBUFFER_LENGTH] = wi;
	++sd->q_last_valid;
}


static inline void irt_scheduling_continue_wi(irt_worker* target, irt_work_item* wi) {
	target->sched_data.pool[(target->sched_data.q_last_valid+1)%IRT_CWBUFFER_LENGTH] = wi;
	++target->sched_data.q_last_valid;
}


irt_work_item* irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi) {
	return irt_scheduling_optional(target, &wi->range, wi->impl_id, wi->parameters);
}


inline irt_work_item* irt_scheduling_optional(irt_worker* target, const irt_work_item_range* range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
	irt_circular_work_buffer *sd = &target->sched_data;
	if(irt_g_worker_count == 1 
			|| sd->q_last_valid - sd->q_last_overwriteable >= IRT_CWBUFFER_LENGTH) {
		irt_work_item *self = target->cur_wi;
		irt_lw_data_item *prev_args = self->parameters;
		irt_work_item_range prev_range = self->range;
		self->parameters = args;
		self->range = *range;
		(irt_context_table_lookup(target->cur_context)->impl_table[impl_id].variants[0].implementation)(self);
		self->parameters = prev_args;
		self->range = prev_range;
		return NULL;
	}
	else {
		irt_work_item *real_wi = _irt_wi_create(target, range, impl_id, args);
		irt_scheduling_assign_wi(target, real_wi);
		return real_wi;
	}
}


int irt_scheduling_iteration(irt_worker* self) {
	irt_circular_work_buffer *sd = &target->sched_data;
	
	// try to take a WI from the pool
	if(sd->p_last_valid >= sd->p_first_valid) {
		sd->p_first_valid++;
		_irt_worker_switch_to_wi(self, sd->pool[sd->p_first_valid-1]);
		return 1;
	}
	
	// if that failed, try to take a work item from the queue
	uint64 first = sd->p_first_valid;
	irt_work_item* wi = sd->queue[first % IRT_CWBUFFER_LENGTH];
	if(first == sd->p_first_valid) // if taken wi still valid

	irt_work_item* new_wi = irt_work_item_cdeque_pop_back(&self->sched_data.queue);
	if(new_wi != NULL) {
		_irt_worker_switch_to_wi(self, new_wi);
		return 1;
	}
	// try to steal a work item from predecessor
	int pred = self->id.value.components.thread-1;
	if(pred < 0) pred = irt_g_worker_count-1;
	irt_work_item* stolen_wi = irt_work_item_cdeque_pop_front(&irt_g_workers[pred]->sched_data.queue);
	if(stolen_wi != NULL) {
		//printf("I'm a dirty thief %d.value\n", self->id.value.components.thread);
		_irt_worker_switch_to_wi(self, stolen_wi);
		return 1;
	}
	// if that failed as well, look in the IPC message queue
	if(_irt_sched_check_ipc_queue(self))
		return 1;
	return 0;
}

void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi) {
	if(target->sched_data.queue.size == 0) {
		irt_work_item_cdeque_insert_back(&target->sched_data.queue, wi);
		irt_signal_worker(target);
		// signal successor
		int succ = (target->id.value.components.thread+1)%irt_g_worker_count;
		irt_signal_worker(irt_g_workers[succ]);
	} else {
		irt_work_item_cdeque_insert_back(&target->sched_data.queue, wi);
	}
}