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
#ifndef __GUARD_IMPL_WORK_GROUP_IMPL_H
#define __GUARD_IMPL_WORK_GROUP_IMPL_H

#include "work_group.h"

#include "impl/work_item.impl.h"
#include "abstraction/atomic.h"
#include "impl/instrumentation_events.impl.h"

static inline irt_work_group* _irt_wg_new() {
	return (irt_work_group*)malloc(sizeof(irt_work_group));
}
static inline void _irt_wg_recycle(irt_work_group* wg) {
	free(wg->redistribute_data_array);
	// TODO reuse list?
	free(wg);
}

irt_work_group* _irt_wg_create(irt_worker* self) {
	irt_work_group* wg = _irt_wg_new();
	wg->id = irt_generate_work_group_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	// IRT_ASSERT((wg->id.thread<100) && (wg->id.index<30000), IRT_ERR_INTERNAL, "ALB! t: %d, id: %d", wg->id.thread, wg->id.index); // TODO DEBUG remove!
	wg->id.cached = wg;
	// wg->distributed = false;
	wg->local_member_count = 0;
	wg->ended_member_count = 0;
	wg->cur_barrier_count = 0;
	wg->tot_barrier_count = 0;
	wg->pfor_count = 0;
	wg->joined_pfor_count = 0;
	wg->redistribute_data_array = NULL;
	wg->cur_sched = irt_g_loop_sched_policy_default;
	irt_spin_init(&wg->lock);
	// create entry in event table
	irt_wg_event_register_create(wg->id);
	irt_inst_region_wg_init(wg);
	return wg;
}

irt_work_group* irt_wg_create() {
	irt_worker* self = irt_worker_get_current();
	irt_work_group* wg = _irt_wg_create(self);
	irt_inst_insert_wg_event(self, IRT_INST_WORK_GROUP_CREATED, wg->id);
	return wg;
}
void irt_wg_end(irt_work_group* wg) {
	irt_inst_region_wg_finalize(wg);
	IRT_DEBUG_ONLY(irt_wg_event_register_id tgid; tgid.full = wg->id.full; tgid.cached = NULL;
	               irt_wg_event_register* reg = irt_wg_event_register_table_lookup(tgid);
	               IRT_ASSERT(reg->handler[IRT_WG_EV_COMPLETED] == NULL, IRT_ERR_INTERNAL, "Unfinished business");
	               IRT_ASSERT(reg->occured_flag[IRT_WG_EV_COMPLETED], IRT_ERR_INTERNAL, "Incomplete triggering"); irt_spin_unlock(&reg->lock);)
	irt_wg_event_register_destroy(wg->id);
	irt_spin_destroy(&wg->lock);
	_irt_wg_recycle(wg);
}

static inline void _irt_wg_end_member(irt_work_group* wg) {
	// IRT_INFO("_irt_wg_end_member: %u / %u\n", wg->ended_member_count, wg->local_member_count);
	/*
	 * Note: We have to store the current value of wg->local_member_count in a local member here,
	 * in order to avoid a race condition. If we do not store the value here and multiple threads
	 * increment the ended_member_count concurrently, the last one might actually already be
	 * done with cleaning up the work group until some other thread tries to read the
	 * local_member_count field of a work group which no longer exists and thus cause a segfault.
	 */
	uint32 member_count = wg->local_member_count;
	if(irt_atomic_add_and_fetch(&wg->ended_member_count, 1, uint32) == member_count) {
		irt_wg_event_trigger(wg->id, IRT_WG_EV_COMPLETED);
		irt_wg_end(wg);
	}
}

void irt_wg_insert(irt_work_group* wg, irt_work_item* wi) {
	// TODO distributed
	if(wi->wg_memberships == NULL) { _irt_wi_allocate_wgs(wi); }
	uint32 mem_num = irt_atomic_fetch_and_add(&wg->local_member_count, 1, uint32);
	uint32 group_num = irt_atomic_fetch_and_add(&wi->num_groups, 1, uint32);
	IRT_ASSERT(group_num < IRT_MAX_WORK_GROUPS, IRT_ERR_INTERNAL, "Some more changes required for a WI to be a member of multiple WGs");
	wi->wg_memberships[group_num].wg_id = wg->id;
	wi->wg_memberships[group_num].num = mem_num;
	wi->wg_memberships[group_num].pfor_count = 0;
	// IRT_INFO("G: % 8lu Mem: % 3d  wi_id: % 8lu  g_n: % 3u\n", wg->id.full, mem_num, wi->id.full, group_num);
}
void irt_wg_remove(irt_work_group* wg, irt_work_item* wi) {
	// TODO distributed
	irt_atomic_dec(&wg->local_member_count, uint32);
	// cleaning up group membership in wi is not necessary, wis may only be removed from groups when they end
}

static inline uint32 irt_wg_get_wi_num(irt_work_group* wg, irt_work_item* wi) {
	uint32 i;
	for(i = 0; i < wi->num_groups; ++i)
		if(wi->wg_memberships[i].wg_id.full == wg->id.full) { break; }
	IRT_ASSERT(wi->wg_memberships[i].wg_id.full == wg->id.full, IRT_ERR_INTERNAL, "irt_wg_get_wi_num: membership not found for wi in wg");
	return wi->wg_memberships[i].num;
}
static inline irt_wi_wg_membership* irt_wg_get_wi_membership(irt_work_group* wg, irt_work_item* wi) {
	uint32 i;
	for(i = 0; i < wi->num_groups; ++i)
		if(wi->wg_memberships[i].wg_id.full == wg->id.full) { break; }
	IRT_ASSERT(wi->wg_memberships[i].wg_id.full == wg->id.full, IRT_ERR_INTERNAL, "irt_wg_get_wi_membership: membership not found for wi in wg");
	return &wi->wg_memberships[i];
}

typedef struct __irt_wg_barrier_event_data {
	irt_work_item* involved_wi;
	irt_worker* join_to;
} _irt_wg_barrier_event_data;
bool _irt_wg_barrier_event_complete(void* user_data) {
	_irt_wg_barrier_event_data* data = (_irt_wg_barrier_event_data*)user_data;
	irt_inst_insert_wi_event(irt_worker_get_current(), IRT_INST_WORK_ITEM_RESUMED_GROUPJOIN, data->involved_wi->id);
	irt_scheduling_continue_wi(data->join_to, data->involved_wi);
	return false;
}
void irt_wg_barrier_scheduled(irt_work_group* wg) {
	irt_worker* self = irt_worker_get_current();
	irt_work_item* swi = self->cur_wi;
	// enter barrier
	IRT_ASSERT(wg->id.index != 0, IRT_ERR_INTERNAL, "WG 0 barrier");
	_irt_wg_barrier_event_data barrier_ev_data = {swi, self};
	irt_wg_event_lambda barrier_lambda = {_irt_wg_barrier_event_complete, &barrier_ev_data, NULL};
	irt_wg_event_handler_register(wg->id, IRT_WG_EV_BARRIER_COMPLETE, &barrier_lambda);
	// check if last
	if(irt_atomic_add_and_fetch(&wg->cur_barrier_count, 1, uint32) == wg->local_member_count) {
		// remove own handler from event register
		irt_wg_event_handler_remove(wg->id, IRT_WG_EV_BARRIER_COMPLETE, &barrier_lambda);
		// trigger barrier completion
		irt_inst_insert_wg_event(self, IRT_INST_WORK_GROUP_BARRIER_COMPLETE, wg->id);
		IRT_ASSERT(irt_atomic_bool_compare_and_swap(&wg->cur_barrier_count, wg->local_member_count, 0, uint32), IRT_ERR_INTERNAL, "Barrier count reset failed");
		irt_wg_event_trigger(wg->id, IRT_WG_EV_BARRIER_COMPLETE);
	} else {
		// suspend
		irt_inst_region_end_measurements(swi);
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_SUSPENDED_BARRIER, swi->id);
		// suspend until allowed to leave barrier
		_irt_worker_switch_from_wi(self, swi);
		irt_inst_region_start_measurements(swi);
		irt_inst_insert_wi_event(irt_worker_get_current(), IRT_INST_WORK_ITEM_RESUMED_BARRIER, swi->id); // self might no longer be self!
	}
}
void irt_wg_barrier_busy(irt_work_group* wg) {
	irt_worker* self = irt_worker_get_current();
	irt_work_item* swi = self->cur_wi;
	// check if last
	uint32 pre_barrier_count = wg->tot_barrier_count;
	if(irt_atomic_add_and_fetch(&wg->cur_barrier_count, 1, uint32) == wg->local_member_count) {
		IRT_ASSERT(irt_atomic_bool_compare_and_swap(&wg->cur_barrier_count, wg->local_member_count, 0, uint32), IRT_ERR_INTERNAL, "Barrier count reset failed");
		irt_inst_insert_wg_event(self, IRT_INST_WORK_GROUP_BARRIER_COMPLETE, wg->id);
		irt_atomic_inc(&wg->tot_barrier_count, uint32);
	} else {
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_SUSPENDED_BARRIER, swi->id);
		while(wg->tot_barrier_count == pre_barrier_count) {
			irt_signal_worker(irt_g_workers[rand() % irt_g_worker_count]);
			irt_busy_ticksleep(1000);
			irt_thread_yield();
		}
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_RESUMED_BARRIER, swi->id);
	}
}
void irt_wg_barrier_smart(irt_work_group* wg) {
	if(wg->local_member_count <= irt_g_worker_count) {
		irt_wg_barrier_busy(wg);
	} else {
		irt_wg_barrier_scheduled(wg);
	}
}
inline void irt_wg_barrier(irt_work_group* wg) {
	irt_wg_barrier_scheduled(wg);
	#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_atomic_add_and_fetch(&irt_g_app_progress, 1, uint64);
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
}


void _irt_wg_allocate_redist_array(irt_work_group* wg) {
	void** arr = (void**)malloc(sizeof(void*) * wg->local_member_count);
	bool worked = irt_atomic_bool_compare_and_swap((uintptr_t*)&wg->redistribute_data_array, (uintptr_t)0, (uintptr_t)arr, uintptr_t);
	if(!worked) { free(arr); }
}

void irt_wg_redistribute(irt_work_group* wg, irt_work_item* this_wi, void* my_data, void* result_data, irt_wg_redistribution_function* func) {
	if(wg->redistribute_data_array == NULL) { _irt_wg_allocate_redist_array(wg); }
	uint32 local_id = irt_wg_get_wi_num(wg, this_wi);
	wg->redistribute_data_array[local_id] = my_data;
	irt_wg_barrier(wg);
	func(wg->redistribute_data_array, local_id, wg->local_member_count, result_data);
	irt_wg_barrier(wg);
}

typedef struct __irt_wg_join_event_data {
	irt_work_item* joining_wi;
	irt_worker* join_to;
} _irt_wg_join_event_data;
bool _irt_wg_join_event(void* user_data) {
	_irt_wg_join_event_data* join_data = (_irt_wg_join_event_data*)user_data;
	irt_inst_insert_wi_event(irt_worker_get_current(), IRT_INST_WORK_ITEM_RESUMED_GROUPJOIN, join_data->joining_wi->id);
	irt_scheduling_continue_wi(join_data->join_to, join_data->joining_wi);
	return false;
}
void irt_wg_join(irt_work_group_id wg_id) {
	irt_worker* self = irt_worker_get_current();
	irt_work_item* swi = self->cur_wi;
	_irt_wg_join_event_data clo = {swi, self};
	irt_wg_event_lambda lambda = {&_irt_wg_join_event, &clo, NULL};
	bool registered = irt_wg_event_handler_check_and_register(wg_id, IRT_WG_EV_COMPLETED, &lambda);
	if(registered) { // if not completed, suspend this wi
		irt_inst_region_end_measurements(swi);
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_SUSPENDED_GROUPJOIN, swi->id);
		_irt_worker_switch_from_wi(self, swi);
		irt_inst_region_start_measurements(swi);
	}
}


#endif // ifndef __GUARD_IMPL_WORK_GROUP_IMPL_H
