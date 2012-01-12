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

#include "work_group.h"

#include "impl/work_item.impl.h"
#include "irt_atomic.h"
#include "impl/instrumentation.impl.h"


static inline irt_work_group* _irt_wg_new() {
	return (irt_work_group*)malloc(sizeof(irt_work_group));
}
static inline void _irt_wg_recycle(irt_work_group* wg) {
	//irt_destroy_performance_table(wg->performance_data);
	free(wg->redistribute_data_array);
	free(wg);
}

irt_work_group* irt_wg_create() {
	irt_work_group* wg = _irt_wg_new();
	wg->id = irt_generate_work_group_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	wg->id.cached = wg;
	wg->distributed = false;
	wg->local_member_count = 0;
	wg->ended_member_count = 0;
	wg->cur_barrier_count_up = 0;
	wg->cur_barrier_count_down = 0;
	wg->pfor_count = 0;
	wg->joined_pfor_count = 0;
	wg->redistribute_data_array = NULL;
	wg->cur_sched = irt_g_loop_sched_policy_default;
	pthread_spin_init(&wg->lock, PTHREAD_PROCESS_PRIVATE);
	irt_wg_instrumentation_event(irt_worker_get_current(), WORK_GROUP_CREATED, wg->id);
	return wg;
}
void irt_wg_destroy(irt_work_group* wg) {
	pthread_spin_destroy(&wg->lock);
	_irt_wg_recycle(wg);
}

static inline void _irt_wg_end_member(irt_work_group* wg) {
	//IRT_INFO("_irt_wg_end_member: %u / %u\n", wg->ended_member_count, wg->local_member_count);
	irt_atomic_inc(&wg->ended_member_count);
	if(irt_atomic_bool_compare_and_swap(&wg->ended_member_count, wg->local_member_count, 0)) { // TODO set to 0 ok? delete group?
		irt_wg_event_trigger(wg->id, IRT_WG_EV_COMPLETED);
	}
}

//static inline void irt_wg_join(irt_work_group* wg) {
//	irt_wg_insert(wg, irt_wi_get_current());
//}
//static inline void irt_wg_leave(irt_work_group* wg) {
//	irt_wg_remove(wg, irt_wi_get_current());
//}

void irt_wg_insert(irt_work_group* wg, irt_work_item* wi) {
	// Todo distributed
	if(wi->wg_memberships == NULL) _irt_wi_allocate_wgs(wi);
	uint32 mem_num = irt_atomic_fetch_and_add(&wg->local_member_count, 1);
	uint32 group_num = irt_atomic_fetch_and_add(&wi->num_groups, 1);
	wi->wg_memberships[group_num].wg_id = wg->id;
	wi->wg_memberships[group_num].num = mem_num;
	wi->wg_memberships[group_num].pfor_count = 0;
	//IRT_INFO("G: % 8lu Mem: % 3d  wi_id: % 8lu  g_n: % 3u\n", wg->id.value.full, mem_num, wi->id.value.full, group_num);
}
void irt_wg_remove(irt_work_group* wg, irt_work_item* wi) {
	// Todo distributed
	irt_atomic_dec(&wg->local_member_count);
	// cleaning up group membership in wi is not necessary, wis may only be removed from groups when they end
}

static inline uint32 irt_wg_get_wi_num(irt_work_group* wg, irt_work_item* wi) {
	uint32 i;
	for(i=0; i<wi->num_groups; ++i) if(wi->wg_memberships[i].wg_id.value.full == wg->id.value.full) break;
	IRT_ASSERT(wi->wg_memberships[i].wg_id.value.full == wg->id.value.full, IRT_ERR_INTERNAL, "irt_wg_get_wi_num: membership not found for wi in wg");
	return wi->wg_memberships[i].num;
}
static inline irt_wi_wg_membership* irt_wg_get_wi_membership(irt_work_group* wg, irt_work_item* wi) {
	uint32 i;
	for(i=0; i<wi->num_groups; ++i) if(wi->wg_memberships[i].wg_id.value.full == wg->id.value.full) break;
	IRT_ASSERT(wi->wg_memberships[i].wg_id.value.full == wg->id.value.full, IRT_ERR_INTERNAL, "irt_wg_get_wi_membership: membership not found for wi in wg");
	return &wi->wg_memberships[i];
}


bool _irt_wg_barrier_check_down(irt_work_item* wi) {
	irt_work_group *wg = (irt_work_group*)wi->ready_check.data;
	return wg->cur_barrier_count_down == 0;
}

bool _irt_wg_barrier_check(irt_work_item* wi) {
	irt_work_group *wg = (irt_work_group*)wi->ready_check.data;
	return wg->cur_barrier_count_up == 0;
}

void irt_wg_joining_barrier(irt_work_group* wg) {
	// check if outstanding work group pfor joins required
	//uint32 pfor_c = wg->pfor_count, joined_pfor_c =  wg->joined_pfor_count;
	//while(joined_pfor_c < pfor_c) {
	//	IRT_DEBUG("% 4d: joined_pfor_c(%d) < pfor_c(%d)\n", irt_wi_get_wg_membership(irt_wi_get_current(), 0).num, joined_pfor_c, pfor_c);
	//	if(irt_atomic_bool_compare_and_swap(&wg->joined_pfor_count, joined_pfor_c, joined_pfor_c+1)) {
	//		IRT_DEBUG("% 4d: JOINING #%d\n", irt_wi_get_wg_membership(irt_wi_get_current(), 0).num, joined_pfor_c+1);
	//		// join the outstanding pfor work item
	//		IRT_ASSERT(pfor_c - (joined_pfor_c+1) < IRT_WG_RING_BUFFER_SIZE, IRT_ERR_OVERFLOW, "Work group ring buffer overflow (due to outstanding pfor joins)");
	//		irt_wi_join(wg->pfor_wi_list[(joined_pfor_c+1) % IRT_WG_RING_BUFFER_SIZE]);
	//	}
	//	pfor_c = wg->pfor_count;
	//	joined_pfor_c = wg->joined_pfor_count;
	//}
	//IRT_DEBUG_ONLY(uint32 mem_num = irt_wi_get_wg_membership(irt_wi_get_current(), 0).num;);
	//IRT_DEBUG("% 4u / % 3d / % 8lu: PRE barrier\n", mem_num, irt_wi_get_wg_membership(irt_wi_get_current(), 0).wg_id.value.components.index, irt_wi_get_current()->id.value.full);
	//irt_wg_barrier(wg);
	//IRT_DEBUG("% 4u / % 3d / % 8lu: POST barrier\n", mem_num, irt_wi_get_wg_membership(irt_wi_get_current(), 0).wg_id.value.components.index, irt_wi_get_current()->id.value.full);
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "Joining barrier used");
}

void irt_wg_barrier(irt_work_group* wg) {
	irt_worker* self = irt_worker_get_current();
	irt_work_item* swi = self->cur_wi;
	irt_wi_instrumentation_event(self, WORK_ITEM_SUSPENDED_BARRIER, self->cur_wi->id);
	// Todo distributed
	// check if barrier down count is 0, otherwise wait for it to be
	if(wg->cur_barrier_count_down != 0) {
		swi->ready_check.fun = &_irt_wg_barrier_check_down;
		swi->ready_check.data = wg;
		irt_scheduling_yield(self, swi);
		swi->ready_check = irt_g_null_readiness_check;
	}
	// enter barrier
	if(irt_atomic_add_and_fetch(&wg->cur_barrier_count_up, 1) < wg->local_member_count) {
		swi->ready_check.fun = &_irt_wg_barrier_check;
		swi->ready_check.data = wg;
		irt_scheduling_yield(self, swi);
		irt_atomic_dec(&wg->cur_barrier_count_down);
		swi->ready_check = irt_g_null_readiness_check;
	} else {
		// last wi to reach barrier, set down count
		wg->cur_barrier_count_down = wg->local_member_count-1;
		//wg->cur_barrier_count_up = 0;
		//if(!irt_atomic_bool_compare_and_swap(&wg->cur_barrier_count_down, 0, wg->local_member_count-1)) IRT_ASSERT(false, IRT_ERR_INTERNAL, "Barrier insanity A");
		if(!irt_atomic_bool_compare_and_swap(&wg->cur_barrier_count_up, wg->local_member_count, 0)) IRT_ASSERT(false, IRT_ERR_INTERNAL, "Barrier insanity B");
	}
}

//void irt_wg_barrier(irt_work_group* wg) {
//	irt_worker* self = irt_worker_get_current();
//	irt_work_item* swi = self->cur_wi;
//	irt_wi_instrumentation_event(self, WORK_ITEM_SUSPENDED_BARRIER, self->cur_wi->id);
//	// Todo distributed
//	// check if barrier down count is 0, otherwise wait for it to be
//	while(wg->cur_barrier_count_down != 0) { irt_scheduling_yield(self, swi);}
//	// enter barrier
//	if(irt_atomic_add_and_fetch(&wg->cur_barrier_count_up, 1) < wg->local_member_count) {
//		while(wg->cur_barrier_count_up != 0) irt_scheduling_yield(self, swi);
//		irt_atomic_dec(&wg->cur_barrier_count_down);
//	} else {
//		// last wi to reach barrier, set down count
//		wg->cur_barrier_count_down = wg->local_member_count-1;
//		wg->cur_barrier_count_up = 0;
//	}
//}

void _irt_wg_allocate_redist_array(irt_work_group* wg) {
	void** arr = (void**)malloc(sizeof(void*)*wg->local_member_count);
	bool worked = irt_atomic_bool_compare_and_swap(&wg->redistribute_data_array, 0, arr);
	if(!worked) free(arr);
}

void irt_wg_redistribute(irt_work_group* wg, irt_work_item* this_wi, void* my_data, void* result_data, irt_wg_redistribution_function* func) {
	if(wg->redistribute_data_array == NULL) _irt_wg_allocate_redist_array(wg);
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
bool _irt_wg_join_event(irt_wg_event_register* wg_event_register, void *user_data) {
	_irt_wg_join_event_data* join_data = (_irt_wg_join_event_data*)user_data;
	irt_scheduling_continue_wi(join_data->join_to, join_data->joining_wi);
	return false;
}
void irt_wg_join(irt_work_group* wg) {
	irt_worker* self = irt_worker_get_current();
	irt_work_item* swi = self->cur_wi;
	_irt_wg_join_event_data clo = {swi, self};
	irt_wg_event_lambda lambda = { &_irt_wg_join_event, &clo, NULL };
	uint32 occ = irt_wg_event_check_and_register(wg->id, IRT_WG_EV_COMPLETED, &lambda);
	if(occ==0) { // if not completed, suspend this wi
		irt_wi_instrumentation_event(self, WORK_ITEM_SUSPENDED_GROUPJOIN, swi->id);
		self->cur_wi = NULL;
		lwt_continue(&self->basestack, &swi->stack_ptr);
	}
}
