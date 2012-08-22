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

#ifdef _MSC_VER
	#include <Windows.h>
#endif


static inline irt_work_group* _irt_wg_new() {
	return (irt_work_group*)malloc(sizeof(irt_work_group));
}
static inline void _irt_wg_recycle(irt_work_group* wg) {
	free(wg->redistribute_data_array);
	free(wg);
}

irt_work_group* _irt_wg_create(irt_worker* self) {
	irt_work_group* wg = _irt_wg_new();
	wg->id = irt_generate_work_group_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	wg->id.cached = wg;
	wg->distributed = false;
	wg->local_member_count = 0;
	wg->ended_member_count = 0;
	wg->cur_barrier_count = 0;
	wg->pfor_count = 0;
	wg->joined_pfor_count = 0;
	wg->redistribute_data_array = NULL;
	wg->cur_sched = irt_g_loop_sched_policy_default;
	irt_spin_init(&wg->lock);
	return wg;
}

irt_work_group* irt_wg_create() {
	irt_worker* self = irt_worker_get_current();
	irt_work_group* wg = _irt_wg_create(self);
	irt_inst_insert_wg_event(self, IRT_INST_WORK_GROUP_CREATED, wg->id);
	return wg;
}
void irt_wg_destroy(irt_work_group* wg) {
	_irt_del_wg_event_register(wg->id);
	irt_spin_destroy(&wg->lock);
	_irt_wg_recycle(wg);
}

static inline void _irt_wg_end_member(irt_work_group* wg) {
	//IRT_INFO("_irt_wg_end_member: %u / %u\n", wg->ended_member_count, wg->local_member_count);
	irt_atomic_inc(&wg->ended_member_count);
	if(irt_atomic_bool_compare_and_swap(&wg->ended_member_count, wg->local_member_count, 0)) { // TODO set to 0 ok? delete group?
		irt_wg_event_trigger(wg->id, IRT_WG_EV_COMPLETED);
		irt_wg_destroy(wg);
	}
}

void irt_wg_insert(irt_work_group* wg, irt_work_item* wi) {
	// Todo distributed
	if(wi->wg_memberships == NULL) _irt_wi_allocate_wgs(wi);
	uint32 mem_num = irt_atomic_fetch_and_add(&wg->local_member_count, 1);
	uint32 group_num = irt_atomic_fetch_and_add(&wi->num_groups, 1);
	wi->wg_memberships[group_num].wg_id = wg->id;
	wi->wg_memberships[group_num].num = mem_num;
	wi->wg_memberships[group_num].pfor_count = 0;
	//IRT_INFO("G: % 8lu Mem: % 3d  wi_id: % 8lu  g_n: % 3u\n", wg->id.full, mem_num, wi->id.full, group_num);
}
void irt_wg_remove(irt_work_group* wg, irt_work_item* wi) {
	// Todo distributed
	irt_atomic_dec(&wg->local_member_count);
	// cleaning up group membership in wi is not necessary, wis may only be removed from groups when they end
}

static inline uint32 irt_wg_get_wi_num(irt_work_group* wg, irt_work_item* wi) {
	uint32 i;
	for(i=0; i<wi->num_groups; ++i) if(wi->wg_memberships[i].wg_id.full == wg->id.full) break;
	IRT_ASSERT(wi->wg_memberships[i].wg_id.full == wg->id.full, IRT_ERR_INTERNAL, "irt_wg_get_wi_num: membership not found for wi in wg");
	return wi->wg_memberships[i].num;
}
static inline irt_wi_wg_membership* irt_wg_get_wi_membership(irt_work_group* wg, irt_work_item* wi) {
	uint32 i;
	for(i=0; i<wi->num_groups; ++i) if(wi->wg_memberships[i].wg_id.full == wg->id.full) break;
	IRT_ASSERT(wi->wg_memberships[i].wg_id.full == wg->id.full, IRT_ERR_INTERNAL, "irt_wg_get_wi_membership: membership not found for wi in wg");
	return &wi->wg_memberships[i];
}

typedef struct __irt_wg_barrier_event_data {
	irt_work_item* involved_wi;
	irt_worker* join_to;
} _irt_wg_barrier_event_data;
bool _irt_wg_barrier_event_complete(irt_wg_event_register* source_event_register, void *user_data) {
	_irt_wg_barrier_event_data* data = (_irt_wg_barrier_event_data*)user_data;
	irt_scheduling_continue_wi(data->join_to, data->involved_wi);
	return false;
}
void irt_wg_barrier(irt_work_group* wg) {
	irt_worker* self = irt_worker_get_current();
 	irt_work_item* swi = self->cur_wi;

	uint32 pre_occurances = irt_wg_event_check(wg->id, IRT_WG_EV_BARRIER_COMPLETE);
	if(irt_atomic_add_and_fetch(&wg->cur_barrier_count, 1) < wg->local_member_count) {
		// enter barrier
		//IRT_INFO("BARRIER - WI %3d: [[[ UP\n", irt_wi_get_wg_num(swi, 0));
		_irt_wg_barrier_event_data barrier_ev_data = {swi, self};
		irt_wg_event_lambda barrier_lambda = {_irt_wg_barrier_event_complete, &barrier_ev_data, NULL};
		if(irt_wg_event_check_gt_and_register(wg->id, IRT_WG_EV_BARRIER_COMPLETE, &barrier_lambda, pre_occurances) == 0) {
			irt_inst_region_add_time(swi);
			irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_SUSPENDED_BARRIER, swi->id);
			// suspend until allowed to leave barrier
			self->cur_wi = NULL;
			lwt_continue(&self->basestack, &swi->stack_ptr);
			irt_inst_region_set_timestamp(swi);
			irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_RESUMED, swi->id);
		}
		//IRT_INFO("BARRIER - WI %3d: ]]] UP\n", irt_wi_get_wg_num(swi, 0));
	} else {
		// last wi to reach barrier, set count and signal event
		if(!irt_atomic_bool_compare_and_swap(&wg->cur_barrier_count, wg->local_member_count, 0)) IRT_ASSERT(false, IRT_ERR_INTERNAL, "Barrier insanity");
		//IRT_INFO("BARRIER - WI %3d: --- TRIGGER\n", irt_wi_get_wg_num(swi, 0));
		irt_wg_event_trigger(wg->id, IRT_WG_EV_BARRIER_COMPLETE);
	}
	//IRT_INFO("BARRIER - WI %3d: EXIT }}}}}}}}}}}}\n", irt_wi_get_wg_num(swi, 0));
}



void _irt_wg_allocate_redist_array(irt_work_group* wg) {
	void** arr = (void**)malloc(sizeof(void*)*wg->local_member_count);
	bool worked = irt_atomic_bool_compare_and_swap((uintptr_t*)&wg->redistribute_data_array, (uintptr_t)0, (uintptr_t)arr);
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
		irt_inst_region_add_time(swi);
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_SUSPENDED_GROUPJOIN, swi->id);
		self->cur_wi = NULL;
		lwt_continue(&self->basestack, &swi->stack_ptr);
		irt_inst_region_set_timestamp(swi);
	}
}
