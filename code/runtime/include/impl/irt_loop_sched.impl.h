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

#include "irt_loop_sched.h"
#include "work_group.h"
#include "utils/timing.h"
#include "impl/irt_optimizer.impl.h"

inline static void _irt_loop_startup(irt_work_group* group, irt_wi_implementation_id impl_id, irt_work_item_range range, irt_loop_sched_data* sched_data) {
#ifdef IRT_RUNTIME_TUNING
	sched_data->participants_complete = 0;
	sched_data->start_time = irt_time_ticks();
	sched_data->policy.participants = MIN(sched_data->policy.participants, group->local_member_count);

#ifdef IRT_RUNTIME_TUNING_EXTENDED
	sched_data->part_times = calloc(sched_data->policy.participants, sizeof(uint64));
#endif
	
	irt_optimizer_starting_pfor(impl_id, range, group);
#endif
}

inline static void _irt_loop_fragment_run(irt_work_item* self, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
	irt_worker* w = irt_worker_get_current();
	w->lazy_count++;
	irt_lw_data_item *prev_args = self->parameters;
	irt_work_item_range prev_range = self->range;
	self->parameters = args;
	self->range = range;
	(irt_context_table_lookup(w->cur_context)->impl_table[impl_id].variants[0].implementation)(self);
	self->parameters = prev_args;
	self->range = prev_range;
}

inline static void irt_schedule_loop_static(irt_work_item* self, irt_work_group* group, 
		irt_work_item_range base_range, irt_wi_implementation_id impl_id, irt_lw_data_item* args, irt_loop_sched_policy policy) {
	irt_wi_wg_membership* mem = irt_wg_get_wi_membership(group, self);
#ifdef IRT_RUNTIME_TUNING
	// do group data management if first to enter pfor
	pthread_spin_lock(&group->lock);
	if(group->pfor_count < mem->pfor_count) {
		group->pfor_count = mem->pfor_count;
		irt_loop_sched_data* sched_data = &group->loop_sched_data[group->pfor_count % IRT_WG_RING_BUFFER_SIZE];
		sched_data->policy = policy;
		_irt_loop_startup(group, impl_id, base_range, sched_data);
	}
	pthread_spin_unlock(&group->lock);
#ifdef IRT_RUNTIME_TUNING_EXTENDED
	irt_loop_sched_data* sched_data = &group->loop_sched_data[mem->pfor_count % IRT_WG_RING_BUFFER_SIZE];
	sched_data->part_times[mem->num] = irt_time_ticks();
#endif
#endif
	
	uint32 id = mem->num;
	uint32 participants = MIN(policy.participants, group->local_member_count);
	uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
	uint64 chunk = numit / participants;
	uint64 rem = numit % participants;
	base_range.begin = base_range.begin + id * chunk * base_range.step;
	// adjust chunk and begin to take care of remainder
	if(id < rem) chunk += 1;
	base_range.begin += MIN(rem, id);
	base_range.end = base_range.begin + chunk * base_range.step;

	_irt_loop_fragment_run(self, base_range, impl_id, args);
}

inline static void irt_schedule_loop_static_chunked(irt_work_item* self, irt_work_group* group, 
		irt_work_item_range base_range, irt_wi_implementation_id impl_id, irt_lw_data_item* args, irt_loop_sched_policy policy) {
	irt_wi_wg_membership* mem = irt_wg_get_wi_membership(group, self);
#ifdef IRT_RUNTIME_TUNING
	// do group data management if first to enter pfor
	pthread_spin_lock(&group->lock);
	if(group->pfor_count < mem->pfor_count) {
		group->pfor_count = mem->pfor_count;
		irt_loop_sched_data* sched_data = &group->loop_sched_data[group->pfor_count % IRT_WG_RING_BUFFER_SIZE];
		sched_data->policy = policy;
		_irt_loop_startup(group, impl_id, base_range, sched_data);
	}
	pthread_spin_unlock(&group->lock);
#ifdef IRT_RUNTIME_TUNING_EXTENDED
	irt_loop_sched_data* sched_data = &group->loop_sched_data[mem->pfor_count % IRT_WG_RING_BUFFER_SIZE];
	sched_data->part_times[mem->num] = irt_time_ticks();
#endif
#endif

	uint32 participants = MIN(policy.participants, group->local_member_count);
	uint64 memstep = policy.param * base_range.step;
	uint64 fullstep = participants * memstep;
	uint32 id = mem->num;

	irt_work_item_range range;
	range.step = base_range.step;
	for(uint64 start = id*memstep + base_range.begin; start < base_range.end; start += fullstep) {
		range.begin = start;
		range.end = MIN(start + memstep, base_range.end);
		_irt_loop_fragment_run(self, range, impl_id, args);
	}
}

inline static void irt_schedule_loop_dynamic(irt_work_item* self, irt_work_group* group, 
		irt_work_item_range base_range, irt_wi_implementation_id impl_id, irt_lw_data_item* args, irt_loop_sched_policy policy) {
	uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
	uint32 participants = MIN(policy.participants, group->local_member_count);
	uint64 chunk = (numit / participants) / 8;
	policy.param = MAX(chunk,1);
	irt_schedule_loop_dynamic_chunked(self, group, base_range, impl_id, args, policy);
}

inline static void irt_schedule_loop_dynamic_chunked(irt_work_item* self, irt_work_group* group, 
		irt_work_item_range base_range, irt_wi_implementation_id impl_id, irt_lw_data_item* args, irt_loop_sched_policy policy) {
	irt_wi_wg_membership* mem = irt_wg_get_wi_membership(group, self);
	
	// do group data management if first to enter pfor
	pthread_spin_lock(&group->lock);
	if(group->pfor_count < mem->pfor_count) {
		// This wi was the first to reach this pfor
		group->pfor_count = mem->pfor_count;
		irt_loop_sched_data* sched_data = &group->loop_sched_data[group->pfor_count % IRT_WG_RING_BUFFER_SIZE];
		sched_data->policy = policy;
		sched_data->completed = base_range.begin;
		_irt_loop_startup(group, impl_id, base_range, sched_data);
	}
	pthread_spin_unlock(&group->lock);

	// TODO check for ring buffer overflow
	irt_loop_sched_data* sched_data = &group->loop_sched_data[mem->pfor_count % IRT_WG_RING_BUFFER_SIZE];
	if(mem->num >= sched_data->policy.participants) return;

#ifdef IRT_RUNTIME_TUNING_EXTENDED
	sched_data->part_times[mem->num] = irt_time_ticks();
#endif

	// calculate params
	uint64 step = policy.param * base_range.step;
	uint64 final = base_range.end;
	
	uint64 comp = sched_data->completed;
	while(comp < final) {
		if(irt_atomic_bool_compare_and_swap(&sched_data->completed, comp, comp+step)) {
			base_range.begin = sched_data->completed-step;
			base_range.end = MIN(sched_data->completed, final);
			_irt_loop_fragment_run(self, base_range, impl_id, args);
		}
		comp = sched_data->completed;
	}
}

inline static void irt_schedule_loop_guided(irt_work_item* self, irt_work_group* group, 
		irt_work_item_range base_range, irt_wi_implementation_id impl_id, irt_lw_data_item* args, irt_loop_sched_policy policy) {
	uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
	uint32 participants = MIN(policy.participants, group->local_member_count);
	uint64 chunk = (numit / participants) / 16;
	policy.param = MAX(chunk,1);
	irt_schedule_loop_guided_chunked(self, group, base_range, impl_id, args, policy);
}

inline static void irt_schedule_loop_guided_chunked(irt_work_item* self, irt_work_group* group, 
		irt_work_item_range base_range, irt_wi_implementation_id impl_id, irt_lw_data_item* args, irt_loop_sched_policy policy) {
	irt_wi_wg_membership* mem = irt_wg_get_wi_membership(group, self);
	
	// do group data management if first to enter pfor
	pthread_spin_lock(&group->lock);
	if(group->pfor_count < mem->pfor_count) {
		// This wi was the first to reach this pfor
		group->pfor_count = mem->pfor_count;
		irt_loop_sched_data* sched_data = &group->loop_sched_data[group->pfor_count % IRT_WG_RING_BUFFER_SIZE];
		sched_data->policy = policy;
		sched_data->completed = base_range.begin;
		uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
		uint64 chunk = (numit / MIN(policy.participants, group->local_member_count) * 2);
		sched_data->block_size = MAX(policy.param, chunk);
		_irt_loop_startup(group, impl_id, base_range, sched_data);
	}
	pthread_spin_unlock(&group->lock);
	
	// TODO check for ring buffer overflow
	irt_loop_sched_data* sched_data = &group->loop_sched_data[mem->pfor_count % IRT_WG_RING_BUFFER_SIZE];
	if(mem->num >= sched_data->policy.participants) return;

#ifdef IRT_RUNTIME_TUNING_EXTENDED
	sched_data->part_times[mem->num] = irt_time_ticks();
#endif

	// calculate params
	uint64 final = base_range.end;
	
	uint64 comp = sched_data->completed;
	while(comp < final) {
		uint64 bsize = sched_data->block_size;
		if(irt_atomic_bool_compare_and_swap(&sched_data->completed, comp, comp+bsize)) {
			uint64 new_bsize = MAX(sched_data->policy.param, bsize*0.8); // TODO factor tweakable?
			//irt_atomic_bool_compare_and_swap(&sched_data->block_size, bsize, new_bsize);
			sched_data->block_size = MIN(new_bsize, sched_data->block_size);
			base_range.begin = comp;
			base_range.end = MIN(comp+bsize, final);
			_irt_loop_fragment_run(self, base_range, impl_id, args);
		}
		comp = sched_data->completed;
	}
}

inline static void irt_schedule_loop(
		irt_work_item* self, irt_work_group* group, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args, const irt_loop_sched_policy* policy) {
	switch(policy->type) {
	case IRT_STATIC: irt_schedule_loop_static(self, group, base_range, impl_id, args, *policy); break;
	case IRT_STATIC_CHUNKED: irt_schedule_loop_static_chunked(self, group, base_range, impl_id, args, *policy); break;
	case IRT_DYNAMIC: irt_schedule_loop_dynamic(self, group, base_range, impl_id, args, *policy); break;
	case IRT_DYNAMIC_CHUNKED: irt_schedule_loop_dynamic_chunked(self, group, base_range, impl_id, args, *policy); break;
	case IRT_GUIDED: irt_schedule_loop_guided(self, group, base_range, impl_id, args, *policy); break;
	case IRT_GUIDED_CHUNKED: irt_schedule_loop_guided_chunked(self, group, base_range, impl_id, args, *policy); break;
	default: IRT_ASSERT(false, IRT_ERR_INTERNAL, "Unknown scheduling policy");
	}
}

void irt_wg_set_loop_scheduling_policy(irt_work_group* group, irt_loop_sched_policy policy) {
	group->cur_sched = policy;
	// TODO safety
}
