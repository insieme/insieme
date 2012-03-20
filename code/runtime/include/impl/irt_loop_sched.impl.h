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

// initializes scheduling data instrumentation for tuning
inline static void _irt_loop_tuning_startup(volatile irt_loop_sched_data* sched_data) {
#ifdef IRT_RUNTIME_TUNING
	sched_data->participants_complete = 0;
	sched_data->start_time = irt_time_ticks();

#ifdef IRT_RUNTIME_TUNING_EXTENDED
	IRT_ASSERT(sched_data->part_times == NULL, IRT_ERR_OVERFLOW, "Suspected loop schedule ring buffer overflow");
	sched_data->part_times = (uint64*)malloc(sched_data->policy.participants * sizeof(uint64));
#endif // IRT_RUNTIME_TUNING_EXTENDED
#endif // IRT_RUNTIME_TUNING
}

// runs a fragment of a loop by scheduling the associated WI
// used by the individual scheduling policies
inline static void _irt_loop_fragment_run(irt_work_item* self, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
	if(range.begin == range.end) return;
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

// implements static loop scheduling:
// each participant gets one fragment and all fragments are as close to equal in size as possible
inline static void irt_schedule_loop_static(irt_work_item* self, uint32 id, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args, const irt_loop_sched_policy *policy) {
	
	// calculate chunk size
	uint32 participants = policy->participants;
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

// implements static loop scheduling with a fixed chunk size:
// each participant gets a number of fragments of the given size, predetermined by its group id
inline static void irt_schedule_loop_static_chunked(irt_work_item* self, uint32 id, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args, const irt_loop_sched_policy *policy) {

	uint64 memstep = policy->param.chunk_size * base_range.step;
	uint64 fullstep = policy->participants * memstep;

	irt_work_item_range range;
	range.step = base_range.step;
	for(uint64 start = id*memstep + base_range.begin; start < base_range.end; start += fullstep) {
		range.begin = start;
		range.end = MIN(start + memstep, base_range.end);
		_irt_loop_fragment_run(self, range, impl_id, args);
	}
}

// implements dynamic loop scheduling with a fixed chunk size
// chunks are distributed on a first-come first-served basis
inline static void irt_schedule_loop_dynamic_chunked(irt_work_item* self, uint32 id, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args, volatile irt_loop_sched_data *sched_data) {

	uint64 step = sched_data->policy.param.chunk_size * base_range.step;
	uint64 final = base_range.end;
	
	uint64 comp = sched_data->completed;
	while(comp < final) {
		if(irt_atomic_bool_compare_and_swap(&sched_data->completed, comp, comp+step)) {
			base_range.begin = comp;
			base_range.end = MIN(comp+step, final);
			_irt_loop_fragment_run(self, base_range, impl_id, args);
		}
		comp = sched_data->completed;
	}
}

// implements dynamic loop scheduling with a fixed chunk size, counting the number of iterations performed by each wi
// chunks are distributed on a first-come first-served basis
inline static void irt_schedule_loop_dynamic_chunked_counting(irt_work_item* self, uint32 id, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args, volatile irt_loop_sched_data *sched_data) {

#ifdef IRT_RUNTIME_TUNING_EXTENDED
	uint64 step = sched_data->policy.param.chunk_size * base_range.step;
	uint64 final = base_range.end;
	
	uint64 comp = sched_data->completed;
	sched_data->part_times[id] = 0;
	while(comp < final) {
		if(irt_atomic_bool_compare_and_swap(&sched_data->completed, comp, comp+step)) {
			base_range.begin = comp;
			base_range.end = MIN(comp+step, final);
			_irt_loop_fragment_run(self, base_range, impl_id, args);
			sched_data->part_times[id] += base_range.end - base_range.begin;
		}
		comp = sched_data->completed;
	}
#else
	irt_throw_string_error(IRT_ERR_INTERNAL, "Tried to use counting scheduling policy even though we aren't in dynamic optimization mode.");
#endif // ifdef IRT_RUNTIME_TUNING_EXTENDED
}

// implements dynamic loop scheduling with a gradually decreasing chunk size
// chunks are distributed on a first-come first-served basis
inline static void irt_schedule_loop_guided_chunked(irt_work_item* self, uint32 id, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args, volatile irt_loop_sched_data *sched_data) {

	uint64 final = base_range.end;

	uint64 comp = sched_data->completed;
	while(comp < final) {
		uint64 bsize = sched_data->block_size;
		if(irt_atomic_bool_compare_and_swap(&sched_data->completed, comp, comp+bsize)) {
			uint64 new_bsize = MAX(sched_data->policy.param.chunk_size, bsize*0.8); // TODO factor tweakable?
			//irt_atomic_bool_compare_and_swap(&sched_data->block_size, bsize, new_bsize);
			sched_data->block_size = MIN(new_bsize, sched_data->block_size);
			base_range.begin = comp;
			base_range.end = MIN(comp+bsize, final);
			_irt_loop_fragment_run(self, base_range, impl_id, args);
		}
		comp = sched_data->completed;
	}
}

// implements loop scheduling using fixed boundaries provided by the user (or the dynamic optimizer)
inline static void irt_schedule_loop_fixed(irt_work_item* self, uint32 id, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args, volatile irt_loop_sched_data *sched_data) {
	
	if(id>0) base_range.begin = sched_data->policy.param.boundaries[id-1];
	if(id<sched_data->policy.participants-1) base_range.end = sched_data->policy.param.boundaries[id];

	_irt_loop_fragment_run(self, base_range, impl_id, args);
}

// implements loop scheduling using fixed shares provided by the user (or the dynamic optimizer)
inline static void irt_schedule_loop_shares(irt_work_item* self, uint32 id, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args, volatile irt_loop_sched_data *sched_data) {
	
	uint64 extent = base_range.end - base_range.begin;
	uint64 start = base_range.begin;
	double pos = 0.0; // do this in floating point to prevent bad distribution because of int rounding
	for(int i = 0; i<id; i++) {
		pos += sched_data->policy.param.shares[i];
	}
	base_range.begin = start + pos*extent;
	pos += sched_data->policy.param.shares[id];
	if(id<sched_data->policy.participants-1) base_range.end = start + pos*extent;
	
	_irt_loop_fragment_run(self, base_range, impl_id, args);
}

// prepare for dynamically scheduled loop with set chunk size before entry
static inline void irt_schedule_loop_dynamic_chunked_prepare(volatile irt_loop_sched_data* sched_data, irt_work_item_range base_range) {
	sched_data->completed = base_range.begin;
}

// prepare for dynamically scheduled loop before entry
static inline void irt_schedule_loop_dynamic_prepare(volatile irt_loop_sched_data* sched_data, irt_work_item_range base_range) {
	uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
	uint64 chunk = (numit / sched_data->policy.participants) / 8;
	sched_data->policy.param.chunk_size = MAX(chunk,1);
	irt_schedule_loop_dynamic_chunked_prepare(sched_data, base_range);
}

// prepare for loop with guided scheduling and minimum size before entry
static inline void irt_schedule_loop_guided_chunked_prepare(volatile irt_loop_sched_data* sched_data, irt_work_item_range base_range) {
	sched_data->completed = base_range.begin;
	uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
	uint64 chunk = (numit / sched_data->policy.participants * 4);
	sched_data->block_size = MAX(sched_data->policy.param.chunk_size, chunk);
}

// prepare for loop with guided scheduling before entry
static inline void irt_schedule_loop_guided_prepare(volatile irt_loop_sched_data* sched_data, irt_work_item_range base_range) {
	uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
	uint64 minchunk = (numit / sched_data->policy.participants) / 16;
	sched_data->policy.param.chunk_size = MAX(minchunk,1);
	irt_schedule_loop_guided_chunked_prepare(sched_data, base_range);
}


void print_effort_estimation(irt_wi_implementation_id impl_id, irt_work_item_range base_range, wi_effort_estimation_func *est_fn) {
	static bool printed[10000];
	if(printed[impl_id]) return;
	printed[impl_id] = true;
	
	printf("\nEffort distribution for wi %d:\n", impl_id);
	if(!est_fn) {
		printf("NO estimation function\n");
		return;
	}

	// - gather
	uint64 extent = base_range.end-base_range.begin;
	uint64 chunk = extent/10;
	if(extent>0 && chunk==0) chunk = 1;
	uint64 effort[10];
	for(int i=0; i<10; ++i) {
		effort[i] = est_fn(chunk*i, chunk*(i+1));
	}

	// - normalize
	uint64 mineffort = effort[0];
	uint64 maxeffort = effort[0];
	for(int i=1; i<10; ++i) {
		if(effort[i]<mineffort) mineffort = effort[i];
		if(effort[i]>maxeffort) maxeffort = effort[i];
	}
	if(maxeffort == 0) {
		printf("Max effort = 0\n");
		return;
	}
	for(int i=0; i<10; ++i) {
		printf("% 3d|", i);
		effort[i] = (effort[i]*100)/maxeffort;
		int j;
		for(j=0; j<effort[i]; ++j) printf("=");
		for(   ; j<100; ++j) printf(" ");
		printf("|\n");
	}
}

inline static void irt_schedule_loop(
		irt_work_item* self, irt_work_group* group, irt_work_item_range base_range, 
		irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
	
	irt_wi_wg_membership* mem = irt_wg_get_wi_membership(group, self);
	mem->pfor_count++;

	// prepare policy if first loop to reach pfor
	pthread_spin_lock(&group->lock);
	if(group->pfor_count < mem->pfor_count) {
		//print_effort_estimation(impl_id, base_range, irt_context_table_lookup(self->context_id)->impl_table[impl_id].variants[0].effort_estimator);

		// run optimizer
		#ifdef IRT_RUNTIME_TUNING
		irt_optimizer_starting_pfor(impl_id, base_range, group);
		#endif

		// define per-loop scheduling policy in group
		group->pfor_count = mem->pfor_count;
		irt_loop_sched_data* sched_data = &group->loop_sched_data[group->pfor_count % IRT_WG_RING_BUFFER_SIZE];
		sched_data->policy = group->cur_sched;
		sched_data->policy.participants = MIN(sched_data->policy.participants, group->local_member_count);

		// do custom scheduler initialization
		switch(sched_data->policy.type) {
		case IRT_STATIC:
		case IRT_STATIC_CHUNKED:
		case IRT_FIXED: 
		case IRT_SHARES: break;
		case IRT_DYNAMIC: irt_schedule_loop_dynamic_prepare(sched_data, base_range); break;
		case IRT_DYNAMIC_CHUNKED: 
		case IRT_DYNAMIC_CHUNKED_COUNTING: irt_schedule_loop_dynamic_chunked_prepare(sched_data, base_range); break;
		case IRT_GUIDED: irt_schedule_loop_guided_prepare(sched_data, base_range); break;
		case IRT_GUIDED_CHUNKED: irt_schedule_loop_guided_chunked_prepare(sched_data, base_range); break;
		default: IRT_ASSERT(false, IRT_ERR_INTERNAL, "Unknown scheduling policy");
		}

		// initialize data for instrumentation
		_irt_loop_tuning_startup(sched_data);
	}
	pthread_spin_unlock(&group->lock);

	// retrieve scheduling data generated for this loop by first entering wi
	IRT_ASSERT(group->pfor_count - mem->pfor_count + 1 < IRT_WG_RING_BUFFER_SIZE, IRT_ERR_OVERFLOW, "Loop scheduling ring buffer overflow");
	volatile irt_loop_sched_data* sched_data = &group->loop_sched_data[mem->pfor_count % IRT_WG_RING_BUFFER_SIZE];
	// return if not scheduled to work in this loop
	if(mem->num >= sched_data->policy.participants) return;
	// if extended tuning information is requested, set start time for self
	#ifdef IRT_RUNTIME_TUNING_EXTENDED
	sched_data->part_times[mem->num] = irt_time_ticks();
	#endif

	// run scheduler
	switch(sched_data->policy.type) {
	case IRT_STATIC: irt_schedule_loop_static(self, mem->num, base_range, impl_id, args, (const irt_loop_sched_policy*)&sched_data->policy); break;
	case IRT_STATIC_CHUNKED: irt_schedule_loop_static_chunked(self, mem->num, base_range, impl_id, args, (const irt_loop_sched_policy*)&sched_data->policy); break;
	case IRT_DYNAMIC: 
	case IRT_DYNAMIC_CHUNKED: irt_schedule_loop_dynamic_chunked(self, mem->num, base_range, impl_id, args, sched_data); break;
	case IRT_DYNAMIC_CHUNKED_COUNTING: irt_schedule_loop_dynamic_chunked_counting(self, mem->num, base_range, impl_id, args, sched_data); break;
	case IRT_GUIDED: 
	case IRT_GUIDED_CHUNKED: irt_schedule_loop_guided_chunked(self, mem->num, base_range, impl_id, args, sched_data); break;
	case IRT_FIXED: irt_schedule_loop_fixed(self, mem->num, base_range, impl_id, args, sched_data); break;
	case IRT_SHARES: irt_schedule_loop_shares(self, mem->num, base_range, impl_id, args, sched_data); break;
	default: IRT_ASSERT(false, IRT_ERR_INTERNAL, "Unknown scheduling policy");
	}

	// gather performance data if required & cleanup
	#ifdef IRT_RUNTIME_TUNING
	#ifdef IRT_RUNTIME_TUNING_EXTENDED
	if(sched_data->policy.type != IRT_DYNAMIC_CHUNKED_COUNTING) // in counting policy, we store iterations in part_times
		sched_data->part_times[mem->num] = irt_time_ticks() - sched_data->part_times[mem->num];
	#endif // ifdef IRT_RUNTIME_TUNING_EXTENDED

	uint32 part_inc;
	do {
		part_inc = sched_data->participants_complete+1;
	} while(!irt_atomic_bool_compare_and_swap(&sched_data->participants_complete, part_inc-1, part_inc));

	if(part_inc == sched_data->policy.participants) {
		#ifdef IRT_RUNTIME_TUNING_EXTENDED
		irt_optimizer_completed_pfor(impl_id, base_range, irt_time_ticks() - sched_data->start_time, sched_data);
		free(sched_data->part_times);
		sched_data->part_times = NULL;
		#else // ifdef IRT_RUNTIME_TUNING_EXTENDED
		irt_optimizer_completed_pfor(impl_id, irt_time_ticks() - sched_data->start_time);
		#endif // ifdef IRT_RUNTIME_TUNING_EXTENDED
	}
	#endif // ifdef IRT_RUNTIME_TUNING
}

void irt_wg_set_loop_scheduling_policy(irt_work_group* group, const irt_loop_sched_policy* policy) {
	// set current group policy, will activate upon next loop entry
	group->cur_sched = *policy;
}
