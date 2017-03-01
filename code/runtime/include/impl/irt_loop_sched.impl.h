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
#ifndef __GUARD_IMPL_IRT_LOOP_SCHED_IMPL_H
#define __GUARD_IMPL_IRT_LOOP_SCHED_IMPL_H

#include <string.h>
#include "irt_loop_sched.h"
#include "work_group.h"
#include "utils/timing.h"
#include "utils/affinity.h"
#include "utils/frequency.h"
#include "impl/irt_optimizer.impl.h"
#include "abstraction/threads.h"
#include "abstraction/impl/threads.impl.h"

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
inline static void _irt_loop_fragment_run(irt_work_item* self, irt_work_item_range range, irt_wi_implementation* impl, irt_lw_data_item* args) {
	if(range.begin == range.end) { return; }
	irt_lw_data_item* prev_args = self->parameters;
	irt_work_item_range prev_range = self->range;
	self->parameters = args;
	self->range = range;
	// Note: As the selection of loop implementation variants may be different from WI impl selection we simply use the first implementation for now
	(impl->variants[0].implementation)(self);
	self->parameters = prev_args;
	self->range = prev_range;
}

// implements static loop scheduling:
// each participant gets one fragment and all fragments are as close to equal in size as possible
inline static void irt_schedule_loop_static(irt_work_item* self, uint32 id, irt_work_item_range base_range, irt_wi_implementation* impl, irt_lw_data_item* args,
                                            const irt_loop_sched_policy* policy) {
	// calculate chunk size
	uint32 participants = policy->participants;
	uint64 range = base_range.end - base_range.begin;
	uint64 numit = range / (base_range.step) + (range % base_range.step > 0);
	uint64 chunk = numit / participants;
	uint64 rem = numit % participants;
	base_range.begin = base_range.begin + id * chunk * base_range.step;

	// adjust chunk and begin to take care of remainder
	if(id < rem) {
		// TODO [_GEMS]: one should not be propagated since gemsclaim compiler is bugged
		uint64 one = 1;
		chunk += one;
	}
	base_range.begin += MIN(rem, id) * base_range.step;
	base_range.end = base_range.begin + chunk * base_range.step;

	_irt_loop_fragment_run(self, base_range, impl, args);
}

// implements static loop scheduling with a fixed chunk size:
// each participant gets a number of fragments of the given size, predetermined by its group id
inline static void irt_schedule_loop_static_chunked(irt_work_item* self, uint32 id, irt_work_item_range base_range, irt_wi_implementation* impl,
                                                    irt_lw_data_item* args, const irt_loop_sched_policy* policy) {
	int64 memstep = policy->param.chunk_size * base_range.step;
	int64 fullstep = policy->participants * memstep;

	irt_work_item_range range;
	range.step = base_range.step;
	if(range.step > 0) {
		for(int64 start = id * memstep + base_range.begin; start < base_range.end; start += fullstep) {
			range.begin = start;
			range.end = MIN(start + memstep, base_range.end);
			_irt_loop_fragment_run(self, range, impl, args);
		}
	} else {
		for(int64 start = id * memstep + base_range.begin; start > base_range.end; start += fullstep) {
			range.begin = start;
			range.end = MAX(start + memstep, base_range.end);
			_irt_loop_fragment_run(self, range, impl, args);
		}
	}
}

// implements dynamic loop scheduling with a fixed chunk size
// chunks are distributed on a first-come first-served basis
inline static void irt_schedule_loop_dynamic_chunked(irt_work_item* self, uint32 id, irt_work_item_range base_range, irt_wi_implementation* impl,
                                                     irt_lw_data_item* args, volatile irt_loop_sched_data* sched_data) {
	uint64 step = sched_data->policy.param.chunk_size * base_range.step;
	uint64 final = base_range.end;

	uint64 comp = sched_data->completed;
	while(comp < final) {
		if(irt_atomic_bool_compare_and_swap(&sched_data->completed, comp, comp + step, uint64)) {
			base_range.begin = comp;
			base_range.end = MIN(comp + step, final);
			_irt_loop_fragment_run(self, base_range, impl, args);
		}
		comp = sched_data->completed;
	}
}

// implements dynamic loop scheduling with a fixed chunk size, counting the number of iterations performed by each wi
// chunks are distributed on a first-come first-served basis
inline static void irt_schedule_loop_dynamic_chunked_counting(irt_work_item* self, uint32 id, irt_work_item_range base_range, irt_wi_implementation* impl,
                                                              irt_lw_data_item* args, volatile irt_loop_sched_data* sched_data) {
#ifdef IRT_RUNTIME_TUNING_EXTENDED
	uint64 step = sched_data->policy.param.chunk_size * base_range.step;
	uint64 final = base_range.end;

	uint64 comp = sched_data->completed;
	sched_data->part_times[id] = 0;
	while(comp < final) {
		if(irt_atomic_bool_compare_and_swap(&sched_data->completed, comp, comp + step, uint64)) {
			base_range.begin = comp;
			base_range.end = MIN(comp + step, final);
			_irt_loop_fragment_run(self, base_range, impl, args);
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
inline static void irt_schedule_loop_guided_chunked(irt_work_item* self, uint32 id, irt_work_item_range base_range, irt_wi_implementation* impl,
                                                    irt_lw_data_item* args, volatile irt_loop_sched_data* sched_data) {
	uint64 final = base_range.end;

	uint64 comp = sched_data->completed;
	while(comp < final) {
		uint64 bsize = sched_data->block_size;
		if(irt_atomic_bool_compare_and_swap(&sched_data->completed, comp, comp + bsize, uint64)) {
			uint64 new_bsize = (uint64)((bsize / base_range.step) * 0.8) * base_range.step;
			new_bsize = MAX(new_bsize, (uint64)(sched_data->policy.param.chunk_size * base_range.step));
			irt_atomic_bool_compare_and_swap(&sched_data->block_size, bsize, new_bsize, uint64);
			base_range.begin = comp;
			base_range.end = MIN(comp + bsize, final);
			_irt_loop_fragment_run(self, base_range, impl, args);
		}
		comp = sched_data->completed;
	}
}

// implements loop scheduling using fixed boundaries provided by the user (or the dynamic optimizer)
inline static void irt_schedule_loop_fixed(irt_work_item* self, uint32 id, irt_work_item_range base_range, irt_wi_implementation* impl, irt_lw_data_item* args,
                                           volatile irt_loop_sched_data* sched_data) {
	if(id > 0) { base_range.begin = sched_data->policy.param.boundaries[id - 1]; }
	if(id < sched_data->policy.participants - 1) { base_range.end = sched_data->policy.param.boundaries[id]; }

	_irt_loop_fragment_run(self, base_range, impl, args);
}

// implements loop scheduling using fixed shares provided by the user (or the dynamic optimizer)
inline static void irt_schedule_loop_shares(irt_work_item* self, uint32 id, irt_work_item_range base_range, irt_wi_implementation* impl, irt_lw_data_item* args,
                                            volatile irt_loop_sched_data* sched_data) {
	int64 extent = base_range.end - base_range.begin;
	int64 start = base_range.begin;
	double pos = 0.0; // do this in floating point to prevent bad distribution because of int rounding
	for(uint32 i = 0; i < id; i++) {
		pos += sched_data->policy.param.shares[i];
	}
	base_range.begin = start + pos * extent;
	pos += sched_data->policy.param.shares[id];
	if(id < sched_data->policy.participants - 1) { base_range.end = start + pos * extent; }

	_irt_loop_fragment_run(self, base_range, impl, args);
}

// prepare for dynamically scheduled loop with set chunk size before entry
static inline void irt_schedule_loop_dynamic_chunked_prepare(volatile irt_loop_sched_data* sched_data, irt_work_item_range base_range) {
	sched_data->completed = base_range.begin;
}

// prepare for dynamically scheduled loop before entry
static inline void irt_schedule_loop_dynamic_prepare(volatile irt_loop_sched_data* sched_data, irt_work_item_range base_range) {
	uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
	uint64 chunk = (numit / sched_data->policy.participants) / 8;
	sched_data->policy.param.chunk_size = MAX(chunk, 1);
	irt_schedule_loop_dynamic_chunked_prepare(sched_data, base_range);
}

// prepare for loop with guided scheduling and minimum size before entry
static inline void irt_schedule_loop_guided_chunked_prepare(volatile irt_loop_sched_data* sched_data, irt_work_item_range base_range) {
	sched_data->completed = base_range.begin;
	int64 numit = (base_range.end - base_range.begin) / (base_range.step);
	int64 chunk = (numit / sched_data->policy.participants * 4);
	sched_data->block_size = MAX(sched_data->policy.param.chunk_size, chunk) * base_range.step;
}

// prepare for loop with guided scheduling before entry
static inline void irt_schedule_loop_guided_prepare(volatile irt_loop_sched_data* sched_data, irt_work_item_range base_range) {
	uint64 numit = (base_range.end - base_range.begin) / (base_range.step);
	uint64 minchunk = (numit / sched_data->policy.participants) / 16;
	sched_data->policy.param.chunk_size = MAX(minchunk, 1);
	irt_schedule_loop_guided_chunked_prepare(sched_data, base_range);
}


void print_effort_estimation(irt_wi_implementation* impl, irt_work_item_range base_range, wi_effort_estimation_func* est_fn) {
	static bool printed[10000];
	if(impl->id < 0 || printed[impl->id]) { return; }
	printed[impl->id] = true;

	printf("\nEffort distribution for wi %d:\n", impl->id);
	if(!est_fn) {
		printf("NO estimation function\n");
		return;
	}

	// - gather
	uint64 extent = base_range.end - base_range.begin;
	uint64 chunk = extent / 10;
	if(extent > 0 && chunk == 0) { chunk = 1; }
	uint64 effort[10];
	for(int i = 0; i < 10; ++i) {
		effort[i] = est_fn(chunk * i, chunk * (i + 1));
	}

	// - normalize
	uint64 mineffort = effort[0];
	uint64 maxeffort = effort[0];
	for(int i = 1; i < 10; ++i) {
		if(effort[i] < mineffort) { mineffort = effort[i]; }
		if(effort[i] > maxeffort) { maxeffort = effort[i]; }
	}
	if(maxeffort == 0) {
		printf("Max effort = 0\n");
		return;
	}
	for(int i = 0; i < 10; ++i) {
		printf("% 3d|", i);
		effort[i] = (effort[i] * 100) / maxeffort;
		uint32 j;
		for(j = 0; j < effort[i]; ++j) {
			printf("=");
		}
		for(; j < 100; ++j) {
			printf(" ");
		}
		printf("|\n");
	}
}

inline static void irt_schedule_loop(irt_work_item* self, irt_work_group* group, irt_work_item_range base_range, irt_wi_implementation* impl,
                                     irt_lw_data_item* args) {
	irt_wi_wg_membership* mem = irt_wg_get_wi_membership(group, self);
	mem->pfor_count++;

	#ifdef IRT_ENABLE_AUTOTUNING
	irt_wi_implementation_variant variant = impl->variants[0];
	irt_worker* worker = irt_worker_get_current();
	uint16 w_id = worker->id.thread;
	if(variant.meta_info && variant.meta_info->autotuning.available) {
		if(w_id < variant.meta_info->autotuning.map.size) {
			if(!irt_affinity_mask_is_single_cpu(worker->affinity, variant.meta_info->autotuning.map.data[w_id])) {
				irt_affinity_mask_clear(&(worker->affinity));
				irt_affinity_mask_set(&(worker->affinity), variant.meta_info->autotuning.map.data[w_id], true);
				irt_set_affinity(worker->affinity, worker->thread);
			}

			if(irt_cpu_freq_cores_have_individual_domains()) {
				if(worker->id.thread < variant.meta_info->autotuning.frequencies.size) {
					irt_cpu_freq_set_frequency_core(variant.meta_info->autotuning.map.data[w_id], variant.meta_info->autotuning.frequencies.data[w_id]);
				}
			}
		}
	}
	#endif // IRT_ENABLE_AUTOTUNING

	// prepare policy if first loop to reach pfor
	irt_spin_lock(&group->lock);
	if(group->pfor_count < mem->pfor_count) {
	// print_effort_estimation(impl, base_range, impl->variants[0].effort_estimator);

	#ifdef IRT_ENABLE_AUTOTUNING
		assert(variant.meta_info->autotuning.number_of_workers > 0 && "Number of workers must be set!");
		group->cur_sched.participants = variant.meta_info->autotuning.number_of_workers;
		group->cur_sched.type = variant.meta_info->autotuning.loop_scheduling;
		group->cur_sched.param.chunk_size = variant.meta_info->autotuning.loop_scheduling_chunk_size;

		if(!irt_cpu_freq_cores_have_individual_domains()) {
			for(uint32 i = 0; i < irt_hw_get_num_sockets(); ++i) {
				irt_cpu_freq_set_frequency_socket(i, variant.meta_info->autotuning.frequencies.data[i]);
			}
		}

		#endif // IRT_ENABLE_AUTOTUNING

		// run optimizer
		#ifdef IRT_RUNTIME_TUNING
		irt_optimizer_starting_pfor(impl, base_range, group);
		#endif

		// define per-loop scheduling policy in group
		group->pfor_count = mem->pfor_count;
		irt_loop_sched_data* sched_data = &group->loop_sched_data[group->pfor_count % IRT_WG_RING_BUFFER_SIZE];

		sched_data->policy = group->cur_sched;

		sched_data->policy.participants = MIN(sched_data->policy.participants, group->local_member_count);

		// initialise data for instrumentation
		_irt_loop_tuning_startup(sched_data);

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
	}
	irt_spin_unlock(&group->lock);

	// retrieve scheduling data generated for this loop by first entering wi
	IRT_ASSERT(group->pfor_count - mem->pfor_count + 1 < IRT_WG_RING_BUFFER_SIZE, IRT_ERR_OVERFLOW, "Loop scheduling ring buffer overflow");
	volatile irt_loop_sched_data* sched_data = &group->loop_sched_data[mem->pfor_count % IRT_WG_RING_BUFFER_SIZE];
	// return if not scheduled to work in this loop
	if(mem->num >= sched_data->policy.participants) { return; }
	// if extended tuning information is requested, set start time for self
	#ifdef IRT_RUNTIME_TUNING_EXTENDED
	sched_data->part_times[mem->num] = irt_time_ticks();
	#endif

	// run scheduler
	switch(sched_data->policy.type) {
	case IRT_STATIC: irt_schedule_loop_static(self, mem->num, base_range, impl, args, (const irt_loop_sched_policy*)&sched_data->policy); break;
	case IRT_STATIC_CHUNKED: irt_schedule_loop_static_chunked(self, mem->num, base_range, impl, args, (const irt_loop_sched_policy*)&sched_data->policy); break;
	case IRT_DYNAMIC:
	case IRT_DYNAMIC_CHUNKED: irt_schedule_loop_dynamic_chunked(self, mem->num, base_range, impl, args, sched_data); break;
	case IRT_DYNAMIC_CHUNKED_COUNTING: irt_schedule_loop_dynamic_chunked_counting(self, mem->num, base_range, impl, args, sched_data); break;
	case IRT_GUIDED:
	case IRT_GUIDED_CHUNKED: irt_schedule_loop_guided_chunked(self, mem->num, base_range, impl, args, sched_data); break;
	case IRT_FIXED: irt_schedule_loop_fixed(self, mem->num, base_range, impl, args, sched_data); break;
	case IRT_SHARES: irt_schedule_loop_shares(self, mem->num, base_range, impl, args, sched_data); break;
	default: IRT_ASSERT(false, IRT_ERR_INTERNAL, "Unknown scheduling policy");
	}

	// gather performance data if required & cleanup
	#ifdef IRT_RUNTIME_TUNING
	#ifdef IRT_RUNTIME_TUNING_EXTENDED
	if(sched_data->policy.type != IRT_DYNAMIC_CHUNKED_COUNTING) { // in counting policy, we store iterations in part_times
		sched_data->part_times[mem->num] = irt_time_ticks() - sched_data->part_times[mem->num];
	}
	#endif // ifdef IRT_RUNTIME_TUNING_EXTENDED

	// increase the number of participants in a thread safe manner
	// such that afterwards only a single thread has each part_inc number
	uint32 part_inc;
	do {
		part_inc = sched_data->participants_complete + 1;
	} while(!irt_atomic_bool_compare_and_swap(&sched_data->participants_complete, part_inc - 1, part_inc, uint32));

	if(part_inc == sched_data->policy.participants) {
	// sched_data no longer volatile, loop completed
	#ifdef IRT_RUNTIME_TUNING_EXTENDED
		irt_optimizer_completed_pfor(impl, base_range, irt_time_ticks() - sched_data->start_time, (irt_loop_sched_data*)sched_data);
		free(sched_data->part_times);
		sched_data->part_times = NULL;
		#else // ifdef IRT_RUNTIME_TUNING_EXTENDED
		irt_optimizer_completed_pfor(impl, irt_time_ticks() - sched_data->start_time, (irt_loop_sched_data*)sched_data);
		#endif // ifdef IRT_RUNTIME_TUNING_EXTENDED
	}
	#endif // ifdef IRT_RUNTIME_TUNING
}

void irt_wg_set_loop_scheduling_policy(irt_work_group* group, const irt_loop_sched_policy* policy) {
	// set current group policy, will activate upon next loop entry
	group->cur_sched = *policy;
}

void irt_loop_sched_policy_init() {
	char* policy_env = getenv(IRT_LOOP_SCHED_POLICY_ENV);
	if(policy_env) {
		char* policy_env_copy = (char*)alloca(strlen(policy_env) * sizeof(char)); // needed for logging output, since strtok modifies the string it's working on
		strcpy(policy_env_copy, policy_env);

		char* policy_str = strtok(policy_env, ",");
		char* chunksize_str = strtok(NULL, ",");
		if(policy_str) {
			irt_log_setting_s("IRT_LOOP_SCHED_POLICY", policy_env_copy);
			if(strcmp("IRT_STATIC", policy_str) == 0) {
				if(chunksize_str) {
					irt_g_loop_sched_policy_default.type = IRT_STATIC_CHUNKED;
					irt_g_loop_sched_policy_default.participants = IRT_SANE_PARALLEL_MAX;
					irt_g_loop_sched_policy_default.param.chunk_size = atoi(chunksize_str);
					IRT_ASSERT(irt_g_loop_sched_policy_default.param.chunk_size > 0, IRT_ERR_INTERNAL, "Chunk size must not be 0");
				} else {
					irt_g_loop_sched_policy_default.type = IRT_STATIC;
					irt_g_loop_sched_policy_default.participants = IRT_SANE_PARALLEL_MAX;
					irt_g_loop_sched_policy_default.param.chunk_size = 0;
				}
			} else if(strcmp("IRT_DYNAMIC", policy_str) == 0) {
				if(chunksize_str) {
					irt_g_loop_sched_policy_default.type = IRT_DYNAMIC_CHUNKED;
					irt_g_loop_sched_policy_default.participants = IRT_SANE_PARALLEL_MAX;
					irt_g_loop_sched_policy_default.param.chunk_size = atoi(chunksize_str);
					IRT_ASSERT(irt_g_loop_sched_policy_default.param.chunk_size > 0, IRT_ERR_INTERNAL, "Chunk size must not be 0");
				} else {
					irt_g_loop_sched_policy_default.type = IRT_DYNAMIC;
					irt_g_loop_sched_policy_default.participants = IRT_SANE_PARALLEL_MAX;
					irt_g_loop_sched_policy_default.param.chunk_size = 0;
				}
			} else if(strcmp("IRT_GUIDED", policy_str) == 0) {
				if(chunksize_str) {
					irt_g_loop_sched_policy_default.type = IRT_GUIDED_CHUNKED;
					irt_g_loop_sched_policy_default.participants = IRT_SANE_PARALLEL_MAX;
					irt_g_loop_sched_policy_default.param.chunk_size = atoi(chunksize_str);
					IRT_ASSERT(irt_g_loop_sched_policy_default.param.chunk_size > 0, IRT_ERR_INTERNAL, "Chunk size must not be 0");
				} else {
					irt_g_loop_sched_policy_default.type = IRT_GUIDED;
					irt_g_loop_sched_policy_default.participants = IRT_SANE_PARALLEL_MAX;
					irt_g_loop_sched_policy_default.param.chunk_size = 0;
				}
			} else {
				fprintf(stderr, "unknown loop scheduler policy requested: %s\n", policy_env_copy);
				#ifdef _GEMS_SIM
				// alloca is implemented as malloc
				free(policy_env_copy);
				#endif
				exit(-1);
			}
		}
		#ifdef _GEMS_SIM
		// alloca is implemented as malloc
		free(policy_env_copy);
		#endif
	} else {
		irt_log_setting_s("IRT_LOOP_SCHED_POLICY", "IRT_STATIC");
		irt_g_loop_sched_policy_default.type = IRT_STATIC;
		irt_g_loop_sched_policy_default.participants = IRT_SANE_PARALLEL_MAX;
		irt_g_loop_sched_policy_default.param.chunk_size = 0;
	}
	irt_g_loop_sched_policy_single = (irt_loop_sched_policy){IRT_DYNAMIC_CHUNKED, IRT_SANE_PARALLEL_MAX, {1000}};
}


#endif // ifndef __GUARD_IMPL_IRT_LOOP_SCHED_IMPL_H
