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
#ifndef __GUARD_IMPL_IR_INTERFACE_IMPL_H
#define __GUARD_IMPL_IR_INTERFACE_IMPL_H

#include "ir_interface.h"

#include "abstraction/atomic.h"
#include "utils/timing.h"
#include "impl/work_item.impl.h"
#include "impl/work_group.impl.h"
#include "impl/irt_loop_sched.impl.h"
#include "impl/irt_optimizer.impl.h"

// irt_work_item* irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
//	irt_wi_wg_membership* mem = irt_wg_get_wi_membership(group, self);
//	mem->pfor_count++;
//	irt_spin_lock(&group->lock);
//	irt_work_item* ret;
//	if(group->pfor_count < mem->pfor_count) {
//		// This wi was the first to reach this pfor
//		group->pfor_count++;
//		ret = irt_wi_create(range, impl_id, args);
//		irt_scheduling_assign_wi(irt_worker_get_current(), ret);
//		group->pfor_wi_list[group->pfor_count % IRT_WG_RING_BUFFER_SIZE] = ret;
//	} else {
//		// Another wi already created the pfor wi
//		IRT_ASSERT(group->pfor_count - mem->pfor_count < IRT_WG_RING_BUFFER_SIZE, IRT_ERR_OVERFLOW, "Work group ring buffer overflow");
//		ret = group->pfor_wi_list[mem->pfor_count % IRT_WG_RING_BUFFER_SIZE];
//	}
//	irt_spin_unlock(&group->lock);
//	return ret;
//}

void _irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range base_range, irt_wi_implementation* impl, irt_lw_data_item* args) {
	// calculate chunk size
	uint32 id = irt_wg_get_wi_num(group, self);
	uint32 participants = group->local_member_count;
	uint64 length = base_range.end - base_range.begin;
	uint64 numit = length / (base_range.step) + (length % base_range.step > 0);
	uint64 chunk = numit / participants;
	uint64 rem = numit % participants;
	base_range.begin = base_range.begin + id * chunk * base_range.step;

	// adjust chunk and begin to take care of remainder
	if(id < rem) { chunk++; }
	base_range.begin += MIN(rem, id) * base_range.step;
	base_range.end = base_range.begin + chunk * base_range.step;

	// run chunk
	irt_work_item_range range = base_range;
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

void irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range range, irt_wi_implementation* impl, irt_lw_data_item* args) {
	// Note: As the selection of loop implementation variants may be different from WI impl selection we simply use the first implementation for now
	irt_optimizer_runtime_data* old_data = irt_optimizer_set_wrapping_optimizations(
	    &(irt_worker_get_current()->cur_wi->impl->variants[irt_worker_get_current()->cur_wi->selected_impl_variant]), &(impl->variants[0]));
	irt_optimizer_apply_dvfs(&(impl->variants[0]));

	irt_schedule_loop(self, group, range, impl, args);

	// Note: As the selection of loop implementation variants may be different from WI impl selection we simply use the first implementation for now
	irt_optimizer_remove_dvfs(&(impl->variants[0]));
	irt_optimizer_compute_optimizations(&(impl->variants[0]), NULL, true);
	irt_optimizer_reset_wrapping_optimizations(&(irt_worker_get_current()->cur_wi->impl->variants[irt_worker_get_current()->cur_wi->selected_impl_variant]),
	                                           old_data);

	#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_atomic_add_and_fetch(&irt_g_app_progress, 1, uint64);
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
}


irt_joinable irt_parallel(const irt_parallel_job* job) {
	// Parallel
	// TODO: make optional, better scheduling,
	// speedup using custom implementation without adding each item individually to group
	#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
	// Note: We use the first implementation here since it may carry optimization data
	// Note: this call and the call of irt_optimizer_set_wrapping_optimizations below maybe should be moved further down just before WI assignment
	irt_optimizer_apply_dct(&job->impl->variants[0]);
	#endif
	irt_work_group* retwg = irt_wg_create();
	irt_joinable ret;
	ret.wg_id = retwg->id;
	uint32 num_threads = (job->max / 2 + job->min / 2);
	num_threads -= num_threads % job->mod;
	if(job->max >= IRT_SANE_PARALLEL_MAX) {
		num_threads = irt_g_degree_of_parallelism;
		irt_work_item* cur_wi = irt_wi_get_current();
		if(cur_wi && cur_wi->default_parallel_wi_count != 0) { num_threads = cur_wi->default_parallel_wi_count; }
	}
	if(num_threads < job->min) { num_threads = job->min; }
	if(num_threads > IRT_SANE_PARALLEL_MAX) { num_threads = IRT_SANE_PARALLEL_MAX; }
	irt_optimizer_set_wrapping_optimizations(&job->impl->variants[0],
	                                         &(irt_worker_get_current()->cur_wi->impl->variants[irt_worker_get_current()->cur_wi->selected_impl_variant]));
	#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_atomic_add_and_fetch(&irt_g_app_progress, num_threads, uint64);
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_work_item** wis = (irt_work_item**)alloca(sizeof(irt_work_item*) * num_threads);
	for(uint32 i = 0; i < num_threads; ++i) {
		wis[i] = irt_wi_create(irt_g_wi_range_one_elem, job->impl, job->args);
		irt_wg_insert(retwg, wis[i]);
	}
	for(uint32 i = 0; i < num_threads; ++i) {
		irt_scheduling_generate_wi(irt_g_workers[i % irt_g_degree_of_parallelism], wis[i]);
	}
	#ifdef _GEMS_SIM
	// alloca is implemented as malloc
	free(wis);
	#endif
	return ret;
}

void irt_set_default_parallel_wi_count(int num_wis) {
	IRT_ASSERT(num_wis > 0, IRT_ERR_INVALIDARGUMENT, "Invalid default number of wis: %d", num_wis);
	irt_wi_get_current()->default_parallel_wi_count = num_wis;
}

irt_joinable irt_task(const irt_parallel_job* job) {
#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_atomic_add_and_fetch(&irt_g_app_progress, 1, uint64);
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_worker* target = irt_worker_get_current();
	IRT_ASSERT(job->max == 1, IRT_ERR_INIT, "Task invalid range");
	return irt_scheduling_optional(target, &irt_g_wi_range_one_elem, job->impl, job->args);
}

void irt_region(const irt_parallel_job* job) {
	irt_work_item* wis = irt_wi_create(irt_g_wi_range_one_elem, job->impl, job->args);

	// Note: We use the first implementation here since it may carry optimization data
	irt_optimizer_runtime_data* old_data = irt_optimizer_set_wrapping_optimizations(
	    &(irt_worker_get_current()->cur_wi->impl->variants[irt_worker_get_current()->cur_wi->selected_impl_variant]), &(job->impl->variants[0]));
	irt_optimizer_apply_dvfs(&(job->impl->variants[0]));

	job->impl->variants[0].implementation(wis);

	irt_optimizer_remove_dvfs(&(job->impl->variants[0]));
	irt_optimizer_compute_optimizations(&(job->impl->variants[0]), NULL, true);
	irt_optimizer_reset_wrapping_optimizations(&(irt_worker_get_current()->cur_wi->impl->variants[irt_worker_get_current()->cur_wi->selected_impl_variant]),
	                                           old_data);

	// TODO need some proper cleanup here. simply freeing the WI will not clean everything which has been created
	free(wis);
}

void irt_merge(irt_joinable joinable) {
#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	irt_atomic_add_and_fetch(&irt_g_app_progress, 1, uint64);
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING

	if(joinable.wi_id.full == irt_work_item_null_id().full) { return; }

	if(joinable.wi_id.id_type == IRT_ID_work_group) {
		irt_wg_join(joinable.wg_id);
	} else {
		irt_wi_join(joinable.wi_id);
	}
}

#if defined(IRT_ENABLE_APP_TIME_ACCOUNTING) && defined(IRT_ENABLE_REGION_INSTRUMENTATION)
#error IRT_ENABLE_APP_TIME_ACCOUNTING and IRT_ENABLE_REGION_INSTRUMENTATION do not work together
#endif

double irt_time_wis_get_total() {
#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	double ret = 0.0;
	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		irt_worker* cur = irt_g_workers[i];
		ret += cur->app_time_total;
		double ls = cur->app_time_last_start;
		if(cur->app_time_running) {
			// add current running time
			struct timespec ts;
			clock_gettime(cur->clockid, &ts);
			ret += (ts.tv_sec * 1000000.0 + ts.tv_nsec / 1000.0) - cur->app_time_last_start;
		}
	}
	return ret;
	#else
	IRT_ASSERT(false, IRT_ERR_INSTRUMENTATION, "irt_time_wis_get_total called without compiling with IRT_ENABLE_APP_TIME_ACCOUNTING");
	return 0.0;
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
}

double irt_time_rts_get_total() {
#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	struct timespec ts;
	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
	return (ts.tv_sec * 1000000.0 + ts.tv_nsec / 1000.0);
	#else
	IRT_ASSERT(false, IRT_ERR_INSTRUMENTATION, "irt_time_rts_get_total called without compiling with IRT_ENABLE_APP_TIME_ACCOUNTING");
	return 0.0;
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
}


uint64 irt_app_progress_get() {
#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	return irt_g_app_progress;
	#else
	IRT_ASSERT(false, IRT_ERR_INSTRUMENTATION, "irt_app_progress_get called without compiling with IRT_ENABLE_APP_TIME_ACCOUNTING");
	return 0;
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING
}

#endif // ifndef __GUARD_IMPL_IR_INTERFACE_IMPL_H
