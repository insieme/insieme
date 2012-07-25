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

#include "ir_interface.h"

#include "irt_atomic.h"
#include "utils/timing.h"
#include "impl/work_item.impl.h"
#include "impl/work_group.impl.h"
#include "impl/irt_loop_sched.impl.h"
#include "impl/irt_optimizer.impl.h"

//irt_work_item* irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
//	irt_wi_wg_membership* mem = irt_wg_get_wi_membership(group, self);
//	mem->pfor_count++;
//	pthread_spin_lock(&group->lock);
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
//	pthread_spin_unlock(&group->lock);
//	return ret;
//}

void irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
	irt_schedule_loop(self, group, range, impl_id, args);
}


irt_joinable* irt_parallel(const irt_parallel_job* job) {
	irt_worker* target = irt_worker_get_current();
		//irt_work_item *self = target->cur_wi;
		//irt_lw_data_item *prev_args = self->parameters;
		//irt_work_item_range prev_range = self->range;
		//self->parameters = job->args;
		//self->range = irt_g_wi_range_one_elem;
		//(irt_context_table_lookup(target->cur_context)->impl_table[job->impl_id].variants[0].implementation)(self);
		//self->parameters = prev_args;
		//self->range = prev_range;
		//_irt_g_immediate_wis++;
		//return NULL;
	if(job->max == 1) {
		// Task
		return (irt_joinable*)irt_scheduling_optional(target, &irt_g_wi_range_one_elem, job->impl_id, job->args);
	} else {
		// Parallel
		// TODO: make optional, better scheduling,
		// speedup using custom implementation without adding each item individually to group
		irt_work_group* retwg = irt_wg_create();
		uint32 num_threads = (job->max/2+job->min/2);
		if(job->max >= IRT_SANE_PARALLEL_MAX) num_threads = irt_g_worker_count;
		num_threads -= num_threads%job->mod;
		if(num_threads<job->min) num_threads = job->min;
		if(num_threads>IRT_SANE_PARALLEL_MAX) num_threads = IRT_SANE_PARALLEL_MAX;
		irt_work_item** wis = (irt_work_item**)alloca(sizeof(irt_work_item*)*num_threads);
		for(uint32 i=0; i<num_threads; ++i) {
			wis[i] = _irt_wi_create(target, &irt_g_wi_range_one_elem, job->impl_id, job->args);
			irt_wg_insert(retwg, wis[i]);
		}
		for(uint32 i=0; i<num_threads; ++i) {
			irt_scheduling_assign_wi(irt_g_workers[(i+irt_g_worker_count/2-1)%irt_g_worker_count], wis[i]);
		}
		return IRT_TAG_WG_PTR(retwg);
	}
}


void irt_merge(irt_joinable* joinable) {
	if(joinable == NULL) return;
	if(IRT_IS_WG_PTR(joinable)) {
		irt_wg_join(IRT_UNTAG_WG_PTR(joinable));
	} else {
		irt_wi_join((irt_work_item*)joinable);
	}
}

