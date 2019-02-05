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
 */

#pragma once
#ifndef __GUARD_IMPL_WORK_ITEM_IMPL_H
#define __GUARD_IMPL_WORK_ITEM_IMPL_H

#include "work_item.h"

#include <stdlib.h>
#include "impl/worker.impl.h"
#include "utils/impl/minlwt.impl.h"
#include "abstraction/atomic.h"
#include "work_group.h"
#include "impl/error_handling.impl.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/irt_events.impl.h"
#include "impl/instrumentation_regions.impl.h"
#include "impl/instrumentation_events.impl.h"
#include "irt_types.h"

#ifdef IRT_ENABLE_TASK_THROUGHPUT_REPORTING
#include <progress_reporting.h>
#endif

static inline irt_wi_wg_membership irt_wi_get_wg_membership(irt_work_item* wi, uint32 index) {
	IRT_ASSERT(index < wi->num_groups, IRT_ERR_INTERNAL, "WG membership access out of range.");
	return wi->wg_memberships[index];
}
static inline uint32 irt_wi_get_wg_num(irt_work_item* wi, uint32 index) {
	IRT_ASSERT(index < wi->num_groups, IRT_ERR_INTERNAL, "WG membership number access out of range.");
	return wi->wg_memberships[index].num;
}
static inline uint32 irt_wi_get_wg_size(irt_work_item* wi, uint32 index) {
	return irt_wi_get_wg(wi, index)->local_member_count;
}
static inline irt_work_group* irt_wi_get_wg(irt_work_item* wi, uint32 index) {
	return irt_wi_get_wg_membership(wi, index).wg_id.cached; // TODO cached distributed crash
}

static inline irt_work_item* irt_wi_get_current() {
	return irt_worker_get_current()->cur_wi;
}

static inline irt_work_item* _irt_wi_new(irt_worker* self) {
	irt_work_item* ret;
	if(self->wi_reuse_stack) {
		ret = self->wi_reuse_stack;
		self->wi_reuse_stack = ret->next_reuse;
		// IRT_DEBUG("WI_RE\n");
	} else {
		ret = (irt_work_item*)malloc(sizeof(irt_work_item));
		// IRT_DEBUG("WI_FU\n");
	}
	return ret;
}
static inline void _irt_wi_recycle(irt_work_item* wi, irt_worker* self) {
	// IRT_DEBUG("WI_CYC\n");
	wi->next_reuse = self->wi_reuse_stack;
	self->wi_reuse_stack = wi;

	/*IRT_VERBOSE_ONLY(
	    irt_work_item* last = self->wi_reuse_stack;
	    int i = 0;
	    while((last = last->next_reuse)) ++i;
	    printf("WI_CCC %d : %d\n", self->id.thread, i);
	    );*/
}

static inline void _irt_wi_allocate_wgs(irt_work_item* wi) {
	wi->wg_memberships = (irt_wi_wg_membership*)malloc(sizeof(irt_wi_wg_membership) * IRT_MAX_WORK_GROUPS);
}

static inline void _irt_print_work_item_range(const irt_work_item_range* r) {
	IRT_INFO("%" PRId64 "..%" PRId64 " : %" PRId64, r->begin, r->end, r->step);
}

static inline void _irt_wi_init(irt_worker* self, irt_work_item* wi, const irt_work_item_range* range, irt_wi_implementation* impl, irt_lw_data_item* params) {
	wi->id = irt_generate_work_item_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	wi->id.cached = wi;
	wi->parent_id = irt_work_item_null_id();
	wi->impl = impl;
	wi->selected_impl_variant = 0;
	wi->parent_num_active_children = NULL;
	wi->default_parallel_wi_count = 0;
	wi->context_id = self->cur_context;
	wi->num_groups = 0;
	wi->_num_active_children = 0;
	wi->num_active_children = &(wi->_num_active_children);
	if(params != NULL) {
		uint32 size = irt_type_get_bytes(self->cur_context.cached, params->type_id);
		if(size <= IRT_WI_PARAM_BUFFER_SIZE) {
			wi->parameters = &wi->param_buffer;
		} else {
			wi->parameters = (irt_lw_data_item*)malloc(size);
		}
		memcpy(wi->parameters, params, size);
	} else {
		wi->parameters = NULL;
	}
	wi->range = *range;
	irt_atomic_store(&wi->state, IRT_WI_STATE_NEW);
	wi->source_id = irt_work_item_null_id();
	wi->num_fragments = 0;
	wi->stack_storage = NULL;
	wi->wg_memberships = NULL;
	// if this WI has a parent (which means it's not the entry point) migrate some values
	if(self->cur_wi) {
		wi->parent_id = self->cur_wi->id;
		wi->parent_num_active_children = self->cur_wi->num_active_children;
		wi->default_parallel_wi_count = self->cur_wi->default_parallel_wi_count;
	}
	#ifdef IRT_ASTEROIDEA_STACKS
	irt_atomic_store(&wi->stack_available, false);
	#endif // IRT_ASTEROIDEA_STACKS
	irt_inst_region_wi_init(wi);
}

irt_work_item* _irt_wi_create(irt_worker* self, const irt_work_item_range* range, irt_wi_implementation* impl, irt_lw_data_item* params) {
	irt_work_item* retval = _irt_wi_new(self);
	_irt_wi_init(self, retval, range, impl, params);
	if(self->cur_wi != NULL) {
		// increment child count in current wi
		irt_atomic_inc(self->cur_wi->num_active_children, uint32);
	}
	// IRT_DEBUG(" * %p created by %p (%d active children, address: %p) \n", (void*) retval, (void*) self->cur_wi, self->cur_wi ?
	// *self->cur_wi->num_active_children : -1, self->cur_wi ? (void*) self->cur_wi->num_active_children : NULL);
	// create entry in event table
	irt_wi_event_register_create(retval->id);
	irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_CREATED, retval->id);
	return retval;
}
static inline irt_work_item* irt_wi_create(irt_work_item_range range, irt_wi_implementation* impl, irt_lw_data_item* params) {
	// instrumentation
	irt_worker* self = irt_worker_get_current();
	irt_work_item* wi = _irt_wi_create(self, &range, impl, params);
	irt_inst_region_list_copy(wi, self->cur_wi); // TODO philipp fix your shit
	return wi;
}
irt_work_item* _irt_wi_create_fragment(irt_work_item* source, irt_work_item_range range) {
	irt_worker* self = irt_worker_get_current();
	irt_work_item* retval = _irt_wi_new(self);
	memcpy(retval, source, sizeof(irt_work_item));
	retval->id = irt_generate_work_item_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	retval->id.cached = retval;
	retval->num_fragments = 0;
	retval->range = range;
	irt_inst_region_list_copy(retval, self->cur_wi);
	irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_CREATED, retval->id);
	if(irt_wi_is_fragment(source)) {
		// splitting fragment wi
		irt_work_item* base_source = source->source_id.cached; // TODO
		retval->source_id = base_source->id;
	} else {
		// splitting non-fragment wi
		retval->source_id = source->id;
	}
	return retval;
}


#ifdef __cplusplus
extern "C" {
#endif
void
#if(defined(_M_IX86) && defined(_MSC_VER)) || (defined(__MINGW32__) && !defined(__MINGW64__))
    __fastcall
#endif
    _irt_wi_trampoline(irt_work_item* wi, wi_implementation_func* func) {
	func(wi);
	irt_wi_end(wi);
}
#ifdef __cplusplus
}
#endif


irt_joinable irt_wi_run_optional(irt_work_item_range range, irt_wi_implementation* impl, irt_lw_data_item* params) {
	irt_worker* worker = irt_worker_get_current();
	irt_work_item* wi = &worker->lazy_wi;
	wi->range = range;
	wi->impl = impl;
	wi->parameters = params;
	return irt_scheduling_optional_wi(worker, wi);
}

// join -------------------------------------------------------------------------------------------

typedef struct __irt_wi_join_event_data {
	irt_work_item* joining_wi;
	irt_worker* join_to;
} _irt_wi_join_event_data;
bool _irt_wi_join_event(void* user_data) {
	_irt_wi_join_event_data* join_data = (_irt_wi_join_event_data*)user_data;
	irt_inst_insert_wi_event(irt_worker_get_current(), IRT_INST_WORK_ITEM_RESUMED_JOIN, join_data->joining_wi->id);
	irt_scheduling_continue_wi(join_data->join_to, join_data->joining_wi);
	return false;
}

void irt_wi_join(irt_work_item_id wi_id) {
	irt_worker* self = irt_worker_get_current();
	irt_work_item* swi = self->cur_wi;
	_irt_wi_join_event_data clo = {swi, self};
	irt_wi_event_lambda lambda = {&_irt_wi_join_event, &clo, NULL};
	bool registered = irt_wi_event_handler_check_and_register(wi_id, IRT_WI_EV_COMPLETED, &lambda);
	if(registered) { // if not completed, suspend this wi
		irt_inst_region_end_measurements(swi);
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_SUSPENDED_JOIN, swi->id);
		_irt_worker_switch_from_wi(self, swi);
		irt_inst_region_start_measurements(swi);
	}
}

// join all ---------------------------------------------------------------------------------------

bool _irt_wi_join_all_event(void* user_data) {
	_irt_wi_join_event_data* join_data = (_irt_wi_join_event_data*)user_data;
	/*   I ... immediate WI    W ... real WI      A ... arbitrary WI
	 *            A0
	 *           /  \
	 *          I0  W0
	 *         /  \
	 *        W1  W2
	 * I0 is waiting in join_all, W0 triggered the event --> needs to be ignored
	 */
	if(*(join_data->joining_wi->num_active_children) > 0) { return true; }

	#ifdef IRT_ASTEROIDEA_STACKS
	/* NOTE:
	 * We have to wait for the stack to be available here in order to avoid a race condition,
	 * which may arise in a small window of vulnerability in the following situation:
	 *
	 *   I ... immediate WI    W ... real WI      A ... arbitrary WI
	 *            A0
	 *           /  \
	 *          I0  W0
	 *         /
	 *        W1
	 *
	 * I0 is suspended, waiting for all of it's children to complete using join_all.
	 * Meanwhile W0 is started and got the stack of A0 for re-use (see function lwt_prepare in file minlwt.impl.h).
	 * Now W1 ends and I0 could continue it's execution. We must not resume the execution here until W0
	 * released the stack again after realizing that it shouldn't have taken it in the first place, since it has
	 * an immediate sibling.
	 */
	while(!irt_atomic_bool_compare_and_swap(&join_data->joining_wi->stack_available, true, false, bool))
		;
		#endif // IRT_ASTEROIDEA_STACKS

	irt_inst_insert_wi_event(irt_worker_get_current(), IRT_INST_WORK_ITEM_RESUMED_JOIN_ALL, join_data->joining_wi->id);
	IRT_DEBUG(" > %p releasing %p\n", (void*)irt_worker_get_current()->finalize_wi, (void*)join_data->joining_wi);
	irt_scheduling_continue_wi(join_data->join_to, join_data->joining_wi);
	return false;
}

void irt_wi_join_all(irt_work_item* wi) {
	// reset the occurrence count
	irt_wi_event_reset(wi->id, IRT_WI_CHILDREN_COMPLETED);
	if(*(wi->num_active_children) == 0) {
		return; // early exit
	}
	// register event
	irt_worker* self = irt_worker_get_current();
	_irt_wi_join_event_data clo = {wi, self};
	irt_wi_event_lambda lambda = {&_irt_wi_join_all_event, &clo, NULL};
	bool registered = irt_wi_event_handler_check_and_register(wi->id, IRT_WI_CHILDREN_COMPLETED, &lambda);
	if(registered) { // if not completed, suspend this wi
		irt_inst_region_end_measurements(wi);
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_SUSPENDED_JOIN_ALL, wi->id);
		#ifdef IRT_ASTEROIDEA_STACKS
		// make stack available for children
		self->share_stack_wi = wi;
		#endif // IRT_ASTEROIDEA_STACKS
		_irt_worker_switch_from_wi(self, wi);
		irt_inst_region_start_measurements(wi);
	} else {
		// check if multi-level immediate wi was signaled instead of current wi
		if(*(wi->num_active_children) != 0) { irt_wi_join_all(wi); }
	}
	IRT_DEBUG(" J %p join_all ended\n", (void*)wi);
}

// end --------------------------------------------------------------------------------------------

void irt_wi_end(irt_work_item* wi) {
	IRT_DEBUG("Wi %p / Worker %p irt_wi_end.", (void*)wi, (void*)irt_worker_get_current());
	irt_worker* worker = irt_worker_get_current();

	// instrumentation update
	irt_inst_region_end_measurements(wi);
	irt_inst_region_propagate_data_from_wi_to_regions(wi);
	irt_inst_insert_wi_event(worker, IRT_INST_WORK_ITEM_END_START, wi->id);

	// check for fragment, handle
	if(irt_wi_is_fragment(wi)) {
		// ended wi was a fragment
		irt_work_item* source = wi->source_id.cached; // TODO
		IRT_DEBUG("Fragment end, remaining %d", source->num_fragments);
		if(irt_atomic_sub_and_fetch(&source->num_fragments, 1, uint32) == 0) { irt_wi_end(source); }
	} else {
		// delete params struct
		if(wi->parameters != &wi->param_buffer) { free(wi->parameters); }
	}

	// update state
	irt_atomic_store(&wi->state, IRT_WI_STATE_DONE);

	// cleanup
	irt_inst_insert_wi_event(worker, IRT_INST_WORK_ITEM_END_FINISHED, wi->id);
	irt_inst_region_wi_finalize(wi);

	IRT_DEBUG(" ! %p end\n", (void*)wi);

	irt_wi_implementation* wimpl = wi->impl;
	irt_optimizer_remove_dvfs(&(wimpl->variants[wi->selected_impl_variant]));
	irt_optimizer_compute_optimizations(&(wimpl->variants[wi->selected_impl_variant]), wi, false);

	#ifdef IRT_ENABLE_TASK_THROUGHPUT_REPORTING
	irt_report_progress(1);
	#endif

	// end
	worker->finalize_wi = wi;
	lwt_end(&worker->basestack);
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "NEVERMORE");
}

void irt_wi_finalize(irt_worker* worker, irt_work_item* wi) {
	lwt_recycle(worker->id.thread, wi);
	// check for parent, if there, notify (only the first WI does not have a parent)
	if(wi->parent_num_active_children) {
		if(irt_atomic_sub_and_fetch(wi->parent_num_active_children, 1, uint32) == 0) {
			// triggering the parent event might fail since the register could already have been destroyed if the parent finalized before we reach this point
			irt_wi_event_trigger_if_exists(wi->parent_id, IRT_WI_CHILDREN_COMPLETED);
		}
	}
	irt_inst_insert_wi_event(worker, IRT_INST_WORK_ITEM_FINALIZED, wi->id);
	IRT_DEBUG(" ^ %p finalize\n", (void*)wi);

	/* NOTE:
	 * Triggering our own completion event that late is necessary to avoid stack-
	 * or heap corruption as a side effect of a race condition in case a parent
	 * WI is waiting for us to terminate using irt_wi_join.
	 * In this case we have to make sure that we are decrementing the correct
	 * parent_num_active_children variable (the one of our actual parent, not a
	 * stale pointer to some arbitrary other WI struct lying on the reuse-stack
	 * or worse - the member of some currently running new WI which reused this
	 * struct. Another scenario which could happen with immediate WIs involved is
	 * that decrementing the variable might cause a stack corruption, since for
	 * immediate WIs these values are moved to the stack before calling the
	 * children's implementation).
	 * Triggering our own completion event after decrementing
	 * parent_num_active_children member makes sure that the pointer is still
	 * valid, since a parent waiting for it's child to die using irt_wi_join will
	 * blocking wait for the event to happen.
	 *
	 * The same reasoning can be applied to moving the ending of work group
	 * membership (and the associated firing of the WG event) down to here. In
	 * this case the problem may arise when joining the WG (i.e. by joining on the
	 * result of irt_parallel).
	 */
	// remove from groups
	for(uint32 g = 0; g < wi->num_groups; ++g) {
		_irt_wg_end_member(wi->wg_memberships[g].wg_id.cached); // TODO
	}
	// notify (the parent) of our end
	irt_wi_event_trigger(wi->id, IRT_WI_EV_COMPLETED);
	irt_wi_event_register_destroy(wi->id);

	// free the WG membership array which may have been allocated
	if(wi->wg_memberships != NULL) { free(wi->wg_memberships); }

	/* NOTE:
	 * The triggering of events just at the end of the finalization and _after_
	 * the decrementing of parent_num_active_children enables us to change the
	 * life cycle of WIs and we actually enforce that each parent dies after all
	 * children have done so and not before that. This is to ensure that each
	 * child may safely access parent_num_active_children.
	 *
	 * This means that a WI always has to wait explicitly (i.e. by using
	 * irt_wi_join or irt_wi_join_all) for it's spawned children to die.
	 */
	IRT_ASSERT(*wi->num_active_children == 0, IRT_ERR_INTERNAL, "Parent died before all children did")

	_irt_wi_recycle(wi, worker);
}

// splitting --------------------------------------------------------------------------------------

void irt_wi_split_uniform(irt_work_item* wi, uint32 elements, irt_work_item** out_wis) {
	// TODO implement custom (faster)
	irt_work_item_range* r = &wi->range;
	uint64* offsets = (uint64*)alloca(sizeof(uint64) * elements);
	uint64 step = (r->end - r->begin) / elements, cur = r->begin;
	for(uint32 i = 0; i < elements; ++i, cur += step) {
		offsets[i] = cur;
	}
	irt_wi_split(wi, elements, offsets, out_wis);
	#ifdef _GEMS_SIM
	// alloca is implemented as malloc
	free(offsets);
	#endif
}
void irt_wi_split_binary(irt_work_item* wi, irt_work_item* out_wis[2]) {
	// TODO implement custom (faster)
	irt_work_item_range* r = &wi->range;
	uint64 offsets[] = {(uint32)r->begin, (uint32)(r->begin + ((r->end - r->begin) / 2))};
	irt_wi_split(wi, 2, offsets, out_wis);
}
void irt_wi_split(irt_work_item* wi, uint32 elements, uint64* offsets, irt_work_item** out_wis) {
	if(elements == 1) {
		out_wis[0] = wi;
		return;
	}
	irt_worker* self = irt_worker_get_current();
	irt_work_item_range range = wi->range;
	for(uint32 i = 0; i < elements; ++i) {
		range.begin = offsets[i];
		range.end = i + 1 < elements ? offsets[i + 1] : wi->range.end;
		out_wis[i] = _irt_wi_create_fragment(wi, range);
	}

	irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_SPLITTED, wi->id);

	if(irt_wi_is_fragment(wi)) {
		irt_work_item* source = wi->source_id.cached;                           // TODO
		irt_atomic_fetch_and_add(&source->num_fragments, elements - 1, uint32); // This needs to be atomic even if it may not look like it
		for(uint32 i = 0; i < source->num_groups; ++i) {
			irt_atomic_fetch_and_add(&(source->wg_memberships[i].wg_id.cached->local_member_count), elements - 1, uint32); // TODO
		}
		// splitting fragment wi, can safely delete
		_irt_wi_recycle(wi, self);
	} else {
		irt_atomic_fetch_and_add(&wi->num_fragments, elements, uint32); // This needs to be atomic even if it may not look like it
		for(uint32 i = 0; i < wi->num_groups; ++i) {
			irt_atomic_fetch_and_add(&(wi->wg_memberships[i].wg_id.cached->local_member_count), elements - 1, uint32); // TODO
		}
	}
}


#endif // ifndef __GUARD_IMPL_WORK_ITEM_IMPL_H
