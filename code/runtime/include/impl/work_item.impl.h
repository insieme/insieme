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

#include "work_item.h"

#include <stdlib.h>
#ifdef WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif
#include "impl/worker.impl.h"
#include "utils/impl/minlwt.impl.h"
#include "irt_atomic.h"
#include "work_group.h"
#include "impl/error_handling.impl.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/irt_events.impl.h"
#include "impl/instrumentation.impl.h"

static inline irt_wi_wg_membership irt_wi_get_wg_membership(irt_work_item *wi, uint32 index) {
	IRT_ASSERT(index<wi->num_groups, IRT_ERR_INTERNAL, "WG membership access out of range.");
	return wi->wg_memberships[index]; 
}
static inline uint32 irt_wi_get_wg_num(irt_work_item *wi, uint32 index) {
	IRT_ASSERT(index<wi->num_groups, IRT_ERR_INTERNAL, "WG membership number access out of range.");
	return wi->wg_memberships[index].num; 
}
static inline uint32 irt_wi_get_wg_size(irt_work_item *wi, uint32 index) {
	return irt_wi_get_wg(wi,index)->local_member_count; 
}
static inline irt_work_group* irt_wi_get_wg(irt_work_item *wi, uint32 index) {
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
		//IRT_DEBUG("WI_RE\n");
	} else {
		ret = (irt_work_item*)malloc(sizeof(irt_work_item));
		ret->wg_memberships = NULL;
		//IRT_DEBUG("WI_FU\n");
	}
	return ret;
}
static inline void _irt_wi_recycle(irt_work_item* wi, irt_worker* self) {
	//IRT_DEBUG("WI_CYC\n");
	wi->next_reuse = self->wi_reuse_stack;
	self->wi_reuse_stack = wi;
	lwt_recycle(self->id.value.components.thread, wi);
	
	/*IRT_VERBOSE_ONLY(
		irt_work_item* last = self->wi_reuse_stack;
		int i = 0;
		while((last = last->next_reuse)) ++i;
		printf("WI_CCC %d : %d\n", self->id.value.components.thread, i);
		);*/
}

static inline void _irt_wi_allocate_wgs(irt_work_item* wi) {
	// TODO make threadsafe
	wi->wg_memberships = (irt_wi_wg_membership*)malloc(sizeof(irt_wi_wg_membership)*IRT_MAX_WORK_GROUPS);
}

static inline void _irt_print_work_item_range(const irt_work_item_range* r) { 
	IRT_INFO("%ld..%ld : %ld", r->begin, r->end, r->step);
}

static inline void _irt_wi_init(irt_context_id context, irt_work_item* wi, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* params) {
	wi->id = irt_generate_work_item_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	wi->id.cached = wi;
	wi->impl_id = impl_id;
	wi->context_id = context;
	wi->num_groups = 0;
	wi->parameters = params;
	wi->range = range;
	wi->state = IRT_WI_STATE_NEW;
	wi->source_id = irt_work_item_null_id();
	wi->num_fragments = 0;
	wi->stack_storage = NULL;
	wi->wg_memberships = NULL;
	wi->region = NULL;
	wi->last_timestamp = 0;
}

irt_work_item* _irt_wi_create(irt_worker* self, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* params) {
	irt_work_item* retval = _irt_wi_new(self);
	_irt_wi_init(self->cur_context, retval, range, impl_id, params);
	if(self->cur_wi != NULL && self->cur_wi->region != NULL)
		retval->region = self->cur_wi->region;
	irt_wi_instrumentation_event(self, WORK_ITEM_CREATED, retval->id);
	return retval;
}
static inline irt_work_item* irt_wi_create(irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* params) {
	return _irt_wi_create(irt_worker_get_current(), range, impl_id, params);
}
irt_work_item* _irt_wi_create_fragment(irt_work_item* source, irt_work_item_range range) {
	irt_worker *self = irt_worker_get_current();
	irt_work_item* retval = _irt_wi_new(self);
	memcpy(retval, source, sizeof(irt_work_item));
	retval->id = irt_generate_work_item_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	retval->id.cached = retval;
	retval->num_fragments = 0;
	retval->range = range;
	irt_wi_instrumentation_event(self, WORK_ITEM_CREATED, retval->id);
	if(irt_wi_is_fragment(source)) {
		// splitting fragment wi
		irt_work_item *base_source = source->source_id.cached; // TODO
		retval->source_id = base_source->id;
	} else {
		// splitting non-fragment wi
		retval->source_id = source->id;
	}
	return retval;
}

irt_work_item* irt_wi_run_optional(irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* params) {
	irt_worker *worker = irt_worker_get_current();
	irt_work_item *wi = &worker->lazy_wi;
	wi->range = range;
	wi->impl_id = impl_id;
	wi->parameters = params;
	return irt_scheduling_optional_wi(worker, wi);
}

typedef struct __irt_wi_join_event_data {
	irt_work_item* joining_wi;
	irt_worker* join_to;
} _irt_wi_join_event_data;
bool _irt_wi_join_event(irt_wi_event_register* source_event_register, void *user_data) {
	_irt_wi_join_event_data* join_data = (_irt_wi_join_event_data*)user_data;
	irt_scheduling_continue_wi(join_data->join_to, join_data->joining_wi);
	return false;
}

void irt_wi_join(irt_work_item* wi) {
	irt_worker* self = irt_worker_get_current();
	irt_work_item* swi = self->cur_wi;
	_irt_wi_join_event_data clo = {swi, self};
	irt_wi_event_lambda lambda = { &_irt_wi_join_event, &clo, NULL };
	uint32 occ = irt_wi_event_check_and_register(wi->id, IRT_WI_EV_COMPLETED, &lambda);
	if(occ==0) { // if not completed, suspend this wi
		irt_instrumentation_region_add_time(swi);
		irt_wi_instrumentation_event(self, WORK_ITEM_SUSPENDED_JOIN, swi->id);
		self->cur_wi = NULL;
		lwt_continue(&self->basestack, &swi->stack_ptr);
		irt_instrumentation_region_set_timestamp(swi);
	}
}

void irt_wi_end(irt_work_item* wi) {
	
	IRT_DEBUG("Wi %p / Worker %p irt_wi_end.", wi, irt_worker_get_current());
	irt_worker *worker = irt_worker_get_current();

	if(worker->lazy_count>0) {
		// ending wi was lazily executed
		worker->lazy_count--;
		return;
	}
	irt_instrumentation_region_add_time(wi);
	irt_wi_instrumentation_event(worker, WORK_ITEM_FINISHED, wi->id);
	wi->state = IRT_WI_STATE_DONE;
	worker->cur_wi = NULL;
	if(irt_wi_is_fragment(wi)) {
		// ended wi was a fragment
		irt_work_item *source = wi->source_id.cached; // TODO
		IRT_DEBUG("Fragment end, remaining %d", source->num_fragments);
		irt_atomic_fetch_and_sub(&source->num_fragments, 1);
		if(source->num_fragments == 0) irt_wi_end(source);
	}
	irt_wi_event_trigger(wi->id, IRT_WI_EV_COMPLETED);
	for(uint32 g=0; g<wi->num_groups; ++g) {
		_irt_wg_end_member(wi->wg_memberships[g].wg_id.cached); // TODO
	}
	_irt_wi_recycle(wi, worker);
	lwt_end(&worker->basestack);
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "NEVERMORE");
}

void irt_wi_split_uniform(irt_work_item* wi, uint32 elements, irt_work_item** out_wis) {
	// TODO implement custom (faster)
	irt_work_item_range *r = &wi->range;
	uint64 *offsets = (uint64*)alloca(sizeof(uint64)*elements);
	uint64 step = (r->end - r->begin) / elements, cur = r->begin;
	for(uint32 i=0; i<elements; ++i, cur+=step) offsets[i] = cur;
	irt_wi_split(wi, elements, offsets, out_wis);
}
void irt_wi_split_binary(irt_work_item* wi, irt_work_item* out_wis[2]) {
	// TODO implement custom (faster)
	irt_work_item_range *r = &wi->range;
	uint64 offsets[] = {r->begin, r->begin + ( (r->end - r->begin) / 2)};
	irt_wi_split(wi, 2, offsets, out_wis);
}
void irt_wi_split(irt_work_item* wi, uint32 elements, uint64* offsets, irt_work_item** out_wis) {
	if(elements == 1) {
		out_wis[0] = wi;
		return;
	}
	irt_worker *self = irt_worker_get_current();
	irt_work_item_range range = wi->range;
	for(uint32 i=0; i<elements; ++i) {
		range.begin = offsets[i];
		range.end = i+1 < elements ? offsets[i+1] : wi->range.end;
		out_wis[i] = _irt_wi_create_fragment(wi, range);
	}
	
	irt_wi_instrumentation_event(self, WORK_ITEM_SPLITTED, wi->id);
	
	if(irt_wi_is_fragment(wi)) {
		irt_work_item* source = wi->source_id.cached; // TODO
		irt_atomic_fetch_and_add(&source->num_fragments, elements - 1); // This needs to be atomic even if it may not look like it
		for(uint32 i=0; i<source->num_groups; ++i) {
			irt_atomic_fetch_and_add(&(source->wg_memberships[i].wg_id.cached->local_member_count), elements - 1); // TODO
		}
		// splitting fragment wi, can safely delete
		_irt_wi_recycle(wi, self);
	} else {
		irt_atomic_fetch_and_add(&wi->num_fragments, elements); // This needs to be atomic even if it may not look like it		
		for(uint32 i=0; i<wi->num_groups; ++i) {
			irt_atomic_fetch_and_add(&(wi->wg_memberships[i].wg_id.cached->local_member_count), elements - 1); // TODO
		}
	}
}
