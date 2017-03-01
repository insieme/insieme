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
#ifndef __GUARD_WORK_ITEM_H
#define __GUARD_WORK_ITEM_H

#include "declarations.h"
#include "instrumentation_events.h"

#include "irt_context.h"
#include "utils/minlwt.h"
#include "irt_scheduling.h"
#include "irt_events.h"
#include "data_item.h"

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(work_item)

typedef enum _irt_work_item_state {
	IRT_WI_STATE_NEW,
	IRT_WI_STATE_STARTED,
	IRT_WI_STATE_SUSPENDED,
	IRT_WI_STATE_DONE,
} irt_work_item_state;

struct _irt_work_item_range {
	int64 begin, end, step;
};
static const irt_work_item_range irt_g_wi_range_one_elem = {0, 1, 1};
static inline int64 irt_wi_range_get_size(const irt_work_item_range* r) {
	return (r->end - r->begin) / r->step;
}
static inline void _irt_print_work_item_range(const irt_work_item_range* r);

typedef bool irt_wi_readiness_check_fun(irt_work_item* wi);
typedef struct _irt_wi_readiness_check {
	irt_wi_readiness_check_fun* fun;
	void* data;
} irt_wi_readiness_check;
irt_wi_readiness_check irt_g_null_readiness_check = {NULL, NULL};

struct _irt_work_item {
	// core functionality
	irt_work_item_id id;
	irt_work_item_id parent_id;
	irt_context_id context_id;
	irt_wi_implementation* impl;
	uint32 selected_impl_variant;
	uint32 default_parallel_wi_count;
	irt_work_item_range range;
	uint32 num_groups;
	volatile uint32 _num_active_children;
	volatile uint32* num_active_children;
	volatile uint32* parent_num_active_children;
	irt_wi_wg_membership* wg_memberships;
	volatile irt_work_item_state state;
	irt_lw_data_item* parameters;
	// wi splitting related
	irt_work_item_id source_id;
	uint32 num_fragments;
	// private implementation details, do not need to be migrated
	irt_work_item* next_reuse;
	lwt_reused_stack* stack_storage;
	lwt_context stack_ptr;
	#ifdef IRT_ASTEROIDEA_STACKS
	volatile bool stack_available;
	#endif
	irt_wi_scheduling_data sched_data;
	#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	irt_inst_region_wi_data* inst_region_data;
	irt_inst_region_list* inst_region_list;
	#endif
	union {
		char param_storage[IRT_WI_PARAM_BUFFER_SIZE];
		irt_lw_data_item param_buffer;
	};
};

/* ------------------------------ operations ----- */

static inline irt_work_item* irt_wi_get_current();

static inline bool irt_wi_is_fragment(irt_work_item* wi) {
	return wi->source_id.full != irt_work_item_null_id().full;
}
static inline irt_wi_wg_membership irt_wi_get_wg_membership(irt_work_item* wi, uint32 index);
static inline uint32 irt_wi_get_wg_num(irt_work_item* wi, uint32 index);
static inline uint32 irt_wi_get_wg_size(irt_work_item* wi, uint32 index);
static inline irt_work_group* irt_wi_get_wg(irt_work_item* wi, uint32 index);

irt_work_item* _irt_wi_create(irt_worker* self, const irt_work_item_range* range, irt_wi_implementation* impl, irt_lw_data_item* params);
static inline irt_work_item* irt_wi_create(irt_work_item_range range, irt_wi_implementation* impl, irt_lw_data_item* params);

// on the WIN64 platform, function _irt_wi_trampoline will be called from a linked obj-file, which implements some
// functionality in assembly code for  -> thus its name must not be mangled by a c++ compiler -> wrap extern "C" around
#ifdef __cplusplus
extern "C" {
#endif
void
#if(defined(_M_IX86) && defined(_MSC_VER)) || (defined(__MINGW32__) && !defined(__MINGW64__))
    __fastcall
#endif
    _irt_wi_trampoline(irt_work_item* wi, wi_implementation_func* func);

#ifdef __cplusplus
}
#endif


irt_joinable irt_wi_run_optional(irt_work_item_range range, irt_wi_implementation* impl, irt_lw_data_item* params);

void irt_wi_join(irt_work_item_id wi_id);
void irt_wi_multi_join(uint32 num_wis, irt_work_item_id* wi_ids); // bad idea
void irt_wi_join_all(irt_work_item* wi);
void irt_wi_end(irt_work_item* wi);
void irt_wi_finalize(irt_worker* worker, irt_work_item* wi);

void irt_wi_split_uniform(irt_work_item* wi, uint32 elements, irt_work_item** out_wis);
void irt_wi_split_binary(irt_work_item* wi, irt_work_item* out_wis[2]);
void irt_wi_split(irt_work_item* wi, uint32 elements, uint64* offsets, irt_work_item** out_wis);


#endif // ifndef __GUARD_WORK_ITEM_H
