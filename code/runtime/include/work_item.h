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

#include "declarations.h"
#include "performance_table.h"

#include "irt_context.h"
#include "utils/minlwt.h"
#include "irt_scheduling.h"

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(work_item);

typedef enum _irt_work_item_state {
	IRT_WI_STATE_NEW, IRT_WI_STATE_STARTED, IRT_WI_STATE_DONE, 
} irt_work_item_state;

struct _irt_work_item_range {
	int64 begin, end, step;
};
static const irt_work_item_range irt_g_wi_range_one_elem = {0,1,1};
static inline int64 irt_wi_range_get_size(const irt_work_item_range* r) { return (r->end - r->begin) / r->step; }
static inline void _irt_print_work_item_range(const irt_work_item_range* r);

typedef bool irt_wi_readiness_check_fun(irt_work_item* wi);
typedef struct _irt_wi_readiness_check {
	irt_wi_readiness_check_fun *fun;
	void *data;
} irt_wi_readiness_check;
irt_wi_readiness_check irt_g_null_readiness_check = { NULL, NULL };

struct _irt_work_item {
	// core functionality
	irt_work_item_id id;
	irt_work_item_id parent_id;
	irt_context_id context_id;
	irt_wi_implementation_id impl_id;
	irt_work_item_range range;
	uint32 num_groups;
	uint32 num_active_children;
	irt_wi_wg_membership *wg_memberships;
	volatile irt_work_item_state state;
	irt_lw_data_item *parameters;
	// wi splitting related
	irt_work_item_id source_id;
	uint32 num_fragments;
	// private implementation details, do not need to be migrated
	irt_work_item *next_reuse;
	lwt_context stack_ptr;
	lwt_reused_stack* stack_storage;
	irt_wi_scheduling_data sched_data;
	// region association for instrumentation
#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	irt_region* region;
	uint64 last_timestamp;
#endif
};

/* ------------------------------ operations ----- */

static inline irt_work_item* irt_wi_get_current();

static inline bool irt_wi_is_fragment(irt_work_item *wi) { return wi->source_id.value.full != irt_work_item_null_id().value.full; }
static inline irt_wi_wg_membership irt_wi_get_wg_membership(irt_work_item *wi, uint32 index);
static inline uint32 irt_wi_get_wg_num(irt_work_item *wi, uint32 index);
static inline uint32 irt_wi_get_wg_size(irt_work_item *wi, uint32 index);
static inline irt_work_group* irt_wi_get_wg(irt_work_item *wi, uint32 index);

irt_work_item* _irt_wi_create(irt_worker* self, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* params);
static inline irt_work_item* irt_wi_create(irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* params);

// on the WIN64 platform, function _irt_wi_trampoline will be called from a linked obj-file, which implements some 
// functionality in assembly code for  -> thus its name must not be mangled by a c++ compiler -> wrap extern "C" around
#ifdef __cplusplus
extern "C" {
#endif
	void
	#ifdef _M_IX86
	__fastcall
	#endif
	_irt_wi_trampoline(irt_work_item *wi, wi_implementation_func* func);

#ifdef __cplusplus
}
#endif

irt_work_item* irt_wi_run_optional(irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* params);

void irt_wi_join(irt_work_item* wi);
void irt_wi_multi_join(uint32 num_wis, irt_work_item** wis); // bad idea
void irt_wi_end(irt_work_item* wi);
void irt_wi_join_all(irt_work_item* wi);

void irt_wi_split_uniform(irt_work_item* wi, uint32 elements, irt_work_item** out_wis);
void irt_wi_split_binary(irt_work_item* wi, irt_work_item* out_wis[2]);
void irt_wi_split(irt_work_item* wi, uint32 elements, uint64* offsets, irt_work_item** out_wis);
