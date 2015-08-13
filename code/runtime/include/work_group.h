/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_WORK_GROUP_H
#define __GUARD_WORK_GROUP_H

#include "declarations.h"
#include "irt_loop_sched.h"

#include "abstraction/threads.h"

#define IRT_BARRIER_HYBRID_TICKS 500000ul

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(work_group)

struct _irt_work_group {
	irt_work_group_id id;
	// bool distributed;	// starts at false, set to true if part of the group is not on the same shared memory node
	// irt_worker_id coordinator;  // only set if distributed == true
	/* implementation stuff */
	irt_spinlock lock;
	volatile uint32 local_member_count;
	volatile uint32 ended_member_count;
	volatile uint32 cur_barrier_count;
	volatile uint32 tot_barrier_count;
	void** redistribute_data_array;
	volatile uint32 pfor_count;        // index of the most recently added pfor
	volatile uint32 joined_pfor_count; // index of the latest joined pfor
	irt_loop_sched_policy cur_sched;   // current scheduling policy
	irt_loop_sched_data loop_sched_data[IRT_WG_RING_BUFFER_SIZE];
	#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	volatile uint64 regions_started;
	volatile uint64 regions_ended;
	volatile uint64* region_data_entries;
	volatile uint64* region_data_exits;
	volatile uint64 region_completions_required[IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE];
	volatile irt_inst_region_wi_data** region_data; //[IRT_INST_REGION_INSTRUMENTATION_RING_BUFFER_SIZE];
	#endif                                          // IRT_ENABLE_REGION_INSTRUMENTATION
};

struct _irt_wi_wg_membership {
	irt_work_group_id wg_id;
	uint32 num;
	uint32 pfor_count;
};

typedef void irt_wg_redistribution_function(void** collected, uint32 local_id, uint32 num_participants, void* out_result);

/* ------------------------------ operations ----- */

irt_work_group* _irt_wg_create(irt_worker* self);
irt_work_group* irt_wg_create();
void irt_wg_destroy(irt_work_group* wg);

static inline void _irt_wg_end_member(irt_work_group* wg);

// inline void irt_wg_join(irt_work_group* wg);
// inline void irt_wg_leave(irt_work_group* wg);

void irt_wg_insert(irt_work_group* wg, irt_work_item* wi);
void irt_wg_remove(irt_work_group* wg, irt_work_item* wi);

static inline uint32 irt_wg_get_wi_num(irt_work_group* wg, irt_work_item* wi);
static inline irt_wi_wg_membership* irt_wg_get_wi_membership(irt_work_group* wg, irt_work_item* wi);

void irt_wg_barrier(irt_work_group* wg);
void irt_wg_joining_barrier(irt_work_group* wg);
void irt_wg_redistribute(irt_work_group* wg, irt_work_item* this_wi, void* my_data, void* result_data, irt_wg_redistribution_function* func);
void irt_wg_join(irt_work_group_id wg_id);

#endif // ifndef __GUARD_WORK_GROUP_H
