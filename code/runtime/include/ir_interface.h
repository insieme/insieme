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
#ifndef __GUARD_IR_INTERFACE_H
#define __GUARD_IR_INTERFACE_H

#include "declarations.h"
#include "instrumentation_regions.h"
#include "utils/timing.h"
#include "utils/impl/timing.impl.h"
#include "work_item.h"
#include "work_group.h"

#include "irt_context.h"
#include "worker.h"
#include "irt_joinable.h"



typedef struct _irt_parallel_job {
	uint32 min;
	uint32 max;
	uint32 mod;
	irt_wi_implementation* impl;
	irt_lw_data_item* args;
} irt_parallel_job;

irt_joinable irt_joinable_null() {
	static irt_joinable null_joinable = { { { 0 } } };
	return null_joinable;
}

/** Distributes the provided work range over the given group. 
 *  Needs to be called by every work item within the group! (OMP semantics)
 */
void irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range range, irt_wi_implementation* impl, irt_lw_data_item* args);

/** From a job description structure, generates a number of parallel work items to perform the job,
 *  and puts them into a shared group. 
 *  The return value is a pointer to either a work item or a work group.
 *  To wait for the completion of the whole parallel job, use "irt_merge".
 */
irt_joinable irt_parallel(const irt_parallel_job* job);

/** Sets the default parallel wi count for the current wi to num_wis.
 */
void irt_set_default_parallel_wi_count(int num_wis);

/** From a job description structure, generates a single work item to perform the job.
 *  No group is created.
 *  The return value is a pointer to a work item.
 *  To wait for the completion of the whole parallel job, use "irt_merge".
 */
irt_joinable irt_task(const irt_parallel_job* job);

/** From a job description structure, generates a single work item to perform the job,
 *  and immediately executes it. 
 */
void irt_region(const irt_parallel_job* job);

/** Waits until a job launched by irt_parallel is finished.
 */
void irt_merge(irt_joinable joinable);

#define IRT_FLUSH(_bla) __sync_synchronize()

#define IRT_BUSYWHILE(_bla) while(_bla) { irt_scheduling_yield(irt_worker_get_current(), irt_wi_get_current()); }

#define par_printf printf

static inline double irt_get_wtime() {
	return irt_time_ns()/1000000000.0;
}

// a wrapper for a call marking the start of a region
static inline void ir_inst_region_start(irt_inst_region_id id) {
	irt_inst_region_start(id);
}

// a wrapper for a call marking the end of a region
static inline void ir_inst_region_end(irt_inst_region_id id) {
	irt_inst_region_end(id);
}

/** Picks a value in the range [begin, end[
 */
static inline uint32 irt_variant_pick(uint32 begin, uint32 end) {
	return begin;
} 

#endif // ifndef __GUARD_IR_INTERFACE_H
