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

// List of available scheduling policies
#define IRT_SCHED_POLICY_STATIC 1
#define IRT_SCHED_POLICY_LAZY_BINARY_SPLIT 2
#define IRT_SCHED_POLICY_STEALING 3
#define IRT_SCHED_POLICY_STEALING_CIRCULAR 4

// default scheduling policy
#ifndef IRT_SCHED_POLICY
#define IRT_SCHED_POLICY IRT_SCHED_POLICY_STATIC
#endif

/* Structure holding scheduling-related data in each worker thread.
 * To be specified by the scheduling policy.
 */
struct _irt_worker_scheduling_data;
typedef struct _irt_worker_scheduling_data irt_worker_scheduling_data;

/* Structure holding scheduling-related data for each work item.
 * To be specified by the scheduling policy.
 */
struct _irt_wi_scheduling_data;
typedef struct _irt_wi_scheduling_data irt_wi_scheduling_data;


/* The scheduling iteration for a worker. Responsibilities include
 * executing work items assigned to the worker (and potentially distributing
 * and/or splitting them) as well as checking the process-wide event queue
 * from time to time.
 */
int irt_scheduling_iteration(irt_worker* self);

/* The scheduling loop which repeatedly runs scheduling iterations.
 * Should take care not to cause too much overhead when there is nothing
 * to schedule.
 */
void irt_scheduling_loop(irt_worker* self);

/* A notification function that should be called whenever a new wi
 * enters the queue of the target worker.
 */
void irt_signal_worker(irt_worker* target);

/* Initialize scheduling-related data in the worker self
 */
void irt_scheduling_init_worker(irt_worker* self);

/* Assigns the work item wi to be executed by target. Since different
 * scheduling policies may manage wis differently, this needs to be provided
 * by the scheduling policy.
 */
void irt_scheduling_assign_wi(irt_worker* target, irt_work_item* wi);

/* Continues execution of already started wi on target. Since different
 * scheduling policies may manage wis differently, this needs to be provided
 * by the scheduling policy.
 */
static inline void irt_scheduling_continue_wi(irt_worker* target, irt_work_item* wi);

/* Either runs wi directly on the current worker, or acts identically to
 * irt_scheduling_assign_wi. The decision depends on the scheduling policy.
 */
inline irt_work_item* irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi);

/* Either runs implementation directly on the current worker, or creates a work item
 * for it and acts identically to irt_scheduling_assign_wi. The decision depends on the scheduling policy.
 */
inline irt_work_item* irt_scheduling_optional(irt_worker* target, const irt_work_item_range* range, 
	irt_wi_implementation_id impl_id, irt_lw_data_item* args);

/* Work item yielding_wi yields on self.
 * Precondition: yielding_wi is self's current_wi
 */
void irt_scheduling_yield(irt_worker* self, irt_work_item* yielding_wi);


#if IRT_SCHED_POLICY == IRT_SCHED_POLICY_STATIC
#include "sched_policies/irt_sched_static.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_LAZY_BINARY_SPLIT
#include "sched_policies/irt_sched_lazy_binary_splitting.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_STEALING
#include "sched_policies/irt_sched_stealing.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_STEALING_CIRCULAR
#include "sched_policies/irt_sched_stealing_circular.h"
#endif
