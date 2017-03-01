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
#ifndef __GUARD_IRT_SCHEDULING_H
#define __GUARD_IRT_SCHEDULING_H

#include "declarations.h"
#include "irt_joinable.h"

// List of available scheduling policies
#define IRT_SCHED_POLICY_STATIC 1
#define IRT_SCHED_POLICY_LAZY_BINARY_SPLIT 2
#define IRT_SCHED_POLICY_STEALING 3
#define IRT_SCHED_POLICY_STEALING_CIRCULAR 4
#define IRT_SCHED_POLICY_UBER 9000

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

/* Set the active degree of parallelism for the runtime system
 * Note that more threads may continue to run until a synchronization point is encountered
 */
void irt_scheduling_set_dop(uint32 parallelism);

/* Set the active degree of parallelism for the runtime system per socket
 * Requires Hwloc
 * Note that more threads may continue to run until a synchronization point is encountered
 */
void irt_scheduling_set_dop_per_socket(uint32 sockets, uint32* dops);

/* The scheduling iteration for a worker. Responsibilities include
 * executing work items assigned to the worker (and potentially distributing
 * and/or splitting them) as well as checking the process-wide event queue
 * from time to time.
 */
inline int irt_scheduling_iteration(irt_worker* self);

/* The scheduling loop which repeatedly runs scheduling iterations.
 * Should take care not to cause too much overhead when there is nothing
 * to schedule.
 */
void irt_scheduling_loop(irt_worker* self);

/* A notification function that should be called whenever a new wi
 * enters the queue of the target worker.
 */
void _irt_signal_worker(irt_worker* target);
#ifdef IRT_WORKER_SLEEPING
#define irt_signal_worker(__target) _irt_signal_worker(__target)
#else
#define irt_signal_worker(__target)
#endif

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
inline irt_joinable irt_scheduling_optional_wi(irt_worker* target, irt_work_item* wi);

/* Either runs implementation directly on the current worker, or creates a work item
 * for it and acts identically to irt_scheduling_assign_wi. The decision depends on the scheduling policy.
 */
inline irt_joinable irt_scheduling_optional(irt_worker* target, const irt_work_item_range* range, irt_wi_implementation* impl, irt_lw_data_item* args);

/* Work item yielding_wi yields on self.
 * Precondition: yielding_wi is self's current_wi
 */
void irt_scheduling_yield(irt_worker* self, irt_work_item* yielding_wi);

/* Prepare worker for sleep. Self must be executing the call.
 * returns true if sleep should proceed, false to stay awake
 */
bool irt_scheduling_worker_sleep(irt_worker* self);

#if IRT_SCHED_POLICY == IRT_SCHED_POLICY_STATIC
#include "sched_policies/irt_sched_static.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_LAZY_BINARY_SPLIT
#include "sched_policies/irt_sched_lazy_binary_splitting.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_STEALING
#include "sched_policies/irt_sched_stealing.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_STEALING_CIRCULAR
#include "sched_policies/irt_sched_stealing_circular.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_UBER
#include "sched_policies/irt_sched_uber.h"
#else
#error "No scheduling policy set"
#endif


#endif // ifndef __GUARD_IRT_SCHEDULING_H
