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
#ifndef __GUARD_WORKER_H
#define __GUARD_WORKER_H

#include "declarations.h"
#include "abstraction/atomic.h"
#include "abstraction/threads.h"
#include "abstraction/spin_locks.h"
#include "work_item.h"
#include "irt_scheduling.h"
#include "utils/minlwt.h"
#include "instrumentation_events.h"
#include "utils/affinity.h"

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(worker)

typedef enum _irt_worker_state {
	IRT_WORKER_STATE_CREATED,
	IRT_WORKER_STATE_READY,
	IRT_WORKER_STATE_START,
	IRT_WORKER_STATE_RUNNING,
	IRT_WORKER_STATE_SLEEPING,
	IRT_WORKER_STATE_DISABLED,
	IRT_WORKER_STATE_WAITING,
	IRT_WORKER_STATE_STOP,
	IRT_WORKER_STATE_JOINED
} irt_worker_state;

struct _irt_worker {
	irt_worker_id id;
	uint64 generator_id;
	irt_affinity_mask affinity;
	irt_thread thread;
	lwt_context basestack;
	irt_context_id cur_context;
	irt_work_item* cur_wi;
	irt_work_item* finalize_wi;
	volatile irt_worker_state state;
	irt_worker_scheduling_data sched_data;
	irt_work_item lazy_wi;

	#ifdef IRT_WORKER_SLEEPING
	bool wake_signal;
	irt_cond_var wait_cond;
	#endif
	irt_cond_var dop_wait_cond;
	irt_spinlock shutdown_lock;

	uint32 default_variant;
	unsigned int rand_seed;

	#ifdef IRT_ASTEROIDEA_STACKS
	irt_work_item* share_stack_wi;
	#endif

	#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	clockid_t clockid;
	double app_time_total;
	double app_time_last_start;
	bool app_time_running;
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING

	#ifdef IRT_ENABLE_INSTRUMENTATION
	irt_instrumentation_event_data_table* instrumentation_event_data;
	#endif
	#ifdef IRT_OCL_INSTR
	irt_ocl_event_table* event_data;
	#endif

	#ifdef IRT_ENABLE_PROGRESS_REPORTING
	IRT_ATOMIC_TYPE(uint64) reported_progress;
	#endif // IRT_ENABLE_PROGRESS_REPORTING

	// memory reuse stuff
	irt_wi_event_register* wi_ev_register_list;
	irt_wg_event_register* wg_ev_register_list;
	irt_work_item* wi_reuse_stack;
	intptr_t* stack_reuse_stack;
};

typedef struct _irt_worker_init_signal {
	uint32 init_count;
	#if !defined(_WIN32) || (WINVER >= 0x0600)
	irt_cond_var init_condvar;
	#endif
	irt_mutex_obj init_mutex;
} irt_worker_init_signal;

/* ------------------------------ operations ----- */

static inline irt_worker* irt_worker_get_current() {
	IRT_ASSERT(irt_g_t_current_worker != NULL, IRT_ERR_INTERNAL, "Called irt_worker_get_current from a non-worker thread or before worker initialization.");
	return irt_g_t_current_worker;
}

void irt_worker_create(uint16 index, irt_affinity_mask affinity, irt_worker_init_signal* signal);
void irt_worker_late_init(irt_worker* self);
void _irt_worker_cancel_all_others();

void _irt_worker_switch_to_wi(irt_worker* self, irt_work_item* wi);
void _irt_worker_switch_from_wi(irt_worker* self, irt_work_item* wi);

void irt_worker_run_immediate_wi(irt_worker* self, irt_work_item* wi);
inline void irt_worker_run_immediate(irt_worker* target, const irt_work_item_range* range, irt_wi_implementation* impl, irt_lw_data_item* args);

void irt_worker_cleanup(irt_worker* self);

#ifdef IRT_VERBOSE
void _irt_worker_print_debug_info(irt_worker* self);
#endif


#endif // ifndef __GUARD_WORKER_H
