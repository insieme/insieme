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
#include "irt_scheduling.h"
#include "utils/timing.h"
#include "impl/instrumentation.impl.h"

#if IRT_SCHED_POLICY == IRT_SCHED_POLICY_STATIC
#include "sched_policies/impl/irt_sched_static.impl.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_LAZY_BINARY_SPLIT
#include "sched_policies/impl/irt_sched_lazy_binary_splitting.impl.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_STEALING
#include "sched_policies/impl/irt_sched_stealing.impl.h"
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_STEALING_CIRCULAR
#include "sched_policies/impl/irt_sched_stealing_circular.impl.h"
#endif

#include <time.h>

void irt_scheduling_loop(irt_worker* self) {
	while(self->state != IRT_WORKER_STATE_STOP) {
#ifdef IRT_WORKER_SLEEPING
		self->have_wait_mutex = false;
#endif // IRT_WORKER_SLEEPING
		// while there is something to do, continue scheduling
		while(irt_scheduling_iteration(self)) 
			IRT_DEBUG("%sWorker %3d scheduled something.\n", self->id.thread==0?"":"\t\t\t\t\t\t", self->id.thread);
#ifdef IRT_WORKER_SLEEPING
		// check if self is the last worker
		uint32 active = irt_g_active_worker_count;
		if(active<=1) continue; // continue scheduling if last active
		if(!irt_atomic_bool_compare_and_swap(&irt_g_active_worker_count, active, active-1)) continue;
		// nothing to schedule, wait for signal
		IRT_DEBUG("%sWorker %3d trying sleep A.\n", self->id.thread==0?"":"\t\t\t\t\t\t", self->id.thread);
		irt_mutex_lock(&self->wait_mutex);
		IRT_DEBUG("%sWorker %3d trying sleep B.\n", self->id.thread==0?"":"\t\t\t\t\t\t", self->id.thread);
		// try one more scheduling iteration, in case something happened before lock
		self->have_wait_mutex = true;
		if(irt_scheduling_iteration(self) || self->state == IRT_WORKER_STATE_STOP) {
			IRT_DEBUG("%sWorker %3d rescheduling before sleep.\n", self->id.thread==0?"":"\t\t\t\t\t\t", self->id.thread);
			irt_atomic_inc(&irt_g_active_worker_count);
			continue;
		}
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SLEEP_START, self->id);
		IRT_DEBUG("%sWorker %3d actually sleeping.\n", self->id.thread==0?"":"\t\t\t\t\t\t", self->id.thread);
		int wait_err = irt_cond_wait(&self->wait_cond, &self->wait_mutex);
		IRT_ASSERT(wait_err == 0, IRT_ERR_INTERNAL, "Worker failed to wait on scheduling condition");
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_SLEEP_END, self->id);
		IRT_DEBUG("%sWorker %3d woken.\n", self->id.thread==0?"":"\t\t\t\t\t\t", self->id.thread);
		// we were woken up by the signal and now own the mutex
		irt_mutex_unlock(&self->wait_mutex);
		irt_atomic_inc(&irt_g_active_worker_count);
#endif // IRT_WORKER_SLEEPING
	}
}

void irt_signal_worker(irt_worker* target) {
#ifdef IRT_WORKER_SLEEPING
	if(irt_g_active_worker_count < irt_g_worker_count) {
		irt_mutex_lock(&target->wait_mutex);
		irt_cond_wake_one(&target->wait_cond);
		//IRT_DEBUG("%sWorker %3d signaled.\n", target->id.thread==0?"":"\t\t\t\t\t\t", target->id.thread);
		irt_mutex_unlock(&target->wait_mutex);
	}
#endif // IRT_WORKER_SLEEPING
}
