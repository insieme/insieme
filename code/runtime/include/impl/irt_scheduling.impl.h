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
#elif IRT_SCHED_POLICY == IRT_SCHED_POLICY_UBER
#include "sched_policies/impl/irt_sched_uber.impl.h"
#endif

#include <time.h>

void irt_scheduling_loop(irt_worker* self) {
	while(self->state != IRT_WORKER_STATE_STOP) {
#ifdef IRT_WORKER_SLEEPING
#endif // IRT_WORKER_SLEEPING
		// while there is something to do, continue scheduling
		while(irt_scheduling_iteration(self)) {
			IRT_DEBUG("%sWorker %3d scheduled something.\n", self->id.thread==0?"":"\t\t\t\t\t\t", self->id.thread);
			self->cur_wi->state = IRT_WI_STATE_SUSPENDED;
			self->cur_wi = NULL;
			if(self->finalize_wi != NULL) {
				irt_wi_finalize(self, self->finalize_wi);
				self->finalize_wi = NULL;
			}
			#ifdef IRT_ASTEROIDEA_STACKS
			if(self->share_stack_wi != NULL) {
				IRT_DEBUG(" ° %p allowing stack stealing: %d children\n", self->share_stack_wi, *self->share_stack_wi->num_active_children);
				self->share_stack_wi->stack_available = true;
				self->share_stack_wi = NULL;
			}
			#endif //IRT_ASTEROIDEA_STACKS
		}
#ifdef IRT_WORKER_SLEEPING
		irt_mutex_lock(&irt_g_active_worker_mutex);
		// check if self is the last worker
		if(irt_g_active_worker_count <= 1 || self->wake_signal) {
			self->wake_signal = false;
			irt_mutex_unlock(&irt_g_active_worker_mutex);
			continue;
		}
		if(!irt_scheduling_worker_sleep(self)) {
			irt_mutex_unlock(&irt_g_active_worker_mutex);
			continue;
		}
		irt_g_active_worker_count--;
		// nothing to schedule, wait for signal
		int wait_err = irt_cond_wait(&self->wait_cond, &irt_g_active_worker_mutex);
		IRT_ASSERT(wait_err == 0, IRT_ERR_INTERNAL, "Worker failed to wait on scheduling condition");
		// we were woken up by the signal and now own the mutex
		irt_g_active_worker_count++;
		irt_atomic_val_compare_and_swap(&self->state, IRT_WORKER_STATE_SLEEPING, IRT_WORKER_STATE_RUNNING);
		irt_mutex_unlock(&irt_g_active_worker_mutex);
#endif // IRT_WORKER_SLEEPING
	}
}

void irt_signal_worker(irt_worker* target) {
#ifdef IRT_WORKER_SLEEPING
	irt_mutex_lock(&irt_g_active_worker_mutex);
	target->wake_signal = true;
	irt_cond_wake_one(&target->wait_cond);
	IRT_DEBUG("%sWorker %3d signaled.\n", target->id.thread==0?"":"\t\t\t\t\t\t", target->id.thread);
	irt_mutex_unlock(&irt_g_active_worker_mutex);
#endif // IRT_WORKER_SLEEPING
}
