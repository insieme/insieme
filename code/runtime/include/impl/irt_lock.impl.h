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
#ifndef __GUARD_IMPL_IRT_LOCK_IMPL_H
#define __GUARD_IMPL_IRT_LOCK_IMPL_H

#include "declarations.h"
#include "irt_lock.h"
#include "abstraction/impl/threads.impl.h"
#include "abstraction/unused.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/worker.impl.h"
#include "utils/impl/minlwt.impl.h"

#define IRT_LOCK_NANOS_BUSY_WAIT 10000
#define IRT_LOCK_NANOS_INTERVAL 500

void irt_lock_init(irt_lock* lock) {
	irt_spin_init(&lock->mutex);
	lock->top = NULL;
	lock->locked = 0;
}

void irt_lock_acquire(irt_lock* lock) {
	// uint64 start_t = irt_time_ns();
	// restart:
	irt_spin_lock(&lock->mutex);
	if(lock->locked) { // suspend if locked
		// if(irt_time_ns()-start_t < IRT_LOCK_NANOS_BUSY_WAIT) {
		//	irt_spin_unlock(&lock->mutex);
		//	irt_busy_nanosleep(IRT_LOCK_NANOS_INTERVAL);
		//	goto restart;
		//}
		irt_worker* wo = irt_worker_get_current();
		irt_work_item* wi = wo->cur_wi;
		locked_wi selflocked = {wi, wo, lock->top};
		lock->top = &selflocked;
		irt_spin_unlock(&lock->mutex);
		irt_inst_insert_wi_event(wo, IRT_INST_WORK_ITEM_SUSPENDED_LOCK, wi->id);
		_irt_worker_switch_from_wi(wo, wi);
	} else { // acquire lock
		lock->locked = 1;
		irt_spin_unlock(&lock->mutex);
	}
}

void irt_lock_release(irt_lock* lock) {
	irt_spin_lock(&lock->mutex);
	if(lock->top) { // release a waiting task
		locked_wi* task = lock->top;
		lock->top = task->next;
		__irt_unused irt_worker* wo = task->worker;
		irt_scheduling_continue_wi(task->worker, task->wi);
		irt_signal_worker(wo);
	} else { // none waiting, lock is now unlocked
		lock->locked = 0;
	}
	irt_spin_unlock(&lock->mutex);
}

bool irt_lock_tryacquire(irt_lock* lock) {
	irt_spin_lock(&lock->mutex);
	if(lock->locked) {
		// return immediately as lock is already acquired
		irt_spin_unlock(&lock->mutex);
		return false;
	}

	lock->locked = 1;
	irt_spin_unlock(&lock->mutex);
	return true;
}

#endif // ifndef __GUARD_IMPL_IRT_LOCK_IMPL_H
