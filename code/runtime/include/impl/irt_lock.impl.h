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
	//uint64 start_t = irt_time_ns();
	//restart:
	irt_spin_lock(&lock->mutex);
	if(lock->locked) { // suspend if locked
		//if(irt_time_ns()-start_t < IRT_LOCK_NANOS_BUSY_WAIT) {
		//	irt_spin_unlock(&lock->mutex);
		//	irt_busy_nanosleep(IRT_LOCK_NANOS_INTERVAL);
		//	goto restart;
		//}
		irt_worker *wo = irt_worker_get_current();
		irt_work_item *wi = wo->cur_wi;
		locked_wi selflocked = {wi, wo, lock->top};
		lock->top = &selflocked;
		irt_spin_unlock(&lock->mutex);
		irt_inst_insert_wi_event(wo, IRT_INST_WORK_ITEM_SUSPENDED_LOCK, wi->id);
		_irt_worker_switch_from_wi(wo, wi);
	}
	else {   // acquire lock
		lock->locked = 1;
		irt_spin_unlock(&lock->mutex);
	}
}

void irt_lock_release(irt_lock* lock) {
	irt_spin_lock(&lock->mutex);
	if(lock->top) { // release a waiting task
		locked_wi *task = lock->top;
		lock->top = task->next;
		__irt_unused irt_worker* wo = task->worker;
		irt_scheduling_continue_wi(task->worker, task->wi);
		irt_signal_worker(wo);
	}
	else {   // none waiting, lock is now unlocked
		lock->locked = 0;
	}
	irt_spin_unlock(&lock->mutex);
}


#endif // ifndef __GUARD_IMPL_IRT_LOCK_IMPL_H
