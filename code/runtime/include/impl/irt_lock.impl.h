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
#include "irt_lock.h"
#include "abstraction/impl/threads.impl.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/worker.impl.h"
#include "utils/impl/minlwt.impl.h"

void irt_lock_init(irt_lock* lock) {
	irt_spin_init(&lock->mutex);
	lock->top = NULL;
	lock->locked = 0;
}

void irt_lock_acquire(irt_lock* lock) {
	irt_spin_lock(&lock->mutex);
	if(lock->locked) { // suspend if locked
		irt_worker *wo = irt_worker_get_current();
		irt_work_item *wi = wo->cur_wi;
		locked_wi selflocked = {wi, wo, lock->top};
		lock->top = &selflocked;
		irt_spin_unlock(&lock->mutex);
		wo->cur_wi = NULL;
		lwt_continue(&wo->basestack, &wi->stack_ptr);
	} else { // acquire lock
		lock->locked = 1;
	}
	irt_spin_unlock(&lock->mutex);
}

void irt_lock_release(irt_lock* lock) {
	irt_spin_lock(&lock->mutex);
	if(lock->top) { // release a waiting task
		locked_wi *task = lock->top;
		lock->top = task->next;
		irt_scheduling_continue_wi(task->worker, task->wi);
	} else { // none waiting, lock is now unlocked
		lock->locked = 0;
	}
	irt_spin_unlock(&lock->mutex);	
}
