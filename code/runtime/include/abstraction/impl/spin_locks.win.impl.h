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
#ifndef __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_WIN_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_WIN_IMPL_H

#include "abstraction/spin_locks.h"

#define IRT_SPIN_LOCKED 1
#define IRT_SPIN_UNLOCKED 0
#define IRT_SPIN_DESTROYED -1 // makes lock variable unusable

void irt_spin_lock(irt_spinlock *lock){
	// if value at destination lock == IRT_SPIN_UNLOCKED, then it will be changed to IRT_SPIN_LOCKED and the loop
	// will exit, otherwise we loop until the condition is met and the lock can be set
	// InterlockedCompareExchange returns the previous value of the Destination (lock) parameter.
	while (IRT_SPIN_LOCKED == InterlockedCompareExchange(lock, IRT_SPIN_LOCKED, IRT_SPIN_UNLOCKED)){}
}

void irt_spin_unlock(irt_spinlock *lock){
	// if lock was set to IRT_SPIN_LOCKED then it will be set to IRT_SPIN_UNLOCKED, otherwise nothing happens
	InterlockedCompareExchange(lock, IRT_SPIN_UNLOCKED, IRT_SPIN_LOCKED);
}

int irt_spin_init(irt_spinlock *lock){
	*lock = IRT_SPIN_UNLOCKED;
	return 0;
}

void irt_spin_destroy(irt_spinlock *lock){
	IRT_ASSERT(InterlockedCompareExchange(lock, IRT_SPIN_DESTROYED, IRT_SPIN_UNLOCKED) == IRT_SPIN_UNLOCKED, IRT_ERR_INTERNAL, "Spin lock was either locked, destroyed or uninitialized when attempting to destroy lock.");
}

#endif // ifndef __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_WIN_IMPL_H
