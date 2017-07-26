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
#ifndef __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_WIN_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_WIN_IMPL_H

#include "abstraction/spin_locks.h"

#define IRT_SPIN_LOCKED 1
#define IRT_SPIN_UNLOCKED 0
#define IRT_SPIN_DESTROYED -1 // makes lock variable unusable

void irt_spin_lock(irt_spinlock* lock) {
	// if value at destination lock == IRT_SPIN_UNLOCKED, then it will be changed to IRT_SPIN_LOCKED and the loop
	// will exit, otherwise we loop until the condition is met and the lock can be set
	// InterlockedCompareExchange returns the previous value of the Destination (lock) parameter.
	while(IRT_SPIN_LOCKED == InterlockedCompareExchange(lock, IRT_SPIN_LOCKED, IRT_SPIN_UNLOCKED)) {
	}
}

void irt_spin_unlock(irt_spinlock* lock) {
	// if lock was set to IRT_SPIN_LOCKED then it will be set to IRT_SPIN_UNLOCKED, otherwise nothing happens
	InterlockedCompareExchange(lock, IRT_SPIN_UNLOCKED, IRT_SPIN_LOCKED);
}

int irt_spin_init(irt_spinlock* lock) {
	*lock = IRT_SPIN_UNLOCKED;
	return 0;
}

void irt_spin_destroy(irt_spinlock* lock) {
	IRT_ASSERT(InterlockedCompareExchange(lock, IRT_SPIN_DESTROYED, IRT_SPIN_UNLOCKED) == IRT_SPIN_UNLOCKED, IRT_ERR_INTERNAL,
	           "Spin lock was either locked, destroyed or uninitialized when attempting to destroy lock.");
}

#endif // ifndef __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_WIN_IMPL_H
