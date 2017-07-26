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
#ifndef __GUARD_ABSTRACTION_SPIN_LOCKS_H
#define __GUARD_ABSTRACTION_SPIN_LOCKS_H

#include "irt_inttypes.h"

#if defined(_WIN32) && !defined(IRT_USE_PTHREADS)
typedef long irt_spinlock;
#elif defined(_GEMS_SIM)
typedef volatile int irt_spinlock;
#else
#include <pthread.h>
typedef pthread_spinlock_t irt_spinlock;
#endif

/** spin until lock is acquired */
inline void irt_spin_lock(irt_spinlock* lock);

/** release lock */
inline void irt_spin_unlock(irt_spinlock* lock);

/** initializing spin lock variable puts it in state unlocked. lock variable can not be shared by different processes */
inline int irt_spin_init(irt_spinlock* lock);

/**	destroy lock variable and free all used resources,
    will cause an error when attempting to destroy an object which is in any state other than unlocked */
inline void irt_spin_destroy(irt_spinlock* lock);

#endif // ifndef __GUARD_ABSTRACTION_SPIN_LOCKS_H
