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
#ifndef __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_PTHREADS_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_PTHREADS_IMPL_H

#include "abstraction/spin_locks.h"

// implementation of threads using pthread library; will be used for both Linux and Windows.

void irt_spin_lock(irt_spinlock* lock) {
	pthread_spin_lock(lock);
}

void irt_spin_unlock(irt_spinlock* lock) {
	pthread_spin_unlock(lock);
}

int irt_spin_init(irt_spinlock* lock) {
	return pthread_spin_init(lock, PTHREAD_PROCESS_PRIVATE);
}

void irt_spin_destroy(irt_spinlock* lock) {
	pthread_spin_destroy(lock);
}

#endif // ifndef __GUARD_ABSTRACTION_IMPL_SPIN_LOCKS_PTHREADS_IMPL_H
