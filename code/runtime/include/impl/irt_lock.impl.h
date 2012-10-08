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

#include "irt_lock.h"
#include "irt_atomic.h"
#include "impl/irt_scheduling.impl.h"
#include "impl/worker.impl.h"

irt_lock* irt_lock_create() {
	irt_lock* ret = (irt_lock*)malloc(sizeof(irt_lock));
	ret->id = irt_generate_lock_id(IRT_LOOKUP_GENERATOR_ID_PTR);
	ret->locked = 0;
	return ret;
}

void irt_lock_acquire(irt_lock* lock) {
	while(!irt_atomic_bool_compare_and_swap(&lock->locked, 0, 1)) {
		irt_worker* cw = irt_worker_get_current();
		//IRT_INFO("Could not acquire lock: %lu\n", cw->cur_wi->id.full);
		irt_scheduling_yield(cw, cw->cur_wi);
	}
	//IRT_INFO("[[ acquired lock: %lu\n", irt_wi_get_current()->id.full);
}

void irt_lock_release(irt_lock* lock) {
	//IRT_INFO("]] released lock: %lu\n", irt_wi_get_current()->id.full);
	lock->locked = 0;
}
