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

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(work_group);

struct _irt_work_group {
	irt_work_group_id id;
	bool distributed;			// starts at false, set to true if part of the group is not on the same shared memory node
	irt_worker_id coordinator;  // only set if distributed == true
	/* implementation stuff */
	uint32 local_member_count;
	uint32 cur_barrier_count_up;
	uint32 cur_barrier_count_down;
};


/* ------------------------------ operations ----- */

irt_work_group* irt_wg_create();
void irt_wg_destroy(irt_work_group* wg);

//static inline void irt_wg_join(irt_work_group* wg);
//static inline void irt_wg_leave(irt_work_group* wg);

void irt_wg_insert(irt_work_group* wg, irt_work_item* wi);
void irt_wg_remove(irt_work_group* wg, irt_work_item* wi);

void irt_wg_barrier(irt_work_group* wg);
void irt_wg_distribute(irt_work_group* wg, irt_distribute_id dist /*, ???*/);

