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

#include "irt_optimizer.h"

void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group) {
	// TODO
	printf("Starting the scheduler optimizer\n");
	// first thing. try dynamic 
	irt_loop_sched_policy dynamic10 = (irt_loop_sched_policy){IRT_DYNAMIC_CHUNKED, 1024, { 10 }};
	irt_wg_set_loop_scheduling_policy(group, &dynamic10);
	//irt_wg_set_loop_scheduling_policy(group, (irt_loop_sched_policy){IRT_DYNAMIC_CHUNKED, 20, 1024});
	//irt_wg_set_loop_scheduling_policy(group, (irt_loop_sched_policy){IRT_DYNAMIC_CHUNKED, 30, 1024});
	//irt_wg_set_loop_scheduling_policy(group, (irt_loop_sched_policy){IRT_DYNAMIC_CHUNKED, 40, 1024});
	//irt_wg_set_loop_scheduling_policy(group, (irt_loop_sched_policy){IRT_DYNAMIC_CHUNKED, 50, 1024});
	//irt_wg_set_loop_scheduling_policy(group, (irt_loop_sched_policy){IRT_DYNAMIC_CHUNKED, 60, 1024});
	//irt_wg_set_loop_scheduling_policy(group, (irt_loop_sched_policy){IRT_DYNAMIC_CHUNKED, 70, 1024});
}

#ifndef IRT_RUNTIME_TUNING_EXTENDED

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 time) {
	// TODO
	printf("Completed pfor % 3d, time: % 10ld\n", impl_id, time);
}

#else

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 total_time, uint64 *participant_times, uint32 num_participants) {
	// TODO
	printf("%d % 10lu\n",impl_id,total_time);
	//printf("Extended Completed pfor % 3d, time: % 10lu, individual times:\n", impl_id, total_time);
	//for(uint32 i=0; i<num_participants; ++i) {
	//	printf("% 2u: % 10lu\n", i, participant_times[i]);
	//}
}

#endif
