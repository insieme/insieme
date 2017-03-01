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
#ifndef __GUARD_IRT_LOOP_SCHED_H
#define __GUARD_IRT_LOOP_SCHED_H

#include "declarations.h"
#include "irt_optimizer.h"
#include "insieme/common/common.h"
#ifdef __cplusplus
using namespace insieme::common;
#endif // __cplusplus

struct _irt_loop_sched_policy {
	irt_loop_sched_policy_type type;
	uint32 participants;
	union {
		int32 chunk_size;
		uint64* boundaries;
		double shares[IRT_MAX_WORKERS];
	} param;
};

static irt_loop_sched_policy irt_g_loop_sched_policy_default;
static irt_loop_sched_policy irt_g_loop_sched_policy_single;

struct _irt_loop_sched_data {
	irt_loop_sched_policy policy;
	volatile uint64 completed;
	volatile uint64 block_size;
	#ifdef IRT_RUNTIME_TUNING
	volatile uint32 participants_complete;
	uint64 start_time;
	#ifdef IRT_RUNTIME_TUNING_EXTENDED
	uint64* part_times;
	#endif
	#endif
};

// schedule a loop using the policy specified for this group
// runs the optimizer and collects instrumentation data if the IRT_RUNTIME_TUNING flag is active
inline static void irt_schedule_loop(irt_work_item* self, irt_work_group* group, irt_work_item_range base_range, irt_wi_implementation* impl,
                                     irt_lw_data_item* args);

// sets the scheduling policy for the given group
// it will activate upon reaching the next loop
void irt_wg_set_loop_scheduling_policy(irt_work_group* group, const irt_loop_sched_policy* policy);


#endif // ifndef __GUARD_IRT_LOOP_SCHED_H
