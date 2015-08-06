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
		uint64 *boundaries;
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
	uint64 *part_times;
#endif
#endif
};

// schedule a loop using the policy specified for this group
// runs the optimizer and collects instrumentation data if the IRT_RUNTIME_TUNING flag is active
inline static void irt_schedule_loop(
    irt_work_item* self, irt_work_group* group, irt_work_item_range base_range,
    irt_wi_implementation* impl, irt_lw_data_item* args);

// sets the scheduling policy for the given group
// it will activate upon reaching the next loop
void irt_wg_set_loop_scheduling_policy(irt_work_group* group, const irt_loop_sched_policy* policy);


#endif // ifndef __GUARD_IRT_LOOP_SCHED_H
