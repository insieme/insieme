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
#ifndef __GUARD_SCHED_POLICIES_IRT_SCHED_STEALING_CIRCULAR_H
#define __GUARD_SCHED_POLICIES_IRT_SCHED_STEALING_CIRCULAR_H

#include "declarations.h"

#ifndef IRT_CWBUFFER_LENGTH
#define IRT_CWBUFFER_LENGTH 16
#endif

#include "utils/circular_work_buffers.h"

typedef struct _irt_cw_data {
	irt_circular_work_buffer queue;
	irt_work_item* overflow_stack;
	irt_spinlock overflow_stack_lock;
	#ifdef IRT_TASK_OPT
	int64 demand;
	#endif // IRT_TASK_OPT
} irt_cw_data;

#define irt_worker_scheduling_data irt_cw_data

// placeholder, not required
#define irt_wi_scheduling_data uint32

#ifdef IRT_TASK_OPT
inline uint32 irt_scheduling_select_taskopt_variant(irt_work_item* wi, irt_worker* wo);
#endif // IRT_TASK_OPT

#if 0
///////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// 64 bit triplet implementation ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct _irt_circular_work_buffer {
	uint64 front_free;
	uint64 back_free;
	int64 size;
	irt_work_item* items[IRT_CWBUFFER_LENGTH];
} irt_circular_work_buffer;

typedef struct _irt_cw_data {
	irt_circular_work_buffer queue;
	irt_circular_work_buffer pool;
} irt_cw_data;

#define irt_worker_scheduling_data irt_cw_data

// placeholder, not required
#define irt_wi_scheduling_data uint32

#endif // 64 bit triplet implementation


#endif // ifndef __GUARD_SCHED_POLICIES_IRT_SCHED_STEALING_CIRCULAR_H
