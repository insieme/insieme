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

#ifndef IRT_CWBUFFER_LENGTH
#define IRT_CWBUFFER_LENGTH 16
#endif

#include "utils/circular_work_buffers.h"

typedef struct _irt_cw_data {
	irt_circular_work_buffer queue;
	irt_circular_work_buffer pool;
#ifdef IRT_TASK_OPT
	int64 demand;
#endif //IRT_TASK_OPT
} irt_cw_data;

#define irt_worker_scheduling_data irt_cw_data

// placeholder, not required
#define irt_wi_scheduling_data uint32

#ifdef IRT_TASK_OPT
inline uint32 irt_scheduling_select_taskopt_variant(irt_work_item* wi, irt_worker* wo);
#endif //IRT_TASK_OPT

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
