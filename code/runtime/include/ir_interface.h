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
#include "utils/timing.h"
#include "utils/impl/timing.impl.h"

typedef struct _irt_parallel_job {
	uint32 min;
	uint32 max;
	uint32 mod;
	irt_wi_implementation_id impl_id;
	irt_lw_data_item* args;
} irt_parallel_job;

/** Creates a work item and immediately enqueues it on the current worker. 
 *  Use "irt_wi_join" on the return value to wait until it finishes.  
 */
irt_work_item* irt_wi_create_and_run(irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* args);

/** Distributes the provided work range over the given group. 
 *  Needs to be called by every work item within the group! (OMP semantics)
 */
void irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* args);

/** From a job description structure, generates a number of parallel work items to perform the job,
 *  and puts them into a shared group. This group is returned. To wait for the completion of the
 *  whole parallel job, use "irt_wg_join".
 */
irt_work_group* irt_parallel(irt_work_group* parent, const irt_parallel_job* job);

#define IRT_FLUSH(_bla) __sync_synchronize()

#define par_printf printf

double omp_get_wtime() {
	return irt_time_ms()/1000.0;
}
