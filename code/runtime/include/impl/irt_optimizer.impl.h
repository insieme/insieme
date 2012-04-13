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

#include "utils/timing.h"
#include "irt_optimizer.h"
#include "impl/irt_context.impl.h"

#include "optimizers/opencl_optimizer.h"
#include "optimizers/shared_mem_effort_estimate_external_load_optimizer.h"

#ifndef IRT_RUNTIME_TUNING

void irt_optimizer_context_startup(irt_context *context) { }
void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group) { }
void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 time, irt_loop_sched_data *sched_data) { }

#else // ifndef IRT_RUNTIME_TUNING

///////////////////////////////////// Context =========================================================================

void irt_optimizer_context_startup(irt_context *context) {
	irt_shared_mem_effort_estimate_external_load_optimizer_context_startup(context);
	irt_opencl_optimizer_context_startup(context); // OpenCL startup
}

///////////////////////////////////// Loops ===========================================================================

void irt_optimizer_starting_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, irt_work_group* group) {
	irt_wi_implementation *impl = &irt_context_get_current()->impl_table[impl_id];

	if(impl->variants[0].features.opencl) {
		irt_opencl_optimizer_starting_pfor(impl, range, group);
	} else {
		irt_shared_mem_effort_estimate_external_load_optimizer_starting_pfor(impl, range, group);
	}
}

#ifndef IRT_RUNTIME_TUNING_EXTENDED

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, uint64 walltime, irt_loop_sched_data* sched_data) {

#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	irt_wi_implementation *impl = &irt_context_get_current()->impl_table[impl_id];

	if(impl->variants[0].features.implicit_region_id >= 0) {
		_irt_aggregated_instrumentation_insert_pfor(impl->variants[0].features.implicit_region_id, walltime, sched_data);
	}
#endif
}

#else

void irt_optimizer_completed_pfor(irt_wi_implementation_id impl_id, irt_work_item_range range, uint64 total_time, irt_loop_sched_data *sched_data) {
	if(impl[0].features.opencl) {
		// nothing
	} else {
		irt_shared_mem_effort_estimate_external_load_optimizer_completed_pfor(impl_id, range, total_time, sched_data);
	}
}

#endif

#endif // ifndef IRT_RUNTIME_TUNING
