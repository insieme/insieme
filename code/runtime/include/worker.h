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

#include <pthread.h>

#include "work_item.h"
#include "irt_scheduling.h"
#include "utils/minlwt.h"
#include "instrumentation.h"
#include "utils/affinity.h"

#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
#include "papi.h"
#endif

#ifdef USE_OPENCL
#include "irt_ocl.h"
#endif

/* ------------------------------ data structures ----- */

IRT_MAKE_ID_TYPE(worker);

typedef enum _irt_worker_state {
	IRT_WORKER_STATE_CREATED, IRT_WORKER_STATE_READY, IRT_WORKER_STATE_START, IRT_WORKER_STATE_RUNNING, IRT_WORKER_STATE_WAITING, IRT_WORKER_STATE_STOP
} irt_worker_state;

struct _irt_worker {
	irt_worker_id id;
	uint64 generator_id;
	irt_affinity_mask affinity;
	irt_thread pthread;
	lwt_context basestack;
	irt_context_id cur_context;
	irt_work_item* cur_wi;
	volatile irt_worker_state state;
	irt_worker_scheduling_data sched_data;
	irt_work_item lazy_wi;
	
	bool have_wait_mutex;
	pthread_cond_t wait_cond;
	pthread_mutex_t wait_mutex;

	uint32 default_variant;

#ifdef IRT_ENABLE_INSTRUMENTATION
	irt_instrumentation_event_data_table* instrumentation_event_data;
#endif
#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	irt_instrumentation_region_data_table* instrumentation_region_data;
	int32 irt_papi_event_set;
	int32 irt_papi_number_of_events;
#endif
#ifdef IRT_OCL_INSTR
	irt_ocl_event_table* event_data;
#endif

	// memory reuse stuff
	irt_wi_event_register *wi_ev_register_list;
	irt_wg_event_register *wg_ev_register_list;
	irt_work_item *wi_reuse_stack;
	intptr_t *stack_reuse_stack;
	irt_region_list* region_reuse_list;
};

typedef struct _irt_worker_init_signal {
	uint32 init_count;
	pthread_cond_t init_condvar;
	pthread_mutex_t init_mutex;
} irt_worker_init_signal;

/* ------------------------------ operations ----- */

static inline irt_worker* irt_worker_get_current() {
	return (irt_worker*)pthread_getspecific(irt_g_worker_key);
}

void irt_worker_create(uint16 index, irt_affinity_mask affinity, irt_worker_init_signal* signal);
void _irt_worker_cancel_all_others();

void _irt_worker_switch_to_wi(irt_worker* self, irt_work_item *wi);

void irt_worker_run_immediate_wi(irt_worker* self, irt_work_item *wi);
inline void irt_worker_run_immediate(irt_worker* target, const irt_work_item_range* range, irt_wi_implementation_id impl_id, irt_lw_data_item* args);

void irt_worker_cleanup(irt_worker* self);

#ifdef IRT_VERBOSE
void _irt_worker_print_debug_info(irt_worker* self);
#endif
