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

#include "declarations.h"

#ifdef IRT_ENABLE_INSTRUMENTATION
void irt_dbg_print_worker_events(int32 wid, int32 num) {
	int32 s = irt_g_workers[wid]->instrumentation_event_data->number_of_elements-1;
	for(uint32 i = s; i>=0 && i>s-num; --i) {
		irt_inst_event_data_output_single(irt_g_workers[wid]->instrumentation_event_data->data[i], stdout, true);
	}
}
#else
void irt_dbg_print_worker_events(int32 wid, int32 num) {
}
#endif

const char* irt_dbg_get_worker_state_string(irt_worker_state state) {
	switch(state) {
	case IRT_WORKER_STATE_CREATED: return "IRT_WORKER_STATE_CREATED";
	case IRT_WORKER_STATE_READY: return "IRT_WORKER_STATE_READY";
	case IRT_WORKER_STATE_START: return "IRT_WORKER_STATE_START";
	case IRT_WORKER_STATE_RUNNING: return "IRT_WORKER_STATE_RUNNING";
	case IRT_WORKER_STATE_SLEEPING: return "IRT_WORKER_STATE_SLEEPING";
	case IRT_WORKER_STATE_WAITING: return "IRT_WORKER_STATE_WAITING";
	case IRT_WORKER_STATE_STOP: return "IRT_WORKER_STATE_STOP";
	default: return "IRT_WORKER_STATE_UNKNOWN";
	}
}

void irt_dbg_print_worker_state(int32 wid) {
	printf("Worker #%03d: %32s - q:%4d\n", wid, irt_dbg_get_worker_state_string(irt_g_workers[wid]->state), 
		irt_cwb_size(&irt_g_workers[wid]->sched_data.queue));
	irt_dbg_print_worker_events(wid, 1);
}

void irt_dbg_print_worker_overview() {
	for(int32 w=0; w<irt_g_worker_count; ++w) {
		irt_dbg_print_worker_state(w);
	}
}
