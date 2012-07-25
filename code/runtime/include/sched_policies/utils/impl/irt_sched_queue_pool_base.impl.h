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

#include "sched_policies/utils/irt_sched_queue_pool_base.h"

IRT_DEFINE_DEQUE(work_item, sched_data.work_deque_next, sched_data.work_deque_prev);
IRT_DEFINE_COUNTED_DEQUE(work_item, sched_data.work_deque_next, sched_data.work_deque_prev);

static inline void irt_scheduling_continue_wi(irt_worker* target, irt_work_item* wi) {
	irt_work_item_deque_insert_back(&target->sched_data.pool, wi);
	irt_signal_worker(target);
}

irt_work_item* irt_scheduling_optional(irt_worker* target, const irt_work_item_range* range, irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
	if(irt_g_worker_count == 1 || target->sched_data.queue.size > irt_g_worker_count+15) {
		//printf("WO %d lazy: queued %d, address: %p\n", target->id.value.components.index, target->sched_data.queue.size,
		//	&target->sched_data.queue.size);
		irt_work_item *self = target->cur_wi;
		irt_lw_data_item *prev_args = self->parameters;
		irt_work_item_range prev_range = self->range;
		self->parameters = args;
		self->range = *range;
		(irt_context_table_lookup(target->cur_context)->impl_table[impl_id].variants[0].implementation)(self);
		self->parameters = prev_args;
		self->range = prev_range;
		return NULL;
	}
	else {
		irt_work_item *real_wi = _irt_wi_create(target, range, impl_id, args);
		irt_scheduling_assign_wi(target, real_wi);
		return real_wi;
	}
}
