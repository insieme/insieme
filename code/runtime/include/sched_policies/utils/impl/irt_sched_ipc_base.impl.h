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
#ifndef __GUARD_SCHED_POLICIES_UTILS_IMPL_IRT_SCHED_IPC_BASE_IMPL_H
#define __GUARD_SCHED_POLICIES_UTILS_IMPL_IRT_SCHED_IPC_BASE_IMPL_H

#include "sched_policies/utils/irt_sched_ipc_base.h"
#include "impl/worker.impl.h"

#ifndef IRT_MIN_MODE
#include "irt_mqueue.h"
#include "impl/irt_mqueue.impl.h"
#endif

static inline int _irt_sched_check_ipc_queue(irt_worker* self) {
	int retval = 0;
	#ifndef IRT_MIN_MODE
	if(irt_g_runtime_behaviour & IRT_RT_MQUEUE) {
		irt_mqueue_msg* received = irt_mqueue_receive();
		if(received) {
			if(received->type == IRT_MQ_NEW_APP) {
				irt_mqueue_msg_new_app* appmsg = (irt_mqueue_msg_new_app*)received;
				irt_client_app* client_app = irt_client_app_create(appmsg->app_name);
				irt_context* prog_context = irt_context_create(client_app, NULL, NULL);
				self->cur_context = prog_context->id;
				_irt_worker_switch_to_wi(self, irt_wi_create(irt_g_wi_range_one_elem, 0, NULL));
				retval = 1;
			}
			free(received);
		}
	}
	#endif
	return retval;
}


#endif // ifndef __GUARD_SCHED_POLICIES_UTILS_IMPL_IRT_SCHED_IPC_BASE_IMPL_H
