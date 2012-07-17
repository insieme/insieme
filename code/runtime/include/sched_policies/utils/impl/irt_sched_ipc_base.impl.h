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

#include "sched_policies/utils/irt_sched_ipc_base.h"
#include "impl/worker.impl.h"
#include "irt_mqueue.h" // include function declaration, even if implementation is not included

static inline int _irt_sched_check_ipc_queue(irt_worker* self) {
	int retval = 0;
	if(irt_g_runtime_behaviour & IRT_RT_MQUEUE) {
		irt_mqueue_msg* received = irt_mqueue_receive();
		if(received) {
			if(received->type == IRT_MQ_NEW_APP) {
				irt_mqueue_msg_new_app* appmsg = (irt_mqueue_msg_new_app*)received;
				irt_client_app* client_app = irt_client_app_create(appmsg->app_name);
				irt_context* prog_context = irt_context_create(client_app);
				self->cur_context = prog_context->id;
				irt_context_table_insert(prog_context);
				_irt_worker_switch_to_wi(self, irt_wi_create(irt_g_wi_range_one_elem, 0, NULL));
				retval = 1;
			}
			free(received);
		}
	}
	return retval;
}
