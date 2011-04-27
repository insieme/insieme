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

#include "worker.h"

#include <stdlib.h>

#include "globals.h"
#include "impl/error_handling.impl.h"
#include "impl/irt_mqueue.impl.h"

static inline irt_worker* irt_worker_get_current() {
	return (irt_worker*)pthread_getspecific(irt_g_worker_key);
}

typedef struct __irt_worker_func_arg {
	irt_worker *generated;
	bool ready;
	irt_affinity_mask affinity;
	uint16 index;
} _irt_worker_func_arg;

void* _irt_worker_func(void *argvp) {
	
	_irt_worker_func_arg *arg = (_irt_worker_func_arg*)argvp;
	arg->generated = (irt_worker*)calloc(1, sizeof(irt_worker));
	arg->generated->generator_count = 1;
	arg->generated->pthread = pthread_self();
	arg->generated->id.value.components.index = 1;
	arg->generated->id.value.components.thread = arg->index;
	arg->generated->id.value.components.node = 0; // TODO correct node id
	arg->generated->id.cached = arg->generated;
	arg->generated->affinity = arg->affinity;
	IRT_ASSERT(pthread_setspecific(irt_g_worker_key, arg->generated) == 0, IRT_ERR_INTERNAL, "Could not set worker threadprivate data");
	arg->ready = true;

	for(;;) {
		// TODO main worker loop
		irt_worker_schedule();
	}

	return NULL;
}

irt_worker* irt_worker_create(uint16 index, irt_affinity_mask affinity) {
	_irt_worker_func_arg arg;
	arg.affinity = affinity;
	arg.index = index;
	arg.ready = false;

	pthread_t thread;

	IRT_ASSERT(pthread_create(&thread, NULL, &_irt_worker_func, &arg) == 0, IRT_ERR_INTERNAL, "Could not create worker thread");

	while(!arg.ready) { } // MARK busy wait

	return arg.generated;
}

void irt_worker_schedule() {
	irt_worker* self = irt_worker_get_current();

	irt_mqueue_msg* received = irt_mqueue_receive();
	if(received) {
		if(received->type == IRT_MQ_NEW_APP) {
			irt_mqueue_msg_new_app* appmsg = (irt_mqueue_msg_new_app*)received;
			irt_client_app* client_app = irt_client_app_create(appmsg->app_name);
			irt_context* prog_context = irt_context_create(client_app);
			irt_context_table_insert(prog_context);
		}
		free(received);
	}
}