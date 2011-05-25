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

#include <mqueue.h>
#include <unistd.h>
#include <alloca.h>

#include "impl/client_app.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"
#include "impl/irt_mqueue.impl.h"
#include "impl/data_item.impl.h"

#ifdef USE_OPENCL 
#include "impl/irt_ocl.impl.h"
#endif

#include "utils/lookup_tables.h"

// error handling
void irt_error_handler(int signal) {
	irt_error* error = (irt_error*)pthread_getspecific(irt_g_error_key);
	fprintf(stderr, "Insieme Runtime Error recieved (thread %p): %s\n", (void*)pthread_self(), irt_errcode_string(error->errcode));
	fprintf(stderr, "Additional information:\n");
	irt_print_error_info(stderr, error);
	exit(-error->errcode);
}

// globals
pthread_key_t irt_g_error_key;
pthread_key_t irt_g_worker_key;
mqd_t irt_g_message_queue;
uint32 irt_g_worker_count;
struct _irt_worker **irt_g_workers;

IRT_CREATE_LOOKUP_TABLE(data_item, lookup_table_next, IRT_ID_HASH, IRT_DATA_ITEM_LT_BUCKETS);
IRT_CREATE_LOOKUP_TABLE(context, lookup_table_next, IRT_ID_HASH, IRT_CONTEXT_LT_BUCKETS);

void irt_init_globals() {
	// not using IRT_ASSERT since environment is not yet set up
	int err_flag = 0;
	err_flag |= pthread_key_create(&irt_g_error_key, NULL);
	err_flag |= pthread_key_create(&irt_g_worker_key, NULL);
	if(err_flag != 0) {
		fprintf(stderr, "Could not create pthread key(s). Aborting.\n");
		exit(-1);
	}
	irt_mqueue_init();
	irt_data_item_table_init();
	irt_context_table_init();
}
void irt_cleanup_globals() {
	irt_mqueue_cleanup();
}

// exit handling
void irt_term_handler(int signal) {
	exit(0);
}
void irt_exit_handler() {
	irt_cleanup_globals();
	IRT_INFO("\nInsieme runtime exiting.\n");
}

int main(int argc, char** argv) {
	if(argc < 2 || argc > 3) {
		IRT_INFO("usage: runtime [libname] [numthreads]\n");
		return -1;
	}

	// initialize error and termination signal handlers
	signal(IRT_SIG_ERR, &irt_error_handler);
	signal(SIGTERM, &irt_term_handler);
	signal(SIGINT, &irt_term_handler);
	atexit(&irt_exit_handler);
	// initialize globals
	irt_init_globals();

	IRT_DEBUG("!!! Starting worker threads");
	// initialize opencl devices
	#ifdef USE_OPENCL
	printf("Running Insieme runtime with OpenCL!\n");
	cl_uint num_devices = irt_ocl_get_num_devices();
	#endif
	
	irt_g_worker_count = 1;
	if(argc >= 3) irt_g_worker_count = atoi(argv[2]);
	irt_g_workers = (irt_worker**)alloca(irt_g_worker_count * sizeof(irt_worker*));
	// initialize workers
	for(int i=0; i<irt_g_worker_count; ++i) {
		irt_g_workers[i] = irt_worker_create(i, 1<<i);
	}
	// start workers
	for(int i=0; i<irt_g_worker_count; ++i) {
		irt_g_workers[i]->start = true;
	}
	IRT_DEBUG("Sending new app msg");
	irt_mqueue_send_new_app(argv[1]);
	IRT_DEBUG("New app msg sent");

	// holy hack batman!
	// reduces performance and is generally fugly, use only for testing
	if(argc >=2) {
		bool stuff_running = true;
		while(stuff_running) {
			sleep(5);
			stuff_running = false;
			for(int i=0; i<irt_g_worker_count; ++i) {
				if(irt_g_workers[i]->cur_wi != NULL || irt_g_workers[i]->queue.start != NULL || irt_g_workers[i]->pool.start != NULL) {
					stuff_running = true;
					break;
				}
			}
		}
	}
	//for(;;) { sleep(60*60); }
	exit(0);
}
