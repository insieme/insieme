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

/** Starts the runtime in standalone mode and executes work item 0.
  * Returns once that wi has finished.
  * worker_count : number of workers to start
  * type_table : insieme type table for the generated context
  * impl_table : work item implementation table for the generated context
  */
void irt_runtime_standalone(uint32 worker_count, irt_type* type_table, irt_wi_implementation* impl_table);

// globals
pthread_key_t irt_g_error_key;
pthread_key_t irt_g_worker_key;
mqd_t irt_g_message_queue;
uint32 irt_g_worker_count;
struct _irt_worker **irt_g_workers;
irt_runtime_behaviour_flags irt_g_runtime_behaviour;

IRT_CREATE_LOOKUP_TABLE(data_item, lookup_table_next, IRT_ID_HASH, IRT_DATA_ITEM_LT_BUCKETS);
IRT_CREATE_LOOKUP_TABLE(context, lookup_table_next, IRT_ID_HASH, IRT_CONTEXT_LT_BUCKETS);
IRT_CREATE_LOOKUP_TABLE(wi_event_register, lookup_table_next, IRT_ID_HASH, IRT_EVENT_LT_BUCKETS);

void irt_init_globals() {
	// not using IRT_ASSERT since environment is not yet set up
	int err_flag = 0;
	err_flag |= pthread_key_create(&irt_g_error_key, NULL);
	err_flag |= pthread_key_create(&irt_g_worker_key, NULL);
	if(err_flag != 0) {
		fprintf(stderr, "Could not create pthread key(s). Aborting.\n");
		exit(-1);
	}
	if(irt_g_runtime_behaviour & IRT_RT_MQUEUE) irt_mqueue_init();
	irt_data_item_table_init();
	irt_context_table_init();
	irt_wi_event_register_table_init();
}
void irt_cleanup_globals() {
	if(irt_g_runtime_behaviour & IRT_RT_MQUEUE) irt_mqueue_cleanup();
}

// exit handling
void irt_term_handler(int signal) {
	exit(0);
}
void irt_exit_handler() {
#ifdef USE_OPENCL
	irt_ocl_release_devices();	
#endif
	irt_cleanup_globals();
	free(irt_g_workers);
	IRT_INFO("\nInsieme runtime exiting.\n");
}

void irt_runtime_start(irt_runtime_behaviour_flags behaviour, uint32 worker_count) {
	irt_g_runtime_behaviour = behaviour;

	// initialize error and termination signal handlers
	signal(IRT_SIG_ERR, &irt_error_handler);
	signal(SIGTERM, &irt_term_handler);
	signal(SIGINT, &irt_term_handler);
	atexit(&irt_exit_handler);
	// initialize globals
	irt_init_globals();

#ifdef USE_OPENCL
	IRT_INFO("Running Insieme runtime with OpenCL!\n");
	irt_ocl_init_devices();
#endif

	IRT_DEBUG("!!! Starting worker threads");
	irt_g_worker_count = worker_count;
	irt_g_workers = (irt_worker**)malloc(irt_g_worker_count * sizeof(irt_worker*));
	// initialize workers
	for(int i=0; i<irt_g_worker_count; ++i) {
		irt_g_workers[i] = irt_worker_create(i, 1<<i);
	}
	// start workers
	for(int i=0; i<irt_g_worker_count; ++i) {
		irt_g_workers[i]->state = IRT_WORKER_STATE_START;
	}
}

void irt_runtime_standalone(uint32 worker_count, irt_type* type_table, irt_wi_implementation* impl_table) {
	irt_runtime_start(IRT_RT_STANDALONE, worker_count);
	pthread_setspecific(irt_g_worker_key, irt_g_workers[0]); // slightly hacky
	irt_context* context = irt_context_create_standalone(type_table, impl_table);
	for(int i=0; i<irt_g_worker_count; ++i) {
		irt_g_workers[i]->cur_context = context->id;
	}
	irt_work_item* main_wi = irt_wi_create(irt_g_wi_range_one_elem, 0, NULL);
	irt_scheduling_assign_wi(irt_g_workers[0], main_wi);
	// TODO solve with event handling
	while(main_wi->state != IRT_WI_STATE_DONE) sleep(5);
}
