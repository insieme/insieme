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
#ifndef __GUARD_STANDALONE_H
#define __GUARD_STANDALONE_H

/*
	definition of global variables, startup, shutdown and signal handling functionality
	actually this file is not related to the standalone mode of the runtime
*/

#include "abstraction/threads.h"
#include "abstraction/impl/threads.impl.h"
#include "abstraction/memory.h"
#include "abstraction/impl/memory.impl.h"

#include "client_app.h"
#include "irt_all_impls.h"
#include "instrumentation_events.h"

#ifdef _GEMS
	#include "include_gems/stdlib.h"
#endif

#ifndef IRT_MIN_MODE
#include "irt_mqueue.h"
#include "impl/irt_mqueue.impl.h"
#endif

/** Starts the runtime in standalone mode and executes work item impl_id.
  * Returns once that wi has finished.
  * worker_count : number of workers to start
  * init_context_fun : fills type tables in context
  * cleanup_context_fun : cleans up the context
  * impl : a pointer to the work-item implementation to be started
  * startup_params : parameter struct for the startup work item (impl_id)
  */
void irt_runtime_standalone(uint32 worker_count, init_context_fun* init_fun, cleanup_context_fun* cleanup_fun, irt_wi_implementation* impl, irt_lw_data_item *startup_params);

// globals
irt_tls_key irt_g_error_key;
irt_mutex_obj irt_g_error_mutex;
irt_mutex_obj irt_g_exit_handler_mutex;
irt_tls_key irt_g_worker_key;
uint32 irt_g_worker_count;
uint32 irt_g_active_worker_count;
irt_mutex_obj irt_g_active_worker_mutex;
#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
uint32 irt_g_enabled_worker_count;
uint32 irt_g_worker_to_enable_count;
irt_cond_bundle irt_g_enable_worker_cond;
#endif
struct _irt_worker **irt_g_workers;
irt_runtime_behaviour_flags irt_g_runtime_behaviour;
#ifndef IRT_MIN_MODE
mqd_t irt_g_message_queue;
#endif

IRT_CREATE_LOCKED_LOOKUP_TABLE(data_item, lookup_table_next, IRT_ID_HASH, IRT_DATA_ITEM_LT_BUCKETS);
IRT_CREATE_LOCKED_LOOKUP_TABLE(context, lookup_table_next, IRT_ID_HASH, IRT_CONTEXT_LT_BUCKETS);
IRT_CREATE_LOCKED_LOOKUP_TABLE(wi_event_register, lookup_table_next, IRT_ID_HASH, IRT_EVENT_LT_BUCKETS);
IRT_CREATE_LOCKED_LOOKUP_TABLE(wg_event_register, lookup_table_next, IRT_ID_HASH, IRT_EVENT_LT_BUCKETS);

static bool irt_g_exit_handling_done;

// initialize global variables and set up global data structures
void irt_init_globals() {
	irt_g_exit_handling_done = false;

	irt_log_init();

	// this call seems superflous but it is not - needs to be investigated TODO
	irt_time_ticks_per_sec_calibration_mark();

	// not using IRT_ASSERT since environment is not yet set up
	int err_flag = 0;
	err_flag |= irt_tls_key_create(&irt_g_error_key);
	err_flag |= irt_tls_key_create(&irt_g_worker_key);
	if(err_flag != 0) {
		fprintf(stderr, "Could not create thread local storage key(s). Aborting.\n");
		exit(-1);
	}
	irt_mutex_init(&irt_g_error_mutex);
	irt_mutex_init(&irt_g_exit_handler_mutex);
	irt_mutex_init(&irt_g_active_worker_mutex);
#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
	irt_cond_bundle_init(&irt_g_enable_worker_cond);
#endif
	irt_data_item_table_init();
	irt_context_table_init();
	irt_wi_event_register_table_init();
	irt_wg_event_register_table_init();
	irt_loop_sched_policy_init();
#ifndef IRT_MIN_MODE
	if(irt_g_runtime_behaviour & IRT_RT_MQUEUE) irt_mqueue_init();
#endif
	// keep this call even without instrumentation, it might be needed for scheduling purposes
	irt_time_ticks_per_sec_calibration_mark();
}

// cleanup global variables and delete global data structures
void irt_cleanup_globals() {
	irt_data_item_table_cleanup();
	irt_context_table_cleanup();
	irt_wi_event_register_table_cleanup();
	irt_wg_event_register_table_cleanup();
#ifndef IRT_MIN_MODE
	if(irt_g_runtime_behaviour & IRT_RT_MQUEUE) irt_mqueue_cleanup();
#endif
	irt_mutex_destroy(&irt_g_error_mutex);
	irt_tls_key_delete(irt_g_error_key);
	irt_tls_key_delete(irt_g_worker_key);
	irt_log_cleanup();
}

// exit handling
// on exit() and termination signals, the irt exit handler should be called
void irt_exit_handler();
void irt_term_handler(int signal) {
	exit(signal);
}
void irt_exit(int i) {
	irt_exit_handler();
	exit(i);
}

// abort handler, to be called in case of segmentation faults of the application, ...
void irt_abort_handler(int signum) {
	// performing cleanup
	irt_exit_handler();
	// reset default behavior for the caught signal (= usually means killing the process)
	signal(signum, SIG_DFL);
	// raise the signal
	raise(signum);
}

// the irt exit handler
// needs to correctly shutdown all workers regardless of the situation it was called in
void irt_exit_handler() {
	// only one thread may execute this routine, when it is done it sets irt_g_exit_handling_done true
	// every other thread which comes after simply exits
	while(irt_mutex_trylock(&irt_g_exit_handler_mutex) != 0)
		if (irt_g_exit_handling_done)
			irt_thread_exit(0);

	if (irt_g_exit_handling_done)
		return;

	// reset the clock frequency of the cores of all workers
#ifndef _WIN32
	if(!irt_affinity_mask_is_empty(irt_g_frequency_setting_modified_mask))
		irt_cpu_freq_reset_frequencies();
#endif

#ifdef USE_OPENCL
	irt_ocl_release_devices();	
#endif
	irt_g_exit_handling_done = true;
	_irt_worker_end_all();
	// keep this call even without instrumentation, it might be needed for scheduling purposes
	irt_time_ticks_per_sec_calibration_mark(); // needs to be done before any time instrumentation processing!
#ifdef IRT_ENABLE_INSTRUMENTATION
	if(irt_g_instrumentation_event_output_is_enabled)
		irt_inst_event_data_output_all(irt_g_instrumentation_event_output_is_binary);
	for(int i = 0; i < irt_g_worker_count; ++i)
		irt_inst_destroy_event_data_table(irt_g_workers[i]->instrumentation_event_data);
#endif

	irt_cleanup_globals();
	free(irt_g_workers);
	irt_mutex_unlock(&irt_g_exit_handler_mutex);
	//IRT_INFO("\nInsieme runtime exiting.\n");
}

// error handling
void irt_error_handler(int signal) {
	irt_mutex_lock(&irt_g_error_mutex); // not unlocked
	_irt_worker_cancel_all_others();
	irt_error* error = (irt_error*)irt_tls_get(irt_g_error_key);
	// gcc will warn when the cast to void* is missing, Visual Studio will not compile with the cast
	irt_thread t;
	irt_thread_get_current(&t);

	#if defined(_WIN32) && !defined(IRT_USE_PTHREADS)
		fprintf(stderr, "Insieme Runtime Error received (thread %i): %s\n", t.thread_id, irt_errcode_string(error->errcode));
	#elif defined(_WIN32)
		fprintf(stderr, "Insieme Runtime Error received (thread %p): %s\n", (void*)t.p, irt_errcode_string(error->errcode));
	#else
		fprintf(stderr, "Insieme Runtime Error received (thread %p): %s\n", (void*)t, irt_errcode_string(error->errcode));
	#endif

	fprintf(stderr, "Additional information:\n");
	irt_print_error_info(stderr, error);
	exit(-error->errcode);
}

// interrupts are ignored
void irt_interrupt_handler(int signal) {
	// do nothing
}

// initialize objects required for signaling threads
void* _irt_init_signalable(irt_worker_init_signal *signal){
	// all Systems other than Windows XP will use condition variables, WinXP uses events to singal threads
	#if defined(WINVER) && (WINVER < 0x0600)
		HANDLE ev = CreateEvent( 
			NULL,							// default security attributes
			TRUE,							// manual-reset event
			FALSE,							// initial state is nonsignaled
			"AllWorkersInitialized"			// object name
		);
		return ev;
	#else
		irt_mutex_init(&(signal->init_mutex));
		irt_cond_var_init(&(signal->init_condvar));
		return NULL;
	#endif
}

// when all workers are created every waiting worker may continue
void _irt_wake_sleeping_workers(irt_worker_init_signal *signal, void *ev_handle){
	// Windows XP Version
	#if defined(WINVER) && (WINVER < 0x0600)
		while (!irt_atomic_bool_compare_and_swap(&(signal->init_count), irt_g_worker_count, irt_g_worker_count)){}
		// wake waiting threads
		SetEvent(ev_handle);
	#else
		irt_mutex_lock(&(signal->init_mutex));
		if (signal->init_count < irt_g_worker_count) {
			irt_cond_wait(&(signal->init_condvar), &(signal->init_mutex));
		}
		irt_mutex_unlock(&(signal->init_mutex));
	#endif
}

void irt_runtime_start(irt_runtime_behaviour_flags behaviour, uint32 worker_count, bool handle_signals) {

	if(worker_count > IRT_MAX_WORKERS) {
		fprintf(stderr, "Runtime configured for maximum of %d workers, %d workers requested, exiting...\n", IRT_MAX_WORKERS, worker_count);
		exit(-1);
	}

	irt_g_runtime_behaviour = behaviour;

#ifndef _GEMS
	// TODO [_GEMS]: signal is not supported by gems platform
	// initialize error and termination signal handlers
	if(handle_signals) {
		signal(IRT_SIG_ERR, &irt_error_handler);
		signal(IRT_SIG_INTERRUPT, &irt_interrupt_handler);
		signal(SIGTERM, &irt_term_handler);
		signal(SIGINT, &irt_term_handler);
		signal(SIGSEGV, &irt_abort_handler);
		atexit(&irt_exit_handler);
	}
#endif
	// initialize globals
	irt_init_globals();
	irt_g_exit_handling_done = false;

	_irt_setup_hardware_info();

#ifdef IRT_ENABLE_INSTRUMENTATION
	irt_inst_set_all_instrumentation_from_env();
	#ifdef _GEMS
		irt_inst_region_select_metrics("rapmi_energy,rapmi_average_power,rapmi_ticks");
	#endif
#endif

	irt_log_comment("starting worker threads");
	irt_log_setting_u("IRT_NUM_WORKERS", worker_count);
	// get worker count & allocate global worker storage
	irt_g_worker_count = worker_count;
	irt_g_active_worker_count = worker_count;
#ifdef IRT_ENABLE_OMPP_OPTIMIZER_DCT
    irt_g_enabled_worker_count = irt_g_worker_count;
    irt_g_worker_to_enable_count = irt_g_worker_count;
#endif
	irt_g_workers = (irt_worker**)malloc(irt_g_worker_count * sizeof(irt_worker*));

	// initialize affinity mapping & load affinity policy
	irt_affinity_init_physical_mapping(&irt_g_affinity_physical_mapping);
	irt_affinity_policy aff_policy = irt_load_affinity_from_env();

	// initialize workers
	static irt_worker_init_signal signal;
	signal.init_count = 0;

	void* ev_handle = _irt_init_signalable(&signal);

	for(uint32 i=0; i<irt_g_worker_count; ++i) {
		irt_worker_create(i, irt_get_affinity(i, aff_policy), &signal);
	}

	// wait until all workers have signaled readyness
	_irt_wake_sleeping_workers(&signal, ev_handle);

	#ifdef USE_OPENCL
		irt_log_comment("Running Insieme runtime with OpenCL!\n");
		irt_ocl_init_devices();
	#endif
}

uint32 irt_get_default_worker_count() {
	uint32 cores;

	if(getenv(IRT_NUM_WORKERS_ENV)) {
		return atoi(getenv(IRT_NUM_WORKERS_ENV));
	}
	else if( (cores = irt_affinity_cores_available()) > 0 )
		return cores;
	else
		return IRT_DEF_WORKERS;

}

bool _irt_runtime_standalone_end_func(irt_wi_event_register* source_event_register, void *condbundlep) {
	irt_cond_bundle *condbundle = (irt_cond_bundle*)condbundlep;
	irt_mutex_lock(&condbundle->mutex);
	irt_cond_wake_one(&condbundle->condvar);
	irt_mutex_unlock(&condbundle->mutex);
	return false;
}

void irt_runtime_run_wi(irt_wi_implementation* impl, irt_lw_data_item *params) {
	irt_work_item* main_wi = _irt_wi_create(irt_g_workers[0], &irt_g_wi_range_one_elem, impl, params);
	// create work group for outermost wi
	irt_work_group* outer_wg = _irt_wg_create(irt_g_workers[0]);
	irt_wg_insert(outer_wg, main_wi);
	// event handling for outer work item [[
	irt_cond_bundle condbundle;
	irt_cond_bundle_init(&condbundle);
	irt_wi_event_lambda handler;
	handler.next = NULL;
	handler.data = &condbundle;
	handler.func = &_irt_runtime_standalone_end_func;
	irt_wi_event_check_and_register(main_wi->id, IRT_WI_EV_COMPLETED, &handler);
	// ]] event handling
	irt_scheduling_assign_wi(irt_g_workers[0], main_wi);

	// wait for workers to finish the main work-item
	irt_mutex_lock(&condbundle.mutex);
	irt_cond_bundle_wait(&condbundle);
}

irt_context* irt_runtime_start_in_context(uint32 worker_count, init_context_fun* init_fun, cleanup_context_fun* cleanup_fun, bool handle_signals) {
    #ifdef _GEMS
        irt_mutex_init(&print_mutex);
    #endif
	IRT_DEBUG("Workers count: %d\n", worker_count);
	irt_runtime_start(IRT_RT_STANDALONE, worker_count, handle_signals);
	irt_tls_set(irt_g_worker_key, irt_g_workers[0]); // slightly hacky
	irt_context* context = irt_context_create_standalone(init_fun, cleanup_fun);

	for(uint32 i=0; i<irt_g_worker_count; ++i) {
		irt_g_workers[i]->cur_context = context->id;
	}

	return context;
}

void irt_runtime_standalone(uint32 worker_count, init_context_fun* init_fun, cleanup_context_fun* cleanup_fun, irt_wi_implementation* impl, irt_lw_data_item *startup_params) {
	irt_context* context = irt_runtime_start_in_context(worker_count, init_fun, cleanup_fun, true);

	if(getenv(IRT_REPORT_ENV)) {
		irt_dbg_print_context(context);
		exit(0);
	}

	irt_runtime_run_wi(impl, startup_params);

	// shut-down context
	irt_context_destroy(context);

	//irt_mutex_unlock(&condbundle.mutex);
	//irt_mutex_destroy(&condbundle.mutex);
	IRT_DEBUG("Exiting ...\n");
	irt_exit_handler();
}


#endif // ifndef __GUARD_STANDALONE_H
