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

#ifndef IRT_MIN_MODE
	#include "impl/irt_mqueue.impl.h"
#endif


#include "abstraction/threads.h"
#include "abstraction/impl/threads.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/work_item.impl.h"
#include "utils/impl/minlwt.impl.h"
#include "utils/affinity.h"
#include "utils/impl/affinity.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/instrumentation.impl.h"
#include "utils/frequency.h"

#ifdef IRT_VERBOSE
void _irt_worker_print_debug_info(irt_worker* self) {
/*	IRT_INFO("======== Worker %d debug info:\n", self->id.thread);
#ifdef USING_MINLWT	
	IRT_INFO("== Base ptr: %p\n", (void*)self->basestack); // casting to void* would break 32 bit compatibility
#else
	IRT_INFO("== Base ptr: %p\n", &(self->basestack)); // casting to void* would break 32 bit compatibility
#endif
	IRT_INFO("== Current wi: %p\n", self->cur_wi);
	IRT_INFO("==== Pool:\n");
	irt_work_item* next_wi = self->sched_data.pool.start;
	while(next_wi != NULL) {
		IRT_INFO("--- Work item %p:\n", (void*)next_wi);
		IRT_INFO("- stack ptr: %p\n", (void*)next_wi->stack_ptr);
		next_wi = next_wi->sched_data.work_deque_next;
	}
	IRT_INFO("==== Queue:\n");
	next_wi = self->sched_data.queue.start;
	while(next_wi != NULL) {
		IRT_INFO("--- Work item %p:\n", (void*)next_wi);
		IRT_INFO("- stack ptr: %p\n", (void*)next_wi->stack_ptr);
		next_wi = next_wi->sched_data.work_deque_next;
	}
	IRT_INFO("========\n");*/
}
#endif

typedef struct __irt_worker_func_arg {
	irt_worker *generated;
	irt_affinity_mask affinity;
	uint16 index;
	irt_worker_init_signal *signal;
} _irt_worker_func_arg;


/** wait until all worker threads are created (signal->init_count == irt_g_worker_count) */
void _irt_await_all_workers_init(irt_worker_init_signal *signal){
	#if defined(WINVER) && (WINVER < 0x0600)
		irt_atomic_inc(&(signal->init_count));
		HANDLE ev = OpenEvent(SYNCHRONIZE, FALSE, "AllWorkersInitialized");
		// wait until master thread signals ev
		WaitForSingleObject(ev, INFINITE);
	#else
		irt_mutex_lock(&signal->init_mutex);
		signal->init_count++;
		if(signal->init_count == irt_g_worker_count) {
			// signal readyness of created thread to master thread
			irt_cond_wake_all(&signal->init_condvar);
		} else {
			irt_cond_wait(&signal->init_condvar, &signal->init_mutex);
		}
		irt_mutex_unlock(&signal->init_mutex);
	#endif
}

void* _irt_worker_func(void *argvp) {
	_irt_worker_func_arg *arg = (_irt_worker_func_arg*)argvp;
	irt_thread t;
	irt_thread_get_current(&t);
	irt_set_affinity(arg->affinity, t);
	arg->generated = (irt_worker*)calloc(1, sizeof(irt_worker));
	irt_worker* self = arg->generated;
	irt_thread_get_current(&(self->thread));
	self->id.index = 1;
	self->id.thread = arg->index;
	self->id.node = 0; // TODO correct node id
	self->id.cached = self;
	self->generator_id = self->id.full;
	self->affinity = arg->affinity;
	self->cur_context = irt_context_null_id();
	self->cur_wi = NULL;
	self->finalize_wi = NULL;
	self->default_variant = 0;
	if(getenv(IRT_DEFAULT_VARIANT_ENV)) {
		self->default_variant = atoi(getenv(IRT_DEFAULT_VARIANT_ENV));
	}

#ifdef IRT_WORKER_SLEEPING
	irt_cond_var_init(&self->wait_cond);
	self->wake_signal = true;
#endif
	
	irt_scheduling_init_worker(self);
	IRT_ASSERT(irt_tls_set(irt_g_worker_key, arg->generated) == 0, IRT_ERR_INTERNAL, "Could not set worker threadprivate data");

#ifdef IRT_ENABLE_INSTRUMENTATION
	self->instrumentation_event_data = irt_inst_create_event_data_table();
#endif
#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	self->instrumentation_region_data = irt_inst_create_region_data_table();
	// initialize papi's threading support and add events to be measured
	//self->irt_papi_number_of_events = 0;
	irt_initialize_papi_thread(&(self->irt_papi_event_set));
#endif
#ifdef IRT_OCL_INSTR
	self->event_data = irt_ocl_create_event_table();
#endif
	
#ifndef _WIN32
	irt_cpu_freq_set_frequency_core_env(self);
#endif

	// init lazy wi
	memset(&self->lazy_wi, 0, sizeof(irt_work_item));
	self->lazy_wi.id.cached = &self->lazy_wi;
	self->lazy_wi.state = IRT_WI_STATE_DONE;
	
	// init reuse lists
	self->wi_ev_register_list = NULL; // prepare some?
	self->wg_ev_register_list = NULL; // prepare some?
	self->wi_reuse_stack = NULL; // prepare some?
	self->stack_reuse_stack = NULL;

	#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
		self->region_reuse_list = irt_inst_create_region_list();
	#endif

	self->state = IRT_WORKER_STATE_READY;
	// TODO instrumentation?

	// store self, free arg
	irt_g_workers[arg->index] = self;
	irt_worker_init_signal *signal = arg->signal;
	free(arg);

	// wait until all workers are initialized
	_irt_await_all_workers_init(signal);

	if(irt_atomic_bool_compare_and_swap(&self->state, IRT_WORKER_STATE_READY, IRT_WORKER_STATE_RUNNING)) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_RUNNING, self->id);
		irt_scheduling_loop(self);
	}
	irt_worker_cleanup(self);
	return NULL;
}

void _irt_worker_switch_to_wi(irt_worker* self, irt_work_item *wi) {
	IRT_ASSERT(self->cur_wi == NULL, IRT_ERR_INTERNAL, "Worker %p _irt_worker_switch_to_wi with non-null current WI", self);
	// wait for previous operations on WI to complete
	while(wi->state != IRT_WI_STATE_NEW && wi->state != IRT_WI_STATE_SUSPENDED);
	IRT_ASSERT(wi->state == IRT_WI_STATE_NEW || wi->state == IRT_WI_STATE_SUSPENDED, 
		IRT_ERR_INTERNAL, "Worker %p switching to WI %p, WI not ready", self, wi);
	self->cur_context = wi->context_id;
	if(wi->state == IRT_WI_STATE_NEW) {
		// start WI from scratch
		wi->state = IRT_WI_STATE_STARTED;
		lwt_prepare(self->id.thread, wi, &self->basestack);
		self->cur_wi = wi;
#ifdef USING_MINLWT
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 1A, new stack ptr: %p.", self, (void*)wi->stack_ptr);
#else
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 1A, new stack ptr: %p.", self, &(wi->stack_ptr));
#endif
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
		irt_inst_region_set_timestamp(wi);
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_STARTED, wi->id);
#ifndef IRT_TASK_OPT
		irt_wi_implementation *wimpl = &(irt_context_table_lookup(self->cur_context)->impl_table[wi->impl_id]);
		if(self->default_variant < wimpl->num_variants) {
			lwt_start(wi, &self->basestack, wimpl->variants[self->default_variant].implementation);
		} else {
			lwt_start(wi, &self->basestack, wimpl->variants[0].implementation);
		}
#else // !IRT_TASK_OPT
        irt_wi_implementation *wimpl = &(irt_context_table_lookup(self->cur_context)->impl_table[wi->impl_id]);
        uint32 opt = wimpl->num_variants > 1 ? irt_scheduling_select_taskopt_variant(wi, self) : 0;
		lwt_start(wi, &self->basestack, wimpl->variants[opt].implementation);
#endif // !IRT_TASK_OPT
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 1B.", self);
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
	} else { 
		// resume WI
		wi->state = IRT_WI_STATE_STARTED;
		self->cur_wi = wi;
#ifdef USING_MINLWT
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 2A, new stack ptr: %p.", self, (void*)wi->stack_ptr);
#else
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 2A, new stack ptr: %p.", self, &(wi->stack_ptr));
#endif
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
		irt_inst_region_set_timestamp(wi);
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_RESUMED, wi->id);
		lwt_continue(&wi->stack_ptr, &self->basestack);
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 2B.", self);
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
	}
}

void irt_worker_run_immediate_wi(irt_worker* self, irt_work_item *wi) {
	irt_worker_run_immediate(self, &wi->range, wi->impl_id, wi->parameters);
}

void irt_worker_run_immediate(irt_worker* target, const irt_work_item_range* range, irt_wi_implementation_id impl_id, irt_lw_data_item* args) {
	irt_inst_insert_wo_event(target, IRT_INST_WORKER_IMMEDIATE_EXEC, target->id);
	irt_work_item *self = target->cur_wi;
	// store current wi data
	irt_lw_data_item *prev_args = self->parameters;
	irt_work_item_range prev_range = self->range;
	irt_wi_implementation_id prev_impl_id = self->impl_id;
	irt_work_item_id prev_source = self->source_id;
	uint32 prev_fragments = self->num_fragments;
	// set new wi data
	self->parameters = args;
	self->range = *range;
	self->impl_id = impl_id;
	self->source_id = irt_work_item_null_id();
	self->num_fragments = 0;
	// need unique active child number, can re-use id (and thus register entry)
	volatile uint32 *prev_parent_active_child_count = self->parent_num_active_children;
	self->parent_num_active_children = self->num_active_children;
	volatile uint32 active_child_count = 0;
	self->num_active_children = &active_child_count;
	// call wi
#ifndef IRT_TASK_OPT
	(irt_context_table_lookup(target->cur_context)->impl_table[impl_id].variants[0].implementation)(self);
#else // !IRT_TASK_OPT
	irt_wi_implementation *wimpl = &(irt_context_table_lookup(target->cur_context)->impl_table[impl_id]);
	uint32 opt = wimpl->num_variants > 1 ? irt_scheduling_select_taskopt_variant(self, target) : 0;
	wimpl->variants[opt].implementation(self);
#endif // !IRT_TASK_OPT
	// restore active child number(s)
	self->num_active_children = self->parent_num_active_children;
	self->parent_num_active_children = prev_parent_active_child_count;
	// restore data
	self->parameters = prev_args;
	self->range = prev_range;
	self->impl_id = prev_impl_id;
	self->source_id = prev_source;
	self->num_fragments = prev_fragments;
}

void irt_worker_create(uint16 index, irt_affinity_mask affinity, irt_worker_init_signal* signal) {
	_irt_worker_func_arg *arg = (_irt_worker_func_arg*)malloc(sizeof(_irt_worker_func_arg));
	arg->affinity = affinity;
	arg->index = index;
	arg->signal = signal;
	irt_thread_create(&_irt_worker_func, arg, NULL);
}

void _irt_worker_cancel_all_others() {
	irt_worker *self = irt_worker_get_current(), *cur;
	for(uint32 i=0; i<irt_g_worker_count; ++i) {
		cur = irt_g_workers[i];
		if(cur != self && cur->state == IRT_WORKER_STATE_RUNNING) {
			cur->state = IRT_WORKER_STATE_STOP;
			irt_inst_insert_wo_event(self, IRT_INST_WORKER_STOP, cur->id);
			irt_thread_cancel(&(cur->thread));
		}
	}
	
}

void _irt_worker_end_all() {
	// get info about calling thread
	irt_thread calling_thread;
	irt_thread_get_current(&calling_thread);

	for(uint32 i=0; i<irt_g_worker_count; ++i) {
		irt_worker *cur = irt_g_workers[i];
		cur->state = IRT_WORKER_STATE_STOP;
		irt_signal_worker(cur);

		// avoid calling thread awaiting its own termination
		if(!irt_thread_check_equality(&calling_thread, &(cur->thread)))
			irt_thread_join(&(cur->thread));   
	}
}


void irt_worker_cleanup(irt_worker* self) {
	// clean up event register reuse stacks
	{ // wi registers
		irt_wi_event_register *cur, *next;
		cur = self->wi_ev_register_list;
		while(cur) {
			next = cur->lookup_table_next;
			free(cur);
			cur = next;
		}
	}
	{ // wg registers
		irt_wg_event_register *cur, *next;
		cur = self->wg_ev_register_list;
		while(cur) {
			next = cur->lookup_table_next;
			free(cur);
			cur = next;
		}
	}
}
