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
#ifndef __GUARD_IMPL_WORKER_IMPL_H
#define __GUARD_IMPL_WORKER_IMPL_H

#include "worker.h"

#include <stdlib.h>

#include "irt_globals.h"

#ifndef IRT_MIN_MODE
#include "impl/irt_mqueue.impl.h"
#endif


#include "abstraction/atomic.h"
#include "abstraction/threads.h"
#include "abstraction/impl/threads.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/work_item.impl.h"
#include "utils/impl/minlwt.impl.h"
#include "utils/affinity.h"
#include "utils/impl/affinity.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/instrumentation_events.impl.h"
#include "meta_information/meta_infos.h"
#include "utils/frequency.h"

#ifdef IRT_VERBOSE
void _irt_worker_print_debug_info(irt_worker* self) {
	/*	IRT_INFO("======== Worker %d debug info:\n", self->id.thread);
	#ifdef USING_MINLWT
	    IRT_INFO("== Base ptr: %p\n", (void*)self->basestack); // casting to void* would break 32 bit compatibility
	#else
	    IRT_INFO("== Base ptr: %p\n", (void*)&(self->basestack)); // casting to void* would break 32 bit compatibility
	#endif
	    IRT_INFO("== Current wi: %p\n", (void*) self->cur_wi);
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
	irt_worker* generated;
	irt_affinity_mask affinity;
	uint16 index;
	irt_worker_init_signal* signal;
} _irt_worker_func_arg;


/** wait until all worker threads are created (signal->init_count == irt_g_worker_count) */
void _irt_await_all_workers_init(irt_worker_init_signal* signal) {
#if defined(WINVER) && (WINVER < 0x0600)
	irt_atomic_inc(&(signal->init_count, uint32));
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

void* _irt_worker_func(void* argvp) {
	_irt_worker_func_arg* arg = (_irt_worker_func_arg*)argvp;
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
	if(getenv(IRT_DEFAULT_VARIANT_ENV)) { self->default_variant = atoi(getenv(IRT_DEFAULT_VARIANT_ENV)); }

	#ifdef IRT_WORKER_SLEEPING
	irt_cond_var_init(&self->wait_cond);
	self->wake_signal = true;
	#endif
	irt_cond_var_init(&self->dop_wait_cond);
	irt_spin_init(&self->shutdown_lock);

	irt_scheduling_init_worker(self);
	IRT_ASSERT(irt_tls_set(irt_g_worker_key, arg->generated) == 0, IRT_ERR_INTERNAL, "Could not set worker threadprivate data");

	#ifdef IRT_ENABLE_APP_TIME_ACCOUNTING
	IRT_ASSERT(pthread_getcpuclockid(t, &self->clockid) == 0, IRT_ERR_INIT, "Failed to retrieve thread clock id");
	self->app_time_total = 0.0;
	self->app_time_last_start = 0.0;
	self->app_time_running = false;
	#endif // IRT_ENABLE_APP_TIME_ACCOUNTING

	#ifdef IRT_ENABLE_INSTRUMENTATION
	self->instrumentation_event_data = irt_inst_create_event_data_table();
	#endif
	#ifdef IRT_OCL_INSTR
	self->event_data = irt_ocl_create_event_table();
	#endif

	//#ifndef _WIN32
	//	irt_cpu_freq_set_frequency_core_env(self);
	//#endif

	// init lazy wi
	memset(&self->lazy_wi, 0, sizeof(irt_work_item));
	self->lazy_wi.id.cached = &self->lazy_wi;
	irt_atomic_store(&self->lazy_wi.state, IRT_WI_STATE_DONE);

	// init reuse lists
	self->wi_ev_register_list = NULL; // prepare some?
	self->wg_ev_register_list = NULL; // prepare some?
	self->wi_reuse_stack = NULL;      // prepare some?
	self->stack_reuse_stack = NULL;

	irt_atomic_store(&self->state, IRT_WORKER_STATE_READY);

	// store self, free arg
	irt_g_workers[arg->index] = self;
	irt_worker_init_signal* signal = arg->signal;
	free(arg);

	// wait until all workers are initialized
	_irt_await_all_workers_init(signal);

	irt_worker_late_init(self);

	if(irt_atomic_bool_compare_and_swap(&self->state, IRT_WORKER_STATE_READY, IRT_WORKER_STATE_RUNNING, uint32)) {
		irt_inst_insert_wo_event(self, IRT_INST_WORKER_RUNNING, self->id);
		irt_scheduling_loop(self);
	}
	irt_inst_region_finalize_worker(self);
	return NULL;
}

uint32 _irt_worker_select_implementation_variant(const irt_worker* self, const irt_work_item* wi) {
	irt_wi_implementation* wimpl = wi->impl;
	#ifndef IRT_TASK_OPT
	
	#ifdef IRT_ENABLE_OPENCL
	for(uint32 i = 0; i < wimpl->num_variants; ++i) {
		irt_wi_implementation_variant *variant = &wimpl->variants[i];
		// check if it has opencl support
		if(irt_meta_info_is_opencl_available(variant->meta_info)) {
			IRT_DEBUG("Worker %p _irt_worker_select_implementation_variant - selected opencl variant: %u.", (void*)self, i);
			return i;
		}
	}
	#endif

	if(self->default_variant < wimpl->num_variants) {
		return self->default_variant;
	} else {
		return 0;
	}
	#else  // !IRT_TASK_OPT
	return wimpl->num_variants > 1 ? irt_scheduling_select_taskopt_variant(wi, self) : 0;
	#endif // !IRT_TASK_OPT
}

void _irt_worker_switch_to_wi(irt_worker* self, irt_work_item* wi) {
	IRT_ASSERT(self->cur_wi == NULL, IRT_ERR_INTERNAL, "Worker %p _irt_worker_switch_to_wi with non-null current WI", (void*)self);
	// wait for previous operations on WI to complete
	irt_work_item_state current_state;
	do {
		current_state = irt_atomic_load(&wi->state);
	} while(current_state != IRT_WI_STATE_NEW && current_state != IRT_WI_STATE_SUSPENDED);

	self->cur_context = wi->context_id;
	if(irt_atomic_load(&wi->state) == IRT_WI_STATE_NEW) {
		// start WI from scratch
		irt_atomic_store(&wi->state, IRT_WI_STATE_STARTED);
		lwt_prepare(self->id.thread, wi, &self->basestack);
		self->cur_wi = wi;
		#ifdef USING_MINLWT
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 1A, new stack ptr: %p.", (void*)self, (void*)wi->stack_ptr);
		#else
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 1A, new stack ptr: %p.", (void*)self, (void*)&(wi->stack_ptr));
		#endif
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
		irt_inst_region_start_measurements(wi);
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_STARTED, wi->id);
		// determine and store the implementation variant to use
		wi->selected_impl_variant = _irt_worker_select_implementation_variant(self, wi);
		irt_wi_implementation_variant* impl_variant = &(wi->impl->variants[wi->selected_impl_variant]);
		// and start that variant
		irt_optimizer_apply_dvfs(impl_variant);
		lwt_start(wi, &self->basestack, impl_variant->implementation);
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 1B.", (void*)self);
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
	} else {
		// resume WI
		irt_atomic_store(&wi->state, IRT_WI_STATE_STARTED);
		self->cur_wi = wi;
		#ifdef USING_MINLWT
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 2A, new stack ptr: %p.", (void*)self, (void*)wi->stack_ptr);
		#else
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 2A, new stack ptr: %p.", (void*)self, (void*)&(wi->stack_ptr));
		#endif
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
		irt_inst_insert_wi_event(self, IRT_INST_WORK_ITEM_RESUMED_UNKNOWN, wi->id);
		irt_wi_implementation* wimpl = wi->impl;
		irt_optimizer_apply_dvfs(&(wimpl->variants[wi->selected_impl_variant]));
		lwt_continue(&wi->stack_ptr, &self->basestack);
		IRT_DEBUG("Worker %p _irt_worker_switch_to_wi - 2B.", (void*)self);
		IRT_VERBOSE_ONLY(_irt_worker_print_debug_info(self));
	}
}

void _irt_worker_switch_from_wi(irt_worker* self, irt_work_item* wi) {
	irt_wi_implementation* wimpl = wi->impl;
	irt_optimizer_remove_dvfs(&(wimpl->variants[wi->selected_impl_variant]));
	lwt_continue(&self->basestack, &wi->stack_ptr);
}

void irt_worker_run_immediate_wi(irt_worker* self, irt_work_item* wi) {
	irt_worker_run_immediate(self, &wi->range, wi->impl, wi->parameters);
}

void irt_worker_run_immediate(irt_worker* target, const irt_work_item_range* range, irt_wi_implementation* impl, irt_lw_data_item* args) {
	irt_inst_insert_wo_event(target, IRT_INST_WORKER_IMMEDIATE_EXEC, target->id);
	irt_work_item* self = target->cur_wi;
	// store current wi data
	irt_lw_data_item* prev_args = self->parameters;
	irt_work_item_range prev_range = self->range;
	irt_wi_implementation* prev_impl = self->impl;
	uint32 prev_selected_impl_variant = self->selected_impl_variant;
	irt_work_item_id prev_source = self->source_id;
	uint32 prev_fragments = self->num_fragments;
	// set new wi data
	self->parameters = args;
	self->range = *range;
	self->impl = impl;
	self->source_id = irt_work_item_null_id();
	self->num_fragments = 0;
	// need unique active child number, can re-use id (and thus register entry)
	volatile uint32* prev_parent_active_child_count = self->parent_num_active_children;
	self->parent_num_active_children = self->num_active_children;
	volatile uint32 active_child_count = 0;
	self->num_active_children = &active_child_count;
	// call wi
	self->selected_impl_variant = _irt_worker_select_implementation_variant(target, self);
	(impl->variants[self->selected_impl_variant].implementation)(self);
	// restore active child number(s)
	self->num_active_children = self->parent_num_active_children;
	self->parent_num_active_children = prev_parent_active_child_count;
	// restore data
	self->parameters = prev_args;
	self->range = prev_range;
	self->impl = prev_impl;
	self->selected_impl_variant = prev_selected_impl_variant;
	self->source_id = prev_source;
	self->num_fragments = prev_fragments;
}

void irt_worker_create(uint16 index, irt_affinity_mask affinity, irt_worker_init_signal* signal) {
	_irt_worker_func_arg* arg = (_irt_worker_func_arg*)malloc(sizeof(_irt_worker_func_arg));
	arg->affinity = affinity;
	arg->index = index;
	arg->signal = signal;
	irt_thread_create(&_irt_worker_func, arg, NULL);
}

void irt_worker_late_init(irt_worker* self) {
	irt_context_id nullid = irt_context_null_id();
	// loop until context id has been set (i.e. is not nullid), which means that the context setup is done
	while(irt_atomic_fetch_and_add(&(self->cur_context.full), 0, uint64) == nullid.full) {
	}
	#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
	irt_inst_region_init_worker(self);
	#endif
}

void _irt_worker_cancel_all_others() {
	irt_worker *self = irt_worker_get_current(), *cur;
	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		cur = irt_g_workers[i];
		if(cur != self && irt_atomic_load(&cur->state) == IRT_WORKER_STATE_RUNNING) {
			irt_atomic_store(&cur->state, IRT_WORKER_STATE_STOP);
			irt_inst_insert_wo_event(self, IRT_INST_WORKER_STOP, cur->id);
			irt_thread_cancel(&(cur->thread));
		}
	}
}

void _irt_worker_end_all() {
	// get info about calling thread
	irt_thread calling_thread;
	irt_thread_get_current(&calling_thread);

	irt_mutex_lock(&irt_g_degree_of_parallelism_mutex);
	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		irt_worker* cur = irt_g_workers[i];
		irt_spin_lock(&cur->shutdown_lock);
		if(irt_atomic_load(&cur->state) == IRT_WORKER_STATE_JOINED) { continue; }
		do {
			irt_atomic_store(&cur->state, IRT_WORKER_STATE_STOP);
			irt_signal_worker(cur);
			// workers stopped by dop setting should be woken up
			irt_cond_wake_one(&cur->dop_wait_cond);
		} while(irt_atomic_load(&cur->state) != IRT_WORKER_STATE_STOP);
		// don't join calling thread
		if(!irt_thread_check_equality(&calling_thread, &(cur->thread))) {
			irt_mutex_unlock(&irt_g_degree_of_parallelism_mutex);
			if(irt_atomic_load(&cur->state) != IRT_WORKER_STATE_JOINED) { irt_thread_join(&(cur->thread)); }
			irt_mutex_lock(&irt_g_degree_of_parallelism_mutex);
			irt_atomic_store(&cur->state, IRT_WORKER_STATE_JOINED);
		}
		irt_spin_unlock(&cur->shutdown_lock);
	}
	irt_mutex_unlock(&irt_g_degree_of_parallelism_mutex);

	// clean up after all workers have finished running
	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		irt_worker_cleanup(irt_g_workers[i]);
	}
}


void irt_worker_cleanup(irt_worker* self) {
	irt_spin_destroy(&self->shutdown_lock);
	// clean up WI reuse stack
	{
		irt_work_item *cur, *next;
		cur = self->wi_reuse_stack;
		while(cur) {
			next = cur->next_reuse;
			free(cur);
			cur = next;
		}
		self->wi_reuse_stack = NULL;
	}
	// clean up event register reuse stacks
	{
		// wi registers
		irt_wi_event_register *cur, *next;
		cur = self->wi_ev_register_list;
		while(cur) {
			next = cur->lookup_table_next;
			irt_spin_destroy(&cur->lock);
			free(cur);
			cur = next;
		}
		self->wi_ev_register_list = NULL;
	}
	{
		// wg registers
		irt_wg_event_register *cur, *next;
		cur = self->wg_ev_register_list;
		while(cur) {
			next = cur->lookup_table_next;
			irt_spin_destroy(&cur->lock);
			free(cur);
			cur = next;
		}
		self->wg_ev_register_list = NULL;
	}
}


#endif // ifndef __GUARD_IMPL_WORKER_IMPL_H
