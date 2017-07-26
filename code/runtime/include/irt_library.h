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
 */

#pragma once
#ifndef __GUARD_IRT_LIBRARY_H
#define __GUARD_IRT_LIBRARY_H

// the following function is outside of the extern c block to allow calling C++ main functions
// This is what the original program main will be renamed to
int _irt_lib_renamed_main(int argc, char** argv);

#ifdef __cplusplus
extern "C" {
#endif


#include "irt_all_impls.h"
#include "standalone.h"
#include "irt_joinable.h"

////////////////////////////////////////////////////////////////// Forward Declarations

void irt_lib_critical_init();

////////////////////////////////////////////////////////////////// Definitions

typedef void (*voidfp)(void*);
typedef void (*contextfp)(irt_context*);

// Lightweight DI used by RT library
typedef struct {
	int32 size;
	voidfp function;
	uint8 data[];
} _irt_lib_lwdi;

// Trampoline function for dynamically generated WIs
void _irt_lib_wi_implementation_func(irt_work_item* wi) {
	_irt_lib_lwdi* lwdi = (_irt_lib_lwdi*)wi->parameters;
	lwdi->function((void*)lwdi->data);
}

/////////////////////////////////////////////////////////////////////////////// Startup

// Context initialization and cleanup for the library use case are empty
void _irt_lib_context_fun(irt_context* c) {
	c->impl_table_size = 0;
	c->info_table_size = 0;
	c->type_table_size = 0;
	c->num_regions = 0;
}

// Lightweight DI used by library startup function
typedef struct {
	int32 size;
	int argc;
	char** argv;
} _irt_lib_startup_lwdi;

#ifndef IRT_LIBRARY_NO_MAIN_FUN

// Trampoline function for library startup
void _irt_lib_wi_startup_func(irt_work_item* wi) {
	_irt_lib_startup_lwdi* lwdi = (_irt_lib_startup_lwdi*)wi->parameters;
	_irt_lib_renamed_main(lwdi->argc, lwdi->argv);
}

// Library main, replaces program main
int main(int argc, char** argv) {
	irt_lib_critical_init();

	irt_wi_implementation_variant impl_var = {&_irt_lib_wi_startup_func, 0, NULL, 0, NULL, NULL, {0}};
	irt_wi_implementation impl = {-1, 1, &impl_var};
	_irt_lib_startup_lwdi params = {-(int32)sizeof(_irt_lib_startup_lwdi), argc, argv};

	irt_runtime_standalone(irt_get_default_worker_count(), &_irt_lib_context_fun, &_irt_lib_context_fun, &impl, (irt_lw_data_item*)&params);
}

#endif

// Starts runtime, runs the function as startup work item, then returns once it completes
void irt_lib_init_run(voidfp fun, void* data, size_t data_size) {
	irt_lib_critical_init();

	irt_wi_implementation_variant impl_var = {_irt_lib_wi_implementation_func, 0, NULL, 0, NULL, NULL, {0}};
	irt_wi_implementation impl = {-1, 1, &impl_var};
	int32 lwdi_size = data_size + sizeof(_irt_lib_lwdi);
	_irt_lib_lwdi* lwdi = (_irt_lib_lwdi*)alloca(lwdi_size);
	*lwdi = (_irt_lib_lwdi){-lwdi_size /* negative == direct size, no data item table entry */, fun};
	memcpy(lwdi->data, data, data_size);

	irt_runtime_standalone(irt_get_default_worker_count(), &_irt_lib_context_fun, &_irt_lib_context_fun, &impl, (irt_lw_data_item*)lwdi);
}

irt_context* irt_lib_g_context;

// Start runtime without scheduling any work items
void irt_lib_init(uint32 worker_count) {
	irt_lib_critical_init();
	irt_lib_g_context = irt_runtime_start_in_context(worker_count, &_irt_lib_context_fun, &_irt_lib_context_fun, true);
}

void irt_lib_init_in_context(uint32 worker_count, contextfp context_init, contextfp context_destroy) {
	irt_lib_critical_init();
	irt_lib_g_context = irt_runtime_start_in_context(worker_count, context_init, context_destroy, true);
}

// Run function as a work item within the (already inited) runtime, from a non-runtime thread
void irt_lib_run(voidfp fun, void* data, size_t data_size) {
	irt_wi_implementation_variant impl_var = {_irt_lib_wi_implementation_func, 0, NULL, 0, NULL, NULL, {0}};
	irt_wi_implementation impl = {-1, 1, &impl_var};
	int32 lwdi_size = data_size + sizeof(_irt_lib_lwdi);
	_irt_lib_lwdi* lwdi = (_irt_lib_lwdi*)alloca(lwdi_size);
	*lwdi = (_irt_lib_lwdi){-lwdi_size /* negative == direct size, no data item table entry */, fun};
	memcpy(lwdi->data, data, data_size);
	irt_runtime_run_wi(&impl, (irt_lw_data_item*)lwdi);
}

// End runtime
void irt_lib_shutdown() {
	if(irt_lib_g_context != NULL) { irt_runtime_end_in_context(irt_lib_g_context); }
	irt_lib_g_context = NULL;
}

// rename other main function
#define main(__argc, __argv) _irt_lib_renamed_main(__argc, __argv)

/////////////////////////////////////////////////////////////////////////////// Utils

irt_work_item* irt_lib_wi_get_current() {
	return irt_wi_get_current();
}

uint32 irt_lib_wi_get_wg_size(irt_work_item* wi, uint32 index) {
	return irt_wi_get_wg_size(wi, index);
}

uint32 irt_lib_wi_get_wg_num(irt_work_item* wi, uint32 index) {
	return irt_wi_get_wg_num(wi, index);
}

uint32 irt_lib_get_default_worker_count() {
	return irt_get_default_worker_count();
}

/////////////////////////////////////////////////////////////////////////////// Parallel

// Execute between "min" and "max" parallel instances of "fun",
// passing "data" of size "data_size" to each of them
irt_joinable irt_lib_parallel(uint32 min, uint32 max, voidfp fun, void* data, size_t data_size) {
	// static wi implementation (immutable)
	static irt_wi_implementation_variant impl_var = {&_irt_lib_wi_implementation_func, 0, NULL, 0, NULL, NULL, {0}};
	static irt_wi_implementation impl = {-1, 1, &impl_var};

	// build lightweight data item
	int32 lwdi_size = data_size + sizeof(_irt_lib_lwdi);
	_irt_lib_lwdi* lwdi = (_irt_lib_lwdi*)alloca(lwdi_size);
	*lwdi = (_irt_lib_lwdi){-lwdi_size /* negative == direct size, no data item table entry */, fun};
	memcpy(lwdi->data, data, data_size);

	// start it
	irt_parallel_job job = {min, max, 1, &impl, (irt_lw_data_item*)lwdi};
	return irt_parallel(&job);
}

void irt_lib_merge_all() {
	irt_wi_join_all(irt_wi_get_current());
}

/////////////////////////////////////////////////////////////////////////////// Loops

typedef void (*loopfp)(int64 index, void* data);

// Lightweight DI used by RT library for pfors
typedef struct {
	int32 size;
	loopfp function;
	uint8 data[];
} _irt_lib_pfor_lwdi;

// Trampoline function for dynamically generated loops
void _irt_lib_wi_pfor_func(irt_work_item* wi) {
	_irt_lib_pfor_lwdi* lwdi = (_irt_lib_pfor_lwdi*)wi->parameters;
	for(int64 i = wi->range.begin; i < wi->range.end; i += wi->range.step) {
		lwdi->function(i, (void*)lwdi->data);
	}
}

void irt_lib_pfor(int64 begin, int64 end, int64 step, loopfp body, void* data, size_t data_size) {
	// static wi implementation (immutable)
	static irt_wi_implementation_variant impl_var = {&_irt_lib_wi_pfor_func, 0, NULL, 0, NULL, NULL, {0}};
	static irt_wi_implementation impl = {-2, 1, &impl_var};

	// build lightweight data item
	int32 lwdi_size = data_size + sizeof(_irt_lib_pfor_lwdi);
	_irt_lib_pfor_lwdi* lwdi = (_irt_lib_pfor_lwdi*)alloca(lwdi_size);
	*lwdi = (_irt_lib_pfor_lwdi){-lwdi_size /* negative == direct size, no data item table entry */, body};
	memcpy(lwdi->data, data, data_size);

	// start it
	irt_work_item_range range = {begin, end, step};
	irt_work_item* wi = irt_wi_get_current();
	irt_pfor(wi, wi->wg_memberships[0].wg_id.cached, range, &impl, (irt_lw_data_item*)lwdi);
}

// optimized (sliced) loops

typedef void (*loopsfp)(int64 start, int64 end, int64 step, void* data);

// Lightweight DI used by RT library for sliced pfors
typedef struct {
	int32 size;
	loopsfp function;
	uint8 data[];
} _irt_lib_pfor_s_lwdi;

// Trampoline function for dynamically generated sliced loops
void _irt_lib_wi_pfor_s_func(irt_work_item* wi) {
	_irt_lib_pfor_s_lwdi* lwdi = (_irt_lib_pfor_s_lwdi*)wi->parameters;
	lwdi->function(wi->range.begin, wi->range.end, wi->range.step, (void*)lwdi->data);
}

void irt_lib_pfor_s(int64 begin, int64 end, int64 step, loopsfp body, void* data, size_t data_size) {
	// static wi implementation (immutable)
	static irt_wi_implementation_variant impl_var = {&_irt_lib_wi_pfor_s_func, 0, NULL, 0, NULL, NULL, {0}};
	static irt_wi_implementation impl = {-2, 1, &impl_var};

	// build lightweight data item
	int32 lwdi_size = data_size + sizeof(_irt_lib_pfor_lwdi);
	_irt_lib_pfor_s_lwdi* lwdi = (_irt_lib_pfor_s_lwdi*)alloca(lwdi_size);
	*lwdi = (_irt_lib_pfor_s_lwdi){-lwdi_size /* negative == direct size, no data item table entry */, body};
	memcpy(lwdi->data, data, data_size);

	// start it
	irt_work_item_range range = {begin, end, step};
	irt_work_item* wi = irt_wi_get_current();
	irt_pfor(wi, wi->wg_memberships[0].wg_id.cached, range, &impl, (irt_lw_data_item*)lwdi);
}

/////////////////////////////////////////////////////////////////////////////// Synchronization

void irt_lib_barrier() {
	irt_wg_barrier(irt_wi_get_wg(irt_wi_get_current(), 0));
}

// a "private" function maintaining the global lock instance
irt_lock* _irt_lib_get_critical_lock() {
	static irt_lock lock;
	return &lock;
}

// inits the critical lock
void irt_lib_critical_init() {
	irt_lock_init(_irt_lib_get_critical_lock());
}

// a function to mark the begin of a critical region
void irt_lib_critical_start() {
	irt_lock_acquire(_irt_lib_get_critical_lock());
}

// a function to mark the end of a critical region
void irt_lib_critical_end() {
	irt_lock_release(_irt_lib_get_critical_lock());
}

#ifdef __cplusplus
} // extern C
#endif
#endif // ifndef __GUARD_IRT_LIBRARY_H
