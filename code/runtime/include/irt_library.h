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
#ifndef __GUARD_IRT_LIBRARY_H
#define __GUARD_IRT_LIBRARY_H

#include "irt_all_impls.h"
#include "standalone.h"

/////////////////////////////////////////////////////////////////////////////// Startup

// This is what the original program main will be renamed to
int _irt_lib_renamed_main(int argc, char** argv);

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
	char **argv;
} _irt_lib_startup_lwdi;

// Trampoline function for library startup
void _irt_lib_wi_startup_func(irt_work_item* wi) {
	_irt_lib_startup_lwdi* lwdi = (_irt_lib_startup_lwdi*)wi->parameters;
	_irt_lib_renamed_main(lwdi->argc, lwdi->argv);
}

// Library main, replaces program main
int main(int argc, char **argv) {
	irt_wi_implementation_variant impl_var = { &_irt_lib_wi_startup_func, 0, NULL, 0, NULL, NULL, NULL };
	irt_wi_implementation impl = { -1, 1, &impl_var };
	_irt_lib_startup_lwdi params = { - (int32)sizeof(_irt_lib_startup_lwdi), argc, argv };

	irt_runtime_standalone(irt_get_default_worker_count(), &_irt_lib_context_fun, &_irt_lib_context_fun, &impl, (irt_lw_data_item*)&params);
}
// Rename original main in order to embed execution in a worker
#define main(__argc, __argv) _irt_lib_renamed_main(__argc, __argv)

/////////////////////////////////////////////////////////////////////////////// Parallel

typedef void (*voidfp)(void*);

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

// Execute between "min" and "max" parallel instances of "fun",
// passing "data" of size "data_size" to each of them
irt_joinable irt_lib_parallel(uint32 min, uint32 max, voidfp fun, void* data, size_t data_size) {

	// static wi implementation (immutable)
	static irt_wi_implementation_variant impl_var = { &_irt_lib_wi_implementation_func, 0, NULL, 0, NULL, NULL, NULL };
	static irt_wi_implementation impl = { -1, 1, &impl_var };

	// build lightweight data item
	int32 lwdi_size = data_size + sizeof(_irt_lib_lwdi);
	_irt_lib_lwdi *lwdi = (_irt_lib_lwdi*)alloca(lwdi_size);
	*lwdi = (_irt_lib_lwdi){ -lwdi_size /* negative == direct size, no data item table entry */, fun };
	memcpy(lwdi->data, data, data_size);

	// start it
	irt_parallel_job job = { min, max, 1, &impl, (irt_lw_data_item*)lwdi };
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
	for(int64 i=wi->range.begin; i<wi->range.end; i+=wi->range.step) {
		lwdi->function(i, (void*)lwdi->data);
	}	
}

void irt_lib_pfor(int64 begin, int64 end, int64 step, loopfp body, void* data, size_t data_size) {

	// static wi implementation (immutable)
	static irt_wi_implementation_variant impl_var = { &_irt_lib_wi_pfor_func, 0, NULL, 0, NULL, NULL, NULL };
	static irt_wi_implementation impl = { -2, 1, &impl_var };

	// build lightweight data item
	int32 lwdi_size = data_size + sizeof(_irt_lib_pfor_lwdi);
	_irt_lib_pfor_lwdi *lwdi = (_irt_lib_pfor_lwdi*)alloca(lwdi_size);
	*lwdi = (_irt_lib_pfor_lwdi){ -lwdi_size /* negative == direct size, no data item table entry */, body };
	memcpy(lwdi->data, data, data_size);

	// start it
	irt_work_item_range range = { begin, end, step };
	irt_work_item *wi = irt_wi_get_current();
	irt_pfor(wi, wi->wg_memberships[0].wg_id.cached, range, &impl, (irt_lw_data_item*)lwdi);
}

#endif // ifndef __GUARD_IRT_LIBRARY_H
