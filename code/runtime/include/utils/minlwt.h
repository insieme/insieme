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
#ifndef __GUARD_UTILS_MINLWT_H
#define __GUARD_UTILS_MINLWT_H

// ----------------------------------------------------------------------------
// minlwt.h -- minimal lightweight thread interface
// - PeterT

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#ifdef _MSC_VER
#include "include_win32\inttypes.h"
#elif defined _GEMS_SIM
#include "include_gems/stdint.h"
#else
#include <inttypes.h>
#endif

#include "declarations.h"

// determine if reusable stacks can be stolen from other worker's pools
//#define LWT_STACK_STEALING_ENABLED

#define LWT_STACK_ALIGNMENT 128

typedef struct _lwt_reused_stack {
	struct _lwt_reused_stack* next;

	#if defined(__GNUC__)
	char stack[] __attribute__((aligned(LWT_STACK_ALIGNMENT)));
	//	char stack[] __attribute__ ((aligned (__BIGGEST_ALIGNMENT__))); // older versions of gcc don't like this
	#elif defined(__MINGW32__) || defined(__MINGW64__)
	char stack[] __attribute__((aligned(LWT_STACK_ALIGNMENT)));
	#elif _MSC_VER
	__declspec(align(LWT_STACK_ALIGNMENT)) char stack[];
	#else
	#pragma warning "Unknown platform, stack unaligned"
	char stack[];
	#endif

} lwt_reused_stack;

#if defined(__x86_64__) || defined(_WIN32) || defined(_GEMS_SIM) || defined(__arm__)
//#if 0 // for testing the ucontext fallback on x64 systems
#define USING_MINLWT 1
typedef intptr_t lwt_context;
#else
#ifdef _MSC_VER
#include "include_win32/ucontext.h"
#else // _MSC_VER
#include <ucontext.h>
#endif // _MSC_VER
typedef ucontext_t lwt_context;
#endif

static inline void lwt_prepare(int tid, irt_work_item* wi, lwt_context* basestack);
static inline void lwt_recycle(int tid, irt_work_item* wi);
void lwt_start(irt_work_item* wi, lwt_context* basestack, wi_implementation_func* func);
void lwt_continue(lwt_context* newstack, lwt_context* basestack);
void lwt_end(lwt_context* basestack);
void lwt_get_stack_ptr(lwt_context* dest);


#endif // ifndef __GUARD_UTILS_MINLWT_H
