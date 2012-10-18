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

// ----------------------------------------------------------------------------
// minlwt.h -- minimal lightweight thread interface
// - PeterT

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#ifdef _MSC_VER
	#include "include_win32\inttypes.h"
#else
	#include <inttypes.h>
#endif

#include "declarations.h"

// determine if reusable stacks can be stolen from other worker's pools 
//#define LWT_STACK_STEALING_ENABLED

typedef struct _lwt_reused_stack {
	struct _lwt_reused_stack *next;

	#if defined(__GNUC__)
		char stack[] __attribute__ ((aligned (128)));
	//	char stack[] __attribute__ ((aligned (__BIGGEST_ALIGNMENT__))); // older versions of gcc don't like this
	#elif defined(__MINGW32__) || defined(__MINGW64__)
		char stack[] __attribute__((aligned(128)));
	#elif _MSC_VER
		__declspec(align(128))
		char stack[];
	#else
		#pragma warning "Unknown platform, stack unaligned" 
		char stack[];
	#endif

} lwt_reused_stack;

#if  defined(__x86_64__) || defined(_WIN32)
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

static inline void lwt_prepare(int tid, irt_work_item *wi, lwt_context *basestack);
static inline void lwt_recycle(int tid, irt_work_item *wi);
void lwt_start(irt_work_item *wi, lwt_context *basestack, wi_implementation_func* func);
void lwt_continue(lwt_context *newstack, lwt_context *basestack);
void lwt_end(lwt_context *basestack);
