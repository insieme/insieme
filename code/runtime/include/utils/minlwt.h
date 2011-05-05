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

#include <inttypes.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "work_item.h"
#include "wi_implementation.h"
#include "impl/error_handling.impl.h"

void lwt_start(irt_work_item *wi, intptr_t *basestack, wi_implementation_func* func);
void lwt_continue(irt_work_item *new_wi, intptr_t *basestack);
void lwt_end(intptr_t *basestack);


// determine system type
#if ( __WORDSIZE == 64 )
#define BUILD_64 1
#else
#if defined(__LP64__) || defined(_LP64)
#define BUILD_64   1
#endif
#endif

#ifdef BUILD_64

// ----------------------------------------------------------------------------
// x86-64 implementation

// launch lwt for wi with implementation func and store current stack address in basestack
void lwt_start(irt_work_item *wi, intptr_t *basestack, wi_implementation_func* func) {
	__asm__ volatile (		
		/* save registers on source stack */
		"push %%rbp \n"
		"push %%rbx \n"
		"push %%rdi \n"
		"push %%r12 \n"
		"push %%r13 \n"
		"push %%r14 \n"
		"push %%r15 \n"
		/* swap stacks */
		"movq %%rsp, (%%rax) \n"	/* save stack pointer in memory */
		"movq %%rsp, %%rbx \n"		/* save current stack pointer in caller save register B */
		"movq (%%rcx), %%rsp \n"	/* exchange stack pointer */
		/* retrieve function address and call */
		/* %rdi still contains arg */
		"call *%%rdx \n"		/* call procedure (using alternative stack) */
/* this call should never return */
//		"movq %%rbx, %%rsp \n"		/* restore stack pointer (of source stack) */
//		/* restore saved registers */
//		"pop %%r15 \n"
//		"pop %%r14 \n"
//		"pop %%r13 \n"
//		"pop %%r12 \n"
//		"pop %%rdi \n"
//		"pop %%rbx \n"
//		"pop %%rbp \n"
		: /* no output registers */
	: "a" (basestack), "c" (&(wi->stack_ptr)), "d" (func) );
}

void lwt_continue(irt_work_item *new_wi, intptr_t *basestack) {
	__asm__ (
		/* save registers on stack */
		"push %%rbp ;"
		"push %%rbx ;"
		"push %%rdi ;"
		"push %%r12 ;"
		"push %%r13 ;"
		"push %%r14 ;"
		"push %%r15 ;"
		/* swap stacks */
		"movq %%rsp, (%%rax) ;" 
		"movq (%%rcx), %%rsp ;"
		/* restore registers for other coroutine */
		"pop %%r15 ;"
		"pop %%r14 ;"
		"pop %%r13 ;"
		"pop %%r12 ;"
		"pop %%rdi ;"
		"pop %%rbx ;"
		"pop %%rbp ;"
		: /* no output registers */
	: "a" (basestack), "c" (new_wi->stack_ptr) );
}

void lwt_end(intptr_t *basestack) {
	__asm__ (
		/* swap stacks */
		"movq (%%rcx), %%rsp ;"
		/* restore registers for original callee */
		"pop %%r15 ;"
		"pop %%r14 ;"
		"pop %%r13 ;"
		"pop %%r12 ;"
		"pop %%rdi ;"
		"pop %%rbx ;"
		"pop %%rbp ;"
		: /* no output registers */
	: "c" (basestack) );
}

#else

// ----------------------------------------------------------------------------
// x86 implementation

// TODO
#pragma error "X86 implementation TODO"

#endif
