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

#include "utils/minlwt.h"
#include "work_item.h"
#include "wi_implementation.h"
#include "impl/error_handling.impl.h"

#ifdef __x86_64__

// ----------------------------------------------------------------------------
// x86-64 implementation

static inline void lwt_prepare(irt_work_item *wi, intptr_t *basestack) {
	wi->stack_start = (intptr_t)malloc(IRT_WI_STACK_SIZE);
	wi->stack_ptr = wi->stack_start + IRT_WI_STACK_SIZE;
}

// launch lwt for wi with implementation func and store current stack address in basestack
__attribute__ ((noinline))
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
//		"movq %%rsp, %%rbx \n"		/* save current stack pointer in caller save register B */
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
	#ifndef NDEBUG
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "NEVERMORE");
	#endif
}
__attribute__ ((noinline))
void lwt_continue(intptr_t *newstack, intptr_t *basestack) {
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
	: "a" (basestack), "c" (newstack) );
}
//__attribute__ ((noinline))
//void lwt_continue(intptr_t *newstack, intptr_t *basestack) {
//	IRT_DEBUG("CONTINUE Newstack before: %p, Basestack before: %p", *newstack, *basestack);
//	lwt_continue_impl(newstack, basestack);
//	IRT_DEBUG("CONTINUE Newstack after: %p, Basestack after: %p", *newstack, *basestack);
//}

// workaround required to tell gcc that lwt_end is not a leaf function
volatile int lwt_dummy = 0;
__attribute__ ((noinline))
void lwt_dummy_func() {
	lwt_dummy++;
}

__attribute__ ((noinline))
void lwt_end(intptr_t *basestack) {
	//IRT_DEBUG("lwt_end - A.");
	if(lwt_dummy) lwt_dummy_func();
	__asm__ volatile (
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
	: "c" (basestack) 
	/* : "%r15", "%r14", "%r13", "%r12", "%rdi", "%rbx", "%rbp", "%rsp", "memory" */ );
	//IRT_DEBUG("lwt_end - B.");
}

#else
//#ifdef __POWERPC__
//
//// ----------------------------------------------------------------------------
//// PPC implementation
//
//__attribute__ ((noinline))
//void lwt_start(irt_work_item *wi, intptr_t *basestack, wi_implementation_func* func) {
//}
//__attribute__ ((noinline))
//void lwt_continue(intptr_t *newstack, intptr_t *basestack) {
//	__asm__ (
//		/* save registers on stack */
//		"mflr 0 \n" // move link register to r0
//		"std 0, 8(r1)" // store link register on stack
//		"stdu 1,-340(1) \n" // make space on stack, store stack pointer 19*8 + 17*8 + 8 + 7*4 + 2*8
//		"std 31, \n"
//		/* swap stacks */
//		""
//		/* restore registers for other coroutine */
//		"addi 1, 1, 324 \n" // clear the stack frame
//		: /* no output registers */
//	: "a" (basestack), "c" (newstack) );
//}
//__attribute__ ((noinline))
//void lwt_end(intptr_t *basestack) {
//}
//
//#else

// ----------------------------------------------------------------------------
// Fallback ucontext implementation

static inline void lwt_prepare(irt_work_item *wi, ucontext_t *basestack) {
	wi->stack_start = (intptr_t)malloc(IRT_WI_STACK_SIZE);
	wi->stack_ptr.uc_link          = basestack;
	wi->stack_ptr.uc_stack.ss_sp   = (char*)wi->stack_start;
	wi->stack_ptr.uc_stack.ss_size = IRT_WI_STACK_SIZE;
	getcontext(&wi->stack_ptr);
}

static inline void lwt_start(irt_work_item *wi, ucontext_t *basestack, wi_implementation_func* func) {
	makecontext(&wi->stack_ptr, (void(*)(void))func, 1, wi);
	swapcontext(basestack, &wi->stack_ptr);
}
static inline lwt_continue(ucontext_t *newstack, ucontext_t *basestack) {
	swapcontext(basestack, newstack);
}
static inline lwt_end(ucontext_t *basestack) {
	setcontext(basestack);
}

//#endif
#endif
