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
#include "irt_atomic.h"

//#include <sys/mman.h> /* not required for now, mmap not used (see below) */

struct _lwt_g_stack_reuse {
	lwt_reused_stack* stacks[IRT_MAX_WORKERS];
} lwt_g_stack_reuse;

lwt_reused_stack* _lwt_get_stack(int w_id) {
	lwt_reused_stack* ret = lwt_g_stack_reuse.stacks[w_id];
#ifdef LWT_STACK_STEALING_ENABLED
	if(ret) {
		if(irt_atomic_bool_compare_and_swap(&lwt_g_stack_reuse.stacks[w_id], ret, ret->next)) {
			//IRT_DEBUG("LWT_RE\n");
			return ret;
		} else {
			return _lwt_get_stack(w_id);
		}
	} else {
		for(int i=0; i<irt_g_worker_count; ++i) {
			ret = lwt_g_stack_reuse.stacks[i];
			if(ret && irt_atomic_bool_compare_and_swap(&lwt_g_stack_reuse.stacks[i], ret, ret->next)) {
				//IRT_DEBUG("LWT_ST\n");
				return ret;
			}
		}
	}
#else
	if(ret) {
		//IRT_DEBUG("LWT_RE\n");
		//IRT_VERBOSE_ONLY(
		//{
		//	int num_stacks=1;
		//	lwt_reused_stack* cur = ret;
		//	while(cur = cur->next) num_stacks++;
		//	printf("-- %d, Reusing stack %p, %d stack(s) available:\n", w_id, ret, num_stacks);
		//	cur = ret;
		//	printf("---- ");
		//	do { printf("%p, ", cur);  } while(cur = cur->next);
		//	printf("\n");
		//});
		lwt_g_stack_reuse.stacks[w_id] = ret->next;
		return ret;
	}
#endif
	
	// create new
	//IRT_DEBUG("LWT_FU\n");
	ret = (lwt_reused_stack*)malloc(sizeof(lwt_reused_stack) + IRT_WI_STACK_SIZE);
	ret->next = NULL;
	return ret;
}

static inline void lwt_recycle(int tid, irt_work_item *wi) {
	if(!wi->stack_storage) return;
#ifdef LWT_STACK_STEALING_ENABLED
	for(;;) {
		lwt_reused_stack* top = lwt_g_stack_reuse.stacks[tid];
		wi->stack_storage->next = top;
		if(irt_atomic_bool_compare_and_swap(&lwt_g_stack_reuse.stacks[tid], top, wi->stack_storage)) {
			//IRT_DEBUG("LWT_CYC\n");
			return;
		} else {
			//IRT_DEBUG("LWT_FCY\n");
		}
	}
#else
	//IRT_VERBOSE_ONLY(
	//{
	//	int num_stacks=0;
	//	if(lwt_g_stack_reuse.stacks[tid]) {
	//		num_stacks=1;
	//		lwt_reused_stack* cur = lwt_g_stack_reuse.stacks[tid];
	//		while(cur = cur->next) num_stacks++;
	//	}
	//	printf("-- %d, Recycling stack %p, %d stack(s) available:\n", tid, wi->stack_storage, num_stacks);
	//	if(lwt_g_stack_reuse.stacks[tid]) {
	//		lwt_reused_stack* cur = lwt_g_stack_reuse.stacks[tid];
	//		printf("---- ");
	//		do { printf("%p, ", cur);  } while(cur = cur->next);
	//		printf("\n");
	//	}
	//});
	wi->stack_storage->next = lwt_g_stack_reuse.stacks[tid];
	lwt_g_stack_reuse.stacks[tid] = wi->stack_storage;
	wi->stack_storage = NULL;
	//IRT_DEBUG("LWT_CYC\n");
#endif
}

#ifdef USING_MINLWT

// ----------------------------------------------------------------------------
// x86-64 implementation

static inline void lwt_prepare(int tid, irt_work_item *wi, intptr_t *basestack) {
	// heap allocated thread memory
	wi->stack_storage = _lwt_get_stack(tid);
	wi->stack_ptr = (intptr_t)(&wi->stack_storage->stack) + IRT_WI_STACK_SIZE;

	// let stack be allocated by the OS kernel
	// see http://www.evanjones.ca/software/threading.html
	// section: Implementing Kernel Threads on Linux
//	wi->stack_ptr = (intptr_t)(mmap(NULL, IRT_WI_STACK_SIZE,
//			PROT_READ | PROT_WRITE,
//			MAP_PRIVATE | MAP_32BIT | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_STACK,
//			-1, 0)
//	);
//	wi->stack_start = wi->stack_ptr - IRT_WI_STACK_SIZE;
}

#ifdef __GNUC__
__attribute__ ((noinline))
#endif
void lwt_continue_impl(irt_work_item *wi /*rdi*/, wi_implementation_func* func /*rsi*/, 
		intptr_t *newstack /*rdx*/, intptr_t *basestack /*rcx*/) {
	__asm__ (
		/* save registers on stack */
		"push %%rbp ;"
		"push %%rbx ;"
		"push %%r12 ;"
		"push %%r13 ;"
		"push %%r14 ;"
		"push %%r15 ;"
		/* swap stacks */
		"movq %%rsp, (%%rcx) ;"
		"movq (%%rdx), %%rsp ;"
		/* call function if func != NULL */
		"movq %%rsi, %%rcx ;"
		"jrcxz .NOCALL ;"
		/* rdi still has wi, rsi still has func, so just call */
		"call *%%rax ;"
		/* restore registers for other coroutine */
		".NOCALL:"
		"pop %%r15 ;"
		"pop %%r14 ;"
		"pop %%r13 ;"
		"pop %%r12 ;"
		"pop %%rbx ;"
		"pop %%rbp ;"
	: /* no output registers */
	: "a" (&_irt_wi_trampoline) );
}
void lwt_start(irt_work_item *wi, intptr_t *basestack, wi_implementation_func* func) {
	IRT_DEBUG("START WI: %p, Basestack: %p, func: %p", wi, *basestack, func);
	lwt_continue_impl(wi, func, &wi->stack_ptr, basestack);
}
void lwt_continue(intptr_t *newstack, intptr_t *basestack) {
	IRT_DEBUG("CONTINUE Newstack before: %p, Basestack before: %p", *newstack, *basestack);
	lwt_continue_impl(NULL, NULL, newstack, basestack);
	IRT_DEBUG("CONTINUE Newstack after: %p, Basestack after: %p", *newstack, *basestack);
}
void lwt_end(intptr_t *basestack) {
	IRT_DEBUG("END Basestack: %p", *basestack);
	intptr_t dummy;
	lwt_continue_impl(NULL, NULL, basestack, &dummy);
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

static inline void lwt_prepare(int tid, irt_work_item *wi, lwt_context *basestack) {
	wi->stack_storage =  _lwt_get_stack(tid);
	wi->stack_ptr.uc_link          = basestack;
	wi->stack_ptr.uc_stack.ss_sp   = (char*)wi->stack_storage->stack;
	wi->stack_ptr.uc_stack.ss_size = IRT_WI_STACK_SIZE;
	getcontext(&wi->stack_ptr);
}

void lwt_start(irt_work_item *wi, lwt_context *basestack, wi_implementation_func* func) {
	makecontext(&wi->stack_ptr, (void(*)(void))func, 1, wi);
	swapcontext(basestack, &wi->stack_ptr);
}
void lwt_continue(lwt_context *newstack, lwt_context *basestack) {
	swapcontext(basestack, newstack);
}
void lwt_end(lwt_context *basestack) {
	setcontext(basestack);
}

//#endif
#endif
