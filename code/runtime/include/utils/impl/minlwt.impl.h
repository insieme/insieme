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
#ifndef __GUARD_UTILS_IMPL_MINLWT_IMPL_H
#define __GUARD_UTILS_IMPL_MINLWT_IMPL_H

#include "utils/minlwt.h"
#include "work_item.h"
#include "wi_implementation.h"
#include "impl/error_handling.impl.h"
#include "abstraction/atomic.h"

//#include <sys/mman.h> /* not required for now, mmap not used (see below) */

struct _lwt_g_stack_reuse {
	lwt_reused_stack* stacks[IRT_MAX_WORKERS];
} lwt_g_stack_reuse;

lwt_reused_stack* _lwt_get_stack(int w_id) {
	lwt_reused_stack* ret = lwt_g_stack_reuse.stacks[w_id];
	#ifdef LWT_STACK_STEALING_ENABLED
	if(ret) {
		if(irt_atomic_bool_compare_and_swap(&lwt_g_stack_reuse.stacks[w_id], ret, ret->next, intptr_t)) {
			// IRT_DEBUG("LWT_RE\n");
			return ret;
		} else {
			return _lwt_get_stack(w_id);
		}
	} else {
		for(int i = 0; i < irt_g_worker_count; ++i) {
			ret = lwt_g_stack_reuse.stacks[i];
			if(ret && irt_atomic_bool_compare_and_swap(&lwt_g_stack_reuse.stacks[i], ret, ret->next, intptr_t)) {
				// IRT_DEBUG("LWT_ST\n");
				return ret;
			}
		}
	}
	#else
	if(ret) {
		// IRT_DEBUG("LWT_RE\n");
		// IRT_VERBOSE_ONLY(
		//{
		//	int num_stacks=1;
		//	lwt_reused_stack* cur = ret;
		//	while(cur = cur->next) num_stacks++;
		//	printf("-- %d, Reusing stack %p, %d stack(s) available:\n", w_id, (void*) ret, num_stacks);
		//	cur = ret;
		//	printf("---- ");
		//	do { printf("%p, ", (void*) cur);  } while(cur = cur->next);
		//	printf("\n");
		//});
		lwt_g_stack_reuse.stacks[w_id] = ret->next;
		return ret;
	}
	#endif

	// create new
	// static unsigned long long total = 0;
	// total += sizeof(lwt_reused_stack) + IRT_WI_STACK_SIZE;
	// printf("Total allocated: %6.2lf MB\n", total/(1024.0*1024.0));
	// TODO [_GEMS]: we need +4 because of gemsclaim compiler generated instruction: when entering a function call the sp is stored on the stack
	ret = (lwt_reused_stack*)malloc(sizeof(lwt_reused_stack) + IRT_WI_STACK_SIZE + 4);

	IRT_ASSERT(ret != NULL, IRT_ERR_IO, "Malloc of lwt stack failed.\n");

	ret->next = NULL;
	return ret;
}

static inline void lwt_recycle(int tid, irt_work_item* wi) {
	if(!wi->stack_storage) {
	#ifdef IRT_ASTEROIDEA_STACKS
		// make parent stack available again
		irt_work_item* parent = wi->parent_id.cached;
		IRT_DEBUG(" + %p returning stack to %p\n", (void*)wi, (void*)parent);
		IRT_ASSERT(parent != NULL, IRT_ERR_INTERNAL, "Asteroidea: No parent and no stack storage.\n");
		IRT_ASSERT(irt_atomic_bool_compare_and_swap(&parent->stack_available, false, true, intptr_t), IRT_ERR_INTERNAL,
		           "Asteroidea: Could not return stack.\n");
		#endif // IRT_ASTEROIDEA_STACKS
		return;
	}
	#ifdef LWT_STACK_STEALING_ENABLED
	for(;;) {
		lwt_reused_stack* top = lwt_g_stack_reuse.stacks[tid];
		wi->stack_storage->next = top;
		if(irt_atomic_bool_compare_and_swap(&lwt_g_stack_reuse.stacks[tid], top, wi->stack_storage, intptr_t)) {
			// IRT_DEBUG("LWT_CYC\n");
			return;
		} else {
			// IRT_DEBUG("LWT_FCY\n");
		}
	}
	#else
	// IRT_VERBOSE_ONLY(
	//{
	//	int num_stacks=0;
	//	if(lwt_g_stack_reuse.stacks[tid]) {
	//		num_stacks=1;
	//		lwt_reused_stack* cur = lwt_g_stack_reuse.stacks[tid];
	//		while(cur = cur->next) num_stacks++;
	//	}
	//	printf("-- %d, Recycling stack %p, %d stack(s) available:\n", tid, (void*) wi->stack_storage, num_stacks);
	//	if(lwt_g_stack_reuse.stacks[tid]) {
	//		lwt_reused_stack* cur = lwt_g_stack_reuse.stacks[tid];
	//		printf("---- ");
	//		do { printf("%p, ", (void*) cur);  } while(cur = cur->next);
	//		printf("\n");
	//	}
	//});
	wi->stack_storage->next = lwt_g_stack_reuse.stacks[tid];
	lwt_g_stack_reuse.stacks[tid] = wi->stack_storage;
	wi->stack_storage = NULL;
	// printf("Reuse!");
	// IRT_DEBUG("LWT_CYC\n");
	#endif
}

#ifdef USING_MINLWT

// ----------------------------------------------------------------------------
// x86-64 implementation

static inline void lwt_prepare(int tid, irt_work_item* wi, intptr_t* basestack) {
#ifdef IRT_ASTEROIDEA_STACKS
	// if parent stack is available, reuse it
	irt_work_item* parent = wi->parent_id.cached;
	if(parent) {
		if(irt_atomic_bool_compare_and_swap(&parent->stack_available, true, false, intptr_t)) {
			/* NOTE:
			 * Here we are in a window of vulnerability and we have to take measures to avoid a race condition.
			 * For an explanation and the solution look in function _irt_wi_join_all_event in file work_item.impl.h
			 */
			// only use the stack if our parent runs no immediate sibling at the moment
			if(parent->num_active_children == wi->parent_num_active_children) {
				IRT_DEBUG(" + %p taking stack from %p\n", (void*)wi, (void*)parent);
				IRT_DEBUG("   %p child count: %d\n", (void*)parent, *parent->num_active_children);
				wi->stack_storage = NULL;
				wi->stack_ptr = parent->stack_ptr - 8;
				wi->stack_ptr -= wi->stack_ptr % LWT_STACK_ALIGNMENT;
				return;
			} else {
				// otherwise give it back, since we shouldn't have taken it in the first place
				IRT_ASSERT(irt_atomic_bool_compare_and_swap(&parent->stack_available, false, true, bool), IRT_ERR_INTERNAL,
				           "Asteroidea: Could not return stack to parent.\n");
			}
		}
	}
	#endif

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


// as assembly syntax varies with each compiler (and platform) we switch implementation depending on compiler
#ifdef _MSC_VER // MS VS Compiler

#include "minlwt.msc.impl.h"

#elif defined(__MINGW64__) || defined(__MINGW32__) // any mingw compiler

#include "minlwt.mingw.impl.h"

#elif defined(_GEMS_SIM)

#include "minlwt.gems.impl.h"

#elif defined(__arm__)

#include "minlwt.arm.impl.h"

#else // eg. GCC (on Linux), if not gcc compiler we will get an error anyway

#include "minlwt.gcc.impl.h"

#endif


void lwt_start(irt_work_item* wi, intptr_t* basestack, wi_implementation_func* func) {
	IRT_DEBUG_ONLY(
	    // dirty hack to print the function pointer in debug output, as function pointers can't be legally casted to void pointers.
	    // This solution may still be undefined behaviour, but it will probably do what it is supposed to do for our debugging purposes
	    union {
		    wi_implementation_func* f;
		    void* pt;
		} hack;
	    hack.f = func; IRT_DEBUG("START WI: %p, Basestack: %p, func: %p", (void*)wi, (void*)*basestack, hack.pt);)
	lwt_continue_impl(wi, func, &wi->stack_ptr, basestack);
}
void lwt_continue(intptr_t* newstack, intptr_t* basestack) {
	IRT_DEBUG("CONTINUE Newstack before: %p, Basestack before: %p", (void*)*newstack, (void*)*basestack);
	lwt_continue_impl(NULL, NULL, newstack, basestack);
	IRT_DEBUG("CONTINUE Newstack after: %p, Basestack after: %p", (void*)*newstack, (void*)*basestack);
}
void lwt_end(intptr_t* basestack) {
	IRT_DEBUG("END Basestack: %p", (void*)*basestack);
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
// void lwt_start(irt_work_item *wi, intptr_t *basestack, wi_implementation_func* func) {
//}
//__attribute__ ((noinline))
// void lwt_continue(intptr_t *newstack, intptr_t *basestack) {
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
// void lwt_end(intptr_t *basestack) {
//}
//
//#else

// ----------------------------------------------------------------------------
// Fallback ucontext implementation

static inline void lwt_prepare(int tid, irt_work_item* wi, lwt_context* basestack) {
	wi->stack_storage = _lwt_get_stack(tid);
	wi->stack_ptr.uc_link = basestack;
	wi->stack_ptr.uc_stack.ss_sp = (char*)wi->stack_storage->stack;
	wi->stack_ptr.uc_stack.ss_size = IRT_WI_STACK_SIZE;
	getcontext(&wi->stack_ptr);
}

void lwt_start(irt_work_item* wi, lwt_context* basestack, wi_implementation_func* func) {
	makecontext(&wi->stack_ptr, (void (*)(void)) & _irt_wi_trampoline, 2, wi, func);
	swapcontext(basestack, &wi->stack_ptr);
}
void lwt_continue(lwt_context* newstack, lwt_context* basestack) {
	swapcontext(basestack, newstack);
}
void lwt_end(lwt_context* basestack) {
	setcontext(basestack);
}

//#endif
#endif


#endif // ifndef __GUARD_UTILS_IMPL_MINLWT_IMPL_H
