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

#if defined(__MINGW64__)

	// x64 implementation

	__attribute__ ((noinline))
	void lwt_continue_impl(irt_work_item *wi /*rcx*/, wi_implementation_func* func /*rdx*/, 
			intptr_t *newstack /*r8*/, intptr_t *basestack /*r9*/) {
		__asm__ (
			/* save callee save registers on stack: RBX, RBP, RDI, RSI, R12, R13, R14, and R15*/
			"push %%rbp ;"
			"push %%rbx ;"
			"push %%rdi ;" 
			"push %%rsi ;"
			"push %%r12 ;"
			"push %%r13 ;"
			"push %%r14 ;"
			"push %%r15 ;"
			/* swap stacks */
			"movq %%rsp, (%%r9) ;"
			"movq (%%r8), %%rsp ;"	// copy value at address pointed to by r8 into rsp
			// save rcx (*wi) in rbx because it will be overwritten by next instruction
			"movq %%rcx, %%rbx ;"
			"movq %%rdx, %%rcx ;"
			/* call function if func != NULL */
			"jrcxz .NOCALL ;"
			/* rdx still holds func address, rcx needs to be restored, then call */
			"movq %%rbx, %%rcx ;"
			"call *%%rax  ;"
			/* restore registers for other coroutine */
			".NOCALL: "
			"pop %%r15  ;"
			"pop %%r14  ;"
			"pop %%r13  ;"
			"pop %%r12  ;"
			"pop %%rsi  ;"
			"pop %%rdi  ;"
			"pop %%rbx  ;"
			"pop %%rbp  ;"
			: /* no output registers */
			: "a" (&_irt_wi_trampoline)
		);
	} 

	__attribute__ ((noinline))
	void lwt_get_stack_ptr(intptr_t *dest /*rcx*/) {
		__asm__ (
			/* store sp */
			"movq %%rsp, (%%rcx);"
		: : );
	}
	

#elif defined(__MINGW32__)

	// 32 bit implementation

	// we use __fastcall calling convention to have first two parameters put into registers ecx and edx
	__attribute__ ((noinline))
	void __fastcall lwt_continue_impl(irt_work_item *wi /*ecx*/, wi_implementation_func* func /*edx*/, 
			intptr_t *newstack /* pushed on stack */, intptr_t *basestack /* pushed on stack */) { 
		__asm__ (
			/* save registers on stack EBX, ESI, EDI, EBP */
			"push %%ebp ;"
			"push %%edi ;"
			"push %%esi ;" 
			"push %%ebx ;"
			
			/* swap stacks */
			
			/* third param newstack at 0x8(%ebp) = 8(%ebp) */
			/* fourth param basestack at 0xc(%ebp) = 12(%ebp) */

			/* save current esp to basestack  */
			"mov 12(%%ebp), %%ebx ;" // write basestack pointer address to ebx
			"mov %%esp, (%%ebx) ;" // at the memory address which ebx points to, write esp

			/* mov newstack into esp */
			"mov 8(%%ebp), %%ebx ;"	// write newstack pointer address to ebx
			"mov (%%ebx), %%esp ;"  // write memory address held by ebx to esp

			// save ecx because it will be overwritten
			"mov %%ecx, %%ebx ;"

			/* mov func address to ecx because we will use jecxz */
			"mov %%edx, %%ecx ;"
			
			/* call function if func != NULL */
			"jecxz .NOCALL ;"
			
			/* edx still holds func address, ecx needs to be restored, then call */
			"mov %%ebx, %%ecx ;"
			"call *%%eax  ;"
			
			/* restore registers for other coroutine */
			".NOCALL: "
			"pop %%ebx  ;"
			"pop %%esi  ;"
			"pop %%edi  ;"
			"pop %%ebp  ;"
			: /* no output registers */
			: "a" (&_irt_wi_trampoline) // move address of trampoline function to eax
		);
	}
	
	__attribute__ ((noinline))
	void __fastcall lwt_get_stack_ptr(intptr_t *dest /*ecx*/) {
		__asm__ (
			/* store sp */
			"movq %%esp, (%%ecx);"
		: : );
	}

#endif




