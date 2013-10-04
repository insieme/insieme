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

void lwt_continue_impl_asm(irt_work_item *wi /*r0*/, wi_implementation_func* func /*r1*/, intptr_t *newstack /*r2*/, intptr_t *basestack /*r3*/);

// TODO [_GEMS]: noinline is not supported by lcc
//__attribute__ ((noinline))
void lwt_continue_impl(irt_work_item *wi, wi_implementation_func* func, intptr_t *newstack, intptr_t *basestack) {
	lwt_continue_impl_asm(wi, func, newstack, basestack);
}

// TODO [_GEMS]: remove useless nops 
asm void lwt_continue_impl_asm(irt_work_item *wi /*r0*/, wi_implementation_func* func /*r1*/, intptr_t *newstack /*r2*/, intptr_t *basestack /*r3*/) {
	
	; /* push caller saved registers (r0-r5) on stack (r14 = SP) */
	
	add r14, -28
	nop;
	sw r0, r14, 24;
	sw r1, r14, 20;
	sw r2, r14, 16;
	sw r3, r14, 12;
	sw r4, r14,  8;
	sw r5, r14,  4;
	
	; /* push LinkRegister (r15) on the stack (r14 = SP) */
	
	sw r15, r14, 0;
	nop;
	
	; /* swap stacks */
	
	sw r14, r3;
	lw r14, r2;
	
	; /* call function if func != NULL */
	
	cmpeq r1, 0;
	bcc ($+3); /* +1 to skip the branch, +1 to skip the nop (branch delay slot) and finally +1 to skip the call*/
	nop;
	call __irt_wi_trampoline; /* r0 still has wi, r1 still has func, so just call */
	nop;
	
	; /* restore registers for other coroutine */
	
	lw r0, r14,  24;
	lw r1, r14,  20;
	lw r2, r14,  16;
	lw r3, r14,  12;
	lw r4, r14,   8;
	lw r5, r14,   4;
	lw r15, r14,  0;
	nop;
	
	b r15;
	add r14, 28;
}

