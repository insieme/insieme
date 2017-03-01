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
void lwt_continue_impl_asm(irt_work_item* wi /*r0*/, wi_implementation_func* func /*r1*/, intptr_t* newstack /*r2*/, intptr_t* basestack /*r3*/);

// TODO [_GEMS]: noinline is not supported by lcc
//__attribute__ ((noinline))
void lwt_continue_impl(irt_work_item* wi, wi_implementation_func* func, intptr_t* newstack, intptr_t* basestack) {
	lwt_continue_impl_asm(wi, func, newstack, basestack);
}

#define LWT_STACK_SIZE 64
#define LWT_STACK_POS_R0 (LWT_STACK_SIZE - 4)
#define LWT_STACK_POS_R1 (LWT_STACK_SIZE - 8)
#define LWT_STACK_POS_R2 (LWT_STACK_SIZE - 12)
#define LWT_STACK_POS_R3 (LWT_STACK_SIZE - 16)
#define LWT_STACK_POS_R4 (LWT_STACK_SIZE - 20)
#define LWT_STACK_POS_R5 (LWT_STACK_SIZE - 24)
#define LWT_STACK_POS_R15 (LWT_STACK_SIZE - 28)
#define LWT_STACK_POS_R6 (LWT_STACK_SIZE - 32)
#define LWT_STACK_POS_R7 (LWT_STACK_SIZE - 36)
#define LWT_STACK_POS_R8 (LWT_STACK_SIZE - 40)
#define LWT_STACK_POS_R9 (LWT_STACK_SIZE - 44)
#define LWT_STACK_POS_R10 (LWT_STACK_SIZE - 48)
#define LWT_STACK_POS_R11 (LWT_STACK_SIZE - 52)
#define LWT_STACK_POS_R12 (LWT_STACK_SIZE - 56)
#define LWT_STACK_POS_R13 (LWT_STACK_SIZE - 60)


// TODO [_GEMS]: remove useless nops
asm void lwt_continue_impl_asm(irt_work_item* wi /*r0*/, wi_implementation_func* func /*r1*/, intptr_t* newstack /*r2*/, intptr_t* basestack /*r3*/) {
	; /* push caller saved registers (r0-r5) on stack (r14 = SP) */

	add r14, -LWT_STACK_SIZE;
	nop;
	sw r0, r14, LWT_STACK_POS_R0;
	sw r1, r14, LWT_STACK_POS_R1;
	sw r2, r14, LWT_STACK_POS_R2;
	sw r3, r14, LWT_STACK_POS_R3;
	sw r4, r14, LWT_STACK_POS_R4;
	sw r5, r14, LWT_STACK_POS_R5;
	sw r6, r14, LWT_STACK_POS_R6;
	sw r7, r14, LWT_STACK_POS_R7;
	sw r8, r14, LWT_STACK_POS_R8;
	sw r9, r14, LWT_STACK_POS_R9;
	sw r10, r14, LWT_STACK_POS_R10;
	sw r11, r14, LWT_STACK_POS_R11;
	sw r12, r14, LWT_STACK_POS_R12;
	sw r13, r14, LWT_STACK_POS_R13;

	; /* push LinkRegister (r15) on the stack (r14 = SP) */

	sw r15, r14, LWT_STACK_POS_R15;
	nop;

	; /* swap stacks */

	sw r14, r3;
	lw r14, r2;

	; /* call function if func != NULL */

	cmpeq r1, 0;
	bcc($ + 3); /* +1 to skip the branch, +1 to skip the nop (branch delay slot) and finally +1 to skip the call*/
	nop;
	call __irt_wi_trampoline; /* r0 still has wi, r1 still has func, so just call */
	nop;

	; /* restore registers for other coroutine */

	lw r0, r14, LWT_STACK_POS_R0;
	lw r1, r14, LWT_STACK_POS_R1;
	lw r2, r14, LWT_STACK_POS_R2;
	lw r3, r14, LWT_STACK_POS_R3;
	lw r4, r14, LWT_STACK_POS_R4;
	lw r5, r14, LWT_STACK_POS_R5;
	lw r15, r14, LWT_STACK_POS_R15;
	lw r6, r14, LWT_STACK_POS_R6;
	lw r7, r14, LWT_STACK_POS_R7;
	lw r8, r14, LWT_STACK_POS_R8;
	lw r9, r14, LWT_STACK_POS_R9;
	lw r10, r14, LWT_STACK_POS_R10;
	lw r11, r14, LWT_STACK_POS_R11;
	lw r12, r14, LWT_STACK_POS_R12;
	lw r13, r14, LWT_STACK_POS_R13;
	nop;

	b r15;
	add r14, LWT_STACK_SIZE;
}
