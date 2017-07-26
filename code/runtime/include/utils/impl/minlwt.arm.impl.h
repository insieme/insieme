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

#if __GNUC_PREREQ(4, 8)
__attribute__((noinline,noclone,optimize(0),aligned(16)))
#else
__attribute__((noinline,noclone,optimize(0)))
#endif
void lwt_continue_impl(irt_work_item *wi /*r0*/, wi_implementation_func* func /*r1*/, intptr_t *newstack /*r2*/, intptr_t *basestack /*r3*/) {

	__asm("push {r4-r11, lr};"    // save LR, R4-R11
	      "vstmdb sp!, {d8-d15};" // save vector regs

	      /* swap stacks */
	      "str r13, [r3];"
	      "ldr r13, [r2];"

	      /* call function if func != NULL */
	      "cmp r1, #0;"
	      "beq endlab;");

	_irt_wi_trampoline(wi, func);

	__asm(
	    /* restore registers for other coroutine */
	    "endlab:"
	    "vldmia sp!, {d8-d15};" // restore vector regs
	    "pop {r4-r11, lr};"     // restore R4-R11, return to saved LR
	    );
}
