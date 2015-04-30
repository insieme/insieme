/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#if __GNUC_PREREQ(4,8)
__attribute__ ((noinline,noclone,optimize(0),aligned(16)))
#else
__attribute__ ((noinline,noclone,optimize(0)))
#endif
void lwt_continue_impl(irt_work_item *wi /*r0*/, wi_implementation_func* func /*r1*/, intptr_t *newstack /*r2*/, intptr_t *basestack /*r3*/) { 

	__asm (
		"push {r4-r11, lr};"     // save LR, R4-R11
		"vstmdb sp!, {d8-d15};"  // save vector regs

		/* swap stacks */
		"str r13, [r3];"
		"ldr r13, [r2];"

		/* call function if func != NULL */
		"cmp r1, #0;"
		"beq endlab;"
	);

	_irt_wi_trampoline(wi, func);

	__asm (
		/* restore registers for other coroutine */
		"endlab:"
		"vldmia sp!, {d8-d15};"  // restore vector regs
		"pop {r4-r11, lr};"      // restore R4-R11, return to saved LR
	);
}

