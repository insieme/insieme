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

// TODO [_GEMS]: noinline is not supported by lcc
//__attribute__ ((noinline))
asm int lwt_continue_impl(irt_work_item *wi /*r0*/, wi_implementation_func* func /*r1*/,
	intptr_t *newstack /*r2*/, intptr_t *basestack /*r3*/) {

	; /* save registers on stack r0-r5 (r14 = SP) */

	add r14, -4;
	sw r0, r14, 0;
	add r14, -4;
	sw r1, r14, 0;
	add r14, -4;
	sw r2, r14, 0;
	add r14, -4;
	sw r3, r14, 0;
	add r14, -4;
	sw r4, r14, 0;
	add r14, -4;
	sw r5, r14, 0;
	
	; /* swap stacks */
	
	sw r14, r3, 0;
	lw r14, r2, 0;
	
	; /* call function if func != NULL */
	
	cmpeq r1, NULL;
	bcc ($+3); /* +1 to skip the branch, +1 to skip the nop (branch delay slot) and finally +1 to skip the call*/
	nop;
	call r1;
	
	; /* restore registers for other coroutine */
	
	lw r5, r14, 0;
	add r14, 4;
	lw r4, r14, 0;
	add r14, 4;
	lw r3, r14, 0;
	add r14, 4;
	lw r2, r14, 0;
	add r14, 4;
	lw r1, r14, 0;
	add r14, 4;
	lw r0, r14, 0;
	add r14, 4;

}
