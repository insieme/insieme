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
// currently only a 64 bit implementation exists
#if __GNUC_PREREQ(4, 8)
__attribute__((noinline,noclone,optimize(0),aligned(16)))
#elif __GNUC_PREREQ(4, 4)
__attribute__((noinline,noclone,optimize(0)))
#else
__attribute__((noinline))
#endif
void lwt_continue_impl(irt_work_item *wi /*rdi*/, wi_implementation_func* func /*rsi*/,
                       intptr_t *newstack /*rdx*/, intptr_t *basestack /*rcx*/) {
	__asm__(
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
	    "jrcxz 1f ;" /* jump to local label 1 - forward */
	    /* rdi still has wi, rsi still has func, so just call */
	    "call *%%rax ;"
	    /* restore registers for other coroutine */
	    "1:" /* the target of the jump */
	    "pop %%r15 ;"
	    "pop %%r14 ;"
	    "pop %%r13 ;"
	    "pop %%r12 ;"
	    "pop %%rbx ;"
	    "pop %%rbp ;"
	    : /* no output registers */
	    : "a"(&_irt_wi_trampoline));
}

__attribute__((noinline)) void lwt_get_stack_ptr(intptr_t* dest /*rdi*/) {
	__asm__(
	    /* store sp */
	    "movq %%rsp, (%%rdi) ;"
	    :
	    :);
}
