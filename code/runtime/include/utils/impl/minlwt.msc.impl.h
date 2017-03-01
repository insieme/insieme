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
// 64bit Win has different calling conventions than 32bit Win
#ifdef _WIN64

// Microsoft VS Compiler does not support inline assembly code in 64 bit apps
// function implemented via assembly code: code/runtime/asm/win64asm.asm
// which is preassebled as code/runtime/asm/win64asm.obj
extern "C" {
void _cdecl lwt_continue_impl(irt_work_item* wi /*rcx*/, wi_implementation_func* func /*rdx*/, intptr_t* newstack /*r8*/, intptr_t* basestack /*r9*/);
}

#elif defined(_M_IX86)

// we use __fastcall calling convention to have first two parameters put into registers ecx and edx
void __fastcall lwt_continue_impl(irt_work_item* wi /*ecx*/, wi_implementation_func* func /*edx*/, intptr_t* newstack, intptr_t* basestack) {
	__asm {
		/* save registers on stack EBX, ESI, EDI, EBP */
		push ebp
		push ebx
		push edi
		push esi
		/* swap stacks */
		
		mov eax, basestack // move address of basestack into eax
		mov dword ptr [eax], esp // write stackpointer address into memory location of basestack
		mov eax, newstack
		mov esp, dword ptr [eax]
		    /* call function if func != NULL */
		mov eax, ecx
		mov ecx, edx
		jecxz NOCALL
		    /* edx still has func, ecx needs to be restored, then call */
		mov ecx, eax
		call _irt_wi_trampoline
		    /* restore registers for other coroutine */
		NOCALL:
		pop esi
		pop edi
		pop ebx
		pop ebp
	}
	;
}

#endif
