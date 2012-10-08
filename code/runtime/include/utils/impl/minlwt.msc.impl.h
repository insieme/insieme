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

// 64bit Win has different calling conventions than 32bit Win
#ifdef _WIN64

	// Microsoft VS Compiler does not support inline assembly code in 64 bit apps
	// function implemented via assembly code: code/runtime/asm/win64asm.asm
	// which is preassebled as code/runtime/asm/win64asm.obj
	extern "C" {
		void _cdecl lwt_continue_impl(irt_work_item *wi /*rcx*/, wi_implementation_func* func /*rdx*/,	intptr_t *newstack /*r8*/, intptr_t *basestack /*r9*/);
	}

#elif defined(_M_IX86)
	
	// we use __fastcall calling convention to have first two parameters put into registers ecx and edx
	void __fastcall lwt_continue_impl(irt_work_item *wi /*ecx*/, wi_implementation_func* func /*edx*/, 
			intptr_t *newstack, intptr_t *basestack) { 

		__asm {
			/* save registers on stack EBX, ESI, EDI, EBP */
			push ebp 
			push ebx 
			push edi 
			push esi  
			/* swap stacks */

			mov eax, basestack	// move address of basestack into eax
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
		};
	}

#endif