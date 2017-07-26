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
#ifndef __GUARD_ABSTRACTION_IMPL_RDTSC_WIN_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_RDTSC_WIN_IMPL_H

#include <Windows.h>
#include "irt_inttypes.h"

// prototypes
/*
extern "C" {
    uint64 irt_time_ticks(void);
    bool irt_time_ticks_available();
    bool irt_time_ticks_constant();
}
*/

// a win64 application will have to link the assembly version
// since there is no support for inline assembly code under win64
#ifndef _WIN64
uint64 irt_time_ticks(void) {
	uint32 a, d;

	__asm {
		rdtsc
		mov a, eax
		mov d, edx
	}
	uint64 a64, d64;
	a64 = a;
	d64 = d;

	return (a64 | (d64 << 32));
}

// checks if rdtsc instruction is available
bool irt_time_ticks_available() {
	unsigned d;
	// with VS compiler, there is no need to preserve EAX, EBX, ECX, EDX, EDI, ESI register, see: http://msdn.microsoft.com/en-us/library/k1a8ss06(v=vs.80).aspx
	__asm {
		mov eax, 0x00000001 // mov targetreg, const
		cpuid
		mov d, edx
	}
	if((d & 0x00000010) > 0) {
		return 1;
	}
	else {
		return 0;
	}
}

bool irt_time_ticks_constant() {
	unsigned d;
	__asm {
		mov eax, 0x80000007
		cpuid
		mov d, edx
	}

	// the 8th bit represents the TscInvariant bit
	if((d & 0x00000100) > 0) {
		return 1;
	}
	else {
		return 0;
	}
}
#endif // NOT _WIN64


#endif // ifndef __GUARD_ABSTRACTION_IMPL_RDTSC_WIN_IMPL_H
