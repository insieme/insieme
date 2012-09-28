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

#pragma once

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
	if((d & 0x00000010) > 0)
		return 1;
	else
		return 0;
}

bool irt_time_ticks_constant() {
	unsigned d;
	__asm {
		mov eax, 0x80000007
		cpuid
		mov d, edx
	}

	// the 8th bit represents the TscInvariant bit
	if((d & 0x00000100) > 0)
		return 1;
	else
		return 0;
}
#endif // NOT _WIN64
