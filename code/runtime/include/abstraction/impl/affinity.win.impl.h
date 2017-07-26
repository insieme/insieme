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
#ifndef __GUARD_ABSTRACTION_IMPL_AFFINITY_WIN_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_AFFINITY_WIN_IMPL_H

#include "declarations.h"
#include "abstraction/affinity.os_dependent.h"

#pragma once

#include "utils/affinity.h"
#include <malloc.h>
#include <math.h>

#ifdef IRT_USE_PTHREADS
#include <sched.h>
#endif

void _irt_print_native_affinity_mask(irt_native_cpu_set mask) {
	uint32 bitmasklength = sizeof(irt_native_cpu_set) * 8;
	irt_native_cpu_set powered = 1;
	printf("native affinity mask (least significant bit first): ");
	for(uint32 i = 0; i < bitmasklength; i++) {
		if((powered & irt_g_affinity_base_mask) != 0) {
			printf("%s", "1");
		} else {
			printf("%s", "0");
		}
		powered = powered << 1; // this will overflow in the last iteration of the loop and yields 0, but the loop is not executed another time then
	}
	printf("\n");
}

void irt_clear_affinity() {
#ifdef IRT_USE_PTHREADS
	SetThreadAffinityMask(pthread_self().p, irt_g_affinity_base_mask);
	#else
	SetThreadAffinityMask(GetCurrentThread(), irt_g_affinity_base_mask);
	#endif
}

void irt_set_affinity(irt_affinity_mask irt_mask, irt_thread thread) {
	// if mask is empty -> clear any affinity and return
	if(irt_affinity_mask_is_empty(irt_mask)) {
		irt_clear_affinity();
		return;
	}

	// under windows it is assumed, that no system will have more than 64 processors, hence we only consider
	// the first bitmask of the mask_quads array
	uint64 mask = irt_mask.mask_quads[0];

	// pthread32 lib has no support for setting affinity, hence this WinAPI call must be used even when
	// using the pthread32 lib; the real handle for the thread is a component of the pthread_t struct
	// keep old_mask var for debugging
	#ifdef IRT_USE_PTHREADS
	DWORD_PTR old_mask = SetThreadAffinityMask(thread.p, mask);
	#else
	DWORD_PTR old_mask = SetThreadAffinityMask(thread.thread_handle, mask);
	#endif
}

uint32 _irt_affinity_next_available_physical(uint32 start) {
	int bitmasklength = sizeof(irt_native_cpu_set) * 8;
	irt_native_cpu_set powered = 1 << start; // eg start=3 => ...001000
	for(uint32 i = start; i < bitmasklength; i++) {
		if((powered & irt_g_affinity_base_mask) != 0) { return i; }
		powered = powered << 1; // this will overflow in the last iteration of the loop and yields 0, but the loop is not executed another time then
	}
	return UINT_MAX;
}

// gets initial affinity mask and sets irt_g_affinity_base_mask
void _irt_affinity_init_base_mask() {
	static bool initialized = false;

	if(initialized) { return; }

	DWORD_PTR sys_affinity_mask; // not really needed
	IRT_ASSERT(GetProcessAffinityMask(GetCurrentProcess(), &irt_g_affinity_base_mask, &sys_affinity_mask), IRT_ERR_INIT,
	           "Error retrieving initial affinity mask.");

	initialized = true;
}

void irt_affinity_init_physical_mapping(irt_affinity_physical_mapping* out_mapping) {
	uint32 cur = 0;
	uint32 i = 0;

	_irt_affinity_init_base_mask();

	for(i = 0; i < IRT_MAX_CORES; ++i) {
		out_mapping->map[i] = _irt_affinity_next_available_physical(cur);
		// printf("Physical affinity map: %u => %u\n", i, out_mapping->map[i]);
		if(out_mapping->map[i] == UINT_MAX) { break; }
		cur = out_mapping->map[i] + 1;
	}
	for(i = i + 1; i < IRT_MAX_CORES; ++i) { // fill remaining invalid cores, if any
		out_mapping->map[i] = UINT_MAX;
		// printf("Physical affinity map: %u => %u\n", i, out_mapping->map[i]);
	}
}

uint32 irt_affinity_cores_available() {
	_irt_affinity_init_base_mask();

	int bitmasklength = sizeof(irt_native_cpu_set) * 8;
	irt_native_cpu_set powered = 1;
	uint32 count = 0;
	for(uint32 i = 0; i < bitmasklength; i++) {
		if((powered & irt_g_affinity_base_mask) != 0) { ++count; }
		powered = powered << 1;
	}
	return count;
}


#endif // ifndef __GUARD_ABSTRACTION_IMPL_AFFINITY_WIN_IMPL_H
