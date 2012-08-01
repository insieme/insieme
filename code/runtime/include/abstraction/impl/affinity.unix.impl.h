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

#include "utils/affinity.h"

void _irt_print_native_affinity_mask(irt_native_cpu_set mask) {
	for(int i=0; i<CPU_SETSIZE; i++) {
		IRT_INFO("%s", CPU_ISSET(i, &mask)?"1":"0");
	}
	IRT_INFO("\n");
}

void irt_clear_affinity() {
	// restore the base affinity mask
	// printf("restoring base affinity:\n"); _irt_print_native_affinity_mask(irt_g_affinity_base_mask); printf("\n");
	IRT_ASSERT(pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &irt_g_affinity_base_mask) == 0, 
		IRT_ERR_INIT, "Error clearing thread affinity.");
}

void irt_set_affinity(irt_affinity_mask irt_mask, pthread_t thread) {
	if(irt_affinity_mask_is_empty(irt_mask)) {
		irt_clear_affinity();
		return;
	}
	cpu_set_t mask;
	CPU_ZERO(&mask);
	for(uint64 i=0; i<IRT_MAX_CORES; ++i)
		if(irt_affinity_mask_is_set(irt_mask, i)) 
			CPU_SET(irt_g_affinity_physical_mapping.map[i], &mask);
	IRT_ASSERT(pthread_setaffinity_np(thread, sizeof(cpu_set_t), &mask) == 0, IRT_ERR_INIT, "Error setting thread affinity.");
}

uint32 _irt_affinity_next_available_physical(uint32 start) {
	for(uint32 i=start; i<CPU_SETSIZE; i++) {
		if(CPU_ISSET(i, &irt_g_affinity_base_mask)) return i;
	}
	return UINT_MAX;
}

void irt_affinity_init_physical_mapping(irt_affinity_physical_mapping *out_mapping) {
	uint32 cur = 0;
	uint32 i = 0;
	IRT_ASSERT(pthread_getaffinity_np(pthread_self(), sizeof(cpu_set_t), &irt_g_affinity_base_mask) == 0, 
		IRT_ERR_INIT, "Error retrieving program base affinity mask.");
	for(i=0; i<IRT_MAX_CORES; ++i) {
		out_mapping->map[i] = _irt_affinity_next_available_physical(cur);
		//printf("Physical affinity map: %u => %u\n", i, out_mapping->map[i]);
		if(out_mapping->map[i] == UINT_MAX) break;
		cur = out_mapping->map[i]+1;
	}
	for(i=i+1; i<IRT_MAX_CORES; ++i) { // fill remaining invalid cores, if any
		out_mapping->map[i] = UINT_MAX;
		//printf("Physical affinity map: %u => %u\n", i, out_mapping->map[i]);
	}
}

uint32 irt_affinity_cores_available() {
	IRT_ASSERT(pthread_getaffinity_np(pthread_self(), sizeof(cpu_set_t), &irt_g_affinity_base_mask) == 0, 
		IRT_ERR_INIT, "Error retrieving program base affinity mask.");
	uint32 count = 0;
	for(uint32 i=0; i<CPU_SETSIZE; ++i) {
		if(CPU_ISSET(i, &irt_g_affinity_base_mask)) ++count;
	}
	return count;
}
