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

#include "declarations.h"
#include "hwinfo.h"
#include "globals.h"

/* needed for CPU_* macros */
#define _GNU_SOURCE 1

#define MAX_CORES 128
#define IRT_ENABLE_AFFINITY_ENV "IRT_ENABLE_AFFINITY"

typedef enum {
	IRT_AFFINITY_NONE = 0,
	IRT_AFFINITY_FILL = 10,
	IRT_AFFINITY_SKIP_1 = 11,
	IRT_AFFINITY_SKIP_2 = 12,
	IRT_AFFINITY_SKIP_3 = 13,
	IRT_AFFINITY_SKIP_4 = 14,
	IRT_AFFINITY_SKIP_5 = 15,
	IRT_AFFINITY_SKIP_6 = 16,
	IRT_AFFINITY_SKIP_7 = 17,
	IRT_AFFINITY_MAX_DISTANCE = 100
//	IRT_AFFINITY_MAX_SPREAD = 200
} irt_affinity_policy;

#include <sched.h>
#include <unistd.h>
#include <pthread.h>

#include "impl/error_handling.impl.h"

void _irt_print_affinity_mask(cpu_set_t mask) {
	for(int i=0; i<MAX_CORES; i++) {
		IRT_INFO("%s", CPU_ISSET(i, &mask)?"1":"0");
	}
	IRT_INFO("\n");
}

void irt_clear_affinity() {
	cpu_set_t mask;
	CPU_ZERO(&mask);
	for(int i=0; i<MAX_CORES; ++i) {
		CPU_SET(i, &mask);
	}
	IRT_ASSERT(pthread_setaffinity_np(pthread_self(), sizeof(mask), &mask) == 0, IRT_ERR_INIT, "Error clearing thread affinity.");
}

void irt_set_affinity(irt_affinity_mask irt_mask, pthread_t thread) {
	if(getenv(IRT_ENABLE_AFFINITY_ENV)) {
		if(irt_mask == 0) {
			irt_clear_affinity();
			return;
		}
		cpu_set_t mask;
		CPU_ZERO(&mask);
		for(int i=0; i<MAX_CORES; ++i) {
			if((irt_mask&1) != 0) CPU_SET(i, &mask);
			irt_mask >>= 1;
		}
		IRT_ASSERT(pthread_setaffinity_np(thread, sizeof(mask), &mask) == 0, IRT_ERR_INIT, "Error setting thread affinity.");
	}
}


irt_affinity_mask _irt_get_affinity_max_distance(uint32 id) {
	uint32 ncpus = irt_get_num_cpus();
	uint32 nworkers = irt_g_worker_count;
	uint32 d = ncpus/nworkers;
	uint32 pos = d;
	for(int i=0; i<nworkers; ++i) {
		if(i == id) return ((irt_affinity_mask)1) << pos;
		pos += 2*d;
		if(pos>=ncpus) {
			pos = 0;
		}
	}
}

static inline irt_affinity_mask irt_get_affinity(uint32 id, irt_affinity_policy policy) {
	if(policy == IRT_AFFINITY_NONE) return 0;
	uint32 skip = policy - IRT_AFFINITY_FILL + 1;
	if(policy == IRT_AFFINITY_MAX_DISTANCE) return _irt_get_affinity_max_distance(id);
	uint32 pos = id*skip;
	uint32 ncpus = irt_get_num_cpus();
	uint32 ret = (pos+(pos+1)/ncpus)%ncpus;
	return ((irt_affinity_mask)1) << ((pos == ncpus-1) ? ncpus-1 : ret);
}

void irt_set_global_affinity_policy(irt_affinity_policy policy);
