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
#define IRT_AFFINITY_POLICY_ENV "IRT_AFFINITY_POLICY"

typedef enum {
	IRT_AFFINITY_NONE = 0,
	IRT_AFFINITY_FIXED = 1,
	IRT_AFFINITY_FILL = 10,
	IRT_AFFINITY_SKIP = 20,
	IRT_AFFINITY_MAX_DISTANCE = 100
//	IRT_AFFINITY_MAX_SPREAD = 200
} irt_affinity_policy_type;

typedef struct {
	irt_affinity_policy_type type;
	uint32 skip_count;
	uint32 fixed_map[MAX_CORES];
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
	for(int i=0; i<irt_get_num_cpus(); ++i) {
		CPU_SET(i, &mask);
	}
	IRT_ASSERT(pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &mask) == 0, IRT_ERR_INIT, "Error clearing thread affinity.");
}

void irt_set_affinity(irt_affinity_mask irt_mask, pthread_t thread) {
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
	IRT_ASSERT(pthread_setaffinity_np(thread, sizeof(cpu_set_t), &mask) == 0, IRT_ERR_INIT, "Error setting thread affinity.");
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
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "Faulty affinity mask request: id %u out of %u", id, irt_g_worker_count);
	return 0xFFFFFFFF;
}

irt_affinity_policy irt_load_affinity_from_env() {
	char* policy_str = getenv(IRT_AFFINITY_POLICY_ENV);
	irt_affinity_policy policy;
	if(policy_str) {
		  char *tok = strtok(policy_str, ", ");
		  if(strcmp("IRT_AFFINITY_NONE", tok) == 0) {
			  policy.type = IRT_AFFINITY_NONE;
		  }
		  else if(strcmp("IRT_AFFINITY_FIXED", tok) == 0) {
			  policy.type = IRT_AFFINITY_FIXED;
			  tok = strtok(NULL, ", ");
			  int i=0;
			  while(tok != NULL) {
				policy.fixed_map[i++] = atoi(tok);
				tok = strtok(NULL, ", ");
			  }
			  if(i!=irt_g_worker_count) IRT_WARN("Fixed affinity mapping specified, but not all workers mapped.\n");
		  }
		  else if(strcmp("IRT_AFFINITY_FILL", tok) == 0) {
			  policy.type = IRT_AFFINITY_FILL;
		  }
		  else if(strcmp("IRT_AFFINITY_SKIP", tok) == 0) {
			  policy.type = IRT_AFFINITY_SKIP;
			  tok = strtok(NULL, ", ");
			  policy.skip_count = atoi(tok);
		  }
		  else if(strcmp("IRT_AFFINITY_MAX_DISTANCE", tok) == 0) {
			  policy.type = IRT_AFFINITY_MAX_DISTANCE;
		  }
		  else {
			  irt_throw_string_error(IRT_ERR_INIT, "Unknown affinity policy type: %s", tok);
		  }
	} else {
		policy.type = IRT_AFFINITY_NONE;
	}
	return policy;
}

static inline irt_affinity_mask irt_get_affinity(uint32 id, irt_affinity_policy policy) {
	if(policy.type == IRT_AFFINITY_NONE) return 0;
	if(policy.type == IRT_AFFINITY_MAX_DISTANCE) return _irt_get_affinity_max_distance(id);
	if(policy.type == IRT_AFFINITY_FIXED) return ((irt_affinity_mask)1) << policy.fixed_map[id];
	uint32 skip = policy.skip_count;
	if(policy.type == IRT_AFFINITY_FILL) skip = 0;
	skip++;
	uint32 pos = id*skip;
	uint32 ncpus = irt_get_num_cpus();
	uint32 ret = (pos+(pos+1)/ncpus)%ncpus;
	return ((irt_affinity_mask)1) << ((pos == ncpus-1) ? ncpus-1 : ret);
}

void irt_set_global_affinity_policy(irt_affinity_policy policy);
