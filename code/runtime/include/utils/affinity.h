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
#include "irt_logging.h"

#include <limits.h>

/* needed for CPU_* macros */
#define _GNU_SOURCE 1

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
	uint32 fixed_map[IRT_MAX_CORES];
} irt_affinity_policy;

typedef struct {
	// within insieme runtime, we wanna have pretty, virtual, consecutive cpu-ids from 0 to (max_avail_cores - 1)
	// the os may provide strange, non-consecutive cpu-ids, so we map virtual cpu-ids to real cpu-ids
	uint32 map[IRT_MAX_CORES];
} irt_affinity_physical_mapping;

static irt_affinity_physical_mapping irt_g_affinity_physical_mapping;
static cpu_set_t irt_g_affinity_base_mask;

#include <sched.h>
#include <pthread.h>

#ifdef _MSC_VER
	#include <io.h>
#else
	#include <unistd.h>
#endif


#include "impl/error_handling.impl.h"

// affinity mask struct handling ////////////////////////////////////////////////////////////////////////////

#define IRT_AFFINITY_MASK_BITS_PER_QUAD ((uint64)64)   // number of processors identifiable through a bitmask
#define IRT_AFFINTY_MASK_NUM_QUADS (IRT_MAX_CORES/IRT_AFFINITY_MASK_BITS_PER_QUAD) // number of bitmasks required to capture every processor

struct _irt_affinity_mask {
	uint64 mask_quads[IRT_AFFINTY_MASK_NUM_QUADS]; // array of bitmasks to identify every single processor
};

static const irt_affinity_mask irt_g_empty_affinity_mask = { { 0 } };

static inline bool irt_affinity_mask_is_empty(const irt_affinity_mask mask) {
	for(uint64 i=0; i<IRT_AFFINTY_MASK_NUM_QUADS; ++i)
		if(mask.mask_quads[i] != ((uint64)0)) return false;
	return true;
}

static inline bool irt_affinity_mask_equals(const irt_affinity_mask maskA, const irt_affinity_mask maskB) {
	for(uint64 i=0; i<IRT_AFFINTY_MASK_NUM_QUADS; ++i)
		if(maskA.mask_quads[i] != maskB.mask_quads[i]) return false;
	return true;
}

static inline bool irt_affinity_mask_is_set(const irt_affinity_mask mask, uint64 cpu) {
	uint64 quad_index = cpu/IRT_AFFINITY_MASK_BITS_PER_QUAD;
	uint64 quad_offset = cpu%IRT_AFFINITY_MASK_BITS_PER_QUAD;
	return ((mask.mask_quads[quad_index] >> quad_offset) & ((uint64)1)) != ((uint64)0);
}


static inline void irt_affinity_mask_set(irt_affinity_mask* mask, uint64 cpu, bool value) {
	uint64 quad_index = cpu/IRT_AFFINITY_MASK_BITS_PER_QUAD;
	uint64 quad_offset = cpu%IRT_AFFINITY_MASK_BITS_PER_QUAD;
	uint64 bit_val = ((uint64)1) << quad_offset;
	if(value)
		mask->mask_quads[quad_index] |=  bit_val;
	else
		mask->mask_quads[quad_index] &= ~(bit_val);
}

static inline void irt_affinity_mask_clear(irt_affinity_mask* mask) {
	for(uint64 i=0; i<IRT_AFFINTY_MASK_NUM_QUADS; ++i)
		mask->mask_quads[i] = ((uint64)0);
}

static inline irt_affinity_mask irt_affinity_mask_create_single_cpu(uint64 cpu) {
	irt_affinity_mask mask;
	irt_affinity_mask_clear(&mask);
	irt_affinity_mask_set(&mask, cpu, true);
	return mask;
}

static inline bool irt_affinity_mask_is_single_cpu(const irt_affinity_mask mask, uint64 cpu) {
	return irt_affinity_mask_equals(mask, irt_affinity_mask_create_single_cpu(cpu));
}

static inline uint32 irt_affinity_mask_get_first_cpu(const irt_affinity_mask mask) {
	for(uint64 i=0; i<IRT_AFFINTY_MASK_NUM_QUADS; ++i) {
			if(mask.mask_quads[i]) {
				for(uint32 j = 0; j < IRT_AFFINITY_MASK_BITS_PER_QUAD; ++j) {
					if(((mask.mask_quads[i]>>j) & 1) != 0)
						return IRT_AFFINITY_MASK_BITS_PER_QUAD*i+j;
				}
			}
	}
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "Requested first CPU in empty affinity mask");
	return 0;
}

// affinity setting for pthreads ////////////////////////////////////////////////////////////////////////////

void _irt_print_native_affinity_mask(cpu_set_t mask) {
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

// affinity policy handling /////////////////////////////////////////////////////////////////////////////////

irt_affinity_mask _irt_get_affinity_max_distance(uint32 id) {
	uint32 ncpus = irt_get_num_cpus();
	uint32 nworkers = irt_g_worker_count;
	uint32 d = ncpus/nworkers;
	uint32 pos = d;
	for(int i=0; i<nworkers; ++i) {
		if(i == id) return irt_affinity_mask_create_single_cpu(pos);
		pos += 2*d;
		if(pos>=ncpus) {
			pos = 0;
		}
	}
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "Faulty affinity mask request: id %u out of %u", id, irt_g_worker_count);
	return irt_g_empty_affinity_mask;
}

irt_affinity_policy irt_load_affinity_from_env() {
	char* policy_str = getenv(IRT_AFFINITY_POLICY_ENV);
	irt_affinity_policy policy = {IRT_AFFINITY_NONE, 0};
	if(policy_str) {
		irt_log_setting_s("IRT_AFFINTIY_POLICY", policy_str);
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
		irt_log_setting_s("IRT_AFFINTIY_POLICY", "IRT_AFFINITY_NONE");
		policy.type = IRT_AFFINITY_NONE;
	}
	return policy;
}

static inline irt_affinity_mask irt_get_affinity(uint32 id, irt_affinity_policy policy) {
	if(policy.type == IRT_AFFINITY_NONE) return irt_g_empty_affinity_mask;
	if(policy.type == IRT_AFFINITY_MAX_DISTANCE) return _irt_get_affinity_max_distance(id);
	if(policy.type == IRT_AFFINITY_FIXED) return irt_affinity_mask_create_single_cpu(policy.fixed_map[id]);
	uint32 skip = policy.skip_count;
	if(policy.type == IRT_AFFINITY_FILL) skip = 0;
	skip++;
	uint32 pos = id*skip;
	uint32 ncpus = irt_get_num_cpus();
	uint32 ret = (pos+(pos+1)/ncpus)%ncpus;
	return irt_affinity_mask_create_single_cpu((pos == ncpus-1) ? ncpus-1 : ret);
}

void irt_set_global_affinity_policy(irt_affinity_policy policy);
