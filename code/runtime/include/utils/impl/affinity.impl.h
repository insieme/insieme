/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#ifndef __GUARD_UTILS_IMPL_AFFINITY_IMPL_H
#define __GUARD_UTILS_IMPL_AFFINITY_IMPL_H

#include "utils/affinity.h"
#include "impl/worker.impl.h"

#include "abstraction/affinity.os_dependent.h"
#include "abstraction/impl/affinity.os_dependent.impl.h"


// operations on affinity mask ------------------------------------

static inline bool irt_affinity_mask_is_empty(const irt_affinity_mask mask) {
	for(uint64 i = 0; i < IRT_AFFINTY_MASK_NUM_QUADS; ++i)
		if(mask.mask_quads[i] != ((uint64)0)) { return false; }
	return true;
}

static inline bool irt_affinity_mask_equals(const irt_affinity_mask maskA, const irt_affinity_mask maskB) {
	for(uint64 i = 0; i < IRT_AFFINTY_MASK_NUM_QUADS; ++i)
		if(maskA.mask_quads[i] != maskB.mask_quads[i]) { return false; }
	return true;
}

static inline bool irt_affinity_mask_is_set(const irt_affinity_mask mask, uint64 cpu) {
	uint64 quad_index = cpu / IRT_AFFINITY_MASK_BITS_PER_QUAD;
	uint64 quad_offset = cpu % IRT_AFFINITY_MASK_BITS_PER_QUAD;
	return ((mask.mask_quads[quad_index] >> quad_offset) & ((uint64)1)) != ((uint64)0);
}


static inline void irt_affinity_mask_set(irt_affinity_mask* mask, uint64 cpu, bool value) {
	uint64 quad_index = cpu / IRT_AFFINITY_MASK_BITS_PER_QUAD;
	uint64 quad_offset = cpu % IRT_AFFINITY_MASK_BITS_PER_QUAD;
	uint64 bit_val = ((uint64)1) << quad_offset;
	if(value) {
		mask->mask_quads[quad_index] |= bit_val;
	} else {
		mask->mask_quads[quad_index] &= ~(bit_val);
	}
}

static inline void irt_affinity_mask_clear(irt_affinity_mask* mask) {
	for(uint64 i = 0; i < IRT_AFFINTY_MASK_NUM_QUADS; ++i) {
		mask->mask_quads[i] = ((uint64)0);
	}
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
	for(uint64 i = 0; i < IRT_AFFINTY_MASK_NUM_QUADS; ++i) {
		if(mask.mask_quads[i]) {
			for(uint32 j = 0; j < IRT_AFFINITY_MASK_BITS_PER_QUAD; ++j) {
				if(((mask.mask_quads[i] >> j) & 1) != 0) { return IRT_AFFINITY_MASK_BITS_PER_QUAD * i + j; }
			}
		}
	}
	// IRT_WARN("No affinity set but affinity information requested.\n");
	return -1;
}


// affinity policy handling ----------------------------------------------------

irt_affinity_mask _irt_get_affinity_max_distance(uint32 id) {
	uint32 ncpus = irt_hw_get_num_cpus();
	uint32 nworkers = irt_g_worker_count;
	uint32 d = ncpus / nworkers;
	uint32 pos = d;
	for(uint32 i = 0; i < nworkers; ++i) {
		if(i == id) { return irt_affinity_mask_create_single_cpu(pos); }
		pos += 2 * d;
		if(pos >= ncpus) { pos = 0; }
	}
	IRT_ASSERT(false, IRT_ERR_INTERNAL, "Faulty affinity mask request: id %u out of %u", id, irt_g_worker_count);
	return irt_g_empty_affinity_mask;
}

irt_affinity_policy irt_load_affinity_from_env() {
	char* policy_str = getenv(IRT_AFFINITY_POLICY_ENV);
	irt_affinity_policy policy = {IRT_AFFINITY_NONE, 0};
	if(policy_str) {
		irt_log_setting_s(IRT_AFFINITY_POLICY_ENV, policy_str);
		char* tok = strtok(policy_str, ", :");
		if(strcmp("IRT_AFFINITY_NONE", tok) == 0) {
			policy.type = IRT_AFFINITY_NONE;
		} else if(strcmp("IRT_AFFINITY_FIXED", tok) == 0) {
			policy.type = IRT_AFFINITY_FIXED;
			tok = strtok(NULL, ", ");
			uint32 i = 0;
			while(tok != NULL) {
				policy.fixed_map[i++] = atoi(tok);
				tok = strtok(NULL, ", ");
			}
			if(i != irt_g_worker_count) { IRT_WARN("Fixed affinity mapping specified, but not all workers mapped.\n"); }
		} else if(strcmp("IRT_AFFINITY_FILL", tok) == 0) {
			policy.type = IRT_AFFINITY_FILL;
		} else if(strcmp("IRT_AFFINITY_SKIP", tok) == 0) {
			policy.type = IRT_AFFINITY_SKIP;
			tok = strtok(NULL, ", ");
			policy.skip_count = atoi(tok);
		} else if(strcmp("IRT_AFFINITY_MAX_DISTANCE", tok) == 0) {
			policy.type = IRT_AFFINITY_MAX_DISTANCE;
		} else {
			irt_throw_string_error(IRT_ERR_INIT, "Unknown affinity policy type: %s", tok);
		}
	} else {
		irt_log_setting_s(IRT_AFFINITY_POLICY_ENV, "IRT_AFFINITY_NONE");
		policy.type = IRT_AFFINITY_NONE;
	}
	return policy;
}

static inline irt_affinity_mask irt_get_affinity(uint32 id, irt_affinity_policy policy) {
	if(policy.type == IRT_AFFINITY_NONE) { return irt_g_empty_affinity_mask; }
	if(policy.type == IRT_AFFINITY_MAX_DISTANCE) { return _irt_get_affinity_max_distance(id); }
	if(policy.type == IRT_AFFINITY_FIXED) { return irt_affinity_mask_create_single_cpu(policy.fixed_map[id]); }
	uint32 skip = policy.skip_count;
	if(policy.type == IRT_AFFINITY_FILL) { skip = 0; }
	skip++;
	uint32 pos = id * skip;
	uint32 ncpus = irt_hw_get_num_cpus();
	uint32 ret = (pos + (pos + 1) / ncpus) % ncpus;
	return irt_affinity_mask_create_single_cpu((pos == ncpus - 1) ? ncpus - 1 : ret);
}

void irt_set_global_affinity_policy(irt_affinity_policy policy) {
	for(uint32 i = 0; i < irt_g_worker_count; ++i) {
		irt_affinity_mask mask = irt_get_affinity(i, policy);
		irt_set_affinity(mask, irt_g_workers[i]->thread);
		irt_g_workers[i]->affinity = mask;
	}
}


#endif // ifndef __GUARD_UTILS_IMPL_AFFINITY_IMPL_H
