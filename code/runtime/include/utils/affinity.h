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
#ifndef __GUARD_UTILS_AFFINITY_H
#define __GUARD_UTILS_AFFINITY_H

// in this file, only prototypes of platform independent functions shall be declared

#include <limits.h>

#include "declarations.h"
#include "hwinfo.h"
#include "irt_globals.h"
#include "irt_logging.h"
#include "abstraction/threads.h"
#include "error_handling.h"

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

#define IRT_AFFINITY_MASK_BITS_PER_QUAD ((uint64)64)                                 // number of processors identifiable through a bitmask
#define IRT_AFFINTY_MASK_NUM_QUADS (IRT_MAX_CORES / IRT_AFFINITY_MASK_BITS_PER_QUAD) // number of bitmasks required to capture every processor

struct _irt_affinity_mask {
	uint64 mask_quads[IRT_AFFINTY_MASK_NUM_QUADS]; // array of bitmasks to identify every single processor
};

static const irt_affinity_mask irt_g_empty_affinity_mask = {{0}};

// include signatures of platform dependent functions
#include "abstraction/affinity.os_dependent.h"

// original affinity mask before any changes to affinity were applied; will be restored
// when calling irt_clear_affinity()
static irt_native_cpu_set irt_g_affinity_base_mask;

// affinity mask struct handling ////////////////////////////////////////////////////////////////////////////

static inline bool irt_affinity_mask_is_empty(const irt_affinity_mask mask);

static inline bool irt_affinity_mask_equals(const irt_affinity_mask maskA, const irt_affinity_mask maskB);

static inline bool irt_affinity_mask_is_set(const irt_affinity_mask mask, uint64 cpu);

static inline void irt_affinity_mask_set(irt_affinity_mask* mask, uint64 cpu, bool value);

static inline void irt_affinity_mask_clear(irt_affinity_mask* mask);

static inline irt_affinity_mask irt_affinity_mask_create_single_cpu(uint64 cpu);

static inline bool irt_affinity_mask_is_single_cpu(const irt_affinity_mask mask, uint64 cpu);

static inline uint32 irt_affinity_mask_get_first_cpu(const irt_affinity_mask mask);

// affinity policy handling /////////////////////////////////////////////////////////////////////////////////

irt_affinity_policy irt_load_affinity_from_env();

static inline irt_affinity_mask irt_get_affinity(uint32 id, irt_affinity_policy policy);

void irt_set_global_affinity_policy(irt_affinity_policy policy);


#endif // ifndef __GUARD_UTILS_AFFINITY_H
