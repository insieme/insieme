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

// in this file, only prototypes of platform independent shall be declared

#include "declarations.h"
#include "hwinfo.h"
#include "globals.h"
#include "irt_logging.h"

#include <limits.h>


/* needed for CPU_* macros */
//#define _GNU_SOURCE 1

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

#include <sched.h>
#include <pthread.h>

#include "impl/error_handling.impl.h"

#define IRT_AFFINITY_MASK_BITS_PER_QUAD ((uint64)64)   // number of processors identifiable through a bitmask
#define IRT_AFFINTY_MASK_NUM_QUADS (IRT_MAX_CORES/IRT_AFFINITY_MASK_BITS_PER_QUAD) // number of bitmasks required to capture every processor

struct _irt_affinity_mask {
	uint64 mask_quads[IRT_AFFINTY_MASK_NUM_QUADS]; // array of bitmasks to identify every single processor
};

static const irt_affinity_mask irt_g_empty_affinity_mask = { { 0 } };

// include signatures of platform dependent functions
#include "abstraction/affinity.os_dependent.h"

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
