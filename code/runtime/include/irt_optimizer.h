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
#ifndef __GUARD_IRT_OPTIMIZER_H
#define __GUARD_IRT_OPTIMIZER_H

#include "declarations.h"

#include "utils/lookup_tables.h"

//#define IRT_RUNTIME_TUNING
//#define IRT_RUNTIME_TUNING_EXTENDED

#ifdef USE_OPENCL
	#define IRT_RUNTIME_TUNING
#endif

void irt_optimizer_context_startup(irt_context *context);
void irt_optimizer_context_destroy(irt_context *context);

void irt_optimizer_starting_pfor(irt_wi_implementation *impl, irt_work_item_range range, irt_work_group* group);

#ifndef IRT_RUNTIME_TUNING_EXTENDED
void irt_optimizer_completed_pfor(irt_wi_implementation *impl, uint64 time, irt_loop_sched_data* sched_data);
#else
void irt_optimizer_completed_pfor(irt_wi_implementation *impl, irt_work_item_range range, uint64 total_time, irt_loop_sched_data *sched_data);
#endif

/* OpenMP+ */

typedef struct _irt_optimizer_resources {
    #define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
    _data_type__ cpu_energy;
    #define ISOLATE_METRIC
    #define ISOLATE_CPU_ENERGY
    #include "irt_metrics.def"

    #define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
    _data_type__ cpu_time;
    #define ISOLATE_METRIC
    #define ISOLATE_CPU_TIME
    #include "irt_metrics.def"

    // TODO: power data type?
    float power;
} irt_optimizer_resources;

// Data types for runtime collected data 

// An id is needed because of hashmap (lookup_table) implementations
struct _irt_optimizer_wi_data;
typedef struct _irt_optimizer_wi_data_id {
    union {
        uint64 full;
        struct {
            // TODO: only a single param clause per region is supported 
            uint32 param_value;
            uint16 frequency;
            uint16 thread_count;
        };
    };
    struct _irt_optimizer_wi_data* cached;
} irt_optimizer_wi_data_id;

typedef struct _irt_optimizer_wi_data {
    irt_optimizer_wi_data_id id;
    struct _irt_optimizer_wi_data* lookup_table_next;
} irt_optimizer_wi_data;

uint32 irt_optimizer_hash(irt_optimizer_wi_data_id id) {
    // Python integer tuple hash
    uint32 value = 0x345678;

    value = (1000003 * value) ^ id.param_value;
    value = (1000003 * value) ^ id.thread_count;
    value = (1000003 * value) ^ id.frequency;
    value = value ^ (3);

    return (value == (uint32)-1) ? -2 : value;
}

typedef struct _irt_optimizer_runtime_data {
    IRT_CREATE_LOCKFREE_LOOKUP_TABLE(optimizer_wi_data, lookup_table_next, irt_optimizer_hash, IRT_OPTIMIZER_LT_BUCKETS);
    irt_optimizer_wi_data_id best;
    irt_optimizer_resources best_resources;
    irt_optimizer_wi_data_id cur;
    irt_optimizer_resources cur_resources;
    irt_spinlock spinlock;
} irt_optimizer_runtime_data;

#ifdef IRT_ENABLE_OMPP_OPTIMIZER
IRT_DEFINE_LOOKUP_TABLE_FUNCTIONS(optimizer_wi_data, lookup_table_next, irt_optimizer_hash, IRT_OPTIMIZER_LT_BUCKETS, 0);
#endif

uint64_t irt_optimizer_pick_in_range(uint64_t max);
void irt_optimizer_compute_optimizations(irt_wi_implementation_variant* variant, irt_work_item* wi);
void irt_optimizer_apply_dvfs(irt_wi_implementation_variant* variant);
void irt_optimizer_remove_dvfs(irt_wi_implementation_variant* variant);
void irt_optimizer_apply_dct(irt_worker* self);
void irt_optimizer_remove_dct(uint32 outer_worker_to_enable_count);
irt_optimizer_runtime_data* irt_optimizer_set_wrapping_optimizations(irt_wi_implementation_variant* variant, irt_wi_implementation_variant* parent_var);
void irt_optimizer_reset_wrapping_optimizations(irt_wi_implementation_variant* variant, irt_optimizer_runtime_data* data);

#endif // ifndef __GUARD_IRT_OPTIMIZER_H
