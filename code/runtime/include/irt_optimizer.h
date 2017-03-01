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
 *
 */
#pragma once
#ifndef __GUARD_IRT_OPTIMIZER_H
#define __GUARD_IRT_OPTIMIZER_H

#include "declarations.h"

#include "utils/lookup_tables.h"

//#define IRT_RUNTIME_TUNING
//#define IRT_RUNTIME_TUNING_EXTENDED

void irt_optimizer_context_startup(irt_context* context);
void irt_optimizer_context_destroy(irt_context* context);

void irt_optimizer_starting_pfor(irt_wi_implementation* impl, irt_work_item_range range, irt_work_group* group);

#ifndef IRT_RUNTIME_TUNING_EXTENDED
void irt_optimizer_completed_pfor(irt_wi_implementation* impl, uint64 time, irt_loop_sched_data* sched_data);
#else
void irt_optimizer_completed_pfor(irt_wi_implementation* impl, irt_work_item_range range, uint64 total_time, irt_loop_sched_data* sched_data);
#endif

/* OpenMP+ */

typedef struct _irt_optimizer_resources {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,                   \
               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                                 \
	_data_type__ cpu_energy;
#define ISOLATE_METRIC
#define ISOLATE_CPU_ENERGY
#include "irt_metrics.def"

#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,                   \
               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                                 \
	_data_type__ wall_time;
#define ISOLATE_METRIC
#define ISOLATE_WALL_TIME
#include "irt_metrics.def"

	int quality;

	int samplings;
} irt_optimizer_resources;

// Data types for runtime collected data

#define IRT_OPTIMIZER_PARAM_COUNT 8
// An id is needed because of hashmap (lookup_table) implementations
typedef struct _irt_optimizer_wi_data_id {
	struct {
		uint64 frequency;
		uint64 thread_count;
		uint64 param_value[IRT_OPTIMIZER_PARAM_COUNT];
	};
	uint64 eos;
} irt_optimizer_wi_data_id;

// uint32 irt_optimizer_hash(irt_optimizer_wi_data_id id) {
//    // Python integer tuple hash
//    uint32 value = 0x345678;
//
//    value = (1000003 * value) ^ id.param_value;
//    value = (1000003 * value) ^ id.thread_count;
//    value = (1000003 * value) ^ id.frequency;
//    value = value ^ (3);
//
//    return (value == (uint32)-1) ? -2 : value;
//}

typedef struct _irt_optimizer_param_qual_range {
	uint32 lowb[IRT_OPTIMIZER_PARAM_COUNT];
	uint32 uppb[IRT_OPTIMIZER_PARAM_COUNT];
	uint32 step[IRT_OPTIMIZER_PARAM_COUNT];
} irt_optimizer_param_qual_range;

typedef struct _irt_optimizer_runtime_data {
	irt_optimizer_wi_data_id best;
	irt_optimizer_resources best_resources;
	irt_optimizer_wi_data_id cur;
	irt_optimizer_resources cur_resources;
	irt_optimizer_wi_data_id max;
	irt_optimizer_param_qual_range param_qual_range;
	bool hc_end;   // true if hill climbing completed
	int16 hc_elem; // element in irt_optimizer_wi_data_id currently hill climbed
	int8 hc_dir;   // current hill climbing direction
	irt_spinlock spinlock;
} irt_optimizer_runtime_data;

uint64 irt_optimizer_pick_in_range(int id, uint64 max, int qual_lb, int qual_ub, int qual_st);
void irt_optimizer_compute_optimizations(irt_wi_implementation_variant* variant, irt_work_item* wi, bool force_computation);
void irt_optimizer_apply_dvfs(irt_wi_implementation_variant* variant);
void irt_optimizer_remove_dvfs(irt_wi_implementation_variant* variant);
uint32 irt_optimizer_apply_dct(irt_wi_implementation_variant* variant);
irt_optimizer_runtime_data* irt_optimizer_set_wrapping_optimizations(irt_wi_implementation_variant* target_variant,
                                                                     irt_wi_implementation_variant* source_variant);
void irt_optimizer_reset_wrapping_optimizations(irt_wi_implementation_variant* target_variant, irt_optimizer_runtime_data* data);

#endif // ifndef __GUARD_IRT_OPTIMIZER_H
