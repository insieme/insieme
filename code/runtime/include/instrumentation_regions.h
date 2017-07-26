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
#ifndef __GUARD_INSTRUMENTATION_REGIONS_H
#define __GUARD_INSTRUMENTATION_REGIONS_H

#include <stdio.h>

#include "declarations.h"

#ifndef IRT_ENABLE_INSTRUMENTATION
//#define IRT_ENABLE_INSTRUMENTATION
#endif


// -----------------------------------------------------------------------------------------------------------------
//													Regions
// -----------------------------------------------------------------------------------------------------------------

typedef uint32 irt_inst_region_id;

uint32 irt_g_inst_region_metric_count = 0;
uint32 irt_g_inst_region_metric_group_count = 0;

#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,                   \
               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                                 \
	uint32 irt_g_region_metric_##_name__##_id;
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,                  \
              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                            \
	uint32 irt_g_region_metric_group_##_name__##_id;
#include "irt_metrics.def"

// create metric flags and group counts for selectively enabling/disabling instrumentation
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,                   \
               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                                 \
	bool irt_g_inst_region_metric_measure_##_name__ = false;
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,                  \
              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                            \
	uint32 irt_g_inst_region_metric_group_##_name__##membership_count = 0;
#include "irt_metrics.def"

typedef enum { IRT_HW_SCOPE_CORE, IRT_HW_SCOPE_SOCKET, IRT_HW_SCOPE_SYSTEM, IRT_HW_SCOPE_NUM_SCOPES } IRT_HW_SCOPES;

typedef enum {
	IRT_METRIC_AGGREGATOR_NONE,
	IRT_METRIC_AGGREGATOR_SUM,
	IRT_METRIC_AGGREGATOR_AVG,
	IRT_METRIC_NUM_AGGREGATORS,
} IRT_METRIC_AGGREGATORS;

typedef struct {
	uint64 id;
	uint64 num_executions;
	irt_spinlock lock;
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		_data_type__ last_##_name__;                                                                                                                           \
		_data_type__ aggregated_##_name__;
	#include "irt_metrics.def"
} irt_inst_region_context_data;

typedef struct {
	volatile uint64 region_entries;
	volatile uint64 region_exits;
	#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__,               \
	               _region_early_start_code__, _region_late_end_code__, _output_conversion_code__)                                                             \
		_data_type__ last_##_name__;                                                                                                                           \
		_data_type__ aggregated_##_name__;
	#include "irt_metrics.def"
} irt_inst_region_wi_data;

typedef struct {
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _init_code_worker__, _finalize_code__, _finalize_code_worker__,                  \
              _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__)                                                            \
	_global_var_decls__;
#include "irt_metrics.def"
} irt_inst_region_context_declarations;

typedef struct {
	irt_inst_region_context_data** items;
	uint64 length;
	uint64 size;
} irt_inst_region_list;

void irt_inst_region_list_copy(irt_work_item* destination, irt_work_item* source);

void irt_inst_region_wi_init(irt_work_item* wi);

void irt_inst_region_wi_finalize(irt_work_item* wi);

void irt_inst_region_wg_init(irt_work_group* wg);

void irt_inst_region_wg_finalize(irt_work_group* wg);

void irt_inst_region_init(irt_context* context);

void irt_inst_region_init_worker(irt_worker* worker);

void irt_inst_region_finalize(irt_context* context);

void irt_inst_region_finalize_worker(irt_worker* worker);

void irt_inst_region_propagate_data_from_wi_to_regions(irt_work_item* wi);

void irt_inst_region_start_measurements(irt_work_item* wi);

void irt_inst_region_end_measurements(irt_work_item* wi);

void irt_inst_region_start(irt_inst_region_id id);

void irt_inst_region_end(irt_inst_region_id id);

void irt_inst_region_select_metrics(const char* selection);

void irt_inst_region_select_metrics_from_env();

void irt_inst_region_debug_output();

void irt_inst_region_output();

irt_inst_region_context_data* irt_inst_region_get_current(irt_work_item* wi);

#endif // #ifndef __GUARD_INSTRUMENTATION_REGIONS_H
