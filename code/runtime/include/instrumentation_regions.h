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

#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
uint32 irt_g_region_metric_##_name__##_id;
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
uint32 irt_g_region_metric_group_##_name__##_id;
#include "irt_metrics.def"

// create metric flags and group counts for selectively enabling/disabling instrumentation
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
bool irt_g_inst_region_metric_measure_##_name__ = false;
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
uint32 irt_g_inst_region_metric_group_##_name__##membership_count = 0;
#include "irt_metrics.def"

typedef enum {
	IRT_HW_SCOPE_CORE,
	IRT_HW_SCOPE_SOCKET,
	IRT_HW_SCOPE_SYSTEM,
	IRT_HW_SCOPE_NUM_SCOPES
} IRT_HW_SCOPES;

typedef enum {
	IRT_METRIC_AGGREGATOR_NONE,
	IRT_METRIC_AGGREGATOR_SUM,
	IRT_METRIC_AGGREGATOR_AVG,
	IRT_METRIC_NUM_AGGREGATORS,
} IRT_METRIC_AGGREGATORS;

typedef struct {
	uint64 id;
	uint64 num_executions;
	uint64 num_entries;
	uint64 num_exits;
	irt_spinlock lock;
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	_data_type__ last_##_name__; \
	_data_type__ aggregated_##_name__;
#include "irt_metrics.def"
} irt_inst_region_context_data;

typedef struct {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	_data_type__ last_##_name__; \
	_data_type__ aggregated_##_name__;
#include "irt_metrics.def"
} irt_inst_region_wi_data;

typedef struct {
#define GROUP(_name__, _global_var_decls__, _local_var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
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

void irt_inst_region_init(irt_context* context);

void irt_inst_region_finalize(irt_context* context);

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
