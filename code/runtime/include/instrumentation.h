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
#include "performance_table.h"

#ifdef USE_OPENCL
#define IRT_ENABLE_INSTRUMENTATION
#endif

#ifndef IRT_ENABLE_INSTRUMENTATION
//#define IRT_ENABLE_INSTRUMENTATION
#endif

#ifndef IRT_ENABLE_REGION_INSTRUMENTATION
#define IRT_ENABLE_REGION_INSTRUMENTATION
#endif

#define IRT_DECLARE_PERFORMANCE_TABLE(__type__) \
	struct _irt_##__type__##_table { \
	uint32 size; \
	uint32 number_of_elements; \
	uint32 blocksize; \
	_irt_##__type__##_data* data; \
}; \
typedef struct _irt_##__type__##_table irt_##__table__##_table; \

// functions for creating and destroying performance tables

irt_instrumentation_event_data_table* irt_inst_create_event_data_table();

void irt_inst_destroy_event_data_table(irt_instrumentation_event_data_table* table);

// initialization functions

void irt_instrumentation_init_energy_instrumentation();

// private event handlers

void _irt_inst_insert_wi_event(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id);
void _irt_inst_insert_wg_event(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id);
void _irt_inst_insert_wo_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id);
void _irt_inst_insert_di_event(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id);
void _irt_inst_insert_db_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id);

// debug output functions

void irt_inst_event_data_output_single(irt_instrumentation_event_data data, FILE* outputfile, bool readable);
void irt_inst_event_data_output_all(bool binary_format);
void irt_inst_event_data_output(irt_worker* worker, bool binary_format);
void irt_inst_region_data_output(irt_worker* worker);
void irt_inst_aggregated_data_output();

// instrumentation function pointer toggle functions

void irt_inst_set_wi_instrumentation(bool enable);
void irt_inst_set_wg_instrumentation(bool enable);
void irt_inst_set_wo_instrumentation(bool enable);
void irt_inst_set_di_instrumentation(bool enable);
void irt_inst_set_db_instrumentation(bool enable);
void irt_inst_set_all_instrumentation(bool enable);

// dummy functions to be used via function pointer to disable 
// instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void _irt_inst_insert_no_wi_event(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id);
void _irt_inst_insert_no_wg_event(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id);
void _irt_inst_insert_no_wo_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id);
void _irt_inst_insert_no_di_event(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id);
void _irt_inst_insert_no_db_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id);


// -----------------------------------------------------------------------------------------------------------------
//													Regions
// -----------------------------------------------------------------------------------------------------------------

typedef uint32 region_id;

uint32 irt_g_inst_metric_count = 0;
uint32 irt_g_inst_group_count = 0;

#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
uint32 irt_g_metric_##_name__##_id;
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
uint32 irt_g_metric_group_##_name__##_id;
#include "irt_metrics.def"

// create metric flags and group counts for selectively enabling/disabling instrumentation
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
bool irt_g_inst_measure_##_name__ = false;
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
uint32 irt_g_inst_group_##_name__##membership_count = 0;
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
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	_data_type__ last_##_name__; \
	_data_type__ aggregated_##_name__;
#include "irt_metrics.def"
} irt_inst_region_struct;

typedef struct {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	_data_type__ last_##_name__; \
	_data_type__ aggregated_##_name__;
#include "irt_metrics.def"
} irt_inst_wi_struct;

typedef struct {
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	_var_decls__;
#include "irt_metrics.def"
} irt_inst_context_struct;

typedef struct {
	irt_inst_region_struct** items;
	uint64 length;
	uint64 size;
} irt_inst_region_list;

void irt_inst_metrics_init();
void irt_inst_metrics_finalize();
void irt_inst_select_region_instrumentation_metrics(const char* selection);
void irt_inst_set_region_instrumentation_from_env();
irt_inst_region_struct* irt_inst_region_get_current();
void irt_inst_propagate_data_from_cur_region_to_parent(irt_work_item* wi);
void irt_inst_propagate_data_from_wi_to_cur_region(irt_work_item* wi);
void irt_inst_region_start_measurements(irt_work_item* wi);
void irt_inst_region_end_measurements(irt_work_item* wi);
void irt_inst_region_init(irt_context* context);
void irt_inst_region_debug_output();
void irt_inst_region_output();
void irt_inst_region_finalize(irt_context* context);
void irt_inst_region_start(region_id id);
void irt_inst_region_end(region_id id);
void irt_inst_region_wi_init(irt_work_item* wi);
void irt_region_instrumentation_setup();
void irt_inst_region_list_copy(irt_work_item* destination, irt_work_item* source);

// -----------------------------------------------------------------------------------------------------------------
//													File Format
// -----------------------------------------------------------------------------------------------------------------

/*
 *  8 byte: char, file version identifier, must read "INSIEME1"!
 * -------------------------------------------------------------
 *  4 byte: uint32, number of event name table entries (=n)
 *  4 byte: char, event group identifier 1
 * 60 byte: char, event name identifier 1
 *  4 byte: char, event group identifier 2
 * 60 byte: char, event name identifier 2
 * ...
 * ...
 * ...
 *  4 byte: char, event group identifier n
 * 60 byte: char, event name identifier n
 * -------------------------------------------------------------
 *  8 byte: uint64, number of events (=m)
 *  8 byte: uint64, timestamp (nanoseconds) 1
 *  2 byte: uint16, event id 1
 *  2 byte: uint16, thread id 1
 *  4 byte: uint32, target index 1
 *  2 byte: uint16, event id 2
 *  2 byte: uint16, thread id 2
 *  4 byte: uint32, target index 2
 *  ...
 *  ...
 *  ...
 *  2 byte: uint16, event id m
 *  2 byte: uint16, thread id m
 *  4 byte: uint32, target index m
 * -------------------------------------------------------------
 * EOF
 * (note: the strings are written without the termination character '\0'!)
 */
