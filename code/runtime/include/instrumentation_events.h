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
#ifndef __GUARD_INSTRUMENTATION_EVENTS_H
#define __GUARD_INSTRUMENTATION_EVENTS_H

#include <stdio.h>

#include "declarations.h"

#ifdef USE_OPENCL
#define IRT_ENABLE_INSTRUMENTATION
#endif

#ifndef IRT_ENABLE_INSTRUMENTATION
//#define IRT_ENABLE_INSTRUMENTATION
#endif

#define IRT_DECLARE_PERFORMANCE_TABLE(__type__)                                                                                                                \
	struct _irt_##__type__##_table {                                                                                                                           \
		uint32 size;                                                                                                                                           \
		uint32 number_of_elements;                                                                                                                             \
		uint32 blocksize;                                                                                                                                      \
		_irt_##__type__##_data* data;                                                                                                                          \
	};                                                                                                                                                         \
	typedef struct _irt_##__type__##_table irt_##__table__##_table;

#define IRT_INST_EVENT(event, group_label, event_label) event,
typedef enum _irt_instrumentation_event {
#include "instrumentation_events.def"
} irt_instrumentation_event;
#undef IRT_INST_EVENT

#define IRT_INST_EVENT(event, group_label, event_label) event_label,
const char* irt_g_instrumentation_event_names[] = {
#include "instrumentation_events.def"
};
#undef IRT_INST_EVENT

#define IRT_INST_EVENT(event, group_label, event_label) group_label,
const char* irt_g_instrumentation_group_names[] = {
#include "instrumentation_events.def"
};
#undef IRT_INST_EVENT

#define IRT_INST_EVENT(event, group_label, event_label) +1
uint32 irt_g_inst_num_event_types = 0
#include "instrumentation_events.def"
    ;
#undef IRT_INST_EVENT

typedef struct _irt_instrumentation_event_data {
	uint64 timestamp;
	union {
		struct {
			uint16 event_id;
			uint16 thread;
			uint32 index;
		};
		uint64 identification;
	};
} irt_instrumentation_event_data;

typedef struct _irt_instrumentation_event_data_table {
	uint32 size;
	uint32 number_of_elements;
	irt_instrumentation_event_data* data;
} irt_instrumentation_event_data_table;

#ifdef USE_OPENCL

typedef struct _irt_inst_ocl_performance_helper {
	uint64 timestamp;
	uint64 workitem_id;
	uint64 event;
	uint64 origin;
} _irt_inst_ocl_performance_helper;

typedef enum {
	IRT_INST_OCL_QUEUED = 0,
	IRT_INST_OCL_SUBMITTED = 1,
	IRT_INST_OCL_STARTED = 2,
	IRT_INST_OCL_FINISHED = 3,
} _irt_inst_ocl_helper_events;

#endif

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
void irt_inst_region_context_data_output(irt_worker* worker);
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

#ifdef IRT_ENABLE_INSTRUMENTATION
// global function pointers to switch instrumentation on/off
void (*irt_inst_insert_wi_event)(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id) = &_irt_inst_insert_no_wi_event;
void (*irt_inst_insert_wg_event)(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id) = &_irt_inst_insert_no_wg_event;
void (*irt_inst_insert_di_event)(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id) = &_irt_inst_insert_no_di_event;
void (*irt_inst_insert_wo_event)(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) = &_irt_inst_insert_no_wo_event;
void (*irt_inst_insert_db_event)(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) = &_irt_inst_insert_no_db_event;
bool irt_g_instrumentation_event_output_is_enabled = false;
bool irt_g_instrumentation_event_output_is_binary = false;

#endif // IRT_ENABLE_INSTRUMENTATION

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

#endif // #ifndef __GUARD_INSTRUMENTATION_EVENTS_H
