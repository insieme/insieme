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

typedef enum {
	PERFORMANCE_DATA_ENTRY_ENERGY = 0, // energy consumed
	PERFORMANCE_DATA_ENTRY_MEMORY_VIRT = 1, // virtual memory size
	PERFORMANCE_DATA_ENTRY_MEMORY_RES = 2, // resident set size
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_1 = 3,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_2 = 4,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_3 = 5,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_4 = 6,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_5 = 7,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_6 = 8,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_7 = 9,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_8 = 10,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_9 = 11,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_10 = 12,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_11 = 13,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_12 = 14,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_13 = 15,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_14 = 16,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_15 = 17,
	PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_16 = 18,
} extended_performance_data_type;
	
typedef enum {
	WORK_ITEM_CREATED = 1000,
	WORK_ITEM_QUEUED = 1100,
	WORK_ITEM_SPLITTED = 1200,
	WORK_ITEM_STARTED = 1300,
	WORK_ITEM_SUSPENDED_IO = 1500,
	WORK_ITEM_SUSPENDED_BARRIER = 1501,
	WORK_ITEM_SUSPENDED_JOIN = 1502,
	WORK_ITEM_SUSPENDED_GROUPJOIN = 1503,
	WORK_ITEM_SUSPENDED_JOIN_ALL = 1504,
	WORK_ITEM_SUSPENDED_UNKOWN = 1599,
	WORK_ITEM_RESUMED = 1600,
	WORK_ITEM_END_START = 1800,
	WORK_ITEM_END_FINISHED = 1850,
	WORK_ITEM_FINISHED = 1900,
} wi_instrumentation_event;

typedef enum {
	WORK_GROUP_CREATED = 2000,
} wg_instrumentation_event;

typedef enum {
	WORKER_CREATED = 3000,
	WORKER_RUNNING = 3100,
	WORKER_SLEEP_START = 3200,
	WORKER_SLEEP_END = 3300,
	WORKER_SLEEP_BUSY_START = 3400,
	WORKER_SLEEP_BUSY_END = 3500,
	WORKER_SCHEDULING_LOOP = 3550,
	WORKER_SCHEDULING_LOOP_END = 3560,
	WORKER_STEAL_TRY = 3600,
	WORKER_STEAL_SUCCESS = 3610,
	WORKER_IMMEDIATE_EXEC = 3700,
	WORKER_STOP = 3800,
} worker_instrumentation_event;

typedef enum {
	DATA_ITEM_CREATED = 4000,
	DATA_ITEM_RECYCLED = 4500,
} di_instrumentation_event;

typedef enum {
	REGION_START = 5000,
	REGION_END = 5100,
} region_instrumentation_event;

typedef struct _irt_region {
	uint64 cputime;
	uint64 start_time;
	struct _irt_region* next;
} irt_region;

typedef struct _irt_region_list {
	irt_region* head;
} irt_region_list;

typedef struct _irt_performance_data {
	uint64 timestamp;
	int32 event;
	uint64 subject_id;
} _irt_performance_data;

typedef struct _irt_pd_table {
	uint32 size;
	uint32 number_of_elements;
	uint32 blocksize;
	_irt_performance_data* data;
} _irt_pd_table;

typedef union _irt_extended_performance_data_raw {
	double value_double;
	uint64 value_uint64;
} _irt_extended_performance_data_raw;

typedef struct _irt_extended_performance_data {
	uint64 timestamp;
	int32 event;
	uint64 subject_id;
	_irt_extended_performance_data_raw data[NUMBER_OF_EXTENDED_PERFORMANCE_DATA_ENTRIES];
} _irt_extended_performance_data;

typedef struct _irt_epd_table {
	uint32 size;
	uint32 number_of_elements;
	uint32 blocksize;
	_irt_extended_performance_data* data;
} _irt_epd_table;

typedef struct _irt_aggregated_performance_data {
	uint64 cputime;
	uint64 walltime;
	uint32 number_of_workers;
	int64 id;
} _irt_aggregated_performance_data;

typedef struct _irt_apd_table {
	uint32 size;
	uint32 number_of_elements;
	uint32 blocksize;
	_irt_aggregated_performance_data* data;
} _irt_apd_table;

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
