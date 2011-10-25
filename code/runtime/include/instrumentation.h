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

//#include "work_item.h"
//#include "worker.h"
//#include "data_item.h"
//#include "work_group.h"

#define IRT_ENABLE_INSTRUMENTATION

typedef enum {
	WORK_ITEM_CREATED = 0,
	WORK_ITEM_QUEUED = 100,
	WORK_ITEM_STARTED = 200,
	WORK_ITEM_SUSPENDED_IO = 300,
	WORK_ITEM_SUSPENDED_BARRIER = 301,
	WORK_ITEM_SUSPENDED_JOIN = 302,
	WORK_ITEM_SUSPENDED_GROUPJOIN = 303,
	WORK_ITEM_SUSPENDED_UNKOWN = 399,
	WORK_ITEM_RESUMED = 400,
	WORK_ITEM_FINISHED = 500,
} wi_instrumentation_event;

typedef enum {
	WORK_GROUP_CREATED = 0,
} wg_instrumentation_event;

typedef enum {
	WORKER_CREATED = 0,
	WORKER_RUNNING = 100,
	WORKER_SLEEP_START = 200,
	WORKER_SLEEP_END = 300,
	WORKER_SLEEP_BUSY_START = 400,
	WORKER_SLEEP_BUSY_END = 500,
	WORKER_STOP = 800,
} worker_instrumentation_event;

typedef enum {
	DATA_ITEM_CREATED = 0,
	DATA_ITEM_RECYCLED = 500,
} data_item_instrumentation_event;

typedef struct _irt_performance_data {
	uint64 timestamp;
	wi_instrumentation_event event;
} _irt_performance_data;

typedef struct irt_pd_table {
	uint32 size;
	uint32 number_of_elements;
	uint32 blocksize;
	_irt_performance_data* data;
} irt_pd_table;

irt_pd_table* irt_create_performance_table(const unsigned blocksize);

void irt_destroy_performance_table(irt_pd_table* table);

void irt_wi_instrumentation_event(irt_work_item* wi, wi_instrumentation_event event);

void irt_wg_instrumentation_event(irt_work_group* wg, wg_instrumentation_event event);

void irt_worker_instrumentation_event(irt_worker* worker, wg_instrumentation_event event);

void irt_di_instrumentation_event(irt_data_item* di, wg_instrumentation_event event);

void irt_wi_instrumentation_output(irt_work_item* wi);

void irt_di_instrumentation_output(irt_data_item* di);

void irt_worker_instrumentation_output(irt_worker* worker);

void irt_wg_instrumentation_output(irt_work_group* wg);

// to be used via function pointer to disable instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void irt_wi_no_instrumentation_event(irt_work_item* wi, wi_instrumentation_event event);

void irt_wg_no_instrumentation_event(irt_work_group* wg, wg_instrumentation_event event);

void irt_worker_no_instrumentation_event(irt_worker* worker, wg_instrumentation_event event);

void irt_di_no_instrumentation_event(irt_data_item* di, wg_instrumentation_event event);


