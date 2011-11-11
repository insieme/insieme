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

#include <locale.h>
#include "utils/timing.h"
#include "instrumentation.h"
#include <pthread.h>

#define IRT_WI_PD_BLOCKSIZE	16
#define IRT_WG_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE
#define IRT_WORKER_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE
#define IRT_DI_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE

#ifdef IRT_ENABLE_INSTRUMENTATION

// global function pointers to switch instrumentation on/off
void (*irt_wi_instrumentation_event)(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) = &_irt_wi_instrumentation_event;
void (*irt_wg_instrumentation_event)(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) = &_irt_wg_instrumentation_event;;
void (*irt_di_instrumentation_event)(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) = &_irt_di_instrumentation_event;
void (*irt_worker_instrumentation_event)(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) = &_irt_worker_instrumentation_event;

// resizes table according to blocksize
void _irt_performance_table_resize(irt_pd_table* table) {
	table->size = table->size + table->blocksize;
	table->data = realloc(table->data, sizeof(_irt_performance_data)*table->size);
}

// =============== functions for creating and destroying performance tables ===============

// allocates memory for performance data, sets all fields
irt_pd_table* irt_create_performance_table(unsigned blocksize) {
	irt_pd_table* table = malloc(sizeof(irt_pd_table));
	table->blocksize = blocksize;
	table->size = table->blocksize * 2;
	table->number_of_elements = 0;
	table->data = malloc(sizeof(_irt_performance_data) * table->size);
	return table;
}

// frees allocated memory
void irt_destroy_performance_table(irt_pd_table* table) {
	free(table->data);
	free(table);
}

void _irt_instrumentation_event_insert_time(irt_worker* worker, const int event, const uint64 id, const uint64 time) {
	_irt_pd_table* table = worker->performance_data;

	if(table->number_of_elements >= table->size)
		_irt_performance_table_resize(table);

	_irt_performance_data* pd = &(table->data[table->number_of_elements++]);

	pd->timestamp = time;
	pd->event = event;
	pd->subject_id = id;
}

// commonly used internal function to record events and timestamps
void _irt_instrumentation_event_insert(irt_worker* worker, const int event, const uint64 id) {
	uint64 time = irt_time_ticks();

	_irt_instrumentation_event_insert_time(worker, event, id, time);
}

// =========== private event handlers =================================

void _irt_wi_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) {
	_irt_instrumentation_event_insert(worker, event, subject_id.value.full);
}

void _irt_wg_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) {
	_irt_instrumentation_event_insert(worker, event, subject_id.value.full);
}

void _irt_worker_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) {
//	_irt_instrumentation_event_insert(worker, event, subject_id.value.full);
}

void _irt_di_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) {
	_irt_instrumentation_event_insert(worker, event, subject_id.value.full);
}

// ================= debug output functions ==================================

void irt_instrumentation_output(irt_worker* worker) {
	setlocale(LC_ALL, "");
	irt_pd_table* table = worker->performance_data;
	printf("INSTRUMENTATION: %10u events for worker %4u\n", table->number_of_elements, worker->id.value.components.thread);

	for(int i = 0; i < table->number_of_elements; ++i) {
		if(table->data[i].event < 2000) { // 1000 <= work item events < 2000
			printf(" WI %14lu ", table->data[i].subject_id);
			switch(table->data[i].event) {
				case WORK_ITEM_CREATED:
					printf("CREATED");
					break;
				case WORK_ITEM_QUEUED:
					printf("QUEUED");
					break;
				case WORK_ITEM_SPLITTED:
					printf("SPLITTED");
					break;
				case WORK_ITEM_STARTED:
					printf("STARTED");
					break;
				case WORK_ITEM_SUSPENDED_BARRIER:
					printf("SUSP_BARRIER");
					break;
				case WORK_ITEM_SUSPENDED_IO:
					printf("SUSP_IO");
					break;
				case WORK_ITEM_SUSPENDED_JOIN:
					printf("SUSP_JOIN");
					break;
				case WORK_ITEM_SUSPENDED_GROUPJOIN:
					printf("SUSP_GROUPJOIN");
					break;
				case WORK_ITEM_RESUMED:
					printf("RESUMED");
					break;
				case WORK_ITEM_FINISHED:
					printf("FINISHED");
					break;
				default:
					printf("UNKNOWN");
			}
		} else if(table->data[i].event < 3000) { // 2000 <= work group events < 3000
			printf(" WG %14lu ", table->data[i].subject_id);
			switch(table->data[i].event) {
				default:
					printf("UNKOWN");
			}

		} else if(table->data[i].event < 4000) { // 3000 <= worker events < 4000
			printf(" WO %14lu ", table->data[i].subject_id);
			switch(table->data[i].event) {
				case WORKER_CREATED:
					printf("CREATED");
					break;
				case WORKER_RUNNING:
					printf("RUNNING");
					break;
				case WORKER_SLEEP_START:
					printf("SLEEP_START");
					break;
				case WORKER_SLEEP_END:
					printf("SLEEP_END");
					break;
				case WORKER_SLEEP_BUSY_START:
					printf("SLEEP_BUSY_START");
					break;
				case WORKER_SLEEP_BUSY_END:
					printf("SLEEP_BUSY_END");
					break;
				case WORKER_STOP:
					printf("STOP");
					break;
				default:
					printf("UNKOWN");
			}
		} else if(table->data[i].event < 5000) { // 4000 <= data item events < 5000
			printf(" DI %14lu ", table->data[i].subject_id);
			switch(table->data[i].event) {
				case DATA_ITEM_CREATED:
					printf("CREATED");
					break;
				case DATA_ITEM_RECYCLED:
					printf("RECYCLED");
					break;
				default:
					printf("UNKOWN");
			}
		}
		printf(":\t\t\tabs %18lu - rel-start %'18lu rel-prev %'18lu\n", table->data[i].timestamp, (table->data[i].timestamp)-(table->data[0].timestamp), (table->data[i].timestamp-table->data[(i>0)?(i-1):0].timestamp));
	}
	printf("\n");
}

// ============================ dummy functions ======================================
// dummy functions to be used via function pointer to disable
// instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void _irt_wi_no_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) { }
void _irt_wg_no_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) { }
void _irt_worker_no_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) { }
void _irt_di_no_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) { }

// ================= instrumentation function pointer toggle functions =======================

void irt_wi_toggle_instrumentation(bool enable) { 
	if(enable)
		irt_wi_instrumentation_event = &_irt_wi_instrumentation_event;
	else 
		irt_wi_instrumentation_event = &_irt_wi_no_instrumentation_event;
}

void irt_wg_toggle_instrumentation(bool enable) { 
	if(enable)
		irt_wg_instrumentation_event = &_irt_wg_instrumentation_event;
	else
		irt_wg_instrumentation_event = &_irt_wg_no_instrumentation_event;
}

void irt_worker_toggle_instrumentation(bool enable) { 
	if(enable)
		irt_worker_instrumentation_event = &_irt_worker_instrumentation_event;
	else
		irt_worker_instrumentation_event = &_irt_worker_no_instrumentation_event;
}

void irt_di_toggle_instrumentation(bool enable) { 
	if(enable)
		irt_di_instrumentation_event = &_irt_di_instrumentation_event;
	else
		irt_di_instrumentation_event = &_irt_di_no_instrumentation_event;
}

void irt_all_toggle_instrumentation(bool enable) {
	irt_wi_toggle_instrumentation(enable);
	irt_wg_toggle_instrumentation(enable);
	irt_worker_toggle_instrumentation(enable);
	irt_di_toggle_instrumentation(enable);
}

#else // if not IRT_ENABLE_INSTRUMENTATION

// ============ to be used if IRT_ENABLE_INSTRUMENTATION is not set ==============

irt_pd_table* irt_wi_create_performance_table(unsigned blocksize) { return NULL; }
void irt_destroy_performance_table(irt_pd_table* table) { }

void irt_wi_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) { }
void irt_wg_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) { }
void irt_worker_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) { }
void irt_di_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) { }

void irt_instrumentation_output(irt_worker* worker) { }

#endif // IRT_ENABLE_IRT_INSTRUMENTATION
