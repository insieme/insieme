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

#define IRT_WI_PD_BLOCKSIZE	8
#define IRT_WG_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE
#define IRT_WORKER_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE
#define IRT_DI_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE

#ifdef IRT_ENABLE_INSTRUMENTATION

// global function pointers to switch instrumentation on/off
void (*irt_wi_instrumentation_event)(irt_work_item* wi, wi_instrumentation_event event) = &_irt_wi_instrumentation_event;
void (*irt_wg_instrumentation_event)(irt_work_group* wg, wg_instrumentation_event event) = &_irt_wg_instrumentation_event;;
void (*irt_di_instrumentation_event)(irt_data_item* di, di_instrumentation_event event) = &_irt_di_instrumentation_event;
void (*irt_worker_instrumentation_event)(irt_worker* worker, worker_instrumentation_event event) = &_irt_worker_instrumentation_event;

// resizes table according to blocksize
void _irt_performance_table_resize(irt_pd_table* table) {
	table->size = table->size + (table->blocksize);
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

// commonly used internal function to record events and timestamps
void _irt_instrumentation_event_insert(irt_pd_table* table, int event) {
	uint64 time = irt_time_ticks();

	if(table->number_of_elements >= table->size)
		_irt_performance_table_resize(table);

	_irt_performance_data* pd = &(table->data[table->number_of_elements++]);

	pd->timestamp = time;
	pd->event = event;
}

// =========== private event handlers =================================

void _irt_wi_instrumentation_event(irt_work_item* wi, wi_instrumentation_event event) {

	_irt_instrumentation_event_insert(wi->performance_data, event);

	IRT_DEBUG_ONLY(if(event == WORK_ITEM_FINISHED) irt_wi_instrumentation_output(wi));
}

void _irt_wg_instrumentation_event(irt_work_group* wg, wg_instrumentation_event event) {
	_irt_instrumentation_event_insert(wg->performance_data, event);
}

void _irt_worker_instrumentation_event(irt_worker* worker, worker_instrumentation_event event) {
	_irt_instrumentation_event_insert(worker->performance_data, event);
}

void _irt_di_instrumentation_event(irt_data_item* di, di_instrumentation_event event) {
	_irt_instrumentation_event_insert(di->performance_data, event);
}

// ================= debug output functions ==================================

void irt_wi_instrumentation_output(irt_work_item* wi) {

	setlocale(LC_ALL, "");

	irt_pd_table* table = wi->performance_data;

	printf("INSTRUMENTATION: %u events for WI_ID %lu WI_IMPL_ID %u FRG? %d:\n", table->number_of_elements, wi->id.value.full, wi->impl_id, irt_wi_is_fragment(wi));

	for(int i = 0; i < table->number_of_elements; ++i) {
		printf(" WI_");
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
				printf(" WI_UNKNOWN_EVENT");
		}
		printf(":\t\t\tabs %18lu - rel-start %'18lu rel-prev %'18lu\n", table->data[i].timestamp, (table->data[i].timestamp)-(table->data[0].timestamp), (table->data[i].timestamp-table->data[(i>0)?(i-1):0].timestamp));
	}
	printf("\n");
}

void irt_wg_instrumentation_output(irt_work_group* wg) { 

	setlocale(LC_ALL, "");

	irt_pd_table* table = wg->performance_data;

	printf("INSTRUMENTATION: %u events for WG_ID %lu\n", table->number_of_elements, wg->id.value.full);

	for(int i = 0; i < table->number_of_elements; ++i) {
		switch(table->data[i].event) {
			default:
				printf(" WORK_GROUP_UNKOWN_EVENT");
		}
		printf(":\t\tabs %18lu - rel-start %'18lu rel-prev %'18lu\n", table->data[i].timestamp, (table->data[i].timestamp)-(table->data[0].timestamp), (table->data[i].timestamp-table->data[(i>0)?(i-1):0].timestamp));
	}
	printf("\n");
}

void irt_worker_instrumentation_output(irt_worker* worker) { 
	
	setlocale(LC_ALL, "");
	
	irt_pd_table* table = worker->performance_data;

	printf("INSTRUMENTATION: %u events for WORKER_ID %lu\n", table->number_of_elements, worker->id.value.full);

	for(int i = 0; i < table->number_of_elements; ++i) {
		switch(table->data[i].event) {
			case WORKER_CREATED:
				printf(" WORKER_CREATED");
				break;
			case WORKER_RUNNING:
				printf(" WORKER_RUNNING");
				break;
			case WORKER_SLEEP_START:
				printf(" WORKER_SLEEP_START");
				break;
			case WORKER_SLEEP_END:
				printf(" WORKER_SLEEP_END");
				break;
			case WORKER_SLEEP_BUSY_START:
				printf(" WORKER_SLEEP_BUSY_START");
				break;
			case WORKER_SLEEP_BUSY_END:
				printf(" WORKER_SLEEP_BUSY_END");
				break;
			case WORKER_STOP:
				printf(" WORKER_STOP");
				break;
			default:
				printf(" WORKER_UNKOWN_EVENT");
		}
		printf(":\t\tabs %18lu - rel-start %'18lu rel-prev %'18lu\n", table->data[i].timestamp, (table->data[i].timestamp)-(table->data[0].timestamp), (table->data[i].timestamp-table->data[(i>0)?(i-1):0].timestamp));
	}
	printf("\n");
}

void irt_di_instrumentation_output(irt_data_item* di) {
	
	setlocale(LC_ALL, "");
	
	irt_pd_table* table = di->performance_data;

	printf("INSTRUMENTATION: %u events for DI_ID: %lu\n", table->number_of_elements, di->id.value.full);

	for(int i = 0; i < table->number_of_elements; ++i) {
		switch(table->data[i].event) {
			case DATA_ITEM_CREATED:
				printf(" DATA_ITEM_CREATED");
				break;
			case DATA_ITEM_RECYCLED:
				printf(" DATA_ITEM_RECYCLED");
				break;
			default:
				printf(" DATA_ITEM_UNKOWN_EVENT");
		}
		printf(":\t\tabs %18lu - rel-start %'18lu rel-prev %'18lu\n", table->data[i].timestamp, (table->data[i].timestamp)-(table->data[0].timestamp), (table->data[i].timestamp-table->data[(i>0)?(i-1):0].timestamp));
	}
	printf("\n");
}

// ============================ dummy functions ======================================
// dummy functions to be used via function pointer to disable
// instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void _irt_wi_no_instrumentation_event(irt_work_item* wi, wi_instrumentation_event event) { }
void _irt_wg_no_instrumentation_event(irt_work_group* wg, wg_instrumentation_event event) { }
void _irt_worker_no_instrumentation_event(irt_worker* worker, worker_instrumentation_event event) { }
void _irt_di_no_instrumentation_event(irt_data_item* di, di_instrumentation_event event) { }

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

void irt_wi_instrumentation_event(irt_work_item* wi, wi_instrumentation_event event) { }
void irt_wg_instrumentation_event(irt_work_group* wg, wg_instrumentation_event event) { }
void irt_worker_instrumentation_event(irt_worker* worker, worker_instrumentation_event event) { }
void irt_di_instrumentation_event(irt_data_item* di, di_instrumentation_event event) { }

void irt_wi_instrumentation_output(irt_work_item* wi) { }
void irt_wg_instrumentation_output(irt_work_group* wg) { }
void irt_worker_instrumentation_output(irt_worker* worker) { }
void irt_di_instrumentation_output(irt_data_item* di) { }

#endif // IRT_ENABLE_IRT_INSTRUMENTATION
