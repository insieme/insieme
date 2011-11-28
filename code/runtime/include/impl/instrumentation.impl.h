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

//#include <locale.h>
#include "utils/timing.h"
#include "instrumentation.h"
#include <pthread.h>
#include <stdio.h>

#define IRT_INST_OUTPUT_PATH "IRT_INST_OUTPUT_PATH"
#define IRT_WI_PD_BLOCKSIZE	512
#define IRT_WG_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE
#define IRT_WORKER_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE
#define IRT_DI_PD_BLOCKSIZE	IRT_WI_PD_BLOCKSIZE

#ifdef IRT_ENABLE_INSTRUMENTATION

// global function pointers to switch instrumentation on/off
void (*irt_wi_instrumentation_event)(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) = &_irt_wi_instrumentation_event;
void (*irt_wg_instrumentation_event)(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) = &_irt_wg_instrumentation_event;;
void (*irt_di_instrumentation_event)(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) = &_irt_di_instrumentation_event;
void (*irt_worker_instrumentation_event)(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) = &_irt_worker_instrumentation_event;
void (*irt_instrumentation_region_start)(region_id id) = &_irt_instrumentation_region_start;
void (*irt_instrumentation_region_end)(region_id id) = &_irt_instrumentation_region_end;

// resizes table according to blocksize
void _irt_performance_table_resize(irt_pd_table* table) {
	table->size = table->size * 2;
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

void _irt_instrumentation_region_start(region_id id) { 
	_irt_instrumentation_event_insert(irt_worker_get_current(), REGION_START, (uint64)id);
}

void _irt_instrumentation_region_end(region_id id) { 
	_irt_instrumentation_event_insert(irt_worker_get_current(), REGION_END, (uint64)id);
}

// ================= debug output functions ==================================

// writes csv files
void irt_instrumentation_output(irt_worker* worker) {
	//setlocale(LC_ALL, "");

	char outputfilename[64];
	char defaultoutput[] = ".";
	char* outputprefix = defaultoutput;
	if(getenv(IRT_INST_OUTPUT_PATH)) outputprefix = getenv(IRT_INST_OUTPUT_PATH);

	sprintf(outputfilename, "%s/worker_event_log.%04u", outputprefix, worker->id.value.components.thread);

	FILE* outputfile = fopen(outputfilename, "w");
	irt_pd_table* table = worker->performance_data;
	//fprintf(outputfile, "INSTRUMENTATION: %10u events for worker %4u\n", table->number_of_elements, worker->id.value.components.thread);

	for(int i = 0; i < table->number_of_elements; ++i) {
		if(table->data[i].event < 2000) { // 1000 <= work item events < 2000
			fprintf(outputfile, "WI,%14lu,\t", table->data[i].subject_id);
			switch(table->data[i].event) {
				case WORK_ITEM_CREATED:
					fprintf(outputfile, "CREATED");
					break;
				case WORK_ITEM_QUEUED:
					fprintf(outputfile, "QUEUED");
					break;
				case WORK_ITEM_SPLITTED:
					fprintf(outputfile, "SPLITTED");
					break;
				case WORK_ITEM_STARTED:
					fprintf(outputfile, "STARTED");
					break;
				case WORK_ITEM_SUSPENDED_BARRIER:
					fprintf(outputfile, "SUSP_BARRIER");
					break;
				case WORK_ITEM_SUSPENDED_IO:
					fprintf(outputfile, "SUSP_IO");
					break;
				case WORK_ITEM_SUSPENDED_JOIN:
					fprintf(outputfile, "SUSP_JOIN");
					break;
				case WORK_ITEM_SUSPENDED_GROUPJOIN:
					fprintf(outputfile, "SUSP_GROUPJOIN");
					break;
				case WORK_ITEM_RESUMED:
					fprintf(outputfile, "RESUMED");
					break;
				case WORK_ITEM_FINISHED:
					fprintf(outputfile, "FINISHED");
					break;
				default:
					fprintf(outputfile, "UNKNOWN");
			}
		} else if(table->data[i].event < 3000) { // 2000 <= work group events < 3000
			fprintf(outputfile, "WG,%14lu,\t", table->data[i].subject_id);
			switch(table->data[i].event) {
				default:
					fprintf(outputfile, "UNKOWN");
			}

		} else if(table->data[i].event < 4000) { // 3000 <= worker events < 4000
			fprintf(outputfile, "WO,%14lu,\t", table->data[i].subject_id);
			switch(table->data[i].event) {
				case WORKER_CREATED:
					fprintf(outputfile, "CREATED");
					break;
				case WORKER_RUNNING:
					fprintf(outputfile, "RUNNING");
					break;
				case WORKER_SLEEP_START:
					fprintf(outputfile, "SLEEP_START");
					break;
				case WORKER_SLEEP_END:
					fprintf(outputfile, "SLEEP_END");
					break;
				case WORKER_SLEEP_BUSY_START:
					fprintf(outputfile, "SLEEP_BUSY_START");
					break;
				case WORKER_SLEEP_BUSY_END:
					fprintf(outputfile, "SLEEP_BUSY_END");
					break;
				case WORKER_STOP:
					fprintf(outputfile, "STOP");
					break;
				default:
					fprintf(outputfile, "UNKOWN");
			}
		} else if(table->data[i].event < 5000) { // 4000 <= data item events < 5000
			fprintf(outputfile, "DI,%14lu,\t", table->data[i].subject_id);
			switch(table->data[i].event) {
				case DATA_ITEM_CREATED:
					fprintf(outputfile, "CREATED");
					break;
				case DATA_ITEM_RECYCLED:
					fprintf(outputfile, "RECYCLED");
					break;
				default:
					fprintf(outputfile, "UNKOWN");
			}
		} else if(table->data[i].event < 6000) { // 5000 <= regions < 6000
			fprintf(outputfile, "RG,%14lu,\t", table->data[i].subject_id);
			switch(table->data[i].event) {
				case REGION_START:
					fprintf(outputfile, "START");
					break;
				case REGION_END:
					fprintf(outputfile, "END");
					break;
				default:
					fprintf(outputfile, "UNKNOWN");
			}
		}
		fprintf(outputfile, ",\t%18lu,%18lu\n", table->data[i].timestamp, irt_time_convert_ticks_to_ns(table->data[i].timestamp));
	}
	fprintf(outputfile, "\n");
	fclose(outputfile);
}

// ============================ dummy functions ======================================
// dummy functions to be used via function pointer to disable
// instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void _irt_wi_no_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) { }
void _irt_wg_no_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) { }
void _irt_worker_no_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) { }
void _irt_di_no_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) { }
void _irt_no_instrumentation_region_start(region_id id) { }
void _irt_no_instrumentation_region_end(region_id id) { }


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

void irt_region_toggle_instrumentation(bool enable) {
	if(enable) {
		irt_instrumentation_region_start = &_irt_instrumentation_region_start;
		irt_instrumentation_region_end = &_irt_instrumentation_region_end;
	} else {
		irt_instrumentation_region_start = &_irt_no_instrumentation_region_start;
		irt_instrumentation_region_end = &_irt_no_instrumentation_region_end;
	}

}

void irt_all_toggle_instrumentation(bool enable) {
	irt_wi_toggle_instrumentation(enable);
	irt_wg_toggle_instrumentation(enable);
	irt_worker_toggle_instrumentation(enable);
	irt_di_toggle_instrumentation(enable);
	irt_region_toggle_instrumentation(enable);
}

#else // if not IRT_ENABLE_INSTRUMENTATION

// ============ to be used if IRT_ENABLE_INSTRUMENTATION is not set ==============

irt_pd_table* irt_wi_create_performance_table(unsigned blocksize) { return NULL; }
void irt_destroy_performance_table(irt_pd_table* table) { }

void irt_wi_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) { }
void irt_wg_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) { }
void irt_worker_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) { }
void irt_di_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) { }

void irt_instrumentation_region_start(region_id id) { }
void irt_instrumentation_region_end(region_id id) { }

void irt_instrumentation_output(irt_worker* worker) { }

#endif // IRT_ENABLE_IRT_INSTRUMENTATION
