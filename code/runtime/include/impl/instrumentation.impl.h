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

//#include <locale.h> // needed to use thousands separator
#include <stdio.h>
#include <sys/stat.h>
#include "utils/timing.h"
#include "utils/memory.h"
#ifdef IRT_ENABLE_ENERGY_INSTRUMENTATION
#include "../pmlib/CInterface.h" // power measurement library
#endif
#include "instrumentation.h"
#include "impl/error_handling.impl.h"
#include "pthread.h"
#include "errno.h"

#ifdef IRT_ENABLE_REGION_INSTRUMENTATION
#include "papi_helper.h"
#endif

#define IRT_INST_OUTPUT_PATH "IRT_INST_OUTPUT_PATH"
#define IRT_INST_WORKER_EVENT_LOGGING "IRT_INST_WORKER_EVENT_LOGGING"
#define IRT_INST_OUTPUT_PATH_CHAR_SIZE 4096
#define IRT_WORKER_PD_BLOCKSIZE	512
#define IRT_REGION_LIST_SIZE 1024
#define ENERGY_MEASUREMENT_SERVER_IP "192.168.64.178"
#define ENERGY_MEASUREMENT_SERVER_PORT 5025

#ifdef IRT_ENABLE_INSTRUMENTATION
// global function pointers to switch instrumentation on/off
void (*irt_wi_instrumentation_event)(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) = &_irt_wi_instrumentation_event;
void (*irt_wg_instrumentation_event)(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) = &_irt_wg_no_instrumentation_event;;
void (*irt_di_instrumentation_event)(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) = &_irt_di_no_instrumentation_event;
void (*irt_worker_instrumentation_event)(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) = &_irt_worker_no_instrumentation_event;
bool irt_instrumentation_event_output_is_enabled = false;

// ============================ dummy functions ======================================
// dummy functions to be used via function pointer to disable
// instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void _irt_wi_no_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) { }
void _irt_wg_no_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) { }
void _irt_worker_no_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) { }
void _irt_di_no_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) { }

// resizes table according to blocksize
void _irt_performance_table_resize(irt_pd_table* table) {
	table->size = table->size * 2;
	table->data = (_irt_performance_data*)realloc(table->data, sizeof(_irt_performance_data)*table->size);
}

// =============== functions for creating and destroying performance tables ===============

// allocates memory for performance data, sets all fields
irt_pd_table* irt_create_performance_table(unsigned blocksize) {
	irt_pd_table* table = (irt_pd_table*)malloc(sizeof(irt_pd_table));
	table->blocksize = blocksize;
	table->size = table->blocksize * 2;
	table->number_of_elements = 0;
	table->data = (_irt_performance_data*)malloc(sizeof(_irt_performance_data) * table->size);
	return table;
}

// frees allocated memory
void irt_destroy_performance_table(irt_pd_table* table) {
	free(table->data);
	free(table);
}

void _irt_instrumentation_event_insert_time(irt_worker* worker, const int event, const uint64 id, const uint64 time) {
	_irt_pd_table* table = worker->performance_data;

	IRT_ASSERT(table->number_of_elements <= table->size, IRT_ERR_INSTRUMENTATION, "Instrumentation: Number of event table entries larger than table size\n")

	if(table->number_of_elements >= table->size) {
		_irt_performance_table_resize(table);
	}
	_irt_performance_data* pd = &(table->data[table->number_of_elements++]);

	pd->timestamp = time;
	pd->event = event;
	pd->subject_id = id;
}


// commonly used internal function to record events and timestamps
void _irt_instrumentation_event_insert(irt_worker* worker, const int event, const uint64 id) {
	uint64 time = irt_time_ticks();

	IRT_ASSERT(worker != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Trying to add event for worker 0!\n");

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
	_irt_instrumentation_event_insert(worker, event, subject_id.value.full);
}

void _irt_di_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) {
	_irt_instrumentation_event_insert(worker, event, subject_id.value.full);
}

// ================= debug output functions ==================================

// writes csv files
void irt_instrumentation_output(irt_worker* worker) {
	if(!irt_instrumentation_event_output_is_enabled)
		return;
	// necessary for thousands separator
	//setlocale(LC_ALL, "");
	//
	
	char outputfilename[IRT_INST_OUTPUT_PATH_CHAR_SIZE];
	char defaultoutput[] = ".";
	char* outputprefix = defaultoutput;
	if(getenv(IRT_INST_OUTPUT_PATH)) outputprefix = getenv(IRT_INST_OUTPUT_PATH);

	struct stat st;
	int stat_retval = stat(outputprefix,&st);
	if(stat_retval != 0)
		mkdir(outputprefix, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

	IRT_ASSERT(stat(outputprefix,&st) == 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Error creating directory for performance log writing: %s\n", strerror(errno));

	sprintf(outputfilename, "%s/worker_event_log.%04u", outputprefix, worker->id.value.components.thread);

	FILE* outputfile = fopen(outputfilename, "w");
	IRT_ASSERT(outputfile != 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Unable to open file for event log writing: %s\n", strerror(errno));
/*	if(outputfile == 0) {
		IRT_DEBUG("Instrumentation: Unable to open file for event log writing\n");
		IRT_DEBUG_ONLY(strerror(errno));
		return;
	}*/
	irt_pd_table* table = worker->performance_data;
	IRT_ASSERT(table != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Worker has no event data!")
	//fprintf(outputfile, "INSTRUMENTATION: %10u events for worker %4u\n", table->number_of_elements, worker->id.value.components.thread);

#ifdef USE_OPENCL
	irt_ocl_event_table* ocl_table = worker->event_data;
	IRT_ASSERT(ocl_table != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Worker has no OpenCL event data!")
	uint64 ocl_offset = 0;
	_irt_inst_ocl_performance_helper* ocl_helper_table = (_irt_inst_ocl_performance_helper*)malloc(ocl_table->num_events * 4 * sizeof(_irt_inst_ocl_performance_helper));
	int ocl_helper_table_number_of_entries = ocl_table->num_events * 4;
	int helper_counter = 0;

	for(int i = 0; i < ocl_table->num_events; ++i) {
		cl_command_type retval;
                cl_int err_code = clGetEventInfo(ocl_table->event_array[i].event, CL_EVENT_COMMAND_TYPE, sizeof(cl_command_type), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL,"Error getting \"event command type\" info: \"%d\"", err_code);

                cl_ulong events[4];
                err_code = clGetEventProfilingInfo(ocl_table->event_array[i].event, CL_PROFILING_COMMAND_QUEUED, sizeof(cl_ulong), &events[IRT_INST_OCL_QUEUED], NULL);
                err_code |= clGetEventProfilingInfo(ocl_table->event_array[i].event, CL_PROFILING_COMMAND_SUBMIT, sizeof(cl_ulong), &events[IRT_INST_OCL_SUBMITTED], NULL);
                err_code |= clGetEventProfilingInfo(ocl_table->event_array[i].event, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &events[IRT_INST_OCL_STARTED], NULL);
                err_code |= clGetEventProfilingInfo(ocl_table->event_array[i].event, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &events[IRT_INST_OCL_FINISHED], NULL);
                IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting profiling info: \"%d\"",  err_code);

		// convert all ocl event information into a flat table, only the ocl_events[j] changes over this run
		for(int j = IRT_INST_OCL_QUEUED; j <= IRT_INST_OCL_FINISHED; ++j) {
			ocl_helper_table[helper_counter].workitem_id = ocl_table->event_array[i].workitem_id;
			ocl_helper_table[helper_counter].timestamp = events[j];
			ocl_helper_table[helper_counter].origin = retval;
			ocl_helper_table[helper_counter].event = j;
			helper_counter++;
		}

	}

	IRT_ASSERT(ocl_helper_table_number_of_entries == helper_counter, IRT_ERR_INSTRUMENTATION, "OCL event counts do not match: helper_counter: %d, table_entries: %d\n", helper_counter, ocl_helper_table_number_of_entries);
	int ocl_helper_table_counter = 0;
#endif

	for(int i = 0; i < table->number_of_elements; ++i) {
		// timestamp to be printed if not updated by opencl event data
//		uint64 temp_timestamp = irt_time_convert_ticks_to_ns(table->data[i].timestamp);
#ifdef USE_OPENCL
		// if a new workitem started, check if there is corresponding opencl event data
		if(table->data[i].event == WORK_ITEM_STARTED) {
			for(int j = 0; j < ocl_helper_table_number_of_entries; ++j) {
				if(ocl_helper_table[j].workitem_id == table->data[i].subject_id ) {//&& ocl_helper_table[j].event == IRT_INST_OCL_QUEUED) {
					ocl_offset = irt_time_convert_ticks_to_ns(table->data[i].timestamp) - ocl_helper_table[j].timestamp;
					break;
				}
			}
		}
		while(ocl_helper_table_counter < ocl_helper_table_number_of_entries && irt_time_convert_ticks_to_ns(table->data[i].timestamp) > ocl_helper_table[ocl_helper_table_counter].timestamp + ocl_offset) {
			uint64 temp_timestamp = ocl_helper_table[ocl_helper_table_counter].timestamp + ocl_offset;
			fprintf(outputfile, "KN,%14lu,\t", ocl_helper_table[ocl_helper_table_counter].workitem_id);
			switch(ocl_helper_table[ocl_helper_table_counter].origin) {
				case CL_COMMAND_NDRANGE_KERNEL:
					fprintf(outputfile, "ND_");
					break;
				case CL_COMMAND_WRITE_BUFFER:
					fprintf(outputfile, "WRITE_");
					break;
				case CL_COMMAND_READ_BUFFER:
					fprintf(outputfile, "READ_");
					break;
				default:
					fprintf(outputfile, "UNKNOWN_");
			}
			switch(ocl_helper_table[ocl_helper_table_counter].event) {
				case IRT_INST_OCL_QUEUED:
				       fprintf(outputfile, "QUEUED");
				       break;
				case IRT_INST_OCL_SUBMITTED:
				       fprintf(outputfile, "SUBMITTED");
				       break;
				case IRT_INST_OCL_STARTED:
				       fprintf(outputfile, "STARTED");
				       break;
				case IRT_INST_OCL_FINISHED:
				       fprintf(outputfile, "FINISHED");
				       break;
				default:
				       fprintf(outputfile, "UNNKOWN");
				       break;
			}
			fprintf(outputfile, ",\t%18lu\n", temp_timestamp);
			ocl_helper_table_counter++;
		}
#endif
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
				case WORK_GROUP_CREATED:
					fprintf(outputfile, "CREATED");
					break;
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
		fprintf(outputfile, ",\t%18lu\n", irt_time_convert_ticks_to_ns(table->data[i].timestamp));
	}
	fclose(outputfile);
#ifdef USE_OPENCL
	free(ocl_helper_table);
#endif
}

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
//	irt_wg_toggle_instrumentation(enable);
//	irt_worker_toggle_instrumentation(enable);
//	irt_di_toggle_instrumentation(enable);
	irt_instrumentation_event_output_is_enabled = enable;
}

void irt_all_toggle_instrumentation_from_env() {
	if(getenv(IRT_INST_WORKER_EVENT_LOGGING)) {
		if(strcmp(getenv(IRT_INST_WORKER_EVENT_LOGGING), "true") == 0) {
			irt_all_toggle_instrumentation(true);
		} else {
			irt_all_toggle_instrumentation(false);
		}
	} else {
		irt_all_toggle_instrumentation(false);
	}
}

#else // if not IRT_ENABLE_INSTRUMENTATION

// ============ to be used if IRT_ENABLE_INSTRUMENTATION is not set ==============

irt_pd_table* irt_create_performance_table(unsigned blocksize) { return NULL; }
void irt_destroy_performance_table(irt_pd_table* table) { }

void irt_wi_instrumentation_event(irt_worker* worker, wi_instrumentation_event event, irt_work_item_id subject_id) { }
void irt_wg_instrumentation_event(irt_worker* worker, wg_instrumentation_event event, irt_work_group_id subject_id) { }
void irt_worker_instrumentation_event(irt_worker* worker, worker_instrumentation_event event, irt_worker_id subject_id) { }
void irt_di_instrumentation_event(irt_worker* worker, di_instrumentation_event event, irt_data_item_id subject_id) { }

void irt_instrumentation_output(irt_worker* worker) { }

#endif // IRT_ENABLE_INSTRUMENTATION

#ifndef IRT_ENABLE_REGION_INSTRUMENTATION
irt_epd_table* irt_create_extended_performance_table(unsigned blocksize) { return NULL; }
void irt_destroy_extended_performance_table(irt_epd_table* table) {}
void irt_instrumentation_region_start(region_id id) { }
void irt_instrumentation_region_end(region_id id) { }
void irt_extended_instrumentation_output(irt_worker* worker) {}
void irt_aggregated_instrumentation_output() {}
void irt_instrumentation_region_set_timestamp(irt_work_item* wi) {}
void irt_instrumentation_region_add_time(irt_work_item* wi) {}
#endif

#ifdef IRT_ENABLE_REGION_INSTRUMENTATION

irt_apd_table* irt_g_aggregated_performance_table;

irt_region_list* irt_region_list_create() {
	irt_region_list* list = (irt_region_list*)malloc(sizeof(irt_region_list));
	irt_region* temp = (irt_region*)malloc(sizeof(irt_region)*IRT_REGION_LIST_SIZE);
	list->head = temp++;
	irt_region* current = list->head;
	for(int i = 0; i < (IRT_REGION_LIST_SIZE-1); ++i) {
		current->next = temp++;
		current = current->next;
	}

	return list;
}

/*void irt_region_list_destroy() {
	irt_region* temp = irt_g_region_list->head;
	irt_region* current = temp;
	while(current->next != NULL) {
		temp = current->next;
		free(current);
		current = temp;
	}
	free(irt_g_region_list);
}*/

irt_region* irt_region_list_new_item(irt_worker* worker) {
	irt_region* retval;
	if(worker->region_reuse_list->head) {
		retval = worker->region_reuse_list->head;
		worker->region_reuse_list->head = retval->next;
	} else {
		retval = (irt_region*)malloc(sizeof(irt_region));
	}
	retval->cputime = 0;
	return retval;
}

void irt_region_list_recycle_item(irt_worker* worker, irt_region* region) {
	region->next = worker->region_reuse_list->head;
	worker->region_reuse_list->head = region;
}

void (*irt_instrumentation_region_start)(region_id id) = &_irt_instrumentation_region_start;
void (*irt_instrumentation_region_end)(region_id id) = &_irt_instrumentation_region_end;

void _irt_no_instrumentation_region_start(region_id id) { }
void _irt_no_instrumentation_region_end(region_id id) { }

// =============== initialization functions ===============

void irt_instrumentation_init_energy_instrumentation() {
#ifdef IRT_ENABLE_ENERGY_INSTRUMENTATION
	// creates a new power measurement library session - parameters: pmCreateNewSession(session_name, server_ip, server_port, logfile_path)
	//pmCreateNewSession("insieme",ENERGY_MEASUREMENT_SERVER_IP, ENERGY_MEASUREMENT_SERVER_PORT, NULL);
	pmCreateNewSession((char*)"insieme", (char*)"192.168.71.178", 5025, NULL);
#endif
}

void _irt_extended_performance_table_resize(irt_epd_table* table) {
	table->size = table->size * 2;
	table->data = (_irt_extended_performance_data*)realloc(table->data, sizeof(_irt_extended_performance_data)*table->size);
}

irt_epd_table* irt_create_extended_performance_table(unsigned blocksize) {
	irt_epd_table* table = (irt_epd_table*)malloc(sizeof(irt_epd_table));
	table->blocksize = blocksize;
	table->size = table->blocksize * 2;
	table->number_of_elements = 0;
	table->data = (_irt_extended_performance_data*)malloc(sizeof(_irt_extended_performance_data) * table->size);
	return table;
}

void irt_destroy_extended_performance_table(irt_epd_table* table) {
	free(table->data);
	free(table);
}

void _irt_aggregated_performance_table_resize() {
	irt_apd_table* table = irt_g_aggregated_performance_table;
	table->size = table->size * 2;
	table->data = (_irt_aggregated_performance_data*)realloc(table->data, sizeof(_irt_aggregated_performance_data)*table->size);
}

void irt_create_aggregated_performance_table(unsigned blocksize) {
	irt_apd_table* table = (irt_apd_table*)malloc(sizeof(irt_apd_table));
	table->blocksize = blocksize;
	table->size = table->blocksize * 2;
	table->number_of_elements = 0;
	table->data = (_irt_aggregated_performance_data*)malloc(sizeof(_irt_aggregated_performance_data) * table->size);
	irt_g_aggregated_performance_table = table;
}

void irt_destroy_aggregated_performance_table() {
	free(irt_g_aggregated_performance_table->data);
	free(irt_g_aggregated_performance_table);
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

void _irt_extended_instrumentation_event_insert(irt_worker* worker, const int event, const uint64 id) {

	_irt_epd_table* table = worker->extended_performance_data;
		
	IRT_ASSERT(table->number_of_elements <= table->size, IRT_ERR_INSTRUMENTATION, "Instrumentation: Number of extended event table entries larger than table size\n")
	
	if(table->number_of_elements >= table->size)
		_irt_extended_performance_table_resize(table);

	_irt_extended_performance_data* epd = &(table->data[table->number_of_elements++]);
	switch(event) {
		case REGION_START:
#ifdef IRT_ENABLE_ENERGY_INSTRUMENTATION
			pmStartSession();
#endif
			epd->event = event;
			epd->subject_id = id;
			epd->data[PERFORMANCE_DATA_ENTRY_ENERGY].value_double = -1;
			// set all papi counter fields for REGION_START to -1 since we don't use them here
			for(int i = PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_1; i < PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_1 + irt_g_number_of_papi_events; ++i)
				epd->data[i].value_uint64 = UINT_MAX;
			PAPI_start(worker->irt_papi_event_set);

			irt_get_memory_usage(&(epd->data[PERFORMANCE_DATA_ENTRY_MEMORY_VIRT].value_uint64), &(epd->data[PERFORMANCE_DATA_ENTRY_MEMORY_RES].value_uint64));
		
			// do time as late as possible to exclude overhead of remaining instrumentation/measurements
			epd->timestamp = irt_time_ticks();
			//epd->timestamp = PAPI_get_virt_cyc(); // counts only since process start and does not include other scheduled processes, but decreased accuracy
			break;

		case REGION_END:
			; // do not remove! bug in gcc!
			// do time as early as possible to exclude overhead of remaining instrumentation/measurements
			//uint64 time = PAPI_get_virt_cyc(); // counts only since process start and does not include other scheduled processes, but decreased accuracy
			uint64 time = irt_time_ticks();
			uint64 papi_temp[IRT_INST_PAPI_MAX_COUNTERS];
		       	PAPI_read(worker->irt_papi_event_set, (long long*)papi_temp);
			PAPI_reset(worker->irt_papi_event_set);
			
			irt_get_memory_usage(&(epd->data[PERFORMANCE_DATA_ENTRY_MEMORY_VIRT].value_uint64), &(epd->data[PERFORMANCE_DATA_ENTRY_MEMORY_RES].value_uint64));

			double energy_consumption = -1;

#ifdef IRT_ENABLE_ENERGY_INSTRUMENTATION
			//TODO: needs to be changed to suspend()
			if(pmStopSession() < 0) // if measurement failed for whatever reason
				energy_consumption = -1; // result is invalid
			else
				pmCalculateDiff(0, 0, 32, &energy_consumption); // 32 = Whr
#endif

			epd->timestamp = time;
			epd->event = event;
			epd->subject_id = id;
			epd->data[PERFORMANCE_DATA_ENTRY_ENERGY].value_double = energy_consumption;
			
			for(int i=PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_1; i<(irt_g_number_of_papi_events+PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_1); ++i)
				epd->data[i].value_uint64 = papi_temp[i-PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_1];
			break;
	}
}

void _irt_aggregated_instrumentation_insert(irt_worker* worker, int64 id, uint64 walltime, uint64 cputime) {

	IRT_ASSERT(irt_g_aggregated_performance_table->number_of_elements <= irt_g_aggregated_performance_table->size, IRT_ERR_INSTRUMENTATION, "Instrumentation: Number of event table entries larger than table size\n")

	if(irt_g_aggregated_performance_table->number_of_elements >= irt_g_aggregated_performance_table->size) {
		_irt_aggregated_performance_table_resize();
	}

	_irt_aggregated_performance_data* apd = &(irt_g_aggregated_performance_table->data[irt_g_aggregated_performance_table->number_of_elements++]);

	apd->walltime = walltime;
	apd->cputime = cputime;
	apd->number_of_workers = 1;
	apd->id = id;
}

void _irt_instrumentation_mark_start(region_id id) {
	irt_worker* worker = irt_worker_get_current();
	_irt_instrumentation_event_insert(worker, REGION_START, (uint64)id);
	_irt_extended_instrumentation_event_insert(worker, REGION_START, (uint64)id);

	irt_region* region = irt_region_list_new_item(worker);
	region->cputime = 0;
	region->start_time = irt_time_ticks();
	region->next = worker->cur_wi->region;

	irt_instrumentation_region_add_time(worker->cur_wi);
	worker->cur_wi->region = region;
	irt_instrumentation_region_set_timestamp(worker->cur_wi);
}

void _irt_instrumentation_mark_end(region_id id, bool insert_aggregated) {
	uint64 timestamp = irt_time_ticks();
	irt_worker* worker = irt_worker_get_current();
	irt_instrumentation_region_add_time(worker->cur_wi);
	irt_instrumentation_region_set_timestamp(worker->cur_wi);

	uint64 ending_region_cputime = worker->cur_wi->region->cputime;

	irt_region* region = worker->cur_wi->region;

	if(insert_aggregated)
		_irt_aggregated_instrumentation_insert(worker, id, timestamp - region->start_time, ending_region_cputime);

	worker->cur_wi->region = region->next;
	irt_region_list_recycle_item(worker, region);

	if(worker->cur_wi->region) { // if the ended region was a nested one, add execution time to outer region
		irt_atomic_fetch_and_add(&(worker->cur_wi->region->cputime), ending_region_cputime);
	}

	_irt_instrumentation_event_insert(worker, REGION_END, (uint64)id);
	_irt_extended_instrumentation_event_insert(worker, REGION_END, (uint64)id);

}

void _irt_instrumentation_region_start(region_id id) {
	_irt_instrumentation_mark_start(id);
}

void _irt_instrumentation_region_end(region_id id) {
	_irt_instrumentation_mark_end(id, true);
}

void _irt_instrumentation_pfor_start(region_id id) {
	_irt_instrumentation_mark_start(id);
}

void _irt_instrumentation_pfor_end(region_id id) {
	_irt_instrumentation_mark_end(id, false);
}

void irt_extended_instrumentation_output(irt_worker* worker) {
	// environmental variable can hold the output path for the performance logs, default is .
	char outputfilename[IRT_INST_OUTPUT_PATH_CHAR_SIZE];
	char defaultoutput[] = ".";
	char* outputprefix = defaultoutput;
	if(getenv(IRT_INST_OUTPUT_PATH)) outputprefix = getenv(IRT_INST_OUTPUT_PATH);

	struct stat st;
	int stat_retval = stat(outputprefix,&st);
	if(stat_retval != 0)
		mkdir(outputprefix, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

	IRT_ASSERT(stat(outputprefix,&st) == 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Error creating directory for performance log writing: %s\n", strerror(errno));

	sprintf(outputfilename, "%s/worker_performance_log.%04u", outputprefix, worker->id.value.components.thread);

	// used to print papi names to the header of the performance logs
	int number_of_papi_events = IRT_INST_PAPI_MAX_COUNTERS;
	int papi_events[IRT_INST_PAPI_MAX_COUNTERS];
	char event_name_temp[PAPI_MAX_STR_LEN];
	int retval = 0;

	if((retval = PAPI_list_events(worker->irt_papi_event_set, papi_events, &number_of_papi_events)) != PAPI_OK) {
		IRT_DEBUG("Instrumentation: Error listing papi events, there will be no performance counter measurement data! Reason: %s\n", PAPI_strerror(retval));
		number_of_papi_events = 0;
	}

	FILE* outputfile = fopen(outputfilename, "w");
	IRT_ASSERT(outputfile != 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Unable to open file for performance log writing: %s\n", strerror(errno));
	fprintf(outputfile, "#subject,id,timestamp_start_(ns),timestamp_end_(ns),virt_memory_start_(kb),virt_memory_end_(kb),res_memory_start_(kb),res_memory_end_(kb),energy_start_(wh),energy_end_(wh)");

	// get the papi event names and print them to the header
	for(int i = 0; i < number_of_papi_events; ++i) {
		PAPI_event_code_to_name(papi_events[i], event_name_temp);
		fprintf(outputfile, ",%s", event_name_temp);
	}
	fprintf(outputfile, "\n");

	irt_epd_table* table = worker->extended_performance_data;
	IRT_ASSERT(table != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Worker has no performance data!")

	// this loop iterates through the table: if REGION_END is found, search reversely for matching REGION_START and output corresponding values in a single line
	for(int i = 0; i < table->number_of_elements; ++i) {
		if(table->data[i].event == REGION_END) {
			// holds data of matching REGION_START
			_irt_extended_performance_data start_data;
			// iterate back through performance entries to find the matching start with this id
			for(int j = i-1; j >= 0; --j) {
				if(table->data[j].subject_id == table->data[i].subject_id)
					if(table->data[j].event == REGION_START) {
						start_data = table->data[j]; //memcpy(&start_data, &(table->data[j]), sizeof(_irt_extended_performance_data));
						break;
					}
				IRT_ASSERT(start_data.timestamp != 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Cannot find a matching start statement\n")
			}
			// single fprintf for performance reasons
			// outputs all data in pairs: value_when_entering_region, value_when_exiting_region
			fprintf(outputfile, "RG,%lu,%lu,%lu,%lu,%lu,%lu,%lu,%1.1f,%1.1f",
					table->data[i].subject_id,
					irt_time_convert_ticks_to_ns(start_data.timestamp), 
					irt_time_convert_ticks_to_ns(table->data[i].timestamp),
					start_data.data[PERFORMANCE_DATA_ENTRY_MEMORY_VIRT].value_uint64, 
					table->data[i].data[PERFORMANCE_DATA_ENTRY_MEMORY_VIRT].value_uint64, 
					start_data.data[PERFORMANCE_DATA_ENTRY_MEMORY_RES].value_uint64, 
					table->data[i].data[PERFORMANCE_DATA_ENTRY_MEMORY_RES].value_uint64, 
					start_data.data[PERFORMANCE_DATA_ENTRY_ENERGY].value_double, 
					table->data[i].data[PERFORMANCE_DATA_ENTRY_ENERGY].value_double);
			// prints all performance counters, assumes that the order of the enums is correct (contiguous from ...COUNTER_1 to ...COUNTER_N
			for(int j = PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_1; j < (irt_g_number_of_papi_events + PERFORMANCE_DATA_ENTRY_PAPI_COUNTER_1); ++j) {
				if( table->data[i].data[j].value_uint64 == UINT_MAX) // used to filter missing results, replace with -1 in output
					fprintf(outputfile, ",-1");
				else
					fprintf(outputfile, ",%lu", table->data[i].data[j].value_uint64);
				}
			fprintf(outputfile, "\n");
		}
	}
	fclose(outputfile);
}

void irt_aggregated_instrumentation_output() {
	// environmental variable can hold the output path for the performance logs, default is .
	char outputfilename[IRT_INST_OUTPUT_PATH_CHAR_SIZE];
	char defaultoutput[] = ".";
	char* outputprefix = defaultoutput;
	if(getenv(IRT_INST_OUTPUT_PATH)) outputprefix = getenv(IRT_INST_OUTPUT_PATH);

	struct stat st;
	int stat_retval = stat(outputprefix,&st);
	if(stat_retval != 0)
		mkdir(outputprefix, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

	IRT_ASSERT(stat(outputprefix,&st) == 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Error creating directory for performance log writing: %s\n", strerror(errno));

	sprintf(outputfilename, "%s/worker_efficiency_log", outputprefix);

	FILE* outputfile = fopen(outputfilename, "w");
	IRT_ASSERT(outputfile != 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Unable to open file for efficiency log writing: %s\n", strerror(errno));

	irt_apd_table* table = irt_g_aggregated_performance_table;
	IRT_ASSERT(table != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Worker has no performance data!")

//	setlocale(LC_ALL, "");

	fprintf(outputfile, "#subject,id,wall_time(ns),cpu_time(ns),num_workers\n");

	for(int i = 0; i < table->number_of_elements; ++i) {
		fprintf(outputfile, "RG,%lu,%lu,%lu,%u\n",
			table->data[i].id,
			irt_time_convert_ticks_to_ns(table->data[i].walltime),
			irt_time_convert_ticks_to_ns(table->data[i].cputime),
			table->data[i].number_of_workers);
	}
	fclose(outputfile);
}

void _irt_aggregated_instrumentation_insert_pfor(int64 id, uint64 walltime, irt_loop_sched_data* sched_data) {

	IRT_ASSERT(irt_g_aggregated_performance_table->number_of_elements <= irt_g_aggregated_performance_table->size, IRT_ERR_INSTRUMENTATION, "Instrumentation: Number of event table entries larger than table size\n")

	if(irt_g_aggregated_performance_table->number_of_elements >= irt_g_aggregated_performance_table->size) {
		_irt_aggregated_performance_table_resize();
	}
	_irt_aggregated_performance_data* apd = &(irt_g_aggregated_performance_table->data[irt_g_aggregated_performance_table->number_of_elements++]);

	apd->walltime = walltime;
	apd->cputime = sched_data->cputime;
	apd->number_of_workers = sched_data->policy.participants;
	apd->id = id;
}

void irt_instrumentation_region_set_timestamp(irt_work_item* wi) {
	wi->last_timestamp = irt_time_ticks();
}

void irt_instrumentation_region_add_time(irt_work_item* wi) {
	uint64 temp = irt_time_ticks();
	if(wi->region) {
//		printf("in wi %18lu, adding %18lu to %18lu of region %p\n", wi->id.value.full, temp - wi->last_timestamp, wi->region->cputime, wi->region);
		irt_atomic_fetch_and_add(&(wi->region->cputime), temp - wi->last_timestamp);
	}
}

#endif
