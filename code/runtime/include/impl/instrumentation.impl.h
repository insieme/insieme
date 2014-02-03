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

#include <locale.h> // needed to use thousands separator
#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>
#include "utils/timing.h"
#include "utils/energy.h"
#include "utils/memory.h"
#include "instrumentation.h"
#include "impl/error_handling.impl.h"
#include "instrumentation_includes.h"

#ifdef IRT_ENABLE_INSTRUMENTATION
// global function pointers to switch instrumentation on/off
void (*irt_inst_insert_wi_event)(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id) = &_irt_inst_insert_no_wi_event;
void (*irt_inst_insert_wg_event)(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id) = &_irt_inst_insert_no_wg_event;
void (*irt_inst_insert_di_event)(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id) = &_irt_inst_insert_no_di_event;
void (*irt_inst_insert_wo_event)(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) = &_irt_inst_insert_no_wo_event;
void (*irt_inst_insert_db_event)(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) = &_irt_inst_insert_no_db_event;
bool irt_g_instrumentation_event_output_is_enabled = false;
bool irt_g_instrumentation_event_output_is_binary = false;

// ============================ dummy functions ======================================
// dummy functions to be used via function pointer to disable
// instrumentation even if IRT_ENABLE_INSTRUMENTATION is set

void _irt_inst_insert_no_wi_event(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id) { }
void _irt_inst_insert_no_wg_event(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id) { }
void _irt_inst_insert_no_wo_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) { }
void _irt_inst_insert_no_di_event(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id) { }
void _irt_inst_insert_no_db_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) { }

// resizes table
void _irt_inst_event_data_table_resize(irt_instrumentation_event_data_table* table) {
	table->size = table->size * 2;
	table->data = (irt_instrumentation_event_data*)realloc(table->data, sizeof(irt_instrumentation_event_data)*table->size);
	IRT_ASSERT(table->data != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Could not perform realloc for event instrumentation data table: %s", strerror(errno))
}

// =============== functions for creating and destroying performance tables ===============

// allocates memory for performance data, sets all fields
irt_instrumentation_event_data_table* irt_inst_create_event_data_table() {
	irt_instrumentation_event_data_table* table = (irt_instrumentation_event_data_table*)malloc(sizeof(irt_instrumentation_event_data_table));
	table->size = IRT_INST_WORKER_PD_BLOCKSIZE * 2;
	table->number_of_elements = 0;
	table->data = (irt_instrumentation_event_data*)malloc(sizeof(irt_instrumentation_event_data) * table->size);
	return table;
}

// frees allocated memory
void irt_inst_destroy_event_data_table(irt_instrumentation_event_data_table* table) {
	if(table != NULL) {
		if(table->data != NULL)
			free(table->data);
		free(table);
	}
}

void _irt_inst_event_insert_time(irt_worker* worker, const int event, const uint64 id, const uint64 time) {
	irt_instrumentation_event_data_table* table = worker->instrumentation_event_data;

	IRT_ASSERT(table->number_of_elements <= table->size, IRT_ERR_INSTRUMENTATION, "Instrumentation: Number of event table entries larger than table size")

	if(table->number_of_elements >= table->size)
		_irt_inst_event_data_table_resize(table);

	irt_instrumentation_event_data* pd = &(table->data[table->number_of_elements]);

	pd->timestamp = time;
	pd->event_id = event;
	pd->index = ((irt_work_item_id*)&id)->index;
	pd->thread = ((irt_work_item_id*)&id)->thread;
	++table->number_of_elements;
}


// commonly used internal function to record events and timestamps
void _irt_inst_event_insert(irt_worker* worker, const int event, const uint64 id) {
	uint64 time = irt_time_ticks();

	IRT_ASSERT(worker != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Trying to add event for worker NULL!");
	IRT_ASSERT(worker == irt_worker_get_current(), IRT_ERR_INSTRUMENTATION, "Instrumentation: Trying to insert event for different worker");

	_irt_inst_event_insert_time(worker, event, id, time);
}

// =========== private event handlers =================================

void _irt_inst_insert_wi_event(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id) {
	_irt_inst_event_insert(worker, event, subject_id.full);
}

void _irt_inst_insert_wg_event(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id) {
	_irt_inst_event_insert(worker, event, subject_id.full);
}

void _irt_inst_insert_wo_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) {
	_irt_inst_event_insert(worker, event, subject_id.full);
}

void _irt_inst_insert_di_event(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id) {
	_irt_inst_event_insert(worker, event, subject_id.full);
}

void _irt_inst_insert_db_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) {
	_irt_inst_event_insert(worker, event, subject_id.full);
}

// ================= debug output functions ==================================

#if defined (USE_OPENCL) && defined (IRT_OCL_INSTR)
    bool irt_g_ocl_temp_event_dump_already_done = false;
#endif

void irt_inst_event_data_output_single(irt_instrumentation_event_data data, FILE* outputfile, bool readable) {
	irt_work_item_id temp_id;
	temp_id.cached = NULL;
	temp_id.index = data.index;
	temp_id.thread = data.thread;
	temp_id.node = 0;
	if(readable) {
		setlocale(LC_ALL, "");
		fprintf(outputfile, "%s,%'16lu,%20s,%'32lu\n", irt_g_instrumentation_group_names[data.event_id], temp_id.full, irt_g_instrumentation_event_names[data.event_id], irt_time_convert_ticks_to_ns(data.timestamp));
	} else
		fprintf(outputfile, "%s,%lu,%s,%lu\n", irt_g_instrumentation_group_names[data.event_id], temp_id.full, irt_g_instrumentation_event_names[data.event_id], irt_time_convert_ticks_to_ns(data.timestamp));
}

void irt_inst_event_data_output_all(bool binary_format) {
	for(int i = 0; i < irt_g_worker_count; ++i)
		irt_inst_event_data_output(irt_g_workers[i], binary_format);
}

// writes csv files
void irt_inst_event_data_output(irt_worker* worker, bool binary_format) {
	// necessary for thousands separator
	//setlocale(LC_ALL, "");
	//
	
	char outputfilename[IRT_INST_OUTPUT_PATH_CHAR_SIZE];
	char defaultoutput[] = ".";
	char* outputprefix = defaultoutput;
	if(getenv(IRT_INST_OUTPUT_PATH_ENV)) outputprefix = getenv(IRT_INST_OUTPUT_PATH_ENV);

	struct stat st;
	int stat_retval = stat(outputprefix,&st);
	if(stat_retval != 0)
		mkdir(outputprefix, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

	IRT_ASSERT(stat(outputprefix,&st) == 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Error creating directory for performance log writing: %s", strerror(errno));

	sprintf(outputfilename, "%s/worker_event_log.%04u", outputprefix, worker->id.thread);

	FILE* outputfile = fopen(outputfilename, "w");
	IRT_ASSERT(outputfile != 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Unable to open file for event log writing: %s", strerror(errno));
/*	if(outputfile == 0) {
		IRT_DEBUG("Instrumentation: Unable to open file for event log writing\n");
		IRT_DEBUG_ONLY(strerror(errno));
		return;
	}*/
	irt_instrumentation_event_data_table* table = worker->instrumentation_event_data;
	IRT_ASSERT(table != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Worker has no event data!")
	//fprintf(outputfile, "INSTRUMENTATION: %10u events for worker %4u\n", table->number_of_elements, worker->id.thread);

#if defined (USE_OPENCL) && defined (IRT_OCL_INSTR)
	irt_ocl_event_table* ocl_table = worker->event_data;
	IRT_ASSERT(ocl_table != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Worker has no OpenCL event data!")
	int64 ocl_offset = 0;
	_irt_inst_ocl_performance_helper* ocl_helper_table = (_irt_inst_ocl_performance_helper*)malloc(ocl_table->num_events * 4 * sizeof(_irt_inst_ocl_performance_helper));
	int ocl_helper_table_num_entries = ocl_table->num_events * 4;
	int helper_counter = 0;

	char ocl_filename[IRT_INST_OUTPUT_PATH_CHAR_SIZE];
	sprintf(ocl_filename, "%s/ocl_event_log", outputprefix);
	FILE* opencl_logfile;

	if(irt_g_ocl_temp_event_dump_already_done)
		opencl_logfile = fopen(ocl_filename, "a");
	else {
		opencl_logfile = fopen(ocl_filename, "w");
		irt_g_ocl_temp_event_dump_already_done = true;
	}

	for(int i = 0; i < ocl_table->num_events; ++i) {
		cl_command_type retval;
		cl_int err_code = clGetEventInfo(ocl_table->event_array[i].cl_event, CL_EVENT_COMMAND_TYPE, sizeof(cl_command_type), &retval, NULL);
		IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL,"Error getting \"event command type\" info: \"%d\"", err_code);

		cl_ulong events[4];
		err_code = clGetEventProfilingInfo(ocl_table->event_array[i].cl_event, CL_PROFILING_COMMAND_QUEUED, sizeof(cl_ulong), &events[IRT_INST_OCL_QUEUED], NULL);
		err_code |= clGetEventProfilingInfo(ocl_table->event_array[i].cl_event, CL_PROFILING_COMMAND_SUBMIT, sizeof(cl_ulong), &events[IRT_INST_OCL_SUBMITTED], NULL);
		err_code |= clGetEventProfilingInfo(ocl_table->event_array[i].cl_event, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &events[IRT_INST_OCL_STARTED], NULL);
		err_code |= clGetEventProfilingInfo(ocl_table->event_array[i].cl_event, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &events[IRT_INST_OCL_FINISHED], NULL);
		IRT_ASSERT(err_code == CL_SUCCESS, IRT_ERR_OCL, "Error getting profiling info: \"%d\"",  err_code);

		// convert all ocl event information into a flat table, only the ocl_events[j] changes over this run
		for(int j = IRT_INST_OCL_QUEUED; j <= IRT_INST_OCL_FINISHED; ++j) {
			ocl_helper_table[helper_counter].workitem_id = ocl_table->event_array[i].workitem_id;
			ocl_helper_table[helper_counter].timestamp = events[j];
			ocl_helper_table[helper_counter].origin = retval;
			ocl_helper_table[helper_counter].event = j;

			fprintf(opencl_logfile, "Worker: %u %hu %hu, KN,%14lu,\t", worker->id.index, worker->id.thread, worker->id.node, ocl_helper_table[helper_counter].workitem_id);
			switch(ocl_helper_table[helper_counter].origin) {
				case CL_COMMAND_NDRANGE_KERNEL:
					fprintf(opencl_logfile, "ND_");
					break;
				case CL_COMMAND_WRITE_BUFFER:
					fprintf(opencl_logfile, "WRITE_");
					break;
				case CL_COMMAND_READ_BUFFER:
					fprintf(opencl_logfile, "READ_");
					break;
				default:
					fprintf(opencl_logfile, "UNKNOWN_");
			}
			switch(ocl_helper_table[helper_counter].event) {
				case IRT_INST_OCL_QUEUED:
					   fprintf(opencl_logfile, "QUEUED");
					   break;
				case IRT_INST_OCL_SUBMITTED:
					   fprintf(opencl_logfile, "SUBMITTED");
					   break;
				case IRT_INST_OCL_STARTED:
					   fprintf(opencl_logfile, "STARTED");
					   break;
				case IRT_INST_OCL_FINISHED:
					   fprintf(opencl_logfile, "FINISHED");
					   break;
				default:
					   fprintf(opencl_logfile, "UNNKOWN");
					   break;
		}

		fprintf(opencl_logfile, ",\t%18lu\n", ocl_helper_table[helper_counter].timestamp);
		helper_counter++;
		}
	}

	fclose(opencl_logfile);

	IRT_ASSERT(ocl_helper_table_num_entries == helper_counter, IRT_ERR_INSTRUMENTATION, "OCL event counts do not match: helper_counter: %d, table_entries: %d", helper_counter, ocl_helper_table_num_entries);
	int ocl_helper_table_counter = 0;
#endif


	if(binary_format) {

		/*
		 * dump everything in binary according to the following format:
		 * (note: the strings are written without the termination character '\0'!)
		 *
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
		 */

		// write version
		const char* header = "INSIEME1";
		fprintf(outputfile, "%s", header);

		// write number of event name table entries followed by group and event names
		fwrite(&(irt_g_inst_num_event_types), sizeof(uint32), 1, outputfile);
		for(int i = 0; i < irt_g_inst_num_event_types; ++i) {
			fprintf(outputfile, "%-4s", irt_g_instrumentation_group_names[i]);
			fprintf(outputfile, "%-60s", irt_g_instrumentation_event_names[i]);
		}

		// write number of events
		const uint64 temp_num_of_elements = (uint64)(table->number_of_elements);
		fwrite(&temp_num_of_elements, sizeof(uint64), 1, outputfile);

		// write event table
		fwrite(table->data, sizeof(irt_instrumentation_event_data), table->number_of_elements, outputfile);

	} else {
		for(int i = 0; i < table->number_of_elements; ++i) {
			irt_inst_event_data_output_single(table->data[i], outputfile, false);
		}
	}

	fclose(outputfile);
#if defined (USE_OPENCL) && defined (IRT_OCL_INSTR)
	free(ocl_helper_table);
#endif
}

// ================= instrumentation function pointer toggle functions =======================

void irt_inst_set_wi_instrumentation(bool enable) {
	if(enable)
		irt_inst_insert_wi_event = &_irt_inst_insert_wi_event;
	else 
		irt_inst_insert_wi_event = &_irt_inst_insert_no_wi_event;
}

void irt_inst_set_wg_instrumentation(bool enable) {
	if(enable)
		irt_inst_insert_wg_event = &_irt_inst_insert_wg_event;
	else
		irt_inst_insert_wg_event = &_irt_inst_insert_no_wg_event;
}

void irt_inst_set_wo_instrumentation(bool enable) {
	if(enable)
		irt_inst_insert_wo_event = &_irt_inst_insert_wo_event;
	else
		irt_inst_insert_wo_event = &_irt_inst_insert_no_wo_event;
}

void irt_inst_set_di_instrumentation(bool enable) {
	if(enable)
		irt_inst_insert_di_event = &_irt_inst_insert_di_event;
	else
		irt_inst_insert_di_event = &_irt_inst_insert_no_di_event;
}

void irt_inst_set_db_instrumentation(bool enable) {
	if(enable)
		irt_inst_insert_db_event = &_irt_inst_insert_db_event;
	else
		irt_inst_insert_db_event = &_irt_inst_insert_no_db_event;
}

void irt_inst_set_all_instrumentation(bool enable) {
	irt_inst_set_wi_instrumentation(enable);
	irt_inst_set_wg_instrumentation(enable);
	irt_inst_set_wo_instrumentation(enable);
	irt_inst_set_di_instrumentation(enable);
	irt_inst_set_db_instrumentation(enable);
	irt_g_instrumentation_event_output_is_enabled = enable;
}

void irt_inst_set_all_instrumentation_from_env() {
	// set whether worker event logging is enabled, and if so, what event types will be logged
	if (getenv(IRT_INST_WORKER_EVENT_LOGGING_ENV) && strcmp(getenv(IRT_INST_WORKER_EVENT_LOGGING_ENV), "true") == 0) {
		irt_log_setting_s(IRT_INST_WORKER_EVENT_LOGGING_ENV, "enabled");
		
		// set whether binary format is enabled
		if(getenv(IRT_INST_BINARY_OUTPUT_ENV) && (strcmp(getenv(IRT_INST_BINARY_OUTPUT_ENV), "true") == 0)) {
			irt_g_instrumentation_event_output_is_binary = true;
			irt_log_setting_s(IRT_INST_BINARY_OUTPUT_ENV, "enabled");
		} else {
			irt_g_instrumentation_event_output_is_binary = false;
			irt_log_setting_s(IRT_INST_BINARY_OUTPUT_ENV, "disabled");
		}

		char* types = getenv(IRT_INST_WORKER_EVENT_TYPES_ENV);
		if(!types || strcmp(types, "") == 0) {
			irt_inst_set_all_instrumentation(true);
			irt_inst_set_db_instrumentation(false);
			irt_log_setting_s(IRT_INST_WORKER_EVENT_TYPES_ENV, "WI,WO,WG,DI");
			return;
		}

		char* tok = strtok(types, ",");
		char log_output[128];
		uint32 log_output_counter = 0;

		do {
			if(strcmp(tok, "WI") == 0) {
				irt_inst_set_wi_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "WI,");
			} else if(strcmp(tok, "WO") == 0) {
				irt_inst_set_wo_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "WO,");
			} else if(strcmp(tok, "WG") == 0) {
				irt_inst_set_wg_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "WG,");
			} else if(strcmp(tok, "DI") == 0) {
				irt_inst_set_di_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "DI,");
			} else if(strcmp(tok, "DB") == 0) {
				irt_inst_set_db_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "DB,");
			}
		} while((tok = strtok(NULL, ",")) != NULL);
		log_output[log_output_counter-1] = '\0';  // remove the last comma and replace with termination symbol
		irt_log_setting_s(IRT_INST_WORKER_EVENT_TYPES_ENV, log_output);
		return;
	}
	irt_inst_set_all_instrumentation(false);
	irt_log_setting_s(IRT_INST_WORKER_EVENT_LOGGING_ENV, "disabled");
}

#else // if not IRT_ENABLE_INSTRUMENTATION

// ============ to be used if IRT_ENABLE_INSTRUMENTATION is not set ==============

irt_instrumentation_event_data_table* irt_inst_create_event_data_table() { return NULL; }
void irt_inst_destroy_event_data_table(irt_instrumentation_event_data_table* table) {}

void irt_inst_insert_wi_event(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id) {}
void irt_inst_insert_wg_event(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id) {}
void irt_inst_insert_wo_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) {}
void irt_inst_insert_di_event(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id) {}
void irt_inst_insert_db_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) {}

void irt_inst_event_data_output(irt_worker* worker, bool binary_format) {}

#endif // IRT_ENABLE_INSTRUMENTATION

// --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//
//																				Regions
//
// --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#ifdef IRT_ENABLE_REGION_INSTRUMENTATION

// make sure scheduling policy is fixed to static
#if !(IRT_SCHED_POLICY == IRT_SCHED_POLICY_STATIC)
	#error "IRT INSTRUMENTATION ONLY SUPPORTS STATIC SCHEDULING AT THIS POINT"
#endif

void _irt_inst_region_stack_push(irt_work_item* wi, irt_inst_region_context_data* region) {
	irt_inst_region_list* list = wi->inst_region_list;

	IRT_ASSERT(list->length <= list->size, IRT_ERR_INSTRUMENTATION, "WI region stack overflow, size %lu length %lu", list->size, list->length)

	if(list->length == list->size) {
		list->size *= 2;
		list->items = (irt_inst_region_context_data**)realloc(list->items, list->size * sizeof(irt_inst_region_context_data*));
	}

	list->items[list->length++] = region;
}

irt_inst_region_context_data* _irt_inst_region_stack_pop(irt_work_item* wi) {
	irt_inst_region_list* list = wi->inst_region_list;

	IRT_ASSERT(list->length > 0, IRT_ERR_INSTRUMENTATION, "Tried to remove a region from a WI that has no region!")

	irt_inst_region_context_data* retval = list->items[list->length-1];
	// remove cur from stack
	list->items[--list->length] = NULL;
	return retval;
}

void _irt_inst_region_start_early_entry_measurements(irt_work_item* wi) {
	irt_inst_region_context_data* rg = irt_inst_region_get_current(wi);
	#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	if(irt_g_inst_region_metric_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */ \
		_region_early_start_code__; \
	}
#include "irt_metrics.def"
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	if(irt_g_inst_region_metric_measure_##_name__) { \
		_region_early_start_code__; \
	}
#include "irt_metrics.def"
}

void _irt_inst_region_end_late_exit_measurements(irt_work_item* wi) {
	irt_inst_region_list* list = wi->inst_region_list;
	uint64 length = list->length;
	IRT_ASSERT(length > 0, IRT_ERR_INSTRUMENTATION, "Tried to get region data from a WI that has no region data")
	irt_inst_region_context_data* rg = list->items[length-1];
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	_data_type__ old_aggregated_##_name__ = rg->aggregated_##_name__;
#include "irt_metrics.def"
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	if(irt_g_inst_region_metric_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */ \
		_region_late_end_code__; \
	}
#include "irt_metrics.def"
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	if(irt_g_inst_region_metric_measure_##_name__) { \
		_region_late_end_code__; \
		rg->last_##_name__ = 0; \
	}
#include "irt_metrics.def"
	for(uint64 i = 0; i < length - 1; ++i) {
		irt_inst_region_context_data* current_region = list->items[i];
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		/* only propagate metrics that were touched by late exit measurements (i.e. only EE/LE metrics) */ \
		if(rg->aggregated_##_name__ != old_aggregated_##_name__)	\
			current_region->aggregated_##_name__ += rg->aggregated_##_name__ - old_aggregated_##_name__;
#include "irt_metrics.def"
	}
}

void _irt_inst_region_metrics_init(irt_context* context) {
	// initialize IDs
	int metric_id = 0;
	int group_id = 0;
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	irt_g_region_metric_##_name__##_id = metric_id++;
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	irt_g_region_metric_group_##_name__##_id = group_id++;
#include "irt_metrics.def"
	irt_g_inst_region_metric_count = metric_id;
	irt_g_inst_region_metric_group_count = group_id;
	// initialize groups
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	_init_code__;
#include "irt_metrics.def"
}

void _irt_inst_region_metrics_finalize(irt_context* context) {
	// finalize groups
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	_finalize_code__;
#include "irt_metrics.def"
}

void irt_inst_region_list_copy(irt_work_item* destination, irt_work_item* source) {
	irt_inst_region_list* src = source->inst_region_list;
	if(src->length > 0) {
		irt_inst_region_list* dst = destination->inst_region_list;

		if(dst->size < src->size) {
			dst->size = src->size;
			dst->items = (irt_inst_region_context_data**)realloc(dst->items, sizeof(irt_inst_region_context_data*)*dst->size);
		}

		dst->length = src->length;
		memcpy(dst->items, src->items, sizeof(irt_inst_region_context_data*)*dst->length);
	}
}

void irt_inst_region_wi_init(irt_work_item* wi) {
	wi->inst_region_list = (irt_inst_region_list*)malloc(sizeof(irt_inst_region_list));
	wi->inst_region_list->size = 8;
	wi->inst_region_list->length = 0;
	wi->inst_region_list->items = (irt_inst_region_context_data**)malloc(sizeof(irt_inst_region_context_data*)*wi->inst_region_list->size);
	wi->inst_region_data = (irt_inst_region_wi_data*)malloc(sizeof(irt_inst_region_wi_data));
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	wi->inst_region_data->last_##_name__ = 0; \
	wi->inst_region_data->aggregated_##_name__ = 0;
#include "irt_metrics.def"
}

void irt_inst_region_wi_finalize(irt_work_item* wi) {
	free(wi->inst_region_list->items);
	free(wi->inst_region_list);
}

void irt_inst_region_init(irt_context* context) {
	context->inst_region_data = (irt_inst_region_context_data*)malloc(context->num_regions * sizeof(irt_inst_region_context_data));
	for(uint32 i = 0; i < context->num_regions; ++i) {
		context->inst_region_data[i].id	 = i;
		context->inst_region_data[i].num_executions = 0;
		context->inst_region_data[i].num_entries = 0;
		context->inst_region_data[i].num_exits = 0;
		irt_spin_init(&context->inst_region_data[i].lock);
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		context->inst_region_data[i].last_##_name__ = 0; \
		context->inst_region_data[i].aggregated_##_name__ = 0;
#include "irt_metrics.def"
	}
	_irt_inst_region_metrics_init(context);
	irt_inst_region_select_metrics_from_env();
}

void irt_inst_region_finalize(irt_context* context) {
	_irt_inst_region_metrics_finalize(context);
	irt_time_ticks_per_sec_calibration_mark(); // needs to be done before any time instrumentation processing!
	irt_inst_region_output();
	//irt_inst_region_debug_output();
	for(uint32 i = 0; i < context->num_regions; ++i) {
		irt_spin_destroy(&context->inst_region_data[i].lock);
	}
	free(context->inst_region_data);
}

void irt_inst_region_propagate_data_from_wi_to_regions(irt_work_item* wi) {
	irt_inst_region_list* list = wi->inst_region_list;

	for(uint64 i = 0; i < list->length; ++i) {
		irt_inst_region_context_data* cur_region = list->items[i];

		IRT_ASSERT(cur_region, IRT_ERR_INSTRUMENTATION, "Tried to get region data from a WI that has no region data")

		irt_spin_lock(&(cur_region->lock));
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		switch(_aggregation__) { \
		case IRT_METRIC_AGGREGATOR_NONE: \
			/* special case e.g. for region-only instrumentation that do not require WI measurements or aggregation */ \
			break; \
		case IRT_METRIC_AGGREGATOR_AVG: \
			cur_region->aggregated_##_name__ = (cur_region->aggregated_##_name__ * cur_region->num_executions + wi->inst_region_data->aggregated_##_name__) / (cur_region->num_executions + 1); \
			break; \
		case IRT_METRIC_AGGREGATOR_SUM: \
		default: \
			cur_region->aggregated_##_name__ += wi->inst_region_data->aggregated_##_name__; \
		}
#include "irt_metrics.def"
		irt_spin_unlock(&(cur_region->lock));
	}
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		wi->inst_region_data->aggregated_##_name__ = 0;
#include "irt_metrics.def"
}

void irt_inst_region_start_measurements(irt_work_item* wi) {
	if(wi->inst_region_list->length <= 0)
		return;

	irt_context* context = irt_context_table_lookup(wi->context_id);

#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	if(irt_g_inst_region_metric_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */ \
		_wi_start_code__; \
	}
#include "irt_metrics.def"
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	if(wi->inst_region_data->last_##_name__ != 0) {	\
		printf("Instrumentation: Encountered start measurements for region %lu but last_" #_name__ " was not zero\n", irt_inst_region_get_current(wi)->id);\
		exit(1); \
	} \
	IRT_ASSERT(wi->inst_region_data->last_##_name__ == 0, IRT_ERR_INSTRUMENTATION, "Started measurement, but previous measurement still in progress (last_%s was not 0)", #_name__) \
	if(irt_g_inst_region_metric_measure_##_name__) { \
		_wi_start_code__; \
	}
#include "irt_metrics.def"
}

void irt_inst_region_end_measurements(irt_work_item* wi) {
	if(wi->inst_region_list->length <= 0)
		return;

	irt_context* context = irt_context_table_lookup(wi->context_id);

#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	if(irt_g_inst_region_metric_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */ \
		wi_end_code__; \
	}
#include "irt_metrics.def"
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
	if(irt_g_inst_region_metric_measure_##_name__) { \
		wi_end_code__; \
		wi->inst_region_data->last_##_name__ = 0; \
	}
#include "irt_metrics.def"
}

// start a region and push it to the region stack of the WI
void irt_inst_region_start(const irt_inst_region_id id) {

	irt_work_item* wi = irt_wi_get_current();
	irt_context* context = irt_context_table_lookup(wi->context_id);
	irt_inst_region_context_data* outer_region = irt_inst_region_get_current(wi);

	IRT_ASSERT(id >= 0 && id < context->num_regions, IRT_ERR_INSTRUMENTATION, "Start of region id %lu requested, but only %u region(s) present", id, context->num_regions)
	irt_inst_region_context_data* inner_region = &(context->inst_region_data[id]);

	uint64 inner_entry_count = irt_atomic_fetch_and_add(&(inner_region->num_entries), 1);
	bool inner_first_entry = ((inner_entry_count - inner_region->num_exits) == 0);

	if(outer_region) {
		IRT_ASSERT(outer_region != inner_region, IRT_ERR_INSTRUMENTATION, "Region %u start encountered, but this region was already started", id)
		irt_inst_region_end_measurements(wi);
		irt_inst_region_propagate_data_from_wi_to_regions(wi);

		if(inner_first_entry)
			_irt_inst_region_end_late_exit_measurements(wi);
	}

	_irt_inst_region_stack_push(wi, inner_region);
	irt_inst_region_start_measurements(wi);

	if(inner_first_entry)
		_irt_inst_region_start_early_entry_measurements(wi);
}

// stop a region and remove it from the region stack of the WI
void irt_inst_region_end(const irt_inst_region_id id) {

	irt_work_item* wi = irt_wi_get_current();
	irt_context* context = irt_context_table_lookup(wi->context_id);
	irt_inst_region_context_data* inner_region = irt_inst_region_get_current(wi);
	
	IRT_ASSERT(id >= 0 && id < context->num_regions, IRT_ERR_INSTRUMENTATION, "End of region id %lu requested, but only %u region(s) present", id, context->num_regions)
	IRT_ASSERT(inner_region, IRT_ERR_INSTRUMENTATION, "Region end occurred while no region was started")
	IRT_ASSERT(inner_region->id == id, IRT_ERR_INSTRUMENTATION, "Region end id %lu did not match currently open region id %lu", id, inner_region->id)
	
	bool inner_last_exit = false;

	irt_inst_region_end_measurements(wi);
	irt_inst_region_propagate_data_from_wi_to_regions(wi);

	uint32 wg_count = wi->num_groups>0?irt_wi_get_wg_size(wi, 0):1;

	if(inner_region->num_entries - irt_atomic_add_and_fetch(&(inner_region->num_exits), 1) == 0) {
		_irt_inst_region_end_late_exit_measurements(wi);
		inner_last_exit = true;
		irt_atomic_fetch_and_sub(&(inner_region->num_entries), wg_count);
		irt_atomic_fetch_and_sub(&(inner_region->num_exits), wg_count);
	}

	// only increase count if wi is not member in any work group or has wg id == 0
	if(wi->num_groups == 0 || (wi->num_groups > 0 && wi->wg_memberships[0].num == 0))
		inner_region->num_executions++;

	_irt_inst_region_stack_pop(wi);

	// if there is a region left open, continue old measurements
	if(wi->inst_region_list->length > 0) {
		irt_inst_region_start_measurements(wi);
		if(inner_last_exit)
			_irt_inst_region_start_early_entry_measurements(wi);
	}
}

// selectively enable region instrumentation metrics - NOTE: a NULL pointer as an argument will enable all metrics!
void irt_inst_region_select_metrics(const char* selection) {
	char enabled_types[4096];
	uint16 enabled_types_counter = 0;

	if(!selection || strcmp(selection, "") == 0) {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		irt_g_inst_region_metric_measure_##_name__ = true; \
		irt_g_inst_region_metric_group_##_group__##membership_count++; \
		enabled_types_counter += sprintf(&(enabled_types[enabled_types_counter]), "%s,", #_name__);
#include "irt_metrics.def"
	} else {
		// need to copy string since strtok requires it to be non-const
		char selection_copy[strlen(selection)];
		strcpy(selection_copy, selection);
		// tokenize
		char* tok = strtok(selection_copy, ",");

		do {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
			if(strcmp(tok, #_name__) == 0) { \
				irt_g_inst_region_metric_measure_##_name__ = true; \
				irt_g_inst_region_metric_group_##_group__##membership_count++; \
				enabled_types_counter += sprintf(&(enabled_types[enabled_types_counter]), "%s,", #_name__); \
			}
#include "irt_metrics.def"
		} while((tok = strtok(NULL, ",")) != NULL);
	}

	// remove the last comma and replace with termination symbol
	enabled_types[enabled_types_counter-1] = '\0';
	irt_log_setting_s(IRT_INST_REGION_INSTRUMENTATION_TYPES_ENV, enabled_types);
}

void irt_inst_region_select_metrics_from_env() {
	if (getenv(IRT_INST_REGION_INSTRUMENTATION_ENV) && strcmp(getenv(IRT_INST_REGION_INSTRUMENTATION_ENV), "true") == 0) {
		irt_log_setting_s(IRT_INST_REGION_INSTRUMENTATION_ENV, "enabled");

		char* metrics = getenv(IRT_INST_REGION_INSTRUMENTATION_TYPES_ENV);
		irt_inst_region_select_metrics(metrics);
	} else {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		irt_g_inst_region_metric_measure_##_name__ = false; \
		irt_g_inst_region_metric_group_##_group__##membership_count = 0;
#include "irt_metrics.def"
		irt_log_setting_s(IRT_INST_REGION_INSTRUMENTATION_ENV, "disabled");
	}
}

void irt_inst_region_debug_output() {
	irt_context* context = irt_context_get_current();
	uint32 num_regions = context->num_regions;
	printf("%u region(s):\n", num_regions);
	for(uint32 i = 0; i < num_regions; ++i) {
		irt_inst_region_context_data* rg = &context->inst_region_data[i];
		printf("region %u\n", i);
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		if(irt_g_inst_region_metric_measure_##_name__) { \
			printf("  aggregated_" #_name__ ":\t" _format_string__ ", last_" #_name__ ":\t" _format_string__,  (_data_type__)((double)rg->aggregated_##_name__ * _output_conversion_code__), rg->last_##_name__); \
			if(rg->last_##_name__ != 0) \
				printf(", warning, field last_" #_name__ " was not 0"); \
			printf("\n"); \
		}
#include "irt_metrics.def"
	}
}

void irt_inst_region_output() {
	char outputfilename[IRT_INST_OUTPUT_PATH_CHAR_SIZE];
	char defaultoutput[] = ".";
	char* outputprefix = defaultoutput;
	if(getenv(IRT_INST_OUTPUT_PATH_ENV)) outputprefix = getenv(IRT_INST_OUTPUT_PATH_ENV);

	struct stat st;
	int stat_retval = stat(outputprefix,&st);
	if(stat_retval != 0)
		mkdir(outputprefix, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

	IRT_ASSERT(stat(outputprefix,&st) == 0, IRT_ERR_INSTRUMENTATION, "Error using directory %s for region data: %s", outputprefix, strerror(errno));

	sprintf(outputfilename, "%s/worker_efficiency_log", outputprefix);

	FILE* outputfile = fopen(outputfilename, "w");
	IRT_ASSERT(outputfile != 0, IRT_ERR_INSTRUMENTATION, "Unable to open region data file for writing: %s", strerror(errno));

	irt_context* context = irt_context_get_current();
	uint32 num_regions = context->num_regions;
	irt_inst_region_context_data* regions = context->inst_region_data;

	// write header
	fprintf(outputfile, "#subject,id,num_executions(unit)");
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		if(irt_g_inst_region_metric_measure_##_name__) { \
			fprintf(outputfile, "," #_name__ "(%s)", #_unit__); \
		}
#include "irt_metrics.def"
	fprintf(outputfile, "\n");

	// write data
	for(uint32 i = 0; i < num_regions; ++i) {
		fprintf(outputfile, "RG,%u,%lu", i, regions[i].num_executions);
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__, _output_conversion_code__) \
		if(irt_g_inst_region_metric_measure_##_name__) { \
			fprintf(outputfile, "," _format_string__, (_data_type__)((double)regions[i].aggregated_##_name__ * _output_conversion_code__)); \
		}
#include "irt_metrics.def"
		fprintf(outputfile, "\n");
	}
	fclose(outputfile);
}

inline irt_inst_region_context_data* irt_inst_region_get_current(irt_work_item* wi) {
	if(wi->inst_region_list->length < 1)
		return NULL;
	return wi->inst_region_list->items[wi->inst_region_list->length-1];
}

// stop a region, do not remove it from the region stack of the WI
//void irt_inst_region_suspend(irt_work_item* wi) {
//	irt_inst_region_end_measurements(wi);
//	irt_inst_region_propagate_data_from_wi_to_regions(wi);
//}
//
//// resume the current region
//void irt_inst_region_continue(irt_work_item* wi) {
//	// resume the current region (since it was not removed by the last suspend)
//	irt_inst_region_list list = wi->inst_region_list;
//	if(list.length > 0)
//		irt_inst_region_start(wi, list.items[list.length-1]);
//}

#else  // if not IRT_ENABLE_REGION_INSTRUMENTATION

void irt_inst_region_list_copy(irt_work_item* destination, irt_work_item* source) { }
void irt_inst_region_wi_init(irt_work_item* wi) { }
void irt_inst_region_wi_finalize(irt_work_item* wi) { }
void irt_inst_region_init(irt_context* context) { }
void irt_inst_region_finalize(irt_context* context) { }
void irt_inst_region_propagate_data_from_wi_to_regions(irt_work_item* wi) { }
void irt_inst_region_start_measurements(irt_work_item* wi) { }
void irt_inst_region_end_measurements(irt_work_item* wi) { }
void irt_inst_region_start(irt_inst_region_id id) { }
void irt_inst_region_end(irt_inst_region_id id) { }
void irt_inst_region_select_metrics(const char* selection) { }
void irt_inst_region_select_metrics_from_env() { }
void irt_inst_region_debug_output() { }
void irt_inst_region_output() { }
irt_inst_region_context_data* irt_inst_region_get_current(irt_work_item* wi) { return NULL; }

#endif //IRT_ENABLE_REGION_INSTRUMENTATION
