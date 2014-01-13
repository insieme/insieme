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
#include "irt_instrumentation_includes.h"

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
	int ocl_helper_table_number_of_entries = ocl_table->num_events * 4;
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

	IRT_ASSERT(ocl_helper_table_number_of_entries == helper_counter, IRT_ERR_INSTRUMENTATION, "OCL event counts do not match: helper_counter: %d, table_entries: %d", helper_counter, ocl_helper_table_number_of_entries);
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
		irt_log_setting_s("IRT_INST_WORKER_EVENT_LOGGING", "enabled");
		
		// set whether binary format is enabled
		if(getenv(IRT_INST_BINARY_OUTPUT_ENV) && (strcmp(getenv(IRT_INST_BINARY_OUTPUT_ENV), "true") == 0)) {
			irt_g_instrumentation_event_output_is_binary = true;
			irt_log_setting_s("IRT_INST_BINARY_OUTPUT", "enabled");
		} else {
			irt_g_instrumentation_event_output_is_binary = false;
			irt_log_setting_s("IRT_INST_BINARY_OUTPUT", "disabled");
		}

		char* types = getenv(IRT_INST_WORKER_EVENT_TYPES_ENV);
		if(!types || strcmp(types, "") == 0) {
			irt_inst_set_all_instrumentation(true);
			irt_inst_set_db_instrumentation(false);
			irt_log_setting_s("IRT_INST_WORKER_EVENT_TYPES", "WI,WO,WG,DI");
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
		irt_log_setting_s("IRT_INST_WORKER_EVENT_TYPES", log_output);
		return;
	}
	irt_inst_set_all_instrumentation(false);
	irt_log_setting_s("IRT_INST_WORKER_EVENT_LOGGING", "disabled");
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

void irt_inst_metrics_init() {
	// initialize IDs
	int metric_id = 0;
	int group_id = 0;
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	irt_g_metric_##_name__##_id = metric_id++;
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	irt_g_metric_group_##_name__##_id = group_id++;
#include "irt_metrics.def"
	irt_g_inst_metric_count = metric_id;
	irt_g_inst_group_count = group_id;
	// initialize groups
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	_init_code__;
#include "irt_metrics.def"
}

void irt_inst_metrics_finalize() {
	// finalize groups
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
_finalize_code__;
#include "irt_metrics.def"
}

// selectively enable region instrumentation metrics - NOTE: a NULL pointer as an argument will enable all metrics!
void irt_inst_select_region_instrumentation_metrics(const char* selection) {
	char enabled_types[4096];
	uint16 enabled_types_counter = 0;

	if(!selection || strcmp(selection, "") == 0) {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
		irt_g_inst_measure_##_name__ = true; \
		irt_g_inst_group_##_group__##membership_count++; \
		enabled_types_counter += sprintf(&(enabled_types[enabled_types_counter]), "%s,", #_name__);
#include "irt_metrics.def"
	} else {
		// need to copy string since strtok requires it to be non-const
		char selection_copy[strlen(selection)];
		strcpy(selection_copy, selection);
		// tokenize
		char* tok = strtok(selection_copy, ",");
		char log_output[128];
		uint32 log_output_counter = 0;

		do {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
			if(strcmp(tok, #_name__) == 0) { \
				irt_g_inst_measure_##_name__ = true; \
				irt_g_inst_group_##_group__##membership_count++; \
				enabled_types_counter += sprintf(&(enabled_types[enabled_types_counter]), "%s,", #_name__); \
			}
#include "irt_metrics.def"
		} while((tok = strtok(NULL, ",")) != NULL);
	}

	// remove the last comma and replace with termination symbol
	enabled_types[enabled_types_counter-1] = '\0';
	irt_log_setting_s("IRT_INST_REGION_INSTRUMENTATION_TYPES", enabled_types);
}

void irt_inst_set_region_instrumentation_from_env() {
	if (getenv(IRT_INST_REGION_INSTRUMENTATION_ENV) && strcmp(getenv(IRT_INST_REGION_INSTRUMENTATION_ENV), "true") == 0) {
		irt_log_setting_s("IRT_INST_REGION_INSTRUMENTATION", "enabled");

		char* metrics = getenv(IRT_INST_REGION_INSTRUMENTATION_TYPES_ENV);
		irt_inst_select_region_instrumentation_metrics(metrics);
	} else {
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
		irt_g_inst_measure_##_name__ = false; \
		irt_g_inst_group_##_group__##membership_count = 0;
#include "irt_metrics.def"
		irt_log_setting_s("IRT_INST_REGION_INSTRUMENTATION", "disabled");
	}
}

inline irt_inst_region_struct* irt_inst_region_get_current(irt_work_item* wi) {
	return wi->inst_region_list->items[wi->inst_region_list->length-1];
}

void irt_inst_propagate_data_from_cur_region_to_parent(irt_work_item* wi) {
	irt_inst_region_list* list = wi->inst_region_list;
	// if there is a parent region, add cur values to it (i.e. inclusive measurements, not exclusive)
//	printf("propagate region to parent: current wi: %lu, current region: %p, region length: %lu\t\tcpu_time: %lu\n", wi->id.full, irt_inst_region_get_current(wi), list->length, irt_inst_region_get_current(wi)->aggregated_cpu_time);
	if(list->length > 1) {
//		printf("more than one region\n");
		irt_inst_region_struct* cur = list->items[list->length-1];
		irt_inst_region_struct* parent = list->items[list->length-2];
		irt_spin_lock(&(parent->lock));
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
		parent->aggregated_##_name__ += cur->aggregated_##_name__; \
//		cur->aggregated_##_name__ = 0;
#include "irt_metrics.def"
		irt_spin_unlock(&(parent->lock));
	}
}

void irt_inst_propagate_data_from_wi_to_cur_region(irt_work_item* wi) {

//	printf("propagating wi to cur region\n");
//	printf("propagate wi to region: current wi: %lu, current region: %p, region length: %lu\t\tcpu time: %lu\n", wi->id.full, irt_inst_region_get_current(wi), wi->inst_region_list->length, irt_inst_region_get_current(wi)->aggregated_cpu_time);


	IRT_ASSERT(wi->inst_region_list->length > 0, IRT_ERR_INSTRUMENTATION, "Tried to get region data from a WI that has no region data")

//	if(wi->inst_region_list->length < 1)
//		return;

	irt_inst_region_struct* cur_region = irt_inst_region_get_current(wi);

	IRT_ASSERT(cur_region != NULL, IRT_ERR_INSTRUMENTATION, "Region pointer of a WI is NULL");

	irt_spin_lock(&(cur_region->lock));
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	switch(_aggregation__) { \
	case IRT_METRIC_AGGREGATOR_NONE: \
		/* special case e.g. for region-only instrumentation that do not require WI measurements or aggregation */ \
		break; \
	case IRT_METRIC_AGGREGATOR_AVG: \
		/*printf("%s: cur region aggregated: %lu, cur region num exec: %lu, wi aggregated: %lu\n", #_name__, cur_region->aggregated_##_name__, cur_region->num_executions,  wi->inst_data->aggregated_##_name__);*/\
		cur_region->aggregated_##_name__ = (cur_region->aggregated_##_name__ * cur_region->num_executions + wi->inst_data->aggregated_##_name__) / (cur_region->num_executions + 1); \
		break; \
	case IRT_METRIC_AGGREGATOR_SUM: \
	default: \
		cur_region->aggregated_##_name__ += wi->inst_data->aggregated_##_name__; \
	} \
	wi->inst_data->aggregated_##_name__ = 0;
#include "irt_metrics.def"
	irt_spin_unlock(&(cur_region->lock));
}

void _irt_inst_region_stack_push(irt_work_item* wi, irt_inst_region_struct* region) {
	irt_inst_region_list* list = wi->inst_region_list;

	IRT_ASSERT(list->length <= list->size, IRT_ERR_INSTRUMENTATION, "WI region stack overflow, size %u length %u", list->size, list->length)

	if(list->length == list->size) {
		list->size *= 2;
		list->items = (irt_inst_region_struct**)realloc(list->items, list->size * sizeof(irt_inst_region_struct*));
	}

	list->items[list->length++] = region;

//	printf("after push length: %u %lu %p\n", list->length, wi->id, wi);
}

void _irt_inst_region_stack_pop(irt_work_item* wi) {
	irt_inst_region_list* list = wi->inst_region_list;

	IRT_ASSERT(list->length > 0, IRT_ERR_INSTRUMENTATION, "Tried to remove a region from a WI that has no region!")

	irt_inst_propagate_data_from_cur_region_to_parent(wi);

	// remove cur from stack
	list->items[--list->length] = NULL;

//	printf("after pop length: %u %lu %p\n", list->length, wi->id, wi);
}

void irt_inst_region_start_measurements(irt_work_item* wi) {
	if(wi->inst_region_list->length <= 0)
		return;

	bool early_entry = false;
//	printf("starting measurements\n");
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	if(irt_g_inst_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */ \
		early_entry = irt_atomic_bool_compare_and_swap(&(irt_inst_region_get_current(wi)->first_entrance_flag), 0, 1); \
		if(early_entry) { /* only do EE code upon first entry */\
			printf("WI %lu is the early entry WI\n", wi->id.full);\
			if(wi->num_groups > 0) { \
				printf("### WI: %lu, setting member count to %lu\n", wi->id.full, irt_wi_get_wg_size(wi, 0)); \
				irt_inst_region_get_current(wi)->remaining_exits = irt_wi_get_wg_size(wi, 0); \
			} else {\
				irt_inst_region_get_current(wi)->remaining_exits = 1;\
			} \
			_region_early_start_code__; \
		} \
		_wi_start_code__; \
	}
#include "irt_metrics.def"
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	if(wi->inst_data->last_##_name__ != 0) {	\
		printf("THIS MUST NOT HAPPEN");\
		exit(1); \
	} \
	IRT_ASSERT(wi->inst_data->last_##_name__ == 0, IRT_ERR_INSTRUMENTATION, "Started measurement, but previous measurement still in progress (last_%s was not 0)", #_name__) \
	if(irt_g_inst_measure_##_name__) { \
		if(early_entry) { /* only do EE code upon first entry */ \
			_region_early_start_code__; \
		} \
		_wi_start_code__; \
	}
#include "irt_metrics.def"
//	printf("start measurement: current wi: %lu, current region: %p, region length: %lu\t\tregion last wall_time %lu wi last wall time: %lu\n", wi->id.full, irt_inst_region_get_current(wi), wi->inst_region_list->length, irt_inst_region_get_current(wi)->last_wall_time, wi->inst_data->last_wall_time);
	printf("start:\tWI: %3lu, RG: %3lu, RG_LEN: %3lu, rg last walltime: %lu, wi last walltime: %lu\n", wi->id.full, irt_inst_region_get_current(wi)->id, wi->inst_region_list->length, irt_inst_region_get_current(wi)->last_wall_time, wi->inst_data->last_wall_time);
}

void irt_inst_region_end_measurements(irt_work_item* wi) {
	if(wi->inst_region_list->length <= 0)
		return;

	bool late_exit = false;
#define GROUP(_name__, _var_decls__, _init_code__, _finalize_code__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	if(irt_g_inst_group_##_name__##membership_count > 0) { /* only count if enabled dynamically (e.g. via env var) */ \
		late_exit = (irt_atomic_sub_and_fetch(&(irt_inst_region_get_current(wi)->remaining_exits), 1) <= 0); \
		printf("### WI: %lu, late exit %s\n", wi->id.full, late_exit?"true":"false"); \
		if(late_exit) { \
			_region_late_end_code__; \
			irt_inst_region_get_current(wi)->first_entrance_flag = false; \
		} \
		wi_end_code__; \
	}
#include "irt_metrics.def"
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	if(irt_g_inst_measure_##_name__) { \
		if(late_exit) { /* only do LE code upon last exit */ \
			_region_late_end_code__; \
			irt_inst_region_get_current(wi)->last_##_name__ = 0; \
		} \
		wi_end_code__; \
		wi->inst_data->last_##_name__ = 0; \
	}
#include "irt_metrics.def"
//	printf("end measurement: current wi: %lu, current region: %p, region length: %lu\t\tregion aggregated wall_time %lu wi aggregated wall time: %lu\n", wi->id.full, irt_inst_region_get_current(wi), wi->inst_region_list->length, irt_inst_region_get_current(wi)->aggregated_wall_time, wi->inst_data->aggregated_wall_time);
	printf("end:\tWI: %3lu, RG: %3lu, RG_LEN: %3lu, rg last walltime: %lu, wi last walltime: %lu\n", wi->id.full, irt_inst_region_get_current(wi)->id, wi->inst_region_list->length, irt_inst_region_get_current(wi)->last_wall_time, wi->inst_data->last_wall_time);
}

void irt_inst_region_init(irt_context* context) {
	context->inst_region_data = (irt_inst_region_struct*)malloc(context->num_regions * sizeof(irt_inst_region_struct));
	for(uint32 i = 0; i < context->num_regions; ++i) {
		context->inst_region_data[i].id	 = i;
		context->inst_region_data[i].num_executions = 0;
		context->inst_region_data[i].remaining_exits = 0;
		context->inst_region_data[i].first_entrance_flag = false;
		irt_spin_init(&context->inst_region_data[i].lock);
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
		context->inst_region_data[i].last_##_name__ = 0; \
		context->inst_region_data[i].aggregated_##_name__ = 0;
#include "irt_metrics.def"
	}
}

void irt_inst_region_debug_output() {
	uint32 num_regions = irt_context_get_current()->num_regions;
	printf("%u region(s):\n", num_regions);
	for(uint32 i = 0; i < num_regions; ++i) {
		printf("region %u:\n", i);
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
		if(irt_g_inst_measure_##_name__) { \
			printf("  " #_name__ ":\t" _format_string__ "\n",  irt_context_get_current()->inst_region_data[i].aggregated_##_name__); \
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

	uint32 num_regions = irt_context_get_current()->num_regions;

	// write header
	fprintf(outputfile, "#subject,id");
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
		if(irt_g_inst_measure_##_name__) { \
			fprintf(outputfile, "," #_name__ "(%s)", #_unit__); \
		}
#include "irt_metrics.def"
	fprintf(outputfile, "\n");

	// write data
	for(uint32 i = 0; i < num_regions; ++i) {
		fprintf(outputfile, "RG,%u", i);
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
		if(irt_g_inst_measure_##_name__) { \
			fprintf(outputfile, "," _format_string__, irt_context_get_current()->inst_region_data[i].aggregated_##_name__); \
		}
#include "irt_metrics.def"
		fprintf(outputfile, "\n");
	}
	fclose(outputfile);
}

void irt_inst_region_finalize(irt_context* context) {
	irt_inst_region_output();
	irt_inst_region_debug_output();
	for(uint32 i = 0; i < context->num_regions; ++i) {
		irt_spin_destroy(&context->inst_region_data[i].lock);
	}
	free(context->inst_region_data);
}

// start a region
void irt_inst_region_start(region_id id) {
	irt_work_item* wi = irt_wi_get_current();
	printf("region start %u on wi %lu\n", id, wi->id.full);
	irt_context* context = irt_context_get_current();
	IRT_ASSERT(id >= 0 && id < context->num_regions, IRT_ERR_INSTRUMENTATION, "Start of region id %u requested, but only %u region(s) present", id, context->num_regions)
	irt_inst_region_struct* region = &(context->inst_region_data[id]);
//	irt_inst_region_start(irt_wi_get_current(), &(context->inst_region_data[id]));
	// if a region is already present (=being measured), stop current measurements
	if(wi->inst_region_list->length > 0) {
		IRT_ASSERT(irt_inst_region_get_current(wi) != region, IRT_ERR_INSTRUMENTATION, "Region %u start encountered, but this region was already started", id)
		irt_inst_region_end_measurements(wi);
		irt_inst_propagate_data_from_wi_to_cur_region(wi);
	}
	_irt_inst_region_stack_push(wi, region);
	irt_inst_region_start_measurements(wi);
}


// stop a region and remove it from the region stack of the WI
void irt_inst_region_end(region_id id) {
	irt_work_item* wi = irt_wi_get_current();
	printf("region end %u on wi %lu\n", id, wi->id.full);

//	printf("region end length: %u %lu %p\n", wi->inst_region_list->length, wi->id, wi);


	IRT_ASSERT(wi->inst_region_list->length > 0, IRT_ERR_INSTRUMENTATION, "Region end occurred while no region was started")

	irt_inst_region_struct* cur = irt_inst_region_get_current(wi);

	IRT_ASSERT(cur == &(irt_context_get_current()->inst_region_data[id]), IRT_ERR_INSTRUMENTATION, "Region end id %u did not match current open region", id)

	irt_inst_region_end_measurements(wi);

//if(wi->num_groups == 0 || (wi->num_groups > 0 && wi->wg_memberships[0].num == 0)) {
//printf("\n\n before propagating:\n");
//irt_inst_region_debug_output();
//}

	irt_inst_propagate_data_from_wi_to_cur_region(wi);
	// only increase count if wi is not member in any work group or has wg id == 0
	if(wi->num_groups == 0 || (wi->num_groups > 0 && wi->wg_memberships[0].num == 0))
		cur->num_executions++;
	_irt_inst_region_stack_pop(wi);

	// if there is a region left open, continue old measurements
	if(wi->inst_region_list->length > 0)
		irt_inst_region_start_measurements(wi);

//if(wi->num_groups == 0 || (wi->num_groups > 0 && wi->wg_memberships[0].num == 0)) {
//printf("\n\n after propagating:\n");
//irt_inst_region_debug_output();
//}
}

void irt_inst_region_wi_init(irt_work_item* wi) {
	wi->inst_region_list = (irt_inst_region_list*)malloc(sizeof(irt_inst_region_list));
	wi->inst_region_list->size = 8;
	wi->inst_region_list->length = 0;
	wi->inst_region_list->items = (irt_inst_region_struct**)malloc(sizeof(irt_inst_region_struct*)*wi->inst_region_list->size);
	wi->inst_data = (irt_inst_wi_struct*)malloc(sizeof(irt_inst_wi_struct));
#define METRIC(_name__, _id__, _unit__, _data_type__, _format_string__, _scope__, _aggregation__, _group__, _wi_start_code__, wi_end_code__, _region_early_start_code__, _region_late_end_code__) \
	wi->inst_data->last_##_name__ = 0; \
	wi->inst_data->aggregated_##_name__ = 0;
#include "irt_metrics.def"
}

void irt_region_instrumentation_setup() {
	irt_energy_select_instrumentation_method();
	irt_inst_set_region_instrumentation_from_env();
}

void irt_inst_region_list_copy(irt_work_item* destination, irt_work_item* source) {
	irt_inst_region_list* src = source->inst_region_list;
	if(src->length > 0) {
		irt_inst_region_list* dst = destination->inst_region_list;

		if(dst->size < src->size) {
			dst->size = src->size;
			dst->items = (irt_inst_region_struct**)realloc(dst->items, sizeof(irt_inst_region_struct*)*dst->size);
		}

		dst->length = src->length;
		memcpy(dst->items, src->items, sizeof(irt_inst_region_struct*)*dst->length);
	}
}

// stop a region, do not remove it from the region stack of the WI
//void irt_inst_region_suspend(irt_work_item* wi) {
//	irt_inst_region_end_measurements(wi);
//	irt_inst_propagate_data_from_wi_to_cur_region(wi);
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

void irt_inst_metrics_init() { }
void irt_inst_metrics_finalize() { }
void irt_inst_select_region_instrumentation_metrics(const char* selection) { }
void irt_inst_set_region_instrumentation_from_env() { }
void irt_inst_propagate_data_from_cur_region_to_parent(irt_work_item* wi) { }
void irt_inst_propagate_data_from_wi_to_cur_region(irt_work_item* wi) { }
void _irt_inst_region_stack_push(irt_work_item* wi, irt_inst_region_struct* region) { }
void _irt_inst_region_stack_pop(irt_work_item* wi) { }
void irt_inst_region_start_measurements(irt_work_item* wi) { }
void irt_inst_region_end_measurements(irt_work_item* wi) { }
void irt_inst_region_init(irt_context* context) { }
void irt_inst_region_debug_output() { }
void irt_inst_region_output() { }
void irt_inst_region_finalize(irt_context* context) { }
void irt_inst_region_start(region_id id) { }
void irt_inst_region_end(region_id id) { }
void irt_inst_region_wi_init(irt_work_item* wi) { }
void irt_region_instrumentation_setup() { }
void irt_inst_region_list_copy(irt_work_item* destination, irt_work_item* source);

#endif //IRT_ENABLE_REGION_INSTRUMENTATION
