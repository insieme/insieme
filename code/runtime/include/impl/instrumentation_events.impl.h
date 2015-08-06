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
#ifndef __GUARD_IMPL_INSTRUMENTATION_EVENTS_IMPL_H
#define __GUARD_IMPL_INSTRUMENTATION_EVENTS_IMPL_H

#include <locale.h> // needed to use thousands separator
#include <stdio.h>
#ifndef _GEMS_SIM
#include <sys/stat.h>
#endif
#include <errno.h>
#include "utils/timing.h"
#include "instrumentation_events.h"
#include "impl/error_handling.impl.h"

#ifdef IRT_ENABLE_INSTRUMENTATION

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
		if(table->data != NULL) {
			free(table->data);
		}
		free(table);
	}
}

void _irt_inst_event_insert_time(irt_worker* worker, const int event, const uint64 id, const uint64 time) {
	irt_instrumentation_event_data_table* table = worker->instrumentation_event_data;
	
	IRT_ASSERT(table->number_of_elements <= table->size, IRT_ERR_INSTRUMENTATION, "Instrumentation: Number of event table entries larger than table size")
	
	if(table->number_of_elements >= table->size) {
		_irt_inst_event_data_table_resize(table);
	}
	
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
	if(readable) {
		fprintf(outputfile, "%s,[%d %d],%20s,%32" PRIu64 "\n", irt_g_instrumentation_group_names[data.event_id], data.thread, data.index,
		        irt_g_instrumentation_event_names[data.event_id], irt_time_convert_ticks_to_ns(data.timestamp));
	}
	else {
		irt_work_item_id temp_id = irt_work_item_null_id();
		temp_id.index = data.index;
		temp_id.thread = data.thread;
		fprintf(outputfile, "%s,%" PRIu64 ",%s,%" PRIu64 "\n", irt_g_instrumentation_group_names[data.event_id], temp_id.full,
		        irt_g_instrumentation_event_names[data.event_id], irt_time_convert_ticks_to_ns(data.timestamp));
	}
}

void irt_inst_event_data_output_all(bool binary_format) {
	for(uint i = 0; i < irt_g_worker_count; ++i) {
		irt_inst_event_data_output(irt_g_workers[i], binary_format);
	}
}

// writes csv files
void irt_inst_event_data_output(irt_worker* worker, bool binary_format) {
	FILE* outputfile = stdout;
	char outputfilename[IRT_INST_OUTPUT_PATH_CHAR_SIZE];
	char defaultoutput[] = ".";
	char* outputprefix = defaultoutput;
	if(getenv(IRT_INST_OUTPUT_PATH_ENV)) {
		outputprefix = getenv(IRT_INST_OUTPUT_PATH_ENV);
	}
	
#ifndef _GEMS_SIM
	struct stat st;
	int stat_retval = stat(outputprefix,&st);
	if(stat_retval != 0) {
		mkdir(outputprefix, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
	}
	
	IRT_ASSERT(stat(outputprefix,&st) == 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Error creating directory for performance log writing: %s", strerror(errno));
	
	sprintf(outputfilename, "%s/worker_event_log.%04u", outputprefix, worker->id.thread);
	
	outputfile = fopen(outputfilename, "w");
	IRT_ASSERT(outputfile != 0, IRT_ERR_INSTRUMENTATION, "Instrumentation: Unable to open file for event log writing: %s", strerror(errno));
	/*	if(outputfile == 0) {
			IRT_DEBUG("Instrumentation: Unable to open file for event log writing\n");
			IRT_DEBUG_ONLY(strerror(errno));
			return;
		}*/
#endif
	irt_instrumentation_event_data_table* table = worker->instrumentation_event_data;
	IRT_ASSERT(table != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Worker has no event data!")
	//fprintf(outputfile, "INSTRUMENTATION: %10u events for worker %4u\n", table->number_of_elements, worker->id.thread);
	
#if defined (USE_OPENCL) && defined (IRT_OCL_INSTR)
	irt_ocl_event_table* ocl_table = worker->event_data;
	IRT_ASSERT(ocl_table != NULL, IRT_ERR_INSTRUMENTATION, "Instrumentation: Worker has no OpenCL event data!")
	int64 ocl_offset = 0;
	_irt_inst_ocl_performance_helper* ocl_helper_table = (_irt_inst_ocl_performance_helper*)malloc(ocl_table->num_events * 4 * sizeof(
	            _irt_inst_ocl_performance_helper));
	int ocl_helper_table_num_entries = ocl_table->num_events * 4;
	int helper_counter = 0;
	
	char ocl_filename[IRT_INST_OUTPUT_PATH_CHAR_SIZE];
	sprintf(ocl_filename, "%s/ocl_event_log", outputprefix);
	FILE* opencl_logfile;
	
	if(irt_g_ocl_temp_event_dump_already_done) {
		opencl_logfile = fopen(ocl_filename, "a");
	}
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
	
	IRT_ASSERT(ocl_helper_table_num_entries == helper_counter, IRT_ERR_INSTRUMENTATION, "OCL event counts do not match: helper_counter: %d, table_entries: %d",
	           helper_counter, ocl_helper_table_num_entries);
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
		for(uint i = 0; i < irt_g_inst_num_event_types; ++i) {
			fprintf(outputfile, "%-4s", irt_g_instrumentation_group_names[i]);
			fprintf(outputfile, "%-60s", irt_g_instrumentation_event_names[i]);
		}
		
		// write number of events
		const uint64 temp_num_of_elements = (uint64)(table->number_of_elements);
		fwrite(&temp_num_of_elements, sizeof(uint64), 1, outputfile);
		
		// write event table
		fwrite(table->data, sizeof(irt_instrumentation_event_data), table->number_of_elements, outputfile);
		
	}
	else {
		for(uint i = 0; i < table->number_of_elements; ++i) {
			irt_inst_event_data_output_single(table->data[i], outputfile, true);
		}
	}
	
#ifndef _GEMS_SIM
	fclose(outputfile);
#endif
	
#if defined (USE_OPENCL) && defined (IRT_OCL_INSTR)
	free(ocl_helper_table);
#endif
}

// ================= instrumentation function pointer toggle functions =======================

void irt_inst_set_wi_instrumentation(bool enable) {
	if(enable) {
		irt_inst_insert_wi_event = &_irt_inst_insert_wi_event;
	}
	else {
		irt_inst_insert_wi_event = &_irt_inst_insert_no_wi_event;
	}
}

void irt_inst_set_wg_instrumentation(bool enable) {
	if(enable) {
		irt_inst_insert_wg_event = &_irt_inst_insert_wg_event;
	}
	else {
		irt_inst_insert_wg_event = &_irt_inst_insert_no_wg_event;
	}
}

void irt_inst_set_wo_instrumentation(bool enable) {
	if(enable) {
		irt_inst_insert_wo_event = &_irt_inst_insert_wo_event;
	}
	else {
		irt_inst_insert_wo_event = &_irt_inst_insert_no_wo_event;
	}
}

void irt_inst_set_di_instrumentation(bool enable) {
	if(enable) {
		irt_inst_insert_di_event = &_irt_inst_insert_di_event;
	}
	else {
		irt_inst_insert_di_event = &_irt_inst_insert_no_di_event;
	}
}

void irt_inst_set_db_instrumentation(bool enable) {
	if(enable) {
		irt_inst_insert_db_event = &_irt_inst_insert_db_event;
	}
	else {
		irt_inst_insert_db_event = &_irt_inst_insert_no_db_event;
	}
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
	if(getenv(IRT_INST_WORKER_EVENT_LOGGING_ENV) && strcmp(getenv(IRT_INST_WORKER_EVENT_LOGGING_ENV), "enabled") == 0) {
		irt_log_setting_s(IRT_INST_WORKER_EVENT_LOGGING_ENV, "enabled");
		
		// set whether binary format is enabled
		if(getenv(IRT_INST_BINARY_OUTPUT_ENV) && (strcmp(getenv(IRT_INST_BINARY_OUTPUT_ENV), "enabled") == 0)) {
			irt_g_instrumentation_event_output_is_binary = true;
			irt_log_setting_s(IRT_INST_BINARY_OUTPUT_ENV, "enabled");
		}
		else {
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
			}
			else if(strcmp(tok, "WO") == 0) {
				irt_inst_set_wo_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "WO,");
			}
			else if(strcmp(tok, "WG") == 0) {
				irt_inst_set_wg_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "WG,");
			}
			else if(strcmp(tok, "DI") == 0) {
				irt_inst_set_di_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "DI,");
			}
			else if(strcmp(tok, "DB") == 0) {
				irt_inst_set_db_instrumentation(true);
				irt_g_instrumentation_event_output_is_enabled = true;
				log_output_counter += sprintf(&(log_output[log_output_counter]), "DB,");
			}
		}
		while((tok = strtok(NULL, ",")) != NULL);
		if(log_output_counter>0) {
			log_output[log_output_counter-1] = '\0';    // remove the last comma and replace with termination symbol
		}
		else {
			log_output[0] = '\0';
		}
		irt_log_setting_s(IRT_INST_WORKER_EVENT_TYPES_ENV, log_output);
		return;
	}
	irt_inst_set_all_instrumentation(false);
	irt_log_setting_s(IRT_INST_WORKER_EVENT_LOGGING_ENV, "disabled");
}

#else // if not IRT_ENABLE_INSTRUMENTATION

// ============ to be used if IRT_ENABLE_INSTRUMENTATION is not set ==============

irt_instrumentation_event_data_table* irt_inst_create_event_data_table() {
	return NULL;
}
void irt_inst_destroy_event_data_table(irt_instrumentation_event_data_table* table) {}

void irt_inst_insert_wi_event(irt_worker* worker, irt_instrumentation_event event, irt_work_item_id subject_id) {}
void irt_inst_insert_wg_event(irt_worker* worker, irt_instrumentation_event event, irt_work_group_id subject_id) {}
void irt_inst_insert_wo_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) {}
void irt_inst_insert_di_event(irt_worker* worker, irt_instrumentation_event event, irt_data_item_id subject_id) {}
void irt_inst_insert_db_event(irt_worker* worker, irt_instrumentation_event event, irt_worker_id subject_id) {}

void irt_inst_event_data_output(irt_worker* worker, bool binary_format) {}

#endif // IRT_ENABLE_INSTRUMENTATION

#endif // #ifdef __GUARD_IMPL_INSTRUMENTATION_EVENTS_IMPL_H
