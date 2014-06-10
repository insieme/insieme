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

/*
 *
 * General instructions on how to use PAPI with the runtime instrumentation system:
 *
 * This file provides helper functions for the PAPI interface. The PAPI events 
 * to be instrumented can be supplied via an environment variable named 
 * IRT_INST_PAPI_EVENTS, separated via colons, e.g.
 * IRT_INST_PAPI_EVENTS="PAPI_TOT_CYC:PAPI_L2_TCM:PAPI_BR_MSP".
 *
 * To find out what events are present on a specific machine and what their names 
 * are, navigate to the PAPI installation directory and execute "./bin/papi_avail -a". 
 * This will give you hardware information including the maximum number of counters 
 * that can be measured in parallel, as well as all preset events that can be counted. 
 * Use the preset names like PAPI_TOT_CYC for the environment variable.
 *
 * Since not all events can be measured in arbitrary combinations, one should check 
 * first whether a chosen combination is possible. For this, execute 
 * "./bin/papi_event_chooser PRESET <papi_event> ...", e.g. 
 * "./bin/papi_event_chooser PRESET PAPI_TOT_CYC PAPI_L2_TCM". The program will tell 
 * you either what preset events can still be added or which event of your list cannot 
 * be counted with another already contained in your list.
 *
 */

#include "worker.h"
#include "irt_context.h"

#ifdef IRT_USE_PAPI
#include "papi.h"


/*
 * takes a list of papi counters as argument and outputs lists of counters that can be counted simultaneously
 */

//int32 irt_check_papi_counter_combinations(const char* param_events) {
//
//	char* list_of_events = (char*)alloca(strlen(param_events)*sizeof(char));
//
//	if(param_events) {
//		if(strlen(param_events) > 0)
//			strcpy(list_of_events, param_events);
//	}
//	IRT_ASSERT(strlen(list_of_events) > 0, IRT_ERR_INSTRUMENTATION, "ERROR: No PAPI event names supplied via parameter or environment variable to check combinations!\n");
//
//	int retval = 0;
//
//	// used for string token parsing
//	char* papi_event_toks[IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS];
//	char* cur_tok;
//	uint32 number_of_events_supplied = 0;
//	//uint32 number_of_events_added = 0;
//
//	// get the first event
//	if((papi_event_toks[0] = strtok(list_of_events, ":")) != NULL)
//		number_of_events_supplied++;
//	else
//		return -1;
//
//	// get all remaining events
//	while((cur_tok = strtok(NULL, ":")) != NULL)
//		papi_event_toks[number_of_events_supplied++] = cur_tok;
//
//	IRT_ASSERT(number_of_events_supplied <= IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS, IRT_ERR_INSTRUMENTATION, "ERROR: Runtime currently supports max %u counters, but %u were supplied!\n", IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS, number_of_events_supplied)
//
//	int matrix[IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS][IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS];
//	int papi_counter_ids[IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS];
//
//	for(int i = 0; i < IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS; ++i) {
//		papi_counter_ids[i] = -1;
//		for(int j = 0; j < IRT_INST_PAPI_MAX_COUNTERS_COMBINATIONS; ++j)
//			matrix[i][j] = -1;
//	}
//
//	//unsigned int j = 0;
//	int event_code;
//	int event_set = 0;
//	uint32 group_index = 0;
//	uint32 event_index = 0;
//
//	// index all counters
//	for(unsigned int i = 0; i < number_of_events_supplied; ++i) {
//		papi_counter_ids[i] = i;
////		printf("papi: %s, counter: %d\n", papi_event_toks[i], papi_counter_ids[i]);
//	}
//
///*	// simple algorithm: pack everything possible into one group until we get a counter that can't be counted with others, then start a new group and continue
//	while(j < number_of_events_supplied) {
//		// if papi event doesn't exist in the papi system, ignore it and move on
//		if((retval = PAPI_event_name_to_code(papi_event_toks[j], &event_code)) != PAPI_OK) {
//			IRT_DEBUG("Instrumentation: Error trying to convert PAPI preset name to event code! Reason: %s\n", PAPI_strerror(retval));
//			++j;
//		} else {
//			// try to add it to an eventset, if it works advance counters
//			if((retval = PAPI_add_event(event_set, event_code)) == PAPI_OK) {
//				matrix[newlength][counter] = papi_counter_ids[j];
//				++counter;
//				++j;
//			// papi event can't be counted with others, start a new eventset
//			} else if(retval == PAPI_ECNFLCT) {
//				PAPI_cleanup_eventset(event_set);
//				++newlength;
//				counter = 0;
//			// other error, most likely: hardware doesn't support the counter, ignore it and move on
//			} else {
//				++j;
//			}
//		}
//	}*/
//
//	// bitmap to remember counters that were already added
//	int32* papi_handled_bitmap = (int32*) malloc(number_of_events_supplied*sizeof(int32));
//
//	// initialize bitmap to zero
//	for(int i = 0; i < number_of_events_supplied; ++i)
//		papi_handled_bitmap[i] = 0;
//
//	uint32 papi_handled_count = 0;
//	uint32 at_least_one_unhandled = 0;
//	int number_of_counters_supported_by_hardware = PAPI_get_opt(PAPI_MAX_HWCTRS, NULL);
//
//	// create a new empty event set
//	PAPI_create_eventset(&event_set);
//
//	// as long as we didn't handle all events that were previously parsed
//	while(papi_handled_count < number_of_events_supplied) {
//		at_least_one_unhandled = 0;
//		for(uint32 i = 0; i < number_of_events_supplied; ++i) {
//			// if the current counter hasn't been handled yet
//			if(papi_handled_bitmap[i] == 0) {
//				// found an event that hasn't been handled yet
//				at_least_one_unhandled = 1;
//				// if event is not known to papi
//				if((retval = PAPI_event_name_to_code(papi_event_toks[i], &event_code)) != PAPI_OK) {
//					papi_handled_bitmap[i] = -1;
//					++papi_handled_count;
//				// if event is known to papi
//				} else {
//					// if papi event can be added to the current event set:
//					// 	save the id to the current group
//					// 	mark and count it as handled
//					if((retval = PAPI_add_event(event_set, event_code)) == PAPI_OK) {
//						matrix[group_index][event_index] = papi_counter_ids[i];
//						papi_handled_bitmap[i] = 1;
//						++event_index;
//						++papi_handled_count;
//					// if there is a conflict with this event
//					} else if (retval == PAPI_ECNFLCT) {
//						// if the conflict is caused by no more available performance registers, start a new group
//						if(event_index == number_of_counters_supported_by_hardware) {
//							PAPI_cleanup_eventset(event_set);
//							PAPI_create_eventset(&event_set);
//							++group_index;
//							event_index = 0;
//							--i; // re-do the current event
//						// this counter is the only one and there is still a conflict, it is a papi or hardware bug => ignore this event
//						} else if (event_index == 0) {
//							papi_handled_bitmap[i] = -1;
//							++papi_handled_count;
//						}
//					// if there is any other error, ignore this event
//					} else {
//						papi_handled_bitmap[i] = -1;
//						++papi_handled_count;
//					}
//				}
//			}
//		}
//
//		// if there was at least one counter that hasn't been handled although we moved through the entire list, start a new group and continue
//		if(at_least_one_unhandled == 1) {
//			PAPI_cleanup_eventset(event_set);
//			PAPI_create_eventset(&event_set);
//			++group_index;
//			event_index = 0;
//		}
//
////		for(int i = 0; i < number_of_events_supplied; ++i) {
////			printf("%d:%d ", i, papi_handled_bitmap[i]);
////		}
////		printf("\n");
//	}
//
//	uint32 needed_to_print = 0;
//
//	for(uint32 i = 0; i < number_of_events_supplied; ++i) {
//		needed_to_print = 0;
//		for(uint32 k = 0; k < number_of_events_supplied; ++k) {
//			if(matrix[i][k] >= 0) {
//				printf("%d ", matrix[i][k]);
//				needed_to_print = 1;
//			}
//		}
//
//		if(needed_to_print == 0)
//			break;
//		printf("\n");
//	}
//
//	return 0; // no problem, all counters can be measured
//}
//
///*
// * outputs lists of counters read from the environment variable that can be counted simultaneously
// */
//
//int irt_check_papi_counter_combinations_from_env() {
//	return irt_check_papi_counter_combinations(getenv(IRT_INST_PAPI_EVENTS));
//}

/*
 * initializes general papi support
 */

void irt_papi_init() {
	// PAPI is already initialized, do nothing
	if(PAPI_is_initialized() != 0)
		return;
	
	int32 retval = 0;
	// initialize papi and check version
	retval = PAPI_library_init(PAPI_VER_CURRENT);
	if(retval > 0 && retval != PAPI_VER_CURRENT) {
		IRT_DEBUG("Instrumentation: PAPI version mismatch: require %d but found %d\n", PAPI_VER_CURRENT, retval);
	} else if (retval < 0) {
		IRT_DEBUG("Error while trying to initialize PAPI! Reason: %s\n", PAPI_strerror(retval));
		if(retval == PAPI_EINVAL)
			IRT_DEBUG("Instrumentation: papi.h version mismatch between compilation and execution!\n");
	}

	if((retval = PAPI_thread_init(pthread_self)) != PAPI_OK)
		IRT_DEBUG("Instrumentation: Error while trying to initialize PAPI's thread support! Reason: %s\n", PAPI_strerror(retval));
}

/*
 * free allocated memory, shutdown papi
 */

void irt_papi_finalize(irt_context* context) {
	free(context->inst_region_metric_group_support_data.papi_eventset);
	PAPI_shutdown();
}

/*
 * starts papi measurements
 */

void irt_papi_start() {
	uint16 worker_id = irt_worker_get_current()->id.thread;
	int32 retval = 0;

	if((retval = PAPI_start(irt_context_get_current()->inst_region_metric_group_support_data.papi_eventset[worker_id])) != PAPI_OK)
		IRT_DEBUG("Instrumentation: Error in PAPI_start! Reason: %s\n", PAPI_strerror(retval));
}

/*
 * stops papi measurements
 */

void irt_papi_stop(long long int* papi_values) {
	uint16 worker_id = irt_worker_get_current()->id.thread;
	int32 retval = 0;
	irt_inst_region_context_declarations* papi_data = &irt_context_get_current()->inst_region_metric_group_support_data;

	if((retval = PAPI_stop(papi_data->papi_eventset[worker_id], papi_values)) != PAPI_OK)
		IRT_DEBUG("Instrumentation: Error in PAPI_stop! Reason: %s\n", PAPI_strerror(retval));
}

/*
 * returns a papi counter value by name
 */

int64 irt_papi_get_value_by_name(irt_context* context, long long int* papi_values, const char* event_name) {
	char event_name_copy[strlen(event_name)];
	strcpy(event_name_copy, event_name);
	uint16 worker_id = irt_worker_get_current()->id.thread;
	int64 retval = 0;
	int32 eventset = context->inst_region_metric_group_support_data.papi_eventset[worker_id];
	int32 num_present_events = PAPI_num_events(eventset);
	int32 present_events[num_present_events];
	int32 target_event_code = 0;

	PAPI_event_name_to_code(event_name_copy, &target_event_code);

	PAPI_list_events(eventset, present_events, &num_present_events);

	for(int i = 0; i < num_present_events; ++i) {
		if(present_events[i] == target_event_code) {
			retval = papi_values[i];
			break;
		}
	}

	return retval;
}

/*
 * select papi events from a comma-delimited string, needs to be thread-safe because it is executed by each worker
 */

void irt_papi_select_events(irt_worker* worker, irt_context* context, const char* papi_events_string) {
	if(!papi_events_string)
		return;

	int32 retval = 0;

	uint16 id = worker->id.thread;
	char* saveptr = NULL;

		context->inst_region_metric_group_support_data.papi_eventset[id] = PAPI_NULL;

		if((retval = PAPI_create_eventset(&context->inst_region_metric_group_support_data.papi_eventset[id])) != PAPI_OK) {
			IRT_DEBUG("Instrumentation: Error creating PAPI event set for worker %hd! Reason: %s\n", id, PAPI_strerror(retval));
			return;
		}

		char papi_events_string_copy[strlen(papi_events_string)];

		strcpy(papi_events_string_copy, papi_events_string);

		char* current_event = strtok_r(papi_events_string_copy, ",", &saveptr);

		while(current_event != NULL) {
			// skip all non-papi events
			if(strncmp(current_event, "PAPI", 4) != 0) {
				current_event = strtok_r(NULL, ",", &saveptr);
				continue;
			}
			// skip event types unknown to papi
			if((retval = PAPI_query_named_event(current_event)) != PAPI_OK) {
				IRT_INFO("Instrumentation: Unknown PAPI event: %s! Reason: %s\n", current_event, PAPI_strerror(retval));
				current_event = strtok_r(NULL, ",", &saveptr);
				continue;
			}
			if((retval = PAPI_add_named_event(context->inst_region_metric_group_support_data.papi_eventset[id], current_event)) != PAPI_OK)
				IRT_INFO("Instrumentation: Error adding PAPI event %s for worker %hd! Reason: %s\n", current_event, id, PAPI_strerror(retval));
			current_event = strtok_r(NULL, ",", &saveptr);
		}
}

/*
 * select papi events from region instrumentation types environment variable
 */

void irt_papi_select_events_from_env(irt_worker* worker, irt_context* context) {
	const char* papi_events_string = getenv(IRT_INST_REGION_INSTRUMENTATION_TYPES_ENV);
	if(papi_events_string && strcmp(papi_events_string, "") != 0)
		irt_papi_select_events(worker, context, papi_events_string);
}

/*
 * allocates memory
 */

void irt_papi_setup(irt_context* context) {
	context->inst_region_metric_group_support_data.papi_eventset = (int32*)malloc(sizeof(int32) * irt_g_worker_count);
}

/*
 * papi thread specific setup, selects events from environment variable
 */

void irt_papi_setup_thread(irt_worker* worker) {
	irt_context* context = irt_context_get_current();
	irt_papi_select_events_from_env(worker, context);
}

#else //IRT_USE_PAPI

void irt_papi_init() { }

void irt_papi_finalize(irt_context* context) { }

void irt_papi_start() { }

void irt_papi_stop(long long int* papi_values) { }

int64 irt_papi_get_value_by_name(irt_context* context, long long int* papi_values, const char* event_name) { return -1; }

void irt_papi_select_events(irt_worker* worker, irt_context* context, const char* papi_events_string) { }

void irt_papi_select_events_from_env(irt_worker* worker, irt_context* context) { }

void irt_papi_setup(irt_context* context) { }

void irt_papi_setup_thread(irt_worker* worker) { }

#endif // IRT_USE_PAPI
