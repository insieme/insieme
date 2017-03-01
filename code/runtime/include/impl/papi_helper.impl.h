/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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

#ifdef IRT_USE_PAPI

#include "papi.h"

#include <string.h>

#include "worker.h"
#include "irt_context.h"

/*
 * initializes general papi support
 */

void irt_papi_init() {
	// PAPI is already initialized, do nothing
	if(PAPI_is_initialized() != 0) { return; }

	int32 retval = 0;
	// initialize papi and check version
	retval = PAPI_library_init(PAPI_VER_CURRENT);
	if(retval > 0 && retval != PAPI_VER_CURRENT) {
		IRT_DEBUG("Instrumentation: PAPI version mismatch: require %d but found %d\n", PAPI_VER_CURRENT, retval);
	} else if(retval < 0) {
		IRT_DEBUG("Instrumentation: Error while trying to initialize PAPI! Reason: %s\n", PAPI_strerror(retval));
		if(retval == PAPI_EINVAL) { IRT_DEBUG("Instrumentation: papi.h version mismatch between compilation and execution!\n"); }
	}

	// must only be called once!
	if((retval = PAPI_thread_init(pthread_self)) != PAPI_OK) {
		IRT_DEBUG("Instrumentation: Error while trying to initialize PAPI's thread support! Reason: %s\n", PAPI_strerror(retval));
	}
}

void irt_papi_shutdown() {
	PAPI_shutdown();
}

#ifdef IRT_ENABLE_REGION_INSTRUMENTATION

/*
 * free allocated memory, shutdown papi
 */

void irt_papi_finalize_context(irt_context* context) {
	free(context->inst_region_metric_group_support_data.papi_eventset);
}

void irt_papi_finalize_worker(irt_worker* worker) {
	irt_context* context = irt_context_get_current();
	PAPI_cleanup_eventset(context->inst_region_metric_group_support_data.papi_eventset[worker->id.thread]);
	PAPI_destroy_eventset(&(context->inst_region_metric_group_support_data.papi_eventset[worker->id.thread]));
}

/*
 * starts papi measurements
 */

void irt_papi_start() {
	uint16 worker_id = irt_worker_get_current()->id.thread;
	int32 retval = 0;

	if((retval = PAPI_start(irt_context_get_current()->inst_region_metric_group_support_data.papi_eventset[worker_id])) != PAPI_OK) {
		IRT_DEBUG("Instrumentation: Error in PAPI_start! Reason: %s\n", PAPI_strerror(retval));
	}
}

/*
 * stops papi measurements
 */

void irt_papi_stop(long long int* papi_values) {
	uint16 worker_id = irt_worker_get_current()->id.thread;
	int32 retval = 0;
	irt_inst_region_context_declarations* papi_data = &irt_context_get_current()->inst_region_metric_group_support_data;

	if((retval = PAPI_stop(papi_data->papi_eventset[worker_id], papi_values)) != PAPI_OK) {
		IRT_DEBUG("Instrumentation: Error in PAPI_stop! Reason: %s\n", PAPI_strerror(retval));
	}
}

/*
 * returns a papi counter value by name
 */

int64 irt_papi_get_value_by_name(irt_context* context, long long int* papi_values, const char* event_name) {
	char event_name_copy[strlen(event_name) + 1];
	strcpy(event_name_copy, event_name);
	uint16 worker_id = irt_worker_get_current()->id.thread;
	int64 retval = 0;
	int32 eventset = context->inst_region_metric_group_support_data.papi_eventset[worker_id];
	int32 num_present_events = PAPI_num_events(eventset);

	if(num_present_events < 1) { return -1; }

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
	if(!papi_events_string) { return; }

	int32 retval = 0;

	uint16 id = worker->id.thread;
	char* saveptr = NULL;

	context->inst_region_metric_group_support_data.papi_eventset[id] = PAPI_NULL;

	if((retval = PAPI_create_eventset(&context->inst_region_metric_group_support_data.papi_eventset[id])) != PAPI_OK) {
		IRT_DEBUG("Instrumentation: Error creating PAPI event set for worker %hd! Reason: %s\n", id, PAPI_strerror(retval));
		return;
	}

	char papi_events_string_copy[strlen(papi_events_string) + 1];

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
		if((retval = PAPI_add_named_event(context->inst_region_metric_group_support_data.papi_eventset[id], current_event)) != PAPI_OK) {
			IRT_INFO("Instrumentation: Error adding PAPI event %s for worker %hd! Reason: %s\n", current_event, id, PAPI_strerror(retval));
		}
		current_event = strtok_r(NULL, ",", &saveptr);
	}
}

/*
 * select papi events from region instrumentation types environment variable
 */

void irt_papi_select_events_from_env(irt_worker* worker, irt_context* context) {
	const char* papi_events_string = getenv(IRT_INST_REGION_INSTRUMENTATION_TYPES_ENV);
	if(papi_events_string && strcmp(papi_events_string, "") != 0) { irt_papi_select_events(worker, context, papi_events_string); }
}

/*
 * allocates memory
 */

void irt_papi_setup_context(irt_context* context) {
	context->inst_region_metric_group_support_data.papi_eventset = (int32*)calloc(irt_g_worker_count, sizeof(int32));
}

/*
 * papi thread specific setup, selects events from environment variable
 */

void irt_papi_setup_worker(irt_worker* worker) {
	irt_context* context = irt_context_get_current();
	irt_papi_select_events_from_env(worker, context);
}

#else // IRT_ENABLE_REGION_INSTRUMENTATION

void irt_papi_finalize_context(irt_context* context) {}

void irt_papi_finalize_worker(irt_worker* worker) {}

void irt_papi_start() {}

void irt_papi_stop(long long int* papi_values) {}

int64 irt_papi_get_value_by_name(irt_context* context, long long int* papi_values, const char* event_name) {
	return -1;
}

void irt_papi_select_events(irt_worker* worker, irt_context* context, const char* papi_events_string) {}

void irt_papi_select_events_from_env(irt_worker* worker, irt_context* context) {}

void irt_papi_setup_context(irt_context* context) {}

void irt_papi_setup_worker(irt_worker* worker) {}

#endif // IRT_ENABLE_REGION_INSTRUMENTATION

#else // IRT_USE_PAPI

void irt_papi_init() {}

void irt_papi_shutdown() {}

#endif // IRT_USE_PAPI

