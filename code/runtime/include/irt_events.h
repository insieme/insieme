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
#ifndef __GUARD_IRT_EVENTS_H
#define __GUARD_IRT_EVENTS_H

#include "declarations.h"
#include "id_generation.h"
#include "utils/lookup_tables.h"

// event debug logging definitions
#ifdef IRT_ENABLE_EVENT_DEBUG_LOGGING
#define _IRT_EVENT_DEBUG_DEFINES(__short__)                                                                                                                    \
	irt_spinlock _irt_##__short__##_event_debug_print_lock;                                                                                                    \
	FILE* _irt_##__short__##_event_debug_log_file;
#define _IRT_EVENT_DEBUG_INIT(__short__)                                                                                                                       \
	irt_spin_init(&_irt_##__short__##_event_debug_print_lock);                                                                                                 \
	_irt_##__short__##_event_debug_log_file = fopen("irt_event_debug_" #__short__ "_event_register_table_log", "w");
#define _IRT_EVENT_DEBUG_DESTROY(__short__)                                                                                                                    \
	fclose(_irt_##__short__##_event_debug_log_file);                                                                                                           \
	irt_spin_destroy(&_irt_##__short__##_event_debug_print_lock);
#define _IRT_EVENT_DEBUG_HEADER(__short__) irt_spin_lock(&_irt_##__short__##_event_debug_print_lock);
#define _IRT_EVENT_DEBUG_FOOTER(__short__, ...)                                                                                                                \
	fprintf(_irt_##__short__##_event_debug_log_file, "\n\n");                                                                                                  \
	fprintf(_irt_##__short__##_event_debug_log_file, __VA_ARGS__);                                                                                             \
	irt_dbg_print_##__short__##_event_register_table(_irt_##__short__##_event_debug_log_file);                                                                 \
	irt_spin_unlock(&_irt_##__short__##_event_debug_print_lock);
#else // IRT_ENABLE_EVENT_DEBUG_LOGGING
#define _IRT_EVENT_DEBUG_DEFINES(__short__)
#define _IRT_EVENT_DEBUG_INIT(__short__)
#define _IRT_EVENT_DEBUG_DESTROY(__short__)
#define _IRT_EVENT_DEBUG_HEADER(__short__)
#define _IRT_EVENT_DEBUG_FOOTER(__short__, ...)
#endif // IRT_ENABLE_EVENT_DEBUG_LOGGING


// Event system declarations
#define IRT_DECLARE_EVENTS(__subject__, __short__, __num_events__)                                                                                             \
                                                                                                                                                               \
	/* A function prototype for event handlers.                                                                                                                \
	 * Event handlers get passed a user data object of arbitrary type. Note that upon                                                                          \
	 * event register destruction this object will not be reclaimed in any way, so keep this                                                                   \
	 * in mind when you pass heap allocated objects or external resources like locks.                                                                          \
	 * The handler function returns a boolean indicating whether the handler should                                                                            \
	 * stay registered for this event or not. Handlers which return true here have to                                                                          \
	 * take extra care not to leak any resources once the event register is destroyed. */                                                                      \
	typedef bool(irt_##__short__##_event_lambda_func)(void* user_data);                                                                                        \
                                                                                                                                                               \
	/* The struct to hold the lambda to call for each event together with the data to pass */                                                                  \
	typedef struct _irt_##__short__##_event_lambda {                                                                                                           \
		irt_##__short__##_event_lambda_func* func;                                                                                                             \
		void* data;                                                                                                                                            \
		struct _irt_##__short__##_event_lambda* next;                                                                                                          \
	} irt_##__short__##_event_lambda;                                                                                                                          \
                                                                                                                                                               \
	/* The internal event register used to manage registered items and events */                                                                               \
	struct _irt_##__short__##_event_register {                                                                                                                 \
		irt_spinlock lock;                                                                                                                                     \
		irt_##__short__##_event_register_id id;                                                                                                                \
		bool occured_flag[__num_events__];                                                                                                                     \
		irt_##__short__##_event_lambda* handler[__num_events__];                                                                                               \
		struct _irt_##__short__##_event_register* lookup_table_next;                                                                                           \
	};                                                                                                                                                         \
	IRT_MAKE_ID_TYPE(__short__##_event_register)                                                                                                               \
                                                                                                                                                               \
	/* Define the fields we may need for debug event logging */                                                                                                \
	_IRT_EVENT_DEBUG_DEFINES(__short__)                                                                                                                        \
                                                                                                                                                               \
                                                                                                                                                               \
	/* Creates a new event register for a given ##__short__##_item identified by ##__short__##_id */                                                           \
	void irt_##__short__##_event_register_create(const irt_##__subject__##_id item_id);                                                                        \
                                                                                                                                                               \
	/* Destroys the event register associated with the ##__short__##_item identified by ##__short__##_id */                                                    \
	void irt_##__short__##_event_register_destroy(const irt_##__subject__##_id item_id);                                                                       \
                                                                                                                                                               \
	/* Registers a new event handler for the ##__short__##_item identified by##__short__##_id,                                                                 \
	 * for the event event_code. This function will register the handler only in case the                                                                      \
	 * event register for the specified item does exist. In this case it will return true. */                                                                  \
	bool irt_##__short__##_event_handler_register(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code,                         \
	                                              irt_##__short__##_event_lambda* handler);                                                                    \
                                                                                                                                                               \
	/* Registers a new event handler for the ##__short__##_item identified by##__short__##_id,                                                                 \
	 * for the event event_code. This function will register the handler only in case the                                                                      \
	 * event register for the specified item does exist and the event didn't already occur.                                                                    \
	 * In this case it will return true (i.e. false will be returned if the register does not                                                                  \
	 * exist or the event already happened) */                                                                                                                 \
	bool irt_##__short__##_event_handler_check_and_register(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code,               \
	                                                        irt_##__short__##_event_lambda* handler);                                                          \
                                                                                                                                                               \
	/* Removes a given event handler for the ##__short__##_item identified by##__short__##_id, for the event event_code */                                     \
	void irt_##__short__##_event_handler_remove(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code,                           \
	                                            irt_##__short__##_event_lambda* handler);                                                                      \
                                                                                                                                                               \
	/* Triggers the event event_code on ##__short__##_id, failing in case the register doesn't exist.                                                          \
	 * This will execute (and potentially remove) all the associated event handlers */                                                                         \
	void irt_##__short__##_event_trigger(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code);                                 \
                                                                                                                                                               \
	/* Triggers the event event_code on ##__short__##_id, doing nothing case the register doesn't exist.                                                       \
	 * This will execute (and potentially remove) all the associated event handlers */                                                                         \
	void irt_##__short__##_event_trigger_if_exists(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code);                       \
                                                                                                                                                               \
	/* Resets the occured flag for the event event_code on ##__short__##_id. */                                                                                \
	void irt_##__short__##_event_reset(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code);


// WI events //////////////////////////////////////

typedef enum _irt_wi_event_code {
	IRT_WI_EV_COMPLETED,       // triggered when this WI is completed
	IRT_WI_CHILDREN_COMPLETED, // triggered when all children are completed
	IRT_WI_EV_NUM              // sentinel
} irt_wi_event_code;

IRT_DECLARE_EVENTS(work_item, wi, IRT_WI_EV_NUM)


// WG events //////////////////////////////////////

typedef enum _irt_wg_event_code {
	IRT_WG_EV_COMPLETED,        // used for WI joining
	IRT_WG_EV_BARRIER_COMPLETE, // indicates all WIs have reached a barrier
	IRT_WG_EV_NUM               // sentinel
} irt_wg_event_code;

IRT_DECLARE_EVENTS(work_group, wg, IRT_WG_EV_NUM)


void irt_event_debug_init() {
	// add each new event type to this functions also
	_IRT_EVENT_DEBUG_INIT(wi)
	_IRT_EVENT_DEBUG_INIT(wg)
}

void irt_event_debug_destroy() {
	// add each new event type to this functions also
	_IRT_EVENT_DEBUG_DESTROY(wi)
	_IRT_EVENT_DEBUG_DESTROY(wg)
}


#endif // ifndef __GUARD_IRT_EVENTS_H
