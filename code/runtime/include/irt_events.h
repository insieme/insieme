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
#ifndef __GUARD_IRT_EVENTS_H
#define __GUARD_IRT_EVENTS_H

#include "declarations.h"
#include "id_generation.h"
#include "utils/lookup_tables.h"

// Event system declarations
#define IRT_DECLARE_EVENTS(__subject__, __short__, __num_events__) \
 \
 /* A function prototype for event handlers.
  * Event handlers get passed a user data object of arbitrary type. Note that upon
  * event register destruction this object will not be reclaimed in any way, so keep this
  * in mind when you pass heap allocated objects or external resources like locks.
  * The handler function returns a boolean indicating whether the handler should
  * stay registered for this event or not. Handlers which return true here have to
  * take extra care not to leak any resources once the event register is destroyed. */ \
typedef bool (irt_##__short__##_event_lambda_func)(void* user_data); \
 \
/* The struct to hold the lambda to call for each event together with the data to pass */ \
typedef struct _irt_##__short__##_event_lambda { \
	irt_##__short__##_event_lambda_func *func; \
	void *data; \
	struct _irt_##__short__##_event_lambda *next; \
} irt_##__short__##_event_lambda; \
 \
/* The internal event register used to manage registered items and events */ \
struct _irt_##__short__##_event_register { \
	irt_spinlock lock; \
	irt_##__short__##_event_register_id id; \
	bool occured_flag[__num_events__]; \
	irt_##__short__##_event_lambda *handler[__num_events__]; \
	struct _irt_##__short__##_event_register *lookup_table_next; \
}; \
IRT_MAKE_ID_TYPE(__short__##_event_register) \
 \
 \
/* Creates a new event register for a given ##__short__##_item identified by ##__short__##_id */ \
void irt_##__short__##_event_register_create(const irt_##__subject__##_id item_id); \
 \
/* Destroys the event register associated with the ##__short__##_item identified by ##__short__##_id */ \
void irt_##__short__##_event_register_destroy(const irt_##__subject__##_id item_id); \
 \
/* Registers a new event handler for the ##__short__##_item identified by##__short__##_id,
 * for the event event_code. This function will register the handler only in case the
 * event register for the specified item does exist. In this case it will return true. */ \
bool irt_##__short__##_event_handler_register(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda* handler); \
 \
/* Registers a new event handler for the ##__short__##_item identified by##__short__##_id,
 * for the event event_code. This function will register the handler only in case the
 * event register for the specified item does exist and the event didn't already occur.
 * In this case it will return true (i.e. false will be returned if the register does not
 * exist or the event already happened) */ \
bool irt_##__short__##_event_handler_check_and_register(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda* handler); \
 \
/* Removes a given event handler for the ##__short__##_item identified by##__short__##_id, for the event event_code */ \
void irt_##__short__##_event_handler_remove(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda* handler); \
 \
/* Triggers the event event_code on ##__short__##_id, failing in case the register doesn't exist. \
 * This will execute (and potentially remove) all the associated event handlers */ \
void irt_##__short__##_event_trigger(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code); \
 \
/* Triggers the event event_code on ##__short__##_id, doing nothing case the register doesn't exist. \
 * This will execute (and potentially remove) all the associated event handlers */ \
void irt_##__short__##_event_trigger_if_exists(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code); \
 \
/* Resets the occured flag for the event event_code on ##__short__##_id. */ \
void irt_##__short__##_event_reset(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code);



// WI events //////////////////////////////////////

typedef enum _irt_wi_event_code {
	IRT_WI_EV_COMPLETED,       // triggered when this WI is completed
	IRT_WI_CHILDREN_COMPLETED, // triggered when all children are completed
	IRT_WI_EV_NUM // sentinel
} irt_wi_event_code;

IRT_DECLARE_EVENTS(work_item, wi, IRT_WI_EV_NUM)


// WG events //////////////////////////////////////

typedef enum _irt_wg_event_code {
	IRT_WG_EV_COMPLETED,        // used for WI joining
	IRT_WG_EV_BARRIER_COMPLETE, // indicates all WIs have reached a barrier
	IRT_WG_EV_NUM // sentinel
} irt_wg_event_code;

IRT_DECLARE_EVENTS(work_group, wg, IRT_WG_EV_NUM)




#endif // ifndef __GUARD_IRT_EVENTS_H
