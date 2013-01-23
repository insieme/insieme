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

#include "declarations.h"

#include "utils/lookup_tables.h"

#define IRT_DECLARE_EVENTS(__subject__, __short__, __num_events__) \
\
typedef bool (irt_##__short__##_event_lambda_func)(irt_##__short__##_event_register* source_event_register, void *user_data); \
\
typedef struct _irt_##__short__##_event_lambda { \
	irt_##__short__##_event_lambda_func *func; \
	void *data; \
	struct _irt_##__short__##_event_lambda *next; \
} irt_##__short__##_event_lambda; \
\
struct _irt_##__short__##_event_register { \
	irt_spinlock lock; \
	irt_##__short__##_event_register_id id; \
	uint32 buffer; \
	uint32 occurrence_count[__num_events__]; \
	irt_##__short__##_event_lambda *handler[__num_events__]; \
	struct _irt_##__short__##_event_register *lookup_table_next; \
}; \
\
/* Registers a new event handler for the ##__short__##_item identified by ##__short__##_id, for the event event_code \
 * Use only when you can be sure that no event has occurred or been registered yet for this ##__short__## */ \
void _irt_##__short__##_event_register_only(irt_##__short__##_event_register *reg); \
/* Registers a new event handler for the ##__short__##_item identified by##__short__##_id, for the event event_code \
 * If the event has already occurred the handler will not be registered and the amount of occurrences will be returned */ \
uint32 irt_##__short__##_event_check_and_register(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda *handler); \
/* Removes a given event handler for the ##__short__##_item identified by##__short__##_id, for the event event_code */ \
void irt_##__short__##_event_remove(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda *handler); \
/* Triggers the event event_code on ##__short__##_id. \
 * This will execute (and potentially remove) all the associated event handlers */ \
void irt_##__short__##_event_trigger(irt_##__subject__##_id wi_id, irt_##__short__##_event_code event_code); \
/* Triggers the event event_code on ##__short__##_id without increasing its occurance count. \
 * This will execute (and potentially remove) all the associated event handlers */ \
void irt_##__short__##_event_trigger_no_count(irt_##__subject__##_id wi_id, irt_##__short__##_event_code event_code);



// WI events //////////////////////////////////////

IRT_MAKE_ID_TYPE(wi_event_register)

typedef enum _irt_wi_event_code {
	IRT_WI_EV_COMPLETED,
	IRT_WI_CHILDREN_COMPLETED, // triggered when all children are completed
	IRT_WI_EV_NUM // sentinel
} irt_wi_event_code;

IRT_DECLARE_EVENTS(work_item, wi, IRT_WI_EV_NUM)

IRT_DEFINE_LOOKUP_TABLE(wi_event_register, lookup_table_next, IRT_ID_HASH, IRT_EVENT_LT_BUCKETS)

// WG events //////////////////////////////////////

IRT_MAKE_ID_TYPE(wg_event_register)

typedef enum _irt_wg_event_code {
	IRT_WG_EV_COMPLETED,				// used for WI joining
	IRT_WG_EV_BARRIER_COMPLETE,			// indicates all WIs have reached a barrier
	IRT_WG_EV_NUM // sentinel
} irt_wg_event_code;

IRT_DECLARE_EVENTS(work_group, wg, IRT_WG_EV_NUM)

IRT_DEFINE_LOOKUP_TABLE(wg_event_register, lookup_table_next, IRT_ID_HASH, IRT_EVENT_LT_BUCKETS)

