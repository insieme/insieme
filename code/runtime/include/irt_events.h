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

IRT_MAKE_ID_TYPE(wi_event_register);

typedef bool (irt_event_lambda_func)(irt_wi_event_register* source_event_register, void *user_data);

typedef struct _irt_event_lambda {
	irt_event_lambda_func *func;
	void *data;
	struct _irt_event_lambda *next;
} irt_event_lambda;

typedef enum _irt_wi_event_code {
	IRT_WI_EV_COMPLETED, 
	IRT_WI_EV_NUM // sentinel
} irt_wi_event_code;

struct _irt_wi_event_register {
	irt_wi_event_register_id id;
	uint32 occurence_count[IRT_WI_EV_NUM];
	irt_event_lambda *handler[IRT_WI_EV_NUM];
	pthread_spinlock_t lock;
	struct _irt_wi_event_register *lookup_table_next;
};

IRT_DEFINE_LOOKUP_TABLE(wi_event_register, lookup_table_next, IRT_ID_HASH, IRT_EVENT_LT_BUCKETS);


/* Registers a new event handler for the work item identified by wi_id, for the event event_code
 * Use only when you can be sure that no event has occurred or been registered yet for this wi */
void _irt_wi_event_register_only(irt_wi_event_register *reg);

/* Registers a new event handler for the work item identified by wi_id, for the event event_code
 * If the event has already occurred the event handler will be executed immediately */
uint32 irt_wi_event_check_and_register(irt_work_item_id wi_id, irt_wi_event_code event_code, irt_event_lambda *handler);

/* Triggers the event event_code on work item wi_id. 
 * This will execute (and potentially remove) all the associated event handlers */
void irt_wi_event_trigger(irt_work_item_id wi_id, irt_wi_event_code event_code);
