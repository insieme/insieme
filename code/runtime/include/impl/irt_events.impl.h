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
#ifndef __GUARD_IMPL_IRT_EVENTS_IMPL_H
#define __GUARD_IMPL_IRT_EVENTS_IMPL_H

#include "irt_events.h"

#define IRT_DEFINE_EVENTS(__subject__, __short__, __num_events__) \
 \
 \
/* Define the functions we need */ \
 \
/* Helper function to get a new or re-used event register from the current worker */ \
irt_##__short__##_event_register* _irt_get_##__short__##_event_register() { \
	irt_worker* self = irt_worker_get_current(); \
	/* Try to get a register from the re-use list */ \
	irt_##__short__##_event_register* reg = self->__short__##_ev_register_list; \
	if(reg) { \
		self->__short__##_ev_register_list = reg->lookup_table_next; \
		reg->lookup_table_next = NULL; \
		memset(reg->occured_flag, false, __num_events__ * sizeof(bool)); \
		memset(reg->handler, 0, __num_events__ * sizeof(irt_##__short__##_event_lambda*)); \
	} else { \
		/* Otherwise we have to create a new one */ \
		reg = (irt_##__short__##_event_register*)calloc(1, sizeof(irt_##__short__##_event_register)); \
	} \
	irt_spin_init(&reg->lock); \
	return reg; \
} \
 \
void _irt_##__short__##_event_register_create(irt_##__subject__##_id __short__##_id) { \
	irt_##__short__##_event_register *reg = _irt_get_##__short__##_event_register(); \
	reg->id.full = __short__##_id.full; \
	reg->id.cached = reg; \
	irt_##__short__##_event_register_table_insert(reg); \
} \
 \
void _irt_##__short__##_event_register_destroy(irt_##__subject__##_id __short__##_id) { \
	irt_##__short__##_event_register_id reg_id; \
	reg_id.full = __short__##_id.full; \
	reg_id.cached = NULL; \
	irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_remove(reg_id); \
	IRT_ASSERT(reg != NULL, IRT_ERR_INTERNAL, "Couldn't find register to remove"); \
	irt_worker* self = irt_worker_get_current(); \
	reg->lookup_table_next = self->__short__##_ev_register_list; \
	self->__short__##_ev_register_list = reg; \
} \
 \
static inline bool _irt_##__short__##_event_handler_check_and_register_impl(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda* handler, bool check_occured) { \
	irt_##__short__##_event_register_id reg_id; \
	reg_id.full = __short__##_id.full; \
	reg_id.cached = NULL; \
	irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_remove(reg_id); \
	if (reg == NULL || (check_occured && reg->occured_flag[event_code])) { \
		/* in case there is no register for this item or the event already happened and we should check for that */ \
		return false; \
	} \
	irt_spin_lock(&reg->lock); \
	/* insert additional handler */ \
	handler->next = reg->handler[event_code]; \
	reg->handler[event_code] = handler; \
	irt_spin_unlock(&reg->lock); \
	return true; \
} \
 \
bool irt_##__short__##_event_handler_register(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda* handler) { \
	return _irt_##__short__##_event_handler_check_and_register_impl(__short__##_id, event_code, handler, false); \
} \
 \
bool irt_##__short__##_event_handler_check_and_register(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda* handler) { \
	return _irt_##__short__##_event_handler_check_and_register_impl(__short__##_id, event_code, handler, true); \
} \
 \
void irt_##__short__##_event_handler_remove(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda* handler) { \
	irt_##__short__##_event_register_id reg_id; \
	reg_id.full = __short__##_id.full; \
	reg_id.cached = NULL; \
	irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_remove(reg_id); \
	IRT_ASSERT(reg != NULL, IRT_ERR_INTERNAL, "Deleting event handler for register that doesn't exist"); \
	irt_spin_lock(&reg->lock); \
	/* go through all event handlers */ \
	irt_##__short__##_event_lambda *cur = reg->handler[event_code]; \
	irt_##__short__##_event_lambda *prev = NULL, *nex = NULL; \
	while(cur != NULL) { \
		nex = cur->next; \
		if(cur == handler) { /* if found, delete */ \
			if(prev == NULL) reg->handler[event_code] = nex; \
			else prev->next = nex; \
			break; \
		} else { /* else keep iterating */ \
			prev = cur; \
		} \
		cur = nex; \
	} \
	IRT_ASSERT(cur != NULL, IRT_ERR_INTERNAL, "Deleting event handler which doesn't exist (but register does)"); \
	irt_spin_unlock(&reg->lock); \
} \
 \
void irt_##__short__##_event_trigger(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code) { \
	irt_##__short__##_event_register_id reg_id; \
	reg_id.full = __short__##_id.full; \
	reg_id.cached = NULL; \
	irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_remove(reg_id); \
	irt_spin_lock(&reg->lock); \
	/* increase event count */ \
	reg->occured_flag[event_code] = true; \
	/* go through all event handlers */ \
	irt_##__short__##_event_lambda *cur = reg->handler[event_code]; \
	irt_##__short__##_event_lambda *prev = NULL, *nex = NULL; \
	while(cur != NULL) { \
		nex = cur->next; \
		if(!cur->func(cur->data)) { /* if the event handler doesn't want to stay registered, remove */ \
			if(prev == NULL) reg->handler[event_code] = nex; \
			else prev->next = nex; \
		} else { /* else keep the handler in the list */ \
			prev = cur; \
		} \
		cur = nex; \
	} \
	irt_spin_unlock(&reg->lock); \
}


// WI events //////////////////////////////////////
IRT_DEFINE_EVENTS(work_item, wi, IRT_WI_EV_NUM)

// WG events //////////////////////////////////////
IRT_DEFINE_EVENTS(work_group, wg, IRT_WG_EV_NUM)



#endif // ifndef __GUARD_IMPL_IRT_EVENTS_IMPL_H
