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

#include "irt_events.h"

#define IRT_DEFINE_EVENTS(__subject__, __short__, __num_events__) \
static inline irt_##__short__##_event_register* _irt_get_##__short__##_event_register() { \
	irt_worker* self = irt_worker_get_current(); \
	/*irt_##__short__##_event_register* reg = self->__short__##_ev_register_list; \
	if(reg) { \
		self->__short__##_ev_register_list = reg->lookup_table_next; \
		reg->lookup_table_next = NULL; \
		memset(reg->occurrence_count, 0, __num_events__*sizeof(uint32)); \
		memset(reg->handler, 0, __num_events__*sizeof(irt_##__short__##_event_lambda*)); \
		return reg; \
	} else {*/ \
		irt_##__short__##_event_register* ret = (irt_##__short__##_event_register*)calloc(1, sizeof(irt_##__short__##_event_register)); \
		irt_spin_init(&ret->lock); /* TODO check destroy */ \
		IRT_ASSERT(ret->buffer == 0, IRT_ERR_INTERNAL, "Oh fuck"); \
		return ret; \
	/*}*/ \
} \
 \
static inline void _irt_del_##__short__##_event_register(irt_##__subject__##_id __short__##_id) { \
	irt_##__short__##_event_register_id reg_id; \
	reg_id.full = __short__##_id.full; \
	reg_id.cached = NULL; \
	irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_lookup(reg_id); \
	irt_worker* self = irt_worker_get_current(); \
	irt_##__short__##_event_register_table_remove(reg_id); \
	reg->lookup_table_next = self->__short__##_ev_register_list; \
	self->__short__##_ev_register_list = reg; \
} \
 \
void _irt_##__short__##_event_register_only(irt_##__short__##_event_register *reg) { \
	/* assert(irt_##__short__##_event_register_table_lookup(reg->id) == NULL); */ \
	irt_##__short__##_event_register_table_insert(reg); \
} \
 \
uint32 irt_##__short__##_event_check(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code) { \
	irt_##__short__##_event_register_id reg_id; \
	reg_id.full = __short__##_id.full; \
	reg_id.cached = NULL; \
	irt_##__short__##_event_register *reg = irt_##__short__##_event_register_table_lookup(reg_id); \
	if(reg) { \
		irt_spin_lock(&reg->lock); \
		uint32 ret = reg->occurrence_count[event_code]; \
		irt_spin_unlock(&reg->lock); \
		return ret; \
	} \
	return 0; \
} \
 \
void irt_##__short__##_event_remove(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda *handler) { \
	irt_##__short__##_event_register_id reg_id; \
	reg_id.full = __short__##_id.full; \
	reg_id.cached = NULL; \
	irt_##__short__##_event_register *reg = irt_##__short__##_event_register_table_lookup(reg_id); \
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
uint32 irt_##__short__##_event_check_gt_and_register(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda *handler, uint32 p_val) { \
	irt_##__short__##_event_register *newreg = _irt_get_##__short__##_event_register(); \
	newreg->id.full = __short__##_id.full; \
	newreg->id.cached = newreg; \
	irt_##__short__##_event_register *reg = irt_##__short__##_event_register_table_lookup_or_insert(newreg); \
	/* put new reg on reuse list if it was not used */ \
	if(reg != newreg) { \
		irt_worker* self = irt_worker_get_current(); \
		newreg->lookup_table_next = self->__short__##_ev_register_list; \
		self->__short__##_ev_register_list = newreg; \
	} \
	irt_spin_lock(&reg->lock); \
	/* check if event already occurred */ \
	if(reg->occurrence_count[event_code] > p_val) { \
		/* if so, return occurrence count */ \
		irt_spin_unlock(&reg->lock); \
		return reg->occurrence_count[event_code]; \
	} \
	/* else insert additional handler */ \
	handler->next = reg->handler[event_code]; \
	reg->handler[event_code] = handler; \
	irt_spin_unlock(&reg->lock); \
	return 0; \
} \
 \
int64 irt_##__short__##_event_check_exists_gt_and_register(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda *handler, uint32 p_val) { \
	irt_##__short__##_event_register_id reg_id; \
	reg_id.full = __short__##_id.full; \
	reg_id.cached = NULL; \
	irt_##__short__##_event_register *reg = irt_##__short__##_event_register_table_lookup(reg_id); \
	/* if reg not available return -1 */ \
	if(reg == NULL) { \
		return -1; \
	} \
	irt_spin_lock(&reg->lock); \
	/* check if event already occurred */ \
	if(reg->occurrence_count[event_code] > p_val) { \
		/* if so, return occurrence count */ \
		irt_spin_unlock(&reg->lock); \
		return reg->occurrence_count[event_code]; \
	} \
	/* else insert additional handler */ \
	handler->next = reg->handler[event_code]; \
	reg->handler[event_code] = handler; \
	irt_spin_unlock(&reg->lock); \
	return 0; \
} \
 \
int64 irt_##__short__##_event_check_exists_and_register(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda *handler) { \
	return irt_##__short__##_event_check_exists_gt_and_register(__short__##_id, event_code, handler, 0); \
} \
uint32 irt_##__short__##_event_check_and_register(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, irt_##__short__##_event_lambda *handler) { \
	return irt_##__short__##_event_check_gt_and_register(__short__##_id, event_code, handler, 0); \
} \
 \
void irt_##__short__##_event_trigger(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code) { \
	irt_##__short__##_event_register *newreg = _irt_get_##__short__##_event_register(); \
	newreg->id.full = __short__##_id.full; \
	newreg->id.cached = newreg; \
	irt_##__short__##_event_register *reg = irt_##__short__##_event_register_table_lookup_or_insert(newreg); \
	/* put new reg on reuse list if it was not used */ \
	if(reg != newreg) { \
		irt_worker* self = irt_worker_get_current(); \
		newreg->lookup_table_next = self->__short__##_ev_register_list; \
		self->__short__##_ev_register_list = newreg; \
	} \
	irt_spin_lock(&reg->lock); \
	/* increase event count */ \
	++reg->occurrence_count[event_code]; \
	/* go through all event handlers */ \
	irt_##__short__##_event_lambda *cur = reg->handler[event_code]; \
	irt_##__short__##_event_lambda *prev = NULL, *nex = NULL; \
	while(cur != NULL) { \
		nex = cur->next; \
		if(!cur->func(reg, cur->data)) { /* if event handled, remove */ \
			if(prev == NULL) reg->handler[event_code] = nex; \
			else prev->next = nex; \
		} else { /* else keep the handler in the list */ \
			prev = cur; \
		} \
		cur = nex; \
	} \
	irt_spin_unlock(&reg->lock); \
} \
 \
void irt_##__short__##_event_trigger_existing(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code) { \
	irt_##__short__##_event_register_id id; \
	id.full = __short__##_id.full; \
	id.cached = NULL; \
	irt_##__short__##_event_register *reg = irt_##__short__##_event_register_table_lookup(id); \
	irt_spin_lock(&reg->lock); \
	/* increase event count */ \
	++(reg->occurrence_count[event_code]); \
	/* go through all event handlers */ \
	irt_##__short__##_event_lambda *cur = reg->handler[event_code]; \
	irt_##__short__##_event_lambda *prev = NULL, *nex = NULL; \
	while(cur != NULL) { \
		nex = cur->next; \
		if(!cur->func(reg, cur->data)) { /* if event handled, remove */ \
			if(prev == NULL) reg->handler[event_code] = nex; \
			else prev->next = nex; \
		} else { /* else keep the handler in the list */ \
			prev = cur; \
		} \
		cur = nex; \
	} \
	irt_spin_unlock(&reg->lock); \
} \
 \
void irt_##__short__##_event_trigger_no_count(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code) { \
	irt_##__short__##_event_register *newreg = _irt_get_##__short__##_event_register(); \
	newreg->id.full = __short__##_id.full; \
	newreg->id.cached = newreg; \
	irt_##__short__##_event_register *reg = irt_##__short__##_event_register_table_lookup_or_insert(newreg); \
	/* put new reg on reuse list if it was not used */ \
	if(reg != newreg) { \
		irt_worker* self = irt_worker_get_current(); \
		newreg->lookup_table_next = self->__short__##_ev_register_list; \
		self->__short__##_ev_register_list = newreg; \
	} \
	irt_spin_lock(&reg->lock); \
	/* go through all event handlers */ \
	irt_##__short__##_event_lambda *cur = reg->handler[event_code]; \
	irt_##__short__##_event_lambda *prev = NULL, *nex = NULL; \
	while(cur != NULL) { \
		nex = cur->next; \
		if(!cur->func(reg, cur->data)) { /* if event handled, remove */ \
			if(prev == NULL) reg->handler[event_code] = nex; \
			else prev->next = nex; \
		} else { /* else keep the handler in the list */ \
			prev = cur; \
		} \
		cur = nex; \
	} \
	irt_spin_unlock(&reg->lock); \
} \
 \
void irt_##__short__##_event_set_occurrence_count(irt_##__subject__##_id __short__##_id, irt_##__short__##_event_code event_code, uint32 count) { \
	irt_##__short__##_event_register *newreg = _irt_get_##__short__##_event_register(); \
	newreg->id.full = __short__##_id.full; \
	newreg->id.cached = newreg; \
	irt_##__short__##_event_register *reg = irt_##__short__##_event_register_table_lookup_or_insert(newreg); \
	/* put new reg on reuse list if it was not used */ \
	if(reg != newreg) { \
		irt_worker* self = irt_worker_get_current(); \
		newreg->lookup_table_next = self->__short__##_ev_register_list; \
		self->__short__##_ev_register_list = newreg; \
	} \
	irt_spin_lock(&reg->lock); \
	/* set event count */ \
	reg->occurrence_count[event_code] = count; \
	irt_spin_unlock(&reg->lock); \
}


// WI events //////////////////////////////////////
IRT_DEFINE_EVENTS(work_item, wi, IRT_WI_EV_NUM);

// WG events //////////////////////////////////////
IRT_DEFINE_EVENTS(work_group, wg, IRT_WG_EV_NUM);

