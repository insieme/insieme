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
#ifndef __GUARD_IMPL_IRT_EVENTS_IMPL_H
#define __GUARD_IMPL_IRT_EVENTS_IMPL_H

#include "irt_events.h"
#include "abstraction/spin_locks.h"


#define IRT_DEFINE_EVENTS(__subject__, __short__, __num_events__)                                                                                              \
                                                                                                                                                               \
                                                                                                                                                               \
	/* Define the functions we need */                                                                                                                         \
                                                                                                                                                               \
	/* Helper function to get a new or re-used event register from the current worker */                                                                       \
	irt_##__short__##_event_register* _irt_get_##__short__##_event_register() {                                                                                \
		irt_worker* self = irt_worker_get_current();                                                                                                           \
		/* Try to get a register from the re-use list */                                                                                                       \
		irt_##__short__##_event_register* reg = self->__short__##_ev_register_list;                                                                            \
		if(reg) {                                                                                                                                              \
			self->__short__##_ev_register_list = reg->lookup_table_next;                                                                                       \
			reg->lookup_table_next = NULL;                                                                                                                     \
			memset(reg->occured_flag, false, __num_events__ * sizeof(bool));                                                                                   \
			memset(reg->handler, 0, __num_events__ * sizeof(irt_##__short__##_event_lambda*));                                                                 \
		} else {                                                                                                                                               \
			/* Otherwise we have to create a new one */                                                                                                        \
			reg = (irt_##__short__##_event_register*)calloc(1, sizeof(irt_##__short__##_event_register));                                                      \
		}                                                                                                                                                      \
		irt_spin_init(&reg->lock);                                                                                                                             \
		return reg;                                                                                                                                            \
	}                                                                                                                                                          \
                                                                                                                                                               \
	void irt_##__short__##_event_register_create(const irt_##__subject__##_id item_id) {                                                                       \
		_IRT_EVENT_DEBUG_HEADER(__short__)                                                                                                                     \
		irt_##__short__##_event_register* reg = _irt_get_##__short__##_event_register();                                                                       \
		reg->id.full = item_id.full;                                                                                                                           \
		reg->id.cached = reg;                                                                                                                                  \
		irt_##__short__##_event_register_table_insert(reg);                                                                                                    \
		_IRT_EVENT_DEBUG_FOOTER(__short__, "Called event_register_create for [%d %d %d] -> reg=%p\n", item_id.node, item_id.thread, item_id.index, (void*)reg) \
	}                                                                                                                                                          \
                                                                                                                                                               \
	void irt_##__short__##_event_register_destroy(const irt_##__subject__##_id item_id) {                                                                      \
		_IRT_EVENT_DEBUG_HEADER(__short__)                                                                                                                     \
		irt_##__short__##_event_register_id reg_id;                                                                                                            \
		reg_id.full = item_id.full;                                                                                                                            \
		reg_id.cached = NULL;                                                                                                                                  \
		irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_remove(reg_id);                                                         \
		/* No locking needed here - the lookup table already locked the lock for us */                                                                         \
		IRT_ASSERT(reg != NULL, IRT_ERR_INTERNAL, "Couldn't find register for [%d %d %d] to remove", item_id.node, item_id.thread, item_id.index);             \
		irt_worker* self = irt_worker_get_current();                                                                                                           \
		reg->lookup_table_next = self->__short__##_ev_register_list;                                                                                           \
		self->__short__##_ev_register_list = reg;                                                                                                              \
		irt_spin_unlock(                                                                                                                                       \
		    &reg->lock); /* For better style we unlock the lock here as it has been locked by the lookup table, even though it won't be used anymore */        \
		_IRT_EVENT_DEBUG_FOOTER(__short__, "Called event_register_destroy for [%d %d %d]\n", item_id.node, item_id.thread, item_id.index)                      \
	}                                                                                                                                                          \
                                                                                                                                                               \
	static inline bool _irt_##__short__##_event_handler_check_and_register_impl(const irt_##__subject__##_id item_id,                                          \
	                                                                            const irt_##__short__##_event_code event_code,                                 \
	                                                                            irt_##__short__##_event_lambda* handler, const bool check_occured) {           \
		_IRT_EVENT_DEBUG_HEADER(__short__)                                                                                                                     \
		irt_##__short__##_event_register_id reg_id;                                                                                                            \
		reg_id.full = item_id.full;                                                                                                                            \
		reg_id.cached = NULL;                                                                                                                                  \
		irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_lookup(reg_id);                                                         \
		if(reg == NULL) {                                                                                                                                      \
			_IRT_EVENT_DEBUG_FOOTER(                                                                                                                           \
			    __short__, "Called event_handler_check_and_register on for [%d %d %d], event_code=%d, handler=%p, check_occured=%d -> reg not found\n",        \
			    item_id.node, item_id.thread, item_id.index, event_code, (void*)handler, check_occured)                                                        \
			/* in case there is no register for this item */                                                                                                   \
			return false;                                                                                                                                      \
		}                                                                                                                                                      \
		/* No locking needed here - the lookup table already locked the lock for us */                                                                         \
		if(check_occured && reg->occured_flag[event_code]) {                                                                                                   \
			irt_spin_unlock(&reg->lock);                                                                                                                       \
			_IRT_EVENT_DEBUG_FOOTER(                                                                                                                           \
			    __short__,                                                                                                                                     \
			    "Called event_handler_check_and_register on for [%d %d %d], event_code=%d, handler=%p, check_occured=%d -> reg=%p - did not register\n",       \
			    item_id.node, item_id.thread, item_id.index, event_code, (void*)handler, check_occured, (void*)reg)                                            \
			/* in case the event already happened and we should check for that */                                                                              \
			return false;                                                                                                                                      \
		}                                                                                                                                                      \
		/* insert additional handler */                                                                                                                        \
		handler->next = reg->handler[event_code];                                                                                                              \
		reg->handler[event_code] = handler;                                                                                                                    \
		irt_spin_unlock(&reg->lock);                                                                                                                           \
		_IRT_EVENT_DEBUG_FOOTER(                                                                                                                               \
		    __short__,                                                                                                                                         \
		    "Called event_handler_check_and_register for [%d %d %d], event_code=%d, handler=%p, check_occured=%d -> reg=%p - registered successfully\n",       \
		    item_id.node, item_id.thread, item_id.index, event_code, (void*)handler, check_occured, (void*)reg)                                                \
		return true;                                                                                                                                           \
	}                                                                                                                                                          \
                                                                                                                                                               \
	bool irt_##__short__##_event_handler_register(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code,                         \
	                                              irt_##__short__##_event_lambda* handler) {                                                                   \
		return _irt_##__short__##_event_handler_check_and_register_impl(item_id, event_code, handler, false);                                                  \
	}                                                                                                                                                          \
                                                                                                                                                               \
	bool irt_##__short__##_event_handler_check_and_register(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code,               \
	                                                        irt_##__short__##_event_lambda* handler) {                                                         \
		return _irt_##__short__##_event_handler_check_and_register_impl(item_id, event_code, handler, true);                                                   \
	}                                                                                                                                                          \
                                                                                                                                                               \
	void irt_##__short__##_event_handler_remove(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code,                           \
	                                            irt_##__short__##_event_lambda* handler) {                                                                     \
		_IRT_EVENT_DEBUG_HEADER(__short__)                                                                                                                     \
		irt_##__short__##_event_register_id reg_id;                                                                                                            \
		reg_id.full = item_id.full;                                                                                                                            \
		reg_id.cached = NULL;                                                                                                                                  \
		irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_lookup(reg_id);                                                         \
		IRT_ASSERT(reg != NULL, IRT_ERR_INTERNAL, "Deleting " #__short__ " event handler for register [%d %d %d] (event_code %d) that doesn't exist",          \
		           item_id.node, item_id.thread, item_id.index, event_code);                                                                                   \
		/* No locking needed here - the lookup table already locked the lock for us */                                                                         \
		/* go through all event handlers */                                                                                                                    \
		irt_##__short__##_event_lambda* cur = reg->handler[event_code];                                                                                        \
		irt_##__short__##_event_lambda *prev = NULL, *nex = NULL;                                                                                              \
		while(cur != NULL) {                                                                                                                                   \
			nex = cur->next;                                                                                                                                   \
			if(cur == handler) { /* if found, delete */                                                                                                        \
				if(prev == NULL)                                                                                                                               \
					reg->handler[event_code] = nex;                                                                                                            \
				else                                                                                                                                           \
					prev->next = nex;                                                                                                                          \
				break;                                                                                                                                         \
			} else { /* else keep iterating */                                                                                                                 \
				prev = cur;                                                                                                                                    \
			}                                                                                                                                                  \
			cur = nex;                                                                                                                                         \
		}                                                                                                                                                      \
		IRT_ASSERT(cur != NULL, IRT_ERR_INTERNAL, "Deleting " #__short__ " event handler which doesn't exist (but register [%d %d %d] does)", item_id.node,    \
		           item_id.thread, item_id.index);                                                                                                             \
		irt_spin_unlock(&reg->lock);                                                                                                                           \
		_IRT_EVENT_DEBUG_FOOTER(__short__, "Called event_handler_remove for [%d %d %d], event_code=%d, handler=%p -> removed handler for reg=%p\n",            \
		                        item_id.node, item_id.thread, item_id.index, event_code, (void*)handler, (void*)reg)                                           \
	}                                                                                                                                                          \
                                                                                                                                                               \
	static inline void _irt_##__short__##_event_trigger_impl(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code,              \
	                                                         const bool assert_if_not_exists) {                                                                \
		_IRT_EVENT_DEBUG_HEADER(__short__)                                                                                                                     \
		irt_##__short__##_event_register_id reg_id;                                                                                                            \
		reg_id.full = item_id.full;                                                                                                                            \
		reg_id.cached = NULL;                                                                                                                                  \
		irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_lookup(reg_id);                                                         \
		if(reg == NULL) {                                                                                                                                      \
			if(assert_if_not_exists) {                                                                                                                         \
				IRT_ASSERT(false, IRT_ERR_INTERNAL, "Triggering " #__short__ " event with no associated register [%d %d %d] for event_code %d", item_id.node,  \
				           item_id.thread, item_id.index, event_code);                                                                                         \
			} else {                                                                                                                                           \
				_IRT_EVENT_DEBUG_FOOTER(__short__, "Called event_trigger for [%d %d %d], event_code=%d -> reg not found\n", item_id.node, item_id.thread,      \
				                        item_id.index, event_code)                                                                                             \
				return;                                                                                                                                        \
			}                                                                                                                                                  \
		}                                                                                                                                                      \
		/* No locking needed here - the lookup table already locked the lock for us */                                                                         \
		reg->occured_flag[event_code] = true;                                                                                                                  \
		/* go through all event handlers */                                                                                                                    \
		irt_##__short__##_event_lambda* cur = reg->handler[event_code];                                                                                        \
		irt_##__short__##_event_lambda *prev = NULL, *nex = NULL;                                                                                              \
		while(cur != NULL) {                                                                                                                                   \
			nex = cur->next;                                                                                                                                   \
			if(!cur->func(cur->data)) { /* if the event handler doesn't want to stay registered, remove */                                                     \
				if(prev == NULL)                                                                                                                               \
					reg->handler[event_code] = nex;                                                                                                            \
				else                                                                                                                                           \
					prev->next = nex;                                                                                                                          \
			} else { /* else keep the handler in the list */                                                                                                   \
				prev = cur;                                                                                                                                    \
			}                                                                                                                                                  \
			cur = nex;                                                                                                                                         \
		}                                                                                                                                                      \
		irt_spin_unlock(&reg->lock);                                                                                                                           \
		_IRT_EVENT_DEBUG_FOOTER(__short__, "Called event_trigger for [%d %d %d], event_code=%d, -> triggered on reg=%p\n", item_id.node, item_id.thread,       \
		                        item_id.index, event_code, (void*)reg)                                                                                         \
	}                                                                                                                                                          \
                                                                                                                                                               \
	void irt_##__short__##_event_trigger(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code) {                                \
		_irt_##__short__##_event_trigger_impl(item_id, event_code, true);                                                                                      \
	}                                                                                                                                                          \
                                                                                                                                                               \
	void irt_##__short__##_event_trigger_if_exists(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code) {                      \
		_irt_##__short__##_event_trigger_impl(item_id, event_code, false);                                                                                     \
	}                                                                                                                                                          \
                                                                                                                                                               \
	void irt_##__short__##_event_reset(const irt_##__subject__##_id item_id, const irt_##__short__##_event_code event_code) {                                  \
		_IRT_EVENT_DEBUG_HEADER(__short__)                                                                                                                     \
		irt_##__short__##_event_register_id reg_id;                                                                                                            \
		reg_id.full = item_id.full;                                                                                                                            \
		reg_id.cached = NULL;                                                                                                                                  \
		irt_##__short__##_event_register* reg = irt_##__short__##_event_register_table_lookup(reg_id);                                                         \
		IRT_ASSERT(reg != NULL, IRT_ERR_INTERNAL, "Triggering " #__short__ " event with no associated register [%d %d %d] for event_code %d", item_id.node,    \
		           item_id.thread, item_id.index, event_code);                                                                                                 \
		/* No locking needed here - the lookup table already locked the lock for us */                                                                         \
		reg->occured_flag[event_code] = false;                                                                                                                 \
		irt_spin_unlock(&reg->lock);                                                                                                                           \
		_IRT_EVENT_DEBUG_FOOTER(__short__, "Called event_reset for [%d %d %d], event_code=%d, -> reset on reg=%p\n", item_id.node, item_id.thread,             \
		                        item_id.index, event_code, (void*)reg)                                                                                         \
	}


// WI events //////////////////////////////////////
IRT_DEFINE_LOCKED_LOOKUP_TABLE_WITH_POST_LOOKUP_ACTION(wi_event_register, lookup_table_next, IRT_ID_HASH, IRT_EVENT_LT_BUCKETS, {
	if(element) { irt_spin_lock(&((irt_wi_event_register*)element)->lock); }
})
IRT_DEFINE_EVENTS(work_item, wi, IRT_WI_EV_NUM)

// WG events //////////////////////////////////////
IRT_DEFINE_LOCKED_LOOKUP_TABLE_WITH_POST_LOOKUP_ACTION(wg_event_register, lookup_table_next, IRT_ID_HASH, IRT_EVENT_LT_BUCKETS, {
	if(element) { irt_spin_lock(&((irt_wg_event_register*)element)->lock); }
})
IRT_DEFINE_EVENTS(work_group, wg, IRT_WG_EV_NUM)


#endif // ifndef __GUARD_IMPL_IRT_EVENTS_IMPL_H
