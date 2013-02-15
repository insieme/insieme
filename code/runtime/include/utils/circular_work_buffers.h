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
#include "irt_atomic.h"

#ifndef IRT_CWBUFFER_LENGTH
#define IRT_CWBUFFER_LENGTH 16
#endif

#define IRT_CWBUFFER_MASK (IRT_CWBUFFER_LENGTH-1)

#if 1

// ============================================================================ Circular work buffers
// front = top, back = bottom
// top INCLUSIVE, bottom EXCLUSIVE
// Length needs to be a power of 2!
//
//  8 |       |
//    |-------|
//  7 |       |  <- top_update
//  6 |       |
//    |-------|
//  5 |#######|  <- top_val
//  4 |#######|
//  3 |#######|
//    |-------|
//  2 |       |  <- bot_val
//  1 |       |
//    |-------|
//  0 |       |  <- bot_update

typedef union _irt_cwb_state {
	uint64 all;
	struct {
		union {
			uint32 top;
			struct {
				uint16 top_val;
				uint16 top_update;
			};
		};
		union {
			uint32 bot;
			struct {
				uint16 bot_val;
				uint16 bot_update;
			};
		};
	};
} irt_cwb_state;

typedef struct _irt_circular_work_buffer {
	volatile irt_cwb_state state;
	irt_work_item* items[IRT_CWBUFFER_LENGTH];
} irt_circular_work_buffer;

// ============================================================================ Circular work buffers implementation

static inline void irt_cwb_init(irt_circular_work_buffer* wb) {
	wb->state.top_val = IRT_CWBUFFER_LENGTH/2;
	wb->state.top_update = IRT_CWBUFFER_LENGTH/2;
	wb->state.bot_val = IRT_CWBUFFER_LENGTH/2;
	wb->state.bot_update = IRT_CWBUFFER_LENGTH/2;
}

static inline uint32 irt_cwb_size(irt_circular_work_buffer* wb) {
	return (wb->state.top_val - wb->state.bot_val) & IRT_CWBUFFER_MASK;
}

void irt_cwb_push_front(irt_circular_work_buffer* wb, irt_work_item* wi) {
	// check feasibility
	irt_cwb_state state, newstate;
	for(;;) {
		state.all = wb->state.all;
		if(state.top_update != state.top_val) continue; // operation in progress on top
		// check for space
		newstate.all = state.all;
		newstate.top_update = (newstate.top_update+1) & IRT_CWBUFFER_MASK;
		if(newstate.top_update == state.bot_update 
			|| newstate.top_update == state.bot_val) continue; // not enough space in buffer, would be full after op
		// if we reach this point and no changes happened, we can perform our op
		if(irt_atomic_bool_compare_and_swap(&wb->state.all, state.all, newstate.all)) break; // repeat if state change since check
	}

	// write actual data to buffer
	//wb->items[newstate.top_update] = wi;
	// finish operation - force compiler to maintain operation order by using atomic for assignment
	//irt_atomic_bool_compare_and_swap(&wb->state.top_val, wb->state.top_val, newstate.top_update);
	(wb->items[newstate.top_update] = wi) && (wb->state.top_val = newstate.top_update);
}

static inline void irt_cwb_push_back(irt_circular_work_buffer* wb, irt_work_item* wi) {
	// check feasibility
	irt_cwb_state state, newstate;
	for(;;) {
		state.all = wb->state.all;
		if(state.bot_update != state.bot_val) continue; // operation in progress on bot
		// check for space
		newstate.all = state.all;
		newstate.bot_update = (newstate.bot_update-1) & IRT_CWBUFFER_MASK;
		if(newstate.bot_update == state.top_update 
			|| newstate.bot_update == state.top_val) continue; // not enough space in buffer, would be full after op
		// if we reach this point and no changes happened, we can perform our op
		if(irt_atomic_bool_compare_and_swap(&wb->state.all, state.all, newstate.all)) break; // repeat if state change since check
	}

	// write actual data to buffer
	//wb->items[newstate.bot_val] = wi;
	// finish operation - force compiler to maintain operation order by using atomic for assignment
	//irt_atomic_bool_compare_and_swap(&wb->state.bot_val, wb->state.bot_val, newstate.bot_update);
	(wb->items[newstate.bot_val] = wi) && (wb->state.bot_val = newstate.bot_update);
}

static inline irt_work_item* irt_cwb_pop_front(irt_circular_work_buffer* wb) {
	// check feasibility
	irt_cwb_state state, newstate;
	for(;;) {
		state.all = wb->state.all;
		if(state.top_val == state.bot_val) return NULL; // empty buffer
		if(state.top_update != state.top_val) continue; // operation in progress on top
		if(state.top_val == state.bot_update) continue; // conflicting op in progress on bot
		
		// decrement update pointer if feasible
		newstate.all = state.all;
		newstate.top_update = (newstate.top_update-1) & IRT_CWBUFFER_MASK;
		
		if(irt_atomic_bool_compare_and_swap(&wb->state.all, state.all, newstate.all)) break; // state change since check
	}

	// read actual data from buffer
	//irt_work_item *ret = wb->items[newstate.top_val];
	// finish operation
	//__sync_synchronize();
	//wb->state.top_val = newstate.top_update;
	irt_work_item *ret;
	(ret = wb->items[newstate.top_val]) && (wb->state.top_val = newstate.top_update);
	return ret;
}
	
static inline irt_work_item* irt_cwb_pop_back(irt_circular_work_buffer* wb) {
	// check feasibility
	irt_cwb_state state, newstate;
	for(;;) {
		state.all = wb->state.all;
		if(state.top_val == state.bot_val) return NULL; // empty buffer
		if(state.bot_update != state.bot_val) continue; // operation in progress on bot
		if(state.bot_val == state.top_update) continue; // conflicting op in progress on top

		// decrement update pointer if feasible
		newstate.all = state.all;
		newstate.bot_update = (newstate.bot_update+1) & IRT_CWBUFFER_MASK;
		if(irt_atomic_bool_compare_and_swap(&wb->state.all, state.all, newstate.all)) break; // state change since check
	}

	// read actual data from buffer
	//irt_work_item *ret = wb->items[newstate.bot_update];
	// finish operation
	//__sync_synchronize();
	//wb->state.bot_val = newstate.bot_update;
	irt_work_item *ret;
	(ret = wb->items[newstate.bot_update]) && (wb->state.bot_val = newstate.bot_update);
	return ret;
}

#else

// ============================================================================ Circular work buffers
// dummy implementation using locks

typedef struct _irt_circular_work_buffer {
	irt_spinlock lock;
	volatile uint16 top, bot;
	irt_work_item* items[IRT_CWBUFFER_LENGTH];
} irt_circular_work_buffer;

// ============================================================================ Circular work buffers implementation

static inline void irt_cwb_init(irt_circular_work_buffer* wb) {
	irt_spin_init(&wb->lock);
	wb->top = IRT_CWBUFFER_LENGTH/2;
	wb->bot = IRT_CWBUFFER_LENGTH/2;
}

static inline uint32 irt_cwb_size(irt_circular_work_buffer* wb) {
	return (wb->top - wb->bot) & IRT_CWBUFFER_MASK;
}

static inline void irt_cwb_push_front(irt_circular_work_buffer* wb, irt_work_item* wi) {
	for(;;) {
		irt_spin_lock(&wb->lock);
		if(irt_cwb_size(wb)==IRT_CWBUFFER_LENGTH-1) {
			irt_spin_unlock(&wb->lock);
			continue;
		}
		wb->top = (wb->top+1) & IRT_CWBUFFER_MASK;
		wb->items[wb->top] = wi;
		irt_spin_unlock(&wb->lock);
		break;
	}
}

static inline void irt_cwb_push_back(irt_circular_work_buffer* wb, irt_work_item* wi) {
	for(;;) {
		irt_spin_lock(&wb->lock);
		if(irt_cwb_size(wb)==IRT_CWBUFFER_LENGTH-1) {
			irt_spin_unlock(&wb->lock);
			continue;
		}
		wb->items[wb->bot] = wi;
		wb->bot = (wb->bot-1) & IRT_CWBUFFER_MASK;
		irt_spin_unlock(&wb->lock);
		break;
	}
}

static inline irt_work_item* irt_cwb_pop_front(irt_circular_work_buffer* wb) {
	irt_spin_lock(&wb->lock);
	if(irt_cwb_size(wb)==0) {
		irt_spin_unlock(&wb->lock);
		return NULL;
	}
	irt_work_item *ret = wb->items[wb->top];
	wb->top = (wb->top-1) & IRT_CWBUFFER_MASK;
	irt_spin_unlock(&wb->lock);
	return ret;
}
	
static inline irt_work_item* irt_cwb_pop_back(irt_circular_work_buffer* wb) {
	irt_spin_lock(&wb->lock);
	if(irt_cwb_size(wb)==0) {
		irt_spin_unlock(&wb->lock);
		return NULL;
	}
	wb->bot = (wb->bot+1) & IRT_CWBUFFER_MASK;
	irt_work_item *ret = wb->items[wb->bot];
	irt_spin_unlock(&wb->lock);
	return ret;
}

#endif