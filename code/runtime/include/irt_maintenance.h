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

// Insieme runtime maintenance event system
// Implements a low-overhead system for the execution of maintenance events at fixed timesteps
//
// Events will be executed at the requested interval at most (subject to OS limits),
// or at intervals up to 50% less. Events processing will be grouped so as to cause 
// the minimum amount of interference with the rest of the system while maintaining 
// these constraints.
//
// Each event is defined by a "irt_maintenance_lambda" structure which needs to be maintained 
// in user code. Its "irt_maintenance_func" will be called at the specified interval and
// should return either the new desired interval (changing intervals incurs a small
// perormance overhead and should be avoided).
//
// Events can be asynchronously added by any threads using "irt_maintenance_register"
// and can be removed from the system by returning a new interval time to 0.


// the minimum interval, in milliseconds, for maintenance events to be executed
#define IRT_MAINTENANCE_MIN_INTERVAL 1

// defines the number of maintenance interval slots, and thus the maximum interval time
// maximum interval = IRT_MAINTENANCE_MIN_INTERVAL * 2 ^ (IRT_MAINTENANCE_SLOTS-1)
#define IRT_MAINTENANCE_SLOTS 32

// data structure definitions /////////////////////////////////////////////////
typedef uint64 irt_maintenance_func(void *user_data);

typedef struct _irt_maintenance_lambda {
	irt_maintenance_func *func;							// the maintenance function to be executed for this event
	void *data;											// user data for this maintenance event
	uint64 interval;									// the interval, in ms, for event execution
	struct _irt_maintenance_lambda *next;				// internally used next pointer
} irt_maintenance_lambda;

typedef struct _irt_maintenance_slot {
	uint64 interval;
	irt_spinlock lock;
	irt_maintenance_lambda *first_lambda;
} irt_maintenance_slot;

// globals ////////////////////////////////////////////////////////////////////

// global register of maintenance events
irt_maintenance_slot irt_g_maintenance_events[IRT_MAINTENANCE_SLOTS];

// current minimum time step (in interval slots) required for maintenance
uint64 irt_g_maintenance_min_interval_slot;

// maintenance thread control variable
bool irt_g_maintenance_thread_active;

// maintenance thread;
irt_thread irt_g_maintenance_thread;
irt_cond_var irt_g_maintenance_cond;
irt_lock_obj irt_g_maintenance_mutex;

// functions //////////////////////////////////////////////////////////////////

// maintenance thread function
void* irt_maintenance_thread_func(void *);

// finds the next smallest exponent of two
// e.g. input = 9 >= 8 = 2^3 -> 3 // input = 578 >= 512 = 2^9 -> 9
static inline uint32 _irt_maintenance_get_next_smallest_exponent_of_two(uint64 input) {
	uint32 out = 0;
	while(input) {
		out++;
		input >>= 1;
	}
	return out-1;
}

// initialize maintenance data structures
void irt_maintenance_init() {
	// intialize data
	uint32 interval = IRT_MAINTENANCE_MIN_INTERVAL;
	for(uint32 i=0; i<IRT_MAINTENANCE_SLOTS; ++i) {
		irt_spin_init(&irt_g_maintenance_events[i].lock);
		irt_g_maintenance_events[i].interval = interval;
		irt_g_maintenance_events[i].first_lambda = NULL;
		interval *= 2;
	}
	irt_g_maintenance_min_interval_slot = IRT_MAINTENANCE_SLOTS-1;
	irt_cond_var_init(&irt_g_maintenance_cond);
	irt_mutex_init(&irt_g_maintenance_mutex);
	// start maintenance thread
	irt_g_maintenance_thread_active = true;
	irt_thread_create(irt_maintenance_thread_func, NULL, &irt_g_maintenance_thread);
}

// cleanup maintenance data structures
void irt_maintenance_cleanup() {
	irt_g_maintenance_thread_active = false;
	irt_cond_wake_one(&irt_g_maintenance_cond);
	irt_thread_join(&irt_g_maintenance_thread);
	for(uint32 i=0; i<IRT_MAINTENANCE_SLOTS; ++i) {
		irt_spin_destroy(&irt_g_maintenance_events[i].lock);
	}
	irt_mutex_destroy(&irt_g_maintenance_mutex);
}

// register a new maintenance lambda
// note: memory is caller-managed!
void irt_maintenance_register(irt_maintenance_lambda *lam) {
	// determine slot to use
	uint64 interval = lam->interval/IRT_MAINTENANCE_MIN_INTERVAL;
	uint64 slot = _irt_maintenance_get_next_smallest_exponent_of_two(interval);
	IRT_ASSERT(slot<IRT_MAINTENANCE_SLOTS, IRT_ERR_INVALIDARGUMENT, 
		"Maintenance register requested for longer interval than available");
	
	// add event in slot
	irt_maintenance_slot *s = &irt_g_maintenance_events[slot];
	irt_spin_lock(&s->lock);
	lam->next = s->first_lambda;
	s->first_lambda = lam;
	irt_spin_unlock(&s->lock);
//printf("added event with desired interval %lu in slot %lu with interval %lu\n", lam->interval, slot, s->interval);

	// atomically update minimum interval slot if required
	for(;;) {
		uint64 mi = irt_g_maintenance_min_interval_slot;
		if(slot < mi) {
			if(irt_atomic_bool_compare_and_swap(&irt_g_maintenance_min_interval_slot, mi, slot)) {
//printf("set sleep slot from %lu to %lu\n", mi, slot);
				// wake maintenance thread, interval was reduced
				irt_cond_wake_one(&irt_g_maintenance_cond);
			}
		} else break;
	}

}

// maintenance thread function
void* irt_maintenance_thread_func(void *) {
	// consecutive maintenance counter to determine events to execute
	uint64 maintenance_count = 0;

	// main maintenance loop
	while(irt_g_maintenance_thread_active) { 
//printf("start iteration, mcount %lu, slot %u\n", maintenance_count, irt_g_maintenance_min_interval_slot);
		uint64 starttime_ns = irt_time_ns();
		uint64 current_min_interval = irt_g_maintenance_min_interval_slot;
		maintenance_count += (1<<current_min_interval);
//printf("new mcount %lu\n", maintenance_count);
	
		// execute maintenance events for this time step
		bool slot_occupied = false;
		for(uint32 i=current_min_interval; i<IRT_MAINTENANCE_SLOTS; ++i) {
			if(i == 0 || maintenance_count%(1<<(i)) == 0) {
//printf("slot %u should be checked\n", i);
				irt_maintenance_slot *s = &irt_g_maintenance_events[i];
				irt_spin_lock(&s->lock);
				irt_maintenance_lambda *lam = s->first_lambda, *prev = NULL;
				while(lam) {
					// execute lambda
					uint64 pre_interval = lam->interval;
					uint64 new_interval = lam->func(lam->data);
					lam->interval = new_interval;
					// return value 0 means stop this maintenance event
					if(new_interval == 0) {
						if(prev) prev->next = lam->next;
						else s->first_lambda = lam->next;
					} else
					// if there is a significant interval change, remove and re-register lambda 
					if(_irt_maintenance_get_next_smallest_exponent_of_two(new_interval/IRT_MAINTENANCE_MIN_INTERVAL) 
					!= _irt_maintenance_get_next_smallest_exponent_of_two(pre_interval/IRT_MAINTENANCE_MIN_INTERVAL)) { 
						if(prev) prev->next = lam->next;
						else s->first_lambda = lam->next;
						irt_maintenance_register(lam);
					}
					lam = lam->next;
				}
				irt_spin_unlock(&s->lock);
				
				// check if previous lowest occupied slot is now free, if so increase interval
				if(!slot_occupied) {
					if(i>current_min_interval) {
						if(irt_atomic_bool_compare_and_swap(&irt_g_maintenance_min_interval_slot, current_min_interval, i)) {
							// after increasing min interval slot, update maintenance count so as not to get out of sync
							maintenance_count += maintenance_count%(1<<irt_g_maintenance_min_interval_slot);
							// we set the current min interval, so we can adjust it
							current_min_interval = i;
						}
					} 
				}
				if(s->first_lambda != NULL) slot_occupied = true;
			}
		}

		// sleep until next required timestep
		uint64 maintenance_time_ns = (irt_time_ns() - starttime_ns);
		uint64 interval_ns = irt_g_maintenance_events[irt_g_maintenance_min_interval_slot].interval * 1000 * 1000;
		if(maintenance_time_ns < interval_ns) {
			uint64 sleeptime_ns = interval_ns - maintenance_time_ns;
//printf("go to sleep, slot: %u   interval: %lu   time: %lu ns\n", irt_g_maintenance_min_interval_slot, irt_g_maintenance_events[irt_g_maintenance_min_interval_slot].interval, sleeptime_ns);
			irt_mutex_lock(&irt_g_maintenance_mutex);
			irt_cond_timedwait(&irt_g_maintenance_cond, &irt_g_maintenance_mutex, sleeptime_ns);
			irt_mutex_unlock(&irt_g_maintenance_mutex);
//printf("woke up\n");
		} else {
			IRT_ASSERT(false, IRT_ERR_INTERNAL, "Maintenance time (%lu ns) exceeded required time slot (%lu ns)", maintenance_time_ns, interval_ns);
		}
	}
}

