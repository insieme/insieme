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
 */

#pragma once
#ifndef __GUARD_ABSTRACTION_THREADS_H
#define __GUARD_ABSTRACTION_THREADS_H


#include "irt_inttypes.h"
//#include "declarations.h"
#include "abstraction/spin_locks.h"

#if defined(_WIN32) && !defined(IRT_USE_PTHREADS)
#include <Windows.h> // keep this or Visual Studio Compiler goes nuts

struct _irt_thread {
	DWORD thread_id;      // uniquely identifies a thread
	HANDLE thread_handle; // just a reference to the thread (multiple handles can refer to the same thread)
};

typedef struct _irt_thread irt_thread;

// Vista and up will use slim reader writer mutex instead of critical section, condition variables are supported too
#if(WINVER >= 0x0600)
typedef SRWLOCK irt_mutex_obj;
typedef CONDITION_VARIABLE irt_cond_var;
#else
typedef int32 irt_cond_var; // dummy typedef such that interface below may stay untouched
typedef HANDLE irt_mutex_obj;
#endif

typedef uint32 irt_tls_key;
#else
#include <pthread.h>
typedef pthread_t irt_thread;
typedef pthread_cond_t irt_cond_var;
typedef pthread_mutex_t irt_mutex_obj;
typedef pthread_key_t irt_tls_key;
#endif

typedef struct _irt_cond_bundle {
	irt_cond_var condvar;
	irt_mutex_obj mutex;
} irt_cond_bundle;

// typedef the signature of function executed by thread
typedef void* irt_thread_func(void*);

/** create a new thread executing fun with parameter args, info about new thread will be saved in t if t is not NULL */
inline void irt_thread_create(irt_thread_func* fun, void* args, irt_thread* t);

/** saves thread information of current thread in t  */
inline void irt_thread_get_current(irt_thread* t);

/** requests cancelation of the given thread */
inline void irt_thread_cancel(irt_thread*);

/** makes calling thread wait for cancellation of thread t, return value of terminated thread is returned */
inline int irt_thread_join(irt_thread* t);

/** exit a thread with specified exit code */
inline void irt_thread_exit(int exit_code);

/** calling thread relinquishes the CPU */
inline void irt_thread_yield();

/** check if two thread objects are equal */
bool irt_thread_check_equality(irt_thread* t1, irt_thread* t2);


/* MUTEX FUNCTIONS ------------------------------------------------------------------- */

/** initialize mutex object */
inline void irt_mutex_init(irt_mutex_obj*);

/** acquire mutex object */
inline void irt_mutex_lock(irt_mutex_obj*);

/** try to acquire mutex object not waiting until mutex is acquired, returns 0 on success, nonzero otherwise */
int irt_mutex_trylock(irt_mutex_obj*);

/** release mutex object */
inline void irt_mutex_unlock(irt_mutex_obj*);

/** destroy the mutex object */
inline void irt_mutex_destroy(irt_mutex_obj*);

/** wake all threads which slept on the condition variable */
inline void irt_cond_wake_all(irt_cond_var*);

/** initialize the condition variable */
inline void irt_cond_var_init(irt_cond_var*);

/** destroy the condition variable */
inline void irt_cond_var_destroy(irt_cond_var*);

/** releases the mutex and sleeps the thread on the condition variable */
inline int irt_cond_wait(irt_cond_var*, irt_mutex_obj*);

/** releases the mutex and sleeps the thread on the condition variable, for a maximum amount of time */
inline int irt_cond_timedwait(irt_cond_var*, irt_mutex_obj*, uint64);

/** singal and wake a thread which is blocked by the condition variable cv */
inline void irt_cond_wake_one(irt_cond_var* cv);

/** initialize the condition variable and associated mutex */
inline void irt_cond_bundle_init(irt_cond_bundle*);

/** releases the mutex and sleeps the thread on the condition variable */
inline int irt_cond_bundle_wait(irt_cond_bundle*);

/** destroys the condition variable and associated mutex */
inline void irt_cond_bundle_destroy(irt_cond_bundle*);

/* THREAD LOCAL STORAGE FUNCTIONS ------------------------------------------------------------------- */

/** creates a new thread local storage key at location k */
inline int irt_tls_key_create(irt_tls_key* k);

/** delete key k, if value is pointer to memory location, caller is responsible for freeing it */
inline void irt_tls_key_delete(irt_tls_key k);

/** get the thread local value for key k */
inline void* irt_tls_get(irt_tls_key k);

/** set the thread local value for key k */
inline int irt_tls_set(irt_tls_key k, void* val);


#endif // ifndef __GUARD_ABSTRACTION_THREADS_H
