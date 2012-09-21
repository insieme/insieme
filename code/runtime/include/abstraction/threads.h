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


#include "irt_inttypes.h"
//#include "declarations.h"

#if defined(_MSC_VER) && !defined(IRT_USE_PTHREADS)
	#include <Windows.h> // keep this or Visual Studio Compiler goes nuts

	struct _irt_thread {
		DWORD thread_id; // uniquely identifies a thread
		HANDLE thread_handle; // just a reference to the thread (multiple handles can refer to the same thread)
	};

	typedef _irt_thread irt_thread;
	typedef long irt_spinlock;

	// Vista and up will use slim reader writer lock instead of critical section, condition variables are supported too
	#if (WINVER >= 0x0600)
		typedef SRWLOCK irt_lock_obj;
		typedef CONDITION_VARIABLE irt_cond_var;
	#else
		typedef int32 irt_cond_var; // dummy typedef such that interface below may stay untouched
		typedef HANDLE irt_lock_obj;
	#endif

	typedef uint32 irt_tls_key;
#else
	#include <pthread.h>
	typedef pthread_t irt_thread;
	typedef pthread_spinlock_t irt_spinlock;
	typedef pthread_cond_t irt_cond_var;
	typedef pthread_mutex_t irt_lock_obj;
	typedef pthread_key_t irt_tls_key;
#endif


// typedef the signature of function executed by thread
typedef void* irt_thread_func(void*);

/** create a new thread executing fun with parameter args, info about new thread will be saved in t if t is not NULL */
inline void irt_thread_create(irt_thread_func *fun, void *args, irt_thread* t);

/** saves thread information of current thread in t  */
inline void irt_thread_get_current(irt_thread *t);

/** requests cancelation of the given thread */
inline void irt_thread_cancel(irt_thread*);

/** makes calling thread wait for cancellation of thread t, return value of terminated thread is returned */
inline int irt_thread_join(irt_thread *t);

/** exit a thread with specified exit code */
inline void irt_thread_exit(int exit_code);

/** check if two thread objects are equal */
bool irt_thread_check_equality(irt_thread *t1, irt_thread *t2);


/* SPIN LOCK FUNCTIONS ------------------------------------------------------------------- */

/** spin until lock is acquired */
inline void irt_spin_lock(irt_spinlock *lock);

/** release lock */
inline void irt_spin_unlock(irt_spinlock *lock);

/** initializing spin lock variable puts it in state unlocked. lock variable can not be shared by different processes */
inline int irt_spin_init(irt_spinlock *lock);

/**	destroy lock variable and free all used resources,
	will cause an error when attempting to destroy an object which is in any state other than unlocked */
inline void irt_spin_destroy(irt_spinlock *lock);



/* MUTEX FUNCTIONS ------------------------------------------------------------------- */

/** initialize lock object */
inline void irt_mutex_init(irt_lock_obj*);

/** acquire lock object */
inline void irt_mutex_lock(irt_lock_obj*);

/** try to acquire lock object not waiting until lock is acquired, returns 0 on success, nonzero otherwise */
int irt_mutex_trylock(irt_lock_obj*);

/** release lock object */
inline void irt_mutex_unlock(irt_lock_obj*);

/** destroy the lock object */
inline void irt_mutex_destroy(irt_lock_obj*);

/** wake all threads which slept on the condition variable */
inline void irt_cond_wake_all(irt_cond_var*);

/** initialize the condition variable */
inline void irt_cond_var_init(irt_cond_var*);

/** releases the lock and sleeps the thread on the condition variable */
inline int irt_cond_wait(irt_cond_var*, irt_lock_obj*);

/** singal and wake a thread which is blocked by the condition variable cv */
inline void irt_cond_wake_one(irt_cond_var *cv);


/* THREAD LOCAL STORAGE FUNCTIONS ------------------------------------------------------------------- */

/** creates a new thread local storage key at location k */
inline int irt_tls_key_create(irt_tls_key* k);

/** delete key k, if value is pointer to memory location, caller is responsible for freeing it */
inline void irt_tls_key_delete(irt_tls_key k);

/** get the thread local value for key k */
inline void* irt_tls_get(irt_tls_key k);

/** set the thread local value for key k */
inline int irt_tls_set(irt_tls_key k, void *val);
