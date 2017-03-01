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
#ifndef __GUARD_ABSTRACTION_IMPL_THREADS_UNIX_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_THREADS_UNIX_IMPL_H

// implementation of threads using pthread library; will be used for both Linux and Windows.

#include "abstraction/threads.h"
#include "error_handling.h"

#ifdef _GEMS_SIM
#include "include_gems/sys_time.h"
#else
#include <sys/time.h>
#endif

void irt_thread_create(irt_thread_func* fun, void* args, irt_thread* t) {
	irt_thread thread;
	if(t == NULL) {
		IRT_ASSERT(pthread_create(&thread, NULL, fun, args) == 0, IRT_ERR_INTERNAL, "Could not create worker thread %p", (void*)t);
	} else {
		IRT_ASSERT(pthread_create(t, NULL, fun, args) == 0, IRT_ERR_INTERNAL, "Could not create worker thread %p", (void*)t);
	}
}

void irt_thread_get_current(irt_thread* t) {
	*t = pthread_self();
}

void irt_thread_cancel(irt_thread* t) {
#ifdef _GEMS_SIM
	// TODO [_GEMS]: missing implementation of pthread_cancel
	IRT_WARN("irt_thread_cancel empty implementation\n");
	#else
	pthread_cancel(*t);
	#endif
}

int irt_thread_join(irt_thread* t) {
	return pthread_join(*t, NULL);
}

void irt_thread_exit(int exit_code) {
	pthread_exit(&exit_code);
}

void irt_thread_yield() {
	pthread_yield();
}

bool irt_thread_check_equality(irt_thread* t1, irt_thread* t2) {
#ifdef _WIN32
	return t1->p == t2->p;
	#else
	return pthread_equal(*t1, *t2);
	#endif
}


/* MUTEX FUNCTIONS ------------------------------------------------------------------- */

void irt_cond_var_init(irt_cond_var* cv) {
	pthread_cond_init(cv, NULL);
}

void irt_cond_var_destroy(irt_cond_var* cv) {
	pthread_cond_destroy(cv);
}

void irt_mutex_init(irt_mutex_obj* m) {
	pthread_mutex_init(m, NULL);
}

void irt_mutex_lock(irt_mutex_obj* m) {
	pthread_mutex_lock(m);
}

int irt_mutex_trylock(irt_mutex_obj* m) {
	return pthread_mutex_trylock(m);
}

void irt_mutex_unlock(irt_mutex_obj* m) {
	pthread_mutex_unlock(m);
}

void irt_mutex_destroy(irt_mutex_obj* m) {
	pthread_mutex_destroy(m);
}

void irt_cond_wake_all(irt_cond_var* cv) {
	// TODO [_GEMS]: is it still true? irt_cond_wake_one added since no irt_cond_wake_all implementation is available
	pthread_cond_broadcast(cv);
}

int irt_cond_wait(irt_cond_var* cv, irt_mutex_obj* m) {
	return pthread_cond_wait(cv, m);
}

int irt_cond_timedwait(irt_cond_var* cv, irt_mutex_obj* m, uint64 time_ns) {
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	uint64 new_nsecs = ts.tv_nsec + time_ns % (1000ULL * 1000 * 1000);
	uint64 new_secs = ts.tv_sec + time_ns / (1000ULL * 1000 * 1000) + new_nsecs / (1000ULL * 1000 * 1000);
	ts.tv_nsec = new_nsecs % (1000ULL * 1000 * 1000);
	ts.tv_sec = new_secs;
	return pthread_cond_timedwait(cv, m, &ts);
}

void irt_cond_wake_one(irt_cond_var* cv) {
	pthread_cond_signal(cv);
}

/* THREAD LOCAL STORAGE FUNCTIONS ------------------------------------------------------------------- */

int irt_tls_key_create(irt_tls_key* k) {
	return pthread_key_create(k, NULL);
}

void irt_tls_key_delete(irt_tls_key k) {
	pthread_key_delete(k);
}

void* irt_tls_get(irt_tls_key k) {
	return pthread_getspecific(k);
}

int irt_tls_set(irt_tls_key k, void* val) {
	return pthread_setspecific(k, val);
}


#endif // ifndef __GUARD_ABSTRACTION_IMPL_THREADS_UNIX_IMPL_H
