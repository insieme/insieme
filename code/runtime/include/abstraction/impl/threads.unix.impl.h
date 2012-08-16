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

#include "abstraction/threads.h"
#include "error_handling.h"

irt_thread irt_thread_create(irt_thread_func *fun, void *args) {
	irt_thread thread;
	IRT_ASSERT(pthread_create(&thread, NULL, fun, args) == 0, IRT_ERR_INTERNAL, "Could not create worker thread");
	return thread;
}

irt_thread irt_current_thread() {
	return pthread_self();
}

void irt_thread_cancel(irt_thread t){
	pthread_cancel(t);
}

int irt_thread_join(irt_thread t){
	int32 return_val;
	int32 *p_ret_val = &return_val;
	pthread_join(t, (void**)&p_ret_val);
	return return_val;
}

void irt_thread_exit(int exit_code){
	pthread_exit(&exit_code);
}


/* SPIN LOCK FUNCTIONS ------------------------------------------------------------------- */

void irt_spin_lock(irt_spinlock *lock){
	pthread_spin_lock(lock);
}

void irt_spin_unlock(irt_spinlock *lock){
	pthread_spin_unlock(lock);
}

int irt_spin_init(irt_spinlock *lock){
	return pthread_spin_init(lock, PTHREAD_PROCESS_PRIVATE);
}

void irt_spin_destroy(irt_spinlock *lock){
	pthread_spin_destroy(lock);
}


/* MUTEX FUNCTIONS ------------------------------------------------------------------- */

void irt_cond_var_init(irt_cond_var *cv) {
	 pthread_cond_init(cv, NULL);
}

void irt_mutex_init(irt_lock_obj *m){
	pthread_mutex_init(m, NULL);
}

void irt_mutex_lock(irt_lock_obj *m){
	pthread_mutex_lock(m);
}

int irt_mutex_trylock(irt_lock_obj* m){
	return pthread_mutex_trylock(m);
}

void irt_mutex_unlock(irt_lock_obj *m){
	pthread_mutex_unlock(m);
}

void irt_mutex_destroy(irt_lock_obj *m){
	pthread_mutex_destroy(m);
}

void irt_cond_wake_all(irt_cond_var *cv){
	pthread_cond_broadcast(cv);
}

int irt_cond_wait(irt_cond_var *cv, irt_lock_obj *m){
	return pthread_cond_wait(cv, m);
}

void irt_cond_wake_one(irt_cond_var *cv){
	pthread_cond_signal(cv);
}


/* THREAD LOCAL STORAGE FUNCTIONS ------------------------------------------------------------------- */

int irt_tls_key_create(irt_tls_key* k){
	return pthread_key_create(k, NULL);
}

void irt_tls_key_delete(irt_tls_key k) {
	pthread_key_delete(k);
}

void* irt_tls_get(irt_tls_key k){
	return pthread_getspecific(k);
}

int irt_tls_set(irt_tls_key k, void *val){
	return pthread_setspecific(k, val);
}


