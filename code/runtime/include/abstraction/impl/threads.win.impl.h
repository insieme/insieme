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
#ifndef __GUARD_ABSTRACTION_IMPL_THREADS_WIN_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_THREADS_WIN_IMPL_H

#include "abstraction/threads.h"
#include "error_handling.h"

#define IRT_SPIN_LOCKED 1
#define IRT_SPIN_UNLOCKED 0
#define IRT_SPIN_DESTROYED -1 // makes lock variable unusable

// little helper struct to pass function and parameter to irt_win_thread_func
typedef struct _irt_win_thread_params {
	irt_thread_func* fun; // function which shall be executed by (new) thread
	void* args;           // parameter for function fun
} irt_win_thread_params;

/*  wrapper function which admits to the required signature for CreateThread
    it simply calls original irt_thread_func function with according parameter
*/
DWORD WINAPI _irt_win_thread_func(void* params) {
	irt_win_thread_params* p = (irt_win_thread_params*)params;
	p->fun(p->args);
	free(params);
	// the void* retruned by fun is never used anyway and the WINAPI required that a DWORD value is returned
	// so we return zero (and not a cast of the void* returned by fun)
	return 0;
}

void irt_thread_create(irt_thread_func* fun, void* args, irt_thread* t) {
	irt_win_thread_params* params = (irt_win_thread_params*)malloc(sizeof(irt_win_thread_params));
	params->fun = fun;
	params->args = args;
	HANDLE thread_handle;
	if(t == NULL) {
		thread_handle = CreateThread(NULL, 0, _irt_win_thread_func, params, 0, NULL);
	} else {
		thread_handle = CreateThread(NULL, 0, _irt_win_thread_func, params, 0, &(t->thread_id));
		t->thread_handle = thread_handle;
	}

	IRT_ASSERT(thread_handle != NULL, IRT_ERR_INTERNAL, "Could not create worker thread");
}

void irt_thread_get_current(irt_thread* t) {
	HANDLE real_handle = NULL;
	HANDLE proc_handle = GetCurrentProcess();
	DuplicateHandle(proc_handle, GetCurrentThread(), proc_handle, &real_handle, 0, TRUE, DUPLICATE_SAME_ACCESS);
	t->thread_handle = real_handle;
	t->thread_id = GetCurrentThreadId();
}

void irt_thread_cancel(irt_thread* t) {
	/*
	from http://msdn.microsoft.com/en-us/library/windows/desktop/ms686717(v=vs.85).aspx :
	TerminateThread can result in the following problems:
	- If the target thread owns a critical section, the critical section will not be released.
	- If the target thread is allocating memory from the heap, the heap lock will not be released.
	- If the target thread is executing certain kernel32 calls when it is terminated, the kernel32 state for the thread's process could be inconsistent.
	- If the target thread is manipulating the global state of a shared DLL, the state of the DLL could be destroyed, affecting other users of the DLL.

	we don't care about all those issues since this function is only called in case of an error and the program
	should exit
	*/
	TerminateThread(t->thread_handle, -1);
}

int irt_thread_join(irt_thread* t) {
	return WaitForSingleObject(t->thread_handle, INFINITE);
}

void irt_thread_exit(int exit_code) {
	ExitThread(exit_code);
}

void irt_thread_yield() {
	IRT_WARN("irt_thread_yield empty implementation");
}

bool irt_thread_check_equality(irt_thread* t1, irt_thread* t2) {
	return t1->thread_id == t2->thread_id;
}

/* MUTEX FUNCTIONS ------------------------------------------------------------------- */

// condition variables are supported in WINDOWS Vista and up, compiling on older platforms will raise an error
// if IRT_WORKER_SLEEPING is defined
void irt_cond_var_init(irt_cond_var* cv) {
#if(WINVER < 0x0600)
#ifdef IRT_WORKER_SLEEPING
#error "IRT_WORKER_SLEEPING is not supported on Windows platforms older than Vista (condition variables unsupported)"
#endif
// NOOP
#else
	InitializeConditionVariable(cv);
#endif
}

void irt_cond_var_destroy(irt_cond_var* cv) {
	// there is no WinAPI call
}

void irt_cond_wake_all(irt_cond_var* cv) {
#if(WINVER < 0x0600)
#ifdef IRT_WORKER_SLEEPING
#error "IRT_WORKER_SLEEPING is not supported on Windows platforms older than Vista (condition variables unsupported)"
#endif
// NOOP
#else
	WakeAllConditionVariable(cv);
#endif
}

int irt_cond_wait(irt_cond_var* cv, irt_mutex_obj* m) {
#if(WINVER < 0x0600)
#ifdef IRT_WORKER_SLEEPING
#error "IRT_WORKER_SLEEPING is not supported on Windows platforms older than Vista (condition variables unsupported)"
#else
	return 0;
#endif
#else
	return !SleepConditionVariableSRW(cv, m, INFINITE, 0);
#endif
}

void irt_cond_wake_one(irt_cond_var* cv) {
#if(WINVER < 0x0600)
#ifdef IRT_WORKER_SLEEPING
#error "IRT_WORKER_SLEEPING is not supported on Windows platforms older than Vista (condition variables unsupported)"
#endif
// NOOP
#else
	WakeConditionVariable(cv);
#endif
}

// Windows Vista and up use slim reader writer locks
#if(WINVER >= 0x0600)

void irt_mutex_init(irt_mutex_obj* m) {
	InitializeSRWLock(m);
}

void irt_mutex_lock(irt_mutex_obj* m) {
	AcquireSRWLockExclusive(m);
}

int irt_mutex_trylock(irt_mutex_obj* m) {
	return !TryAcquireSRWLockExclusive(m);
}

void irt_mutex_unlock(irt_mutex_obj* m) {
	ReleaseSRWLockExclusive(m);
}

void irt_mutex_destroy(irt_mutex_obj* m) {
	// there is no WinAPI call
}

// everything below Vista will use Semaphores
// Semaphores differ from a Mutex and a CriticalSection object in such a way: if a thread has currently
// decremented the semaphore count to zero, and irt_mutex_lock is called again, that thread will be blocked
// The opposite is true for Mutex and CriticalSection: If a thread already owns the Mutex/CriticalSection and it calls irt_mutex_lock again,
// it won't be blocked
#else

void irt_mutex_init(irt_mutex_obj* m) {
	*m = CreateSemaphore(NULL, 1, 1, NULL);
}

void irt_mutex_lock(irt_mutex_obj* m) {
	WaitForSingleObject(*m, INFINITE); // decreases semaphore count by 1 if it is greater than 0, else waits until it becomes greater than 0
}

int irt_mutex_trylock(irt_mutex_obj* m) {
	DWORD res = WaitForSingleObject(*m, 1); // wait 1ms for m being signalled, then return
	if(res == WAIT_OBJECT_0) {
		return 0;
	} else {
		return -1;
	}
}

void irt_mutex_unlock(irt_mutex_obj* m) {
	ReleaseSemaphore(*m, 1, NULL); // increase semaphore count by 1
}

void irt_mutex_destroy(irt_mutex_obj* m) {
	CloseHandle(*m);
}

#endif


/* THREAD LOCAL STORAGE FUNCTIONS ------------------------------------------------------------------- */

int irt_tls_key_create(irt_tls_key* k) {
	*k = TlsAlloc();
	if(*k == TLS_OUT_OF_INDEXES) { return -1; }
	return 0;
}

void irt_tls_key_delete(irt_tls_key k) {
	TlsFree(k);
}

void* irt_tls_get(irt_tls_key k) {
	return TlsGetValue(k);
}

int irt_tls_set(irt_tls_key k, void* val) {
	return !TlsSetValue(k, val);
}

#endif // ifndef __GUARD_ABSTRACTION_IMPL_THREADS_WIN_IMPL_H
