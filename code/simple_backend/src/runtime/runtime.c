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

/**
 * The Insieme simple_backend runtime system implementation.
 */

#include "insieme/simple_backend/runtime/runtime.h"
#include "insieme/simple_backend/runtime/isbr_barrier.h"

#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>

#include <pthread.h>

#define PTHREADCHECK(_test) { int res = _test; assert(res==0); }

#define NUMTHREADS_ENVVAR "ISBR_NUMTHREADS"

// -------------------------------------------------------------------------------------- internals declarations

struct _isbr_threadGroupImpl {
	unsigned numThreads;
	pthread_t* threads;
	isbr_barrier_t barrier;
} isbr_ThreadGroupImpl;

pthread_key_t isbr_getJobArgKey();
void* isbr_jobRunner(void *arg);
unsigned isbr_determineThreadNum(isbr_Job* jobDescription);

// -------------------------------------------------------------------------------------- public function implementation

unsigned isbr_getMaxThreads() {
	// not defined in all Pthread implementations, just return constant for now
	return 1<<16;
}

isbr_ThreadGroup isbr_parallel(isbr_Job* jobDescription) {
	// perform initialization
	isbr_getJobArgKey();

	unsigned numThreads = isbr_determineThreadNum(jobDescription);
	isbr_JobArgs* args = (isbr_JobArgs*) calloc(numThreads, sizeof(isbr_JobArgs));
	isbr_ThreadGroup threadGroup = (isbr_ThreadGroup) calloc(1, sizeof(isbr_ThreadGroupImpl));

	// initialize threadgroup
	threadGroup->numThreads = numThreads;
	isbr_barrier_init(&threadGroup->barrier, numThreads);
	threadGroup->threads = (pthread_t*) calloc(numThreads, sizeof(pthread_t));
	for(unsigned i=0; i<numThreads; ++i) {
		args[i].index = i;
		args[i].size = numThreads;
		args[i].context = jobDescription;
		args[i].group = threadGroup;
	}
	pthread_attr_t attr;
	pthread_attr_init(&attr);

	// Start threads, don't be tempted to join this with the loop above
	for(unsigned i=0; i<numThreads; ++i) {
		pthread_create(&(threadGroup->threads[i]), &attr, &isbr_jobRunner, &args[i]);
	}
	return threadGroup;
}

void isbr_merge(isbr_ThreadGroup group) {
	for(unsigned i=0; i<group->numThreads; ++i) {
		pthread_join(group->threads[i], NULL);
	}
}

void isbr_barrier(isbr_ThreadGroup group) {
	if (!group) return;
	isbr_barrier_wait(&group->barrier);
}

unsigned isbr_getThreadId(unsigned level) {
	// TODO: implement level
	isbr_JobArgs* args = (isbr_JobArgs*)pthread_getspecific(isbr_getJobArgKey());
	if (args) {
		return args->index;
	}
	return 0;
}

unsigned isbr_getGroupSize(unsigned level) {
	// TODO: implement level
	isbr_JobArgs* args = (isbr_JobArgs*)pthread_getspecific(isbr_getJobArgKey());
	if (args) {
		return args->size;
	}
	return 0;
}

isbr_ThreadGroup isbr_getThreadGroup(unsigned level) {
	// TODO: implement level
	isbr_JobArgs* args = ((isbr_JobArgs*)pthread_getspecific(isbr_getJobArgKey()));
	if (args) {
		return args->group;
	}
	return 0;
}

void isbr_pfor(isbr_ThreadGroup group, isbr_PForRange range, void (*fun)(isbr_PForRange range)) {

	// if not within parallel context
	if (!group) {
		(*fun)(range);
		return;
	}

	// TODO: implement group other than current
	// stupid non-normalized loops
	isbr_PForRange myRange;
	// TODO handle stupid corner cases
	long long numit = (range.end - range.start) / (range.step);
	long long chunk = numit / group->numThreads;
	myRange.context = range.context;
	myRange.start = range.start + isbr_getThreadId(0) * chunk * range.step;
	myRange.end = myRange.start + chunk * range.step;
	myRange.step = range.step;
	if(isbr_getThreadId(0) == isbr_getGroupSize(0)-1) myRange.end = range.end;
	(*fun)(myRange);
}

// -------------------------------------------------------------------------------------- internals implementation

pthread_key_t isbr_getJobArgKey() {
	static bool initialized = false;
	static pthread_key_t jobArgKey;
	if(!initialized) {
		PTHREADCHECK(pthread_key_create(&jobArgKey, NULL /* destructor */));
		initialized = true;
	}
	return jobArgKey;
}

void* isbr_jobRunner(void *arg) {
	isbr_JobArgs *jobArgs = (isbr_JobArgs*)arg;
	PTHREADCHECK(pthread_setspecific(isbr_getJobArgKey(), arg));
	jobArgs->context->fun(jobArgs->context);
	return NULL;
}


unsigned isbr_determineThreadNum(isbr_Job* jobDescription) {
	char* envThreadNumStr = getenv(NUMTHREADS_ENVVAR);
	unsigned threadNum = 1;
	if(envThreadNumStr) {
		threadNum = atoi(envThreadNumStr);
	}
	if(threadNum>jobDescription->max) threadNum = jobDescription->max;
	if(threadNum<jobDescription->min) threadNum = jobDescription->min;
	return threadNum;
}



isbr_lock* isbr_lock_create() {
	isbr_lock* ret = (isbr_lock*)malloc(sizeof(isbr_lock));
	ret->locked = 0;
	return ret;
}

void isbr_lock_acquire(isbr_lock* lock) {
	while(!__sync_bool_compare_and_swap(&lock->locked, 0, 1)) { /* busy waiting */ }
}

void isbr_lock_release(isbr_lock* lock) {
	lock->locked = 0;
}
