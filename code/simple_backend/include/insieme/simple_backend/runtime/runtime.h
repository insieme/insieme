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

// Insieme simple runtime

#pragma once

struct _isbr_job;
struct _isbr_threadGroupImpl;

typedef struct _isbr_threadGroupImpl* isbr_ThreadGroup;

typedef struct _isbr_jobArgs {
	unsigned index, size;
	struct _isbr_job* context;
	isbr_ThreadGroup group;
} isbr_JobArgs;

typedef struct _isbr_job {
	unsigned structSize;
	unsigned min, max;
	void (*fun)(struct _isbr_job*);
} isbr_Job;

typedef struct _isbr_pforRange {
	long long start, end, step;
	void* context;
} isbr_PForRange;


isbr_ThreadGroup isbr_parallel(isbr_Job*);

void isbr_merge(isbr_ThreadGroup group);
void isbr_barrier(isbr_ThreadGroup group);

void isbr_merge_all();

unsigned isbr_getMaxThreads();
unsigned isbr_getThreadId(unsigned level);
unsigned isbr_getGroupSize(unsigned level);
isbr_ThreadGroup isbr_getThreadGroup(unsigned level);

void isbr_pfor(isbr_ThreadGroup group, isbr_PForRange range, void (*fun)(isbr_PForRange range));

#define ISBR_FLUSH(_bla) __sync_synchronize()

#define par_printf printf

// -- locks --

typedef struct _isbr_lock {
	int locked;
} isbr_lock;

isbr_lock* isbr_lock_create();
void isbr_lock_acquire(isbr_lock* lock);
void isbr_lock_release(isbr_lock* lock);


