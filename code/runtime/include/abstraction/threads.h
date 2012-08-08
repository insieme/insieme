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

// for now, we are also working with pthreads under windows

#include "irt_inttypes.h"
#include "declarations.h"

#if defined(_MSC_VER) && !defined(IRT_USE_PTHREADS)
	typedef HANDLE irt_thread;
#else
	#include <pthread.h>
	typedef pthread_t irt_thread;
#endif


// typedef the signature of function executed by thread
typedef void* irt_thread_func(void*);

/** create a new thread executing fun with parameter args */
inline irt_thread irt_thread_create(irt_thread_func *fun, void *args);

/** returns irt_thread identifier of current thread */
inline irt_thread irt_current_thread();

/** requests cancelation of the given thread */
inline void irt_thread_cancel(irt_thread);

/** makes calling thread wait for cancellation of thread t, return value of terminated thread is returned */
inline int32 irt_thread_join(irt_thread t);