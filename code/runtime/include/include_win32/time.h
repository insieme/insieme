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

/*
* Implementations of unix time functions (as in <time.h>) for windows, such that original
* functionality using those functions may remain untouched
*/

#pragma once

#include "irt_inttypes.h"
#include "error_handling.h"
#include <Windows.h>

// there is a definition for timespec in pthread.h
#if !defined(_TIMESPEC_DEFINED)
#define _TIMESPEC_DEFINED
struct timespec {
	long  tv_sec;         /* seconds */
	long  tv_nsec;        /* nanoseconds */
};
#endif

/* implementation of nanosleep for windows, which is has a resolution of 1ms at best */
int nanosleep(const struct timespec *rqtp, struct timespec *rmtp){
	// there is nothing which is much more accurate than Sleep, see: http://stackoverflow.com/questions/7827062/is-there-a-windows-equivalent-of-nanosleep
	Sleep(rqtp->tv_sec*1000 + rqtp->tv_nsec*0.000001);
	return 0;
}

int clock_gettime(int X, struct timespec *ts) 
{ 
	IRT_ASSERT(X == CLOCK_REALTIME, IRT_ERR_INVALIDARGUMENT, "clock_getttime for windows only allows CLOCK_REALTIME");

    LARGE_INTEGER counter; 
    double nseconds; 
    static double incrementsPerNanoSec;
    static int initialized = 0; 
 
	// QueryPerformanceFrequency should only be called once, since it never changes
    if (!initialized) { 
        LARGE_INTEGER performanceFrequency; 
        initialized = 1;
        QueryPerformanceFrequency(&performanceFrequency); // this gives a quote how many times per second the high performance counter is incremented
        incrementsPerNanoSec = (double)performanceFrequency.QuadPart / 1e9; 
    }

	// get counter value
    QueryPerformanceCounter(&counter);
 
    nseconds = (double)counter.QuadPart / incrementsPerNanoSec; 
    counter.QuadPart = nseconds; 
    ts->tv_sec = counter.QuadPart / 1e9; 
    ts->tv_nsec = counter.QuadPart % 1000000000;
    return (0);
} 
