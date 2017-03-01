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
#ifndef __GUARD_UTILS_TIMING_H
#define __GUARD_UTILS_TIMING_H

// prototype of functions using rdtsc
#include "abstraction/rdtsc.h"

#ifdef _WIN32
#define CLOCK_REALTIME 1 // just to have some definition for CLOCK_REALTIME
#include "include_win32/time.h"
#include "include_win32/sys_time.h"
#elif defined _GEMS_SIM
#include "include_gems/time.h"
#include "include_gems/sys_time.h"
#else
#include <time.h>
#include <unistd.h>
#include <sys/time.h>
#endif


/*
// at this point it could still be, that CLOCK_REALTIME is not defined (eg with MinGWs time.h)
#ifndef CLOCK_REALTIME
    #define CLOCK_REALTIME 1
#endif

// same holds for struct timespec
#if !defined(_TIMESPEC_DEFINED)
#define _TIMESPEC_DEFINED
struct timespec {
    long  tv_sec;         // seconds
    long  tv_nsec;        // nanoseconds
};
#endif
*/


uint64 irt_g_time_ticks_per_sec = 0;

// ====== sleep functions ======================================

int irt_nanosleep_timespec(const struct timespec* wait_time);

int irt_nanosleep(uint64 wait_time);

void irt_busy_nanosleep(uint64 wait_time);

void irt_busy_ticksleep(uint64 wait_ticks);

// ====== clock cycle measurements ======================================

// get timespan from epoch in ms
uint64 irt_time_ms();

// get timespan from epoch in ns
uint64 irt_time_ns();

// measures number of clock ticks over 100 ms, sets irt_g_time_ticks_per_sec and returns the value
uint64 irt_time_set_ticks_per_sec();

/*
 * sets irt_g_time_ticks_per_sec which is the reference clock count
 *
 * The function must be called twice, at startup and shutdown of the runtime. If there is a
 * file "insieme_reference_cpu_clocks" in the tmp dir, it will read and use this value. If
 * there is no such file, it will count the time and clocks at startup and shutdown of the
 * runtime (the runtime's run time is extended up to at least 1 second to increase the
 * accuracy), compute the value, write the file and set irt_g_time_ticks_per_sec.
 */
uint64 irt_time_ticks_per_sec_calibration_mark();

// converts clock ticks to nanoseconds
uint64 irt_time_convert_ticks_to_ns(uint64 ticks);


#endif // ifndef __GUARD_UTILS_TIMING_H
