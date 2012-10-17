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

// prototype of functions using rdtsc
#include "abstraction/rdtsc.h"

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
