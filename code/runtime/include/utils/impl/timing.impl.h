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
 */

// platform independent implementations of timing functions

#pragma once
#ifndef __GUARD_UTILS_IMPL_TIMING_IMPL_H
#define __GUARD_UTILS_IMPL_TIMING_IMPL_H

#include "utils/timing.h"
#include "abstraction/impl/rdtsc.impl.h"
#include "irt_inttypes.h"
#include "utils/filesystem.h"
#include "irt_logging.h"

#ifdef _GEMS_SIM
#include "include_gems/impl/time.impl.h"
#endif

// ====== sleep functions ======================================

int irt_nanosleep_timespec(const struct timespec* wait_time) {
	return nanosleep(wait_time, NULL);
}

int irt_nanosleep(uint64 wait_time) {
	struct timespec t = {(time_t)(wait_time / 1000000000ull), (time_t)(wait_time % 1000000000ull)};
	return nanosleep(&t, NULL);
}

volatile int _irt_g_dummy_val;

void irt_busy_nanosleep(uint64 wait_time) {
	struct timeval tv;
	gettimeofday(&tv, 0);
	uint64 micro_before = tv.tv_sec * 1000000ull + tv.tv_usec;
	uint64 micro_now = micro_before;
	while(micro_before + wait_time / 1000ull > micro_now) {
		_irt_g_dummy_val++;
		gettimeofday(&tv, 0);
		micro_now = tv.tv_sec * 1000000ull + tv.tv_usec;
	}
}

void irt_busy_ticksleep(uint64 wait_ticks) {
	uint64 end = irt_time_ticks() + wait_ticks;
	while(irt_time_ticks() < end) {
		_irt_g_dummy_val++;
	}
}


// ====== time related functions ======================================

uint64 irt_time_ms() {
	struct timeval tv;
	uint64 time;
	gettimeofday(&tv, NULL);
	time = tv.tv_sec * 1000 + tv.tv_usec / 1000;
	return time;
}


uint64 irt_time_ns() { // ask philip if we can implement this using ticks
	struct timeval tv;
	uint64 time;
	gettimeofday(&tv, NULL);
	time = tv.tv_sec * 1000 * 1000 * 1000 + tv.tv_usec * 1000;
	return time;
}

// measures number of clock ticks over 100 ms, sets irt_g_time_ticks_per_sec and returns the value
uint64 irt_time_set_ticks_per_sec() {
	uint64 before = 0, after = 0;
	struct timespec time_before, time_after;
	clock_gettime(CLOCK_REALTIME, &time_before);
	before = irt_time_ticks();
	irt_busy_nanosleep(1000ull * 1000 * 100);
	after = irt_time_ticks();
	clock_gettime(CLOCK_REALTIME, &time_after);
	irt_g_time_ticks_per_sec =
	    (uint64)((after - before) * 1e9) / ((time_after.tv_sec * 1e9 + time_after.tv_nsec) - (time_before.tv_sec * 1e9 + time_before.tv_nsec));
	return irt_g_time_ticks_per_sec;
}

/*
 * sets irt_g_time_ticks_per_sec which is the reference clock count
 *
 * The function must be called twice, at startup and shutdown of the runtime. If there is a
 * file "insieme_reference_cpu_clocks" in the tmp dir, it will read and use this value. If
 * there is no such file, it will count the time and clocks at startup and shutdown of the
 * runtime (the runtime's run time is extended up to at least 1 second to increase the
 * accuracy), compute the value, write the file and set irt_g_time_ticks_per_sec.
 */

uint64 irt_time_ticks_per_sec_calibration_mark() {
#ifdef _GEMS_SIM
	irt_g_time_ticks_per_sec = GEMS_CORE_FREQ_MHZ * 1e6;
	return irt_g_time_ticks_per_sec;
	#else
	static uint64 before = 0;
	// static struct timespec time_before;
	static struct timeval time_before;

	if(before == 0) {
		if(irt_time_ticks_constant()) {
			irt_log_setting_s("irt_time_ticks_constant", "yes");
		} else {
			irt_log_setting_s("irt_time_ticks_constant", "no");
		}
		before = irt_time_ticks(); // has a resolution of 1 clock cycle (yay!)
		// clock_gettime(CLOCK_REALTIME, &time_before); // has a resolution of 256 nanoseconds in linux, but there's nothing better...
		gettimeofday(&time_before, NULL);
		return 0;
	} else {
		char path[4096];
		FILE* temp_time_file;
		int retval = 0;
		uint64 reference_clock = 0;

		sprintf(path, "%s/insieme_reference_cpu_clocks", irt_get_tmp_dir());

		if((temp_time_file = fopen(path, "r")) != NULL) {
			if((retval = fscanf(temp_time_file, "%" PRIu64, &reference_clock)) == 1) {
				if(reference_clock >= 1e6 && reference_clock <= 1e11) { // 1MHz < reference_clock < 100 GHz
					irt_g_time_ticks_per_sec = reference_clock;
					fclose(temp_time_file);
					irt_log_comment("Using stored reference clock");
					return irt_g_time_ticks_per_sec;
				}
			}
			fclose(temp_time_file);
		}

		uint64 after = irt_time_ticks();
		// struct timespec time_after;
		struct timeval time_after;
		// clock_gettime(CLOCK_REALTIME, &time_after);
		gettimeofday(&time_after, NULL);

		// check the run time R of the runtime, if R was below a second, wait additional 1 - R seconds to increase accuracy of measurement
		uint64 elapsed_time = ((time_after.tv_sec * 1e6 + time_after.tv_usec) - (time_before.tv_sec * 1e6 + time_before.tv_usec));
		if(elapsed_time < 1e6) {
			irt_busy_nanosleep((1e6 - elapsed_time) * 1e3);
			after = irt_time_ticks();
			// clock_gettime(CLOCK_REALTIME, &time_after);
			gettimeofday(&time_after, NULL);
		}

		irt_g_time_ticks_per_sec =
		    (uint64)((after - before) * 1e6) / ((time_after.tv_sec * 1e6 + time_after.tv_usec) - (time_before.tv_sec * 1e6 + time_before.tv_usec));

		if((temp_time_file = fopen(path, "w")) != NULL) {
			fprintf(temp_time_file, "%" PRIu64, irt_g_time_ticks_per_sec);
			fclose(temp_time_file);
		}

		return irt_g_time_ticks_per_sec;
	}
	#endif
}

// converts clock ticks to nanoseconds
uint64 irt_time_convert_ticks_to_ns(uint64 ticks) {
	return (uint64)(ticks / ((double)irt_g_time_ticks_per_sec / 1000000000));
}


#endif // ifndef __GUARD_UTILS_IMPL_TIMING_IMPL_H
