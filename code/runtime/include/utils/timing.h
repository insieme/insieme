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

#include <time.h>
#include <unistd.h>
#include <sys/time.h>
#include "irt_inttypes.h"

uint64 irt_g_time_ticks_per_sec = 0;

uint64 irt_time_ms() {
	struct timeval tv;
	uint64 time;
	gettimeofday(&tv, 0);
	time = tv.tv_sec * 1000 + tv.tv_usec/1000;
	return time;
}

// ====== sleep functions ======================================

#ifdef WIN32
struct timespec {
    long  tv_sec;         /* seconds */
	long  tv_nsec;        /* nanoseconds */
};

int irt_nanosleep_timespec(const struct timespec* wait_time) {
	Sleep(wait_time->tv_sec*1000 + wait_time->tv_nsec/1000);
	return 0; // it seems that Sleep always succeedes
}
#else
int irt_nanosleep_timespec(const struct timespec* wait_time) {
	return nanosleep(wait_time, NULL);
}
#endif

int irt_nanosleep(uint64 wait_time) {
	struct timespec t = {wait_time/1000000000ull, wait_time%1000000000ull};
	return nanosleep(&t, NULL);
}

volatile int _irt_g_dummy_val;

void irt_busy_nanosleep(uint64 wait_time) {
	struct timeval tv;
	gettimeofday(&tv, 0);
	uint64 micro_before = tv.tv_sec * 1000000ull + tv.tv_usec;
	uint64 micro_now = micro_before;
	while(micro_before + wait_time/1000ull > micro_now) {
		_irt_g_dummy_val++;
		gettimeofday(&tv, 0);
		micro_now = tv.tv_sec * 1000000ull + tv.tv_usec;
	}
}

// ====== clock cycle measurements ======================================
//
// supporting x86_64 and ppc
//
// no general variant by design to raise 
// compiler errors in case of a new architecture

// ====== AMD64 (X86_64) machines ===========================

#if defined __x86_64__

uint64 irt_time_ticks(void) {
	volatile uint64 a, d;
	__asm__ __volatile__("rdtsc" : "=a" (a), "=d" (d));
	return (a | (d << 32));
}

// checks if rdtsc instruction is available
bool irt_time_ticks_available() {
	volatile unsigned d;
	__asm__ volatile("cpuid" : "=d" (d) : "a" (0x00000001) : "ebx", "ecx");
	if((d & 0x00000010) > 0)
		return 1;
	else
		return 0;
}

// checks if rdtsc readings are constant over frequency changes
bool irt_time_ticks_constant() {
	volatile unsigned d;
	__asm__ volatile("cpuid" : "=d" (d) : "a" (0x80000007) : "ebx", "ecx");
	if((d & 0x00000100) > 0)
		return 1;
	else
		return 0;
}

// ====== i386 (x86_32) machines  ===========================

#elif defined __i386__

uint64 irt_time_ticks(void) {
	volatile uint32 a;
	asm volatile("rdtsc" : "=a" (a), "=d" (d));
	return (a | (((uint64)d) << 32));
}


// ====== PowerPC machines ===========================

#else // deliberately fails for new architectures

uint64 irt_time_ticks(void) {
	int64 upper0, upper1, lower;

	__asm__ volatile("\
	mfspr %[upper0], 269 \n\
	mfspr %[lower] , 268 \n\
	mfspr %[upper1], 269 "
	: [lower] "=r" (lower), 
	[upper0] "=r" (upper0),
	[upper1] "=r" (upper1)
	);  

	return (uint64)(((upper1 ^ ((upper0 ^ upper1) & (lower>>31)))<<32) | lower);
}

#endif // architecture branch

// measures number of clock ticks over 100 ms, sets irt_g_time_ticks_per_sec and returns the value
uint64 irt_time_set_ticks_per_sec() {
	uint64 before = 0, after = 0;
	before = irt_time_ticks();
	irt_busy_nanosleep(1000ull*1000*100);
	after = irt_time_ticks();
	irt_g_time_ticks_per_sec = (after - before) * 10;
	return irt_g_time_ticks_per_sec;
}

// converts clock ticks to nanoseconds
uint64 irt_time_convert_ticks_to_ns(uint64 ticks) {
	return (uint64)(ticks/((double)irt_g_time_ticks_per_sec/1000000000));
}

