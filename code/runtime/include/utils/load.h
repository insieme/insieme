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
#ifndef __GUARD_UTILS_LOAD_H
#define __GUARD_UTILS_LOAD_H

#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "errno.h"

// static values to remember last state of the process/system
static unsigned long last_total_user = 0, last_total_user_low = 0, last_total_system = 0, last_total_idle = 0, last_user_time = 0, last_kernel_time = 0;

/*
 * return the load produced by this process, obtained via /proc/self/stat, needs two calls to measure load during this interval
 * needs to be checked for every operating system, since different kernels/distributions might show different data
 * furthermore this function is based on fgets, hence avoid calls in short time intervals!
 * TODO: replace with a more sophisticated, general and faster system
 */

void get_load_own(unsigned long* time) {
	FILE* file;
	unsigned long user_time, kernel_time;
	file = fopen("/proc/self/stat", "r");
	if(file == 0) {
		IRT_DEBUG("Instrumentation: Unable to open file for process load readings, reason: %s\n", strerror(errno));
		return;
	}
	//            pid com sta ppi pgr ses tty tpg flg mflt cmlt jflt cjlt usr sys cusr csys
	if(fscanf(file, "%*d %*s %*c %*d %*d %*d %*d %*d %*u %*u %*u %*u %*u %lu %lu %*d %*d", &user_time, &kernel_time) != 2) {
		IRT_DEBUG("Instrumentation: Unable to read process load data, reason: %s\n", strerror(errno));
		fclose(file);
		return;
	}
	fclose(file);
	// overflow "handling"
	if(user_time < last_user_time || kernel_time < last_kernel_time) {
		*time = 0;
	} else {
		*time = (user_time - last_user_time) + (kernel_time - last_kernel_time);
	}
	last_user_time = user_time;
	last_kernel_time = kernel_time;
}

/*
 * return the times the system was busy (system time) and idle, obtained via /proc/stat, needs two calls to measure load during this interval
 * needs to be checked for every operating system, since different kernels/distributions might show different data
 * furthermore this function is based on fgets, hence avoid calls in short time intervals!
 * TODO: replace with a more sophisticated, general system
 */

void get_load_system(unsigned long* system_time, unsigned long* idle_time) {
	FILE* file;
	unsigned long total_user, total_user_low, total_system, total_idle;
	file = fopen("/proc/stat", "r");
	if(file == 0) {
		IRT_DEBUG("Instrumentation: Unable to open file for system load readings, reason: %s\n", strerror(errno));
		return;
	}
	if(fscanf(file, "cpu %lu %lu %lu %lu", &total_user, &total_user_low, &total_system, &total_idle) != 4) {
		IRT_DEBUG("Instrumentation: Unable to read system load data, reason: %s\n", strerror(errno));
		fclose(file);
		return;
	}
	fclose(file);
	// overflow "handling"
	if(total_user < last_total_user || total_user_low < last_total_user_low || total_system < last_total_system || total_idle < last_total_idle) {
		*system_time = 0;
		*idle_time = 0;
	} else {
		*system_time = (total_user - last_total_user) + (total_user_low - last_total_user_low) + (total_system - last_total_system);
		*idle_time = (total_idle - last_total_idle);
	}
	last_total_user = total_user;
	last_total_user_low = total_user_low;
	last_total_system = total_system;
	last_total_idle = total_idle;
}

/*
 * returns the percentage of the external load between 0 and 1, needs two calls to measure load during this interval
 * external load = (system time - own time) / (system time + idle time)
 */

double get_load_external() {
	unsigned long proc_time = 0, system_time = 0, idle_time = 0;
	double ext;
	get_load_own(&proc_time);
	get_load_system(&system_time, &idle_time);
	// granularity problems, division by 0 if full load
	if(idle_time == 0) { idle_time = 1; }
	ext = (system_time - proc_time) / ((double)system_time + idle_time);
	return ext;
}


#endif // ifndef __GUARD_UTILS_LOAD_H
