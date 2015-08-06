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
#ifndef __GUARD_UTILS_MEMORY_H
#define __GUARD_UTILS_MEMORY_H

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

/*
 * Gets the virtual memory and resident set sizes in KB of the process calling it
 * TODO: needs to be replaced by getrusage() but this is not supported on all systems?
 */

void irt_get_memory_usage(unsigned long* virt_size, unsigned long* res_size) {
	static int32 position_cache_virt = 0, position_cache_res = 0;
	FILE* file = fopen("/proc/self/status", "r");
	
	*virt_size = 0;
	*res_size = 0;
	
	// ok, I am checking here for something that CAN'T happen but apparently it still does happen sometimes
	// if I am a process I can read my own stats in /proc/self - therefore this path must always exist...theoretically...
	if(file == 0) {
		IRT_DEBUG("Instrumentation: Unable to open file for memory consumption readings, reason: %s\n", strerror(errno));
		return;
	}
	else if(position_cache_virt == 0) {   // first call, no position cached
		if(fscanf(file, "%*[^B]B VmSize:\t%lu", virt_size) != 1) {
			IRT_DEBUG("Instrumentation: Unable to read VmSize, reason: %s\n", strerror(errno));    // lookup entry
			fclose(file);
			return;
		}
		if((position_cache_virt = ftell(file)) == -1) {
			IRT_DEBUG("Instrumentation: Unable to ftell for VmSize, reason: %s\n", strerror(errno));    // save stream position
			fclose(file);
			return;
		}
		if(fscanf(file, " kB VmLck:\t%*u kB VmHWM:\t%*u kB VmRSS:\t%lu", res_size) != 1) {
			IRT_DEBUG("Instrumentation: Unable to read VmRSS, reason: %s\n", strerror(errno));    // skip useless info and read VmRSS
			fclose(file);
			return;
		}
		if((position_cache_res = ftell(file)) == -1) {
			IRT_DEBUG("Instrumentation: Unable to ftell for VmRSS, reason: %s\n", strerror(errno));    // save stream position
			fclose(file);
			return;
		}
	}
	else {   // if not first call, use cached positions, assumes max 8 digits
		char str[9];
		if(fseek(file, position_cache_virt-8, SEEK_SET) != 0) {
			IRT_DEBUG("Instrumentation: Unable to seek to VmSize position, reason: %s\n", strerror(errno));
			fclose(file);
			return;
		}
		if(fgets(str, 9, file) == NULL) {
			IRT_DEBUG("Instrumentation: Unable to fgets VmSize, reason: %s\n", strerror(errno));
			fclose(file);
			return;
		}
		*virt_size = atol(str);
		if(fseek(file, position_cache_res-8, SEEK_SET) != 0) {
			IRT_DEBUG("Instrumentation: Unable to seek to VmRSS position, reason: %s\n", strerror(errno));
			fclose(file);
			return;
		}
		if(fgets(str, 9, file) == NULL) {
			IRT_DEBUG("Instrumentation: Unable to fgets VmRSS\n, reason: %s\n", strerror(errno));
			fclose(file);
			return;
		}
		*res_size = atol(str);
	}
	fclose(file);
}


#endif // ifndef __GUARD_UTILS_MEMORY_H
