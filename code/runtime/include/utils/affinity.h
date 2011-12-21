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

/* needed for CPU_* macros */
#define _GNU_SOURCE 1

#define MAX_CORES 128
#define IRT_ENABLE_AFFINITY_ENV "IRT_ENABLE_AFFINITY"

#include <sched.h>
#include <unistd.h>
#include <pthread.h>

#include "impl/error_handling.impl.h"

void _irt_print_affinity_mask(cpu_set_t mask) {
	for(int i=0; i<MAX_CORES; i++) {
		IRT_INFO("%s", CPU_ISSET(i, &mask)?"1":"0");
	}
	IRT_INFO("\n");
}

void irt_clear_affinity() {
	cpu_set_t mask;
	CPU_ZERO(&mask);
	for(int i=0; i<MAX_CORES; ++i) {
		CPU_SET(i, &mask);
	}
	IRT_ASSERT(pthread_setaffinity_np(pthread_self(), sizeof(mask), &mask) == 0, IRT_ERR_INIT, "Error clearing thread affinity.");
}

void irt_set_affinity(irt_affinity_mask irt_mask) {
	if(getenv(IRT_ENABLE_AFFINITY_ENV)) {
		cpu_set_t mask;
		CPU_ZERO(&mask);
		for(int i=0; i<MAX_CORES; ++i) {
			if((irt_mask&1) != 0) CPU_SET(i, &mask);
			irt_mask >>= 1;
		}
		IRT_ASSERT(pthread_setaffinity_np(pthread_self(), sizeof(mask), &mask) == 0, IRT_ERR_INIT, "Error setting thread affinity.");
	}
}
