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
#ifndef __GUARD_INCLUDE_GEMS_IMPL_TIME_IMPL_H
#define __GUARD_INCLUDE_GEMS_IMPL_TIME_IMPL_H

#include "include_gems/time.h"
#include "irt_inttypes.h"
#include "error_handling.h"
#include "utils/timing.h"

int clock_gettime(int clk_id, struct timespec *tp) {
	unsigned int cur_time;
	
	IRT_ASSERT(clk_id == CLOCK_REALTIME, IRT_ERR_INVALIDARGUMENT, "clock_getttime for gemsclaim only allows CLOCK_REALTIME");
	
	if(tp == NULL) {
		return -1;
	}
	
	cur_time = *(unsigned int*)(0x08000000);
	
	tp->tv_sec 	= cur_time / 1000000;
	tp->tv_nsec	= (cur_time - tp->tv_sec) * 1000;
	
	return 0;
}

// TODO [_GEMS]: missing implementation
int nanosleep(const struct timespec *req, struct timespec *rem) {
	if(req == NULL) {
		return -1;
	}
	
	uint64 nano = req->tv_sec * 1000000000ull + req->tv_nsec;
	irt_busy_nanosleep(nano);
	
	return 0;
}

#endif // ifndef __GUARD_INCLUDE_GEMS_IMPL_TIME_IMPL_H
