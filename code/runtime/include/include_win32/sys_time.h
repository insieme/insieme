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
/*
* Implementations of unix time functions (as in <sys/time.h>) for windows, such that original
* functionality using those functions may remain untouched
*/

#include "irt_inttypes.h"

/*
 timeval struct exists within windows, timezone is obsolete anyway and should be NULL,
 timezone hence is not considered
*/
int32 gettimeofday(struct timeval* tv, void* tz) {
	FILETIME ft;
	uint64 tmpres = 0;
	memset(&ft, 0, sizeof(ft));
	GetSystemTimeAsFileTime(&ft);

	tmpres = ft.dwHighDateTime;
	tmpres <<= 32;
	tmpres |= ft.dwLowDateTime;

	tmpres /= 10; /*convert into microseconds*/
	// const __int64 DELTA_EPOCH_IN_MICROSECS = 11644473600000000;
	// tmpres -= DELTA_EPOCH_IN_MICROSECS;  //uncomment if you want to use unix epoch instead of windows epoch
	tv->tv_sec = (long)(tmpres / 1000000);
	tv->tv_usec = (long)(tmpres % 1000000);

	return 0;
}
