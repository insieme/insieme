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
#ifndef __GUARD_UTILS_HOISTING_H
#define __GUARD_UTILS_HOISTING_H

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#ifndef IRT_MIN_MODE

#ifdef _WIN32

#else
#include <dlfcn.h>
#endif

#define DLOPEN_UNIQUE_BUFFSIZE 256

// TODO: dlopen_unique should become an irt_* function

#ifdef _WIN32
// TODO: implement this for windows
void* dlopen_unique(const char* filename, int flag) {
	return NULL;
}
#else

void* dlopen_unique(const char* filename, int flag) {
	static unsigned count = 0;
	char uniquename[DLOPEN_UNIQUE_BUFFSIZE];
	unsigned cc = count++; // TODO should use atomic op for thread safety
	int retval = 0;
	retval = snprintf(uniquename, DLOPEN_UNIQUE_BUFFSIZE, "%s.%d", filename, cc);
	assert(retval < DLOPEN_UNIQUE_BUFFSIZE);
	char command[DLOPEN_UNIQUE_BUFFSIZE];
	retval = snprintf(command, DLOPEN_UNIQUE_BUFFSIZE, "cp %s %s", filename, uniquename);
	assert(retval < DLOPEN_UNIQUE_BUFFSIZE);
	retval = system(command);
	assert(retval == 0);
	return dlopen(uniquename, flag);
}

#endif

#endif


#endif // ifndef __GUARD_UTILS_HOISTING_H
