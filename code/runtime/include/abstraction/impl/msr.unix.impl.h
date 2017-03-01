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
#ifndef __GUARD_ABSTRACTION_IMPL_MSR_UNIX_IMPL_H
#define __GUARD_ABSTRACTION_IMPL_MSR_UNIX_IMPL_H

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

int32 _irt_open_msr(uint32 core) {
	char path_to_msr[512];
	int32 file;

	sprintf(path_to_msr, "/dev/cpu/%u/msr", core);

	if((file = open(path_to_msr, O_RDONLY)) < 0) {
		IRT_DEBUG("Instrumentation: Unable to open MSR file for reading, file %s, reason: %s\n", path_to_msr, strerror(errno));
		return -1;
	}

	return file;
}

int64 _irt_read_msr(int32 file, int32 subject) {
	int64 data;

	if(pread(file, &data, sizeof data, subject) != sizeof data) {
		IRT_DEBUG("Instrumentation: Unable to read MSR file descriptor %d, reason: %s\n", file, strerror(errno));
		return -1.0;
	}

	return data;
}

int32 _irt_close_msr(int32 file) {
	return close(file);
}


#endif // ifndef __GUARD_ABSTRACTION_IMPL_MSR_UNIX_IMPL_H
