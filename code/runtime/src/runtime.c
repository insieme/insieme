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

#include "declarations.h"
#include "runtime.h"

#include "irt_all_impls.h"
#include "standalone.h"

int main(int argc, char** argv) {
#ifndef IRT_MIN_MODE
	if(argc < 2 || argc > 3) {
		IRT_INFO("usage: runtime [libname] [numthreads]\n");
		return -1;
	}

	uint32 worker_count = 1;
	if(argc >= 3) { worker_count = atoi(argv[2]); }
	irt_runtime_start(IRT_RT_MQUEUE, worker_count);

	IRT_DEBUG("Sending new app msg");
	irt_mqueue_send_new_app(argv[1]);
	IRT_DEBUG("New app msg sent");

	for(;;) {
		irt_nanosleep(60 * 60 * 1e9);
	}

	exit(0);
	#else  // IRT_MIN_MODE
	printf("Runtime built in minimal mode (IRT_MIN_MODE) can not be used in service mode!\n");
	exit(-1);
	#endif // IRT_MIN_MODE
}
