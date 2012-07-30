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
	if(argc >= 3) worker_count = atoi(argv[2]);
	irt_runtime_start(IRT_RT_MQUEUE, worker_count);

	IRT_DEBUG("Sending new app msg");
	irt_mqueue_send_new_app(argv[1]);
	IRT_DEBUG("New app msg sent");

	for(;;) { irt_nanosleep(60*60*1e9); }

	exit(0);
#else //IRT_MIN_MODE
	printf("Runtime built in minimal mode (IRT_MIN_MODE) can not be used in service mode!\n");
	exit(-1);
#endif //IRT_MIN_MODE
}
