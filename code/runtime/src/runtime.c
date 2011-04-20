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

#include <mqueue.h>

#include "impl/client_app.impl.h"
#include "impl/irt_context.impl.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"
#include "impl/irt_mqueue.impl.h"


// error handling
void irt_error_handler(int signal) {
	irt_error* error = (irt_error*)pthread_getspecific(irt_g_error_key);
	fprintf(stderr, "Insieme Runtime Error recieved: %s\n", irt_errcode_string(error->errcode));
	fprintf(stderr, "Additional information:\n");
	irt_print_error_info(stderr, error);
	exit(-error->errcode);
}

// globals
pthread_key_t irt_g_error_key;
pthread_key_t irt_g_worker_key;
mqd_t irt_g_message_queue;

void irt_init_globals() {
	// not using IRT_ASSERT since environment is not yet set up
	assert(pthread_key_create(&irt_g_error_key, NULL) == 0);
	assert(pthread_key_create(&irt_g_worker_key, NULL) == 0);
	irt_mqueue_init();
}
void irt_cleanup_globals() {
	irt_mqueue_cleanup();
}

int main(int argc, char** argv) {
	if(argc!=2) {
		printf("usage: runtime [libname]\n");
		return -1;
	}

	// initialize error signal handler
	signal(IRT_SIG_ERR, &irt_error_handler);
	// initialize globals
	irt_init_globals();

	static const uint32 work_count = 8;
	irt_worker* workers[work_count];
	for(int i=0; i<work_count; ++i) {
		workers[i] = irt_create_worker(~0, i);
	}

	//irt_client_app* client_app = irt_client_app_create(argv[1]);
	//irt_context* prog_context = irt_context_create(client_app);

	irt_cleanup_globals();
	return 0;
}


