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

#include "client_app.h"

#include "utils/hoisting.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"

#ifndef IRT_MIN_MODE

irt_client_app* irt_client_app_create(const char* library_file_name) {
	irt_client_app *app = (irt_client_app*)malloc(sizeof(irt_client_app));
	#ifndef IRT_MIN_MODE
		app->id = irt_generate_client_app_id(IRT_LOOKUP_GENERATOR_ID_PTR);
		app->library = dlopen_unique(library_file_name, RTLD_NOW);
		IRT_ASSERT(app->library != NULL, IRT_ERR_IO, "Could not load library %s\nError: %s\n", library_file_name, dlerror());
	
		app->init_context = (init_context_fun*)dlsym(app->library, IRT_APP_INIT_CONTEXT_NAME);
		IRT_ASSERT(app->init_context != NULL, IRT_ERR_APP, "Insieme init function not found in library %s\nError: %s\n", library_file_name, dlerror());
		app->cleanup_context = (cleanup_context_fun*)dlsym(app->library, IRT_APP_CLEANUP_CONTEXT_NAME);
		IRT_ASSERT(app->cleanup_context != NULL, IRT_ERR_APP, "Insieme cleanup function not found in library %s\nError: %s\n", library_file_name, dlerror());
	#endif
	return app;
}

void irt_client_app_destroy(irt_client_app* app) {
	#ifndef IRT_MIN_MODE
		dlclose(app->library);
		free(app);
	#endif
}

#endif // ifndef IRT_MIN_MODE

