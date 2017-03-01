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
#ifndef __GUARD_IMPL_CLIENT_APP_IMPL_H
#define __GUARD_IMPL_CLIENT_APP_IMPL_H

#include "client_app.h"

#include "utils/hoisting.h"
#include "impl/error_handling.impl.h"
#include "impl/worker.impl.h"

#ifndef IRT_MIN_MODE

irt_client_app* irt_client_app_create(const char* library_file_name) {
	irt_client_app* app = (irt_client_app*)malloc(sizeof(irt_client_app));
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


#endif // ifndef __GUARD_IMPL_CLIENT_APP_IMPL_H
