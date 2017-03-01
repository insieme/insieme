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
#ifndef __GUARD_IMPL_ERROR_HANDLING_IMPL_H
#define __GUARD_IMPL_ERROR_HANDLING_IMPL_H

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "error_handling.h"

#include "irt_globals.h"

#include "abstraction/threads.h"
#include "abstraction/impl/threads.impl.h"

#include "impl/worker.impl.h"

void irt_error_handler(irt_error* error) {
	irt_mutex_lock(&irt_g_error_mutex); // not unlocked, prevent multiple reports

	irt_thread t;
	irt_thread_get_current(&t);

	#if defined(_WIN32) && !defined(IRT_USE_PTHREADS)
	fprintf(stderr, "Insieme Runtime Error received (thread %i): %s\n", t.thread_id, irt_errcode_string(error->errcode));
	#elif defined(_WIN32)
	fprintf(stderr, "Insieme Runtime Error received (thread %p): %s\n", (void*)t.p, irt_errcode_string(error->errcode));
	#else
	fprintf(stderr, "Insieme Runtime Error received (thread %p): %s\n", (void*)t, irt_errcode_string(error->errcode));
	#endif

	fprintf(stderr, "Additional information:\n");
	irt_print_error_info(stderr, error);

	// suppress exit handling
	irt_g_exit_handling_done = true;
	exit(-error->errcode);
}

void irt_throw_string_error(irt_errcode code, const char* message, ...) {
	va_list args;
	va_start(args, message);
	char buffer[512];
	uint32 additional_bytes = vsnprintf(buffer, 512, message, args) + 1;
	va_end(args);

	irt_error* err = (irt_error*)malloc(sizeof(irt_error) + additional_bytes);
	err->errcode = code;
	err->additional_bytes = additional_bytes;
	strncpy(((char*)err) + sizeof(irt_error), buffer, additional_bytes);
	irt_error_handler(err);
}

const char* irt_errcode_string(irt_errcode code) {
	static const char* irt_errcode_strings[] = {
	#define IRT_ERROR(_err) #_err,
	#include "irt_errors.def"
	};
	return irt_errcode_strings[code];
}

void irt_print_error_info(FILE* target, irt_error* error) {
	if(error->additional_bytes) { fprintf(target, "%s\n", (char*)error + sizeof(irt_error)); }
}

#endif // ifndef __GUARD_IMPL_ERROR_HANDLING_IMPL_H
