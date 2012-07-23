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

#include "declarations.h"

#include <signal.h>
#include <stdio.h>

//#define IRT_VERBOSE 1

#ifdef WIN32 
#define IRT_SIG_ERR SIGABRT
#define IRT_SIG_INTERRUPT SIGINT
#else
#define IRT_SIG_ERR SIGUSR1
#define IRT_SIG_INTERRUPT SIGUSR2
#endif
/* ------------------------------ data structures ----- */

typedef enum _irt_errcode {
	IRT_ERR_NONE,				// no error
	IRT_ERR_IO,					// I/O error
	IRT_ERR_INIT,				// error related to initialization
	IRT_ERR_INTERNAL,			// internal error caused by runtime system
	IRT_ERR_OVERFLOW,			// overflow of an internal IR buffer
	IRT_ERR_APP,				// error caused by the user application running on the IRT
	IRT_ERR_OCL,				// error caused by the opencl runtime system
	IRT_ERR_INSTRUMENTATION,	// error related to the instrumentation system
	IRT_ERR_INVALIDARGUMENT		// 
} irt_errcode;

struct _irt_error {
	irt_errcode errcode;
	uint32 additional_bytes;
};


/* ------------------------------ operations ----- */

#if !defined(NDEBUG) || defined(IRT_VERBOSE)
#define IRT_ASSERT(__condition, __errcode, __message, ...) \
if(!(__condition)) { \
	fprintf(stderr, "IRT Assertion failure in %s#%d:\n", __FILE__, __LINE__); \
	irt_throw_string_error(__errcode, __message, ##__VA_ARGS__); \
}
#define IRT_WARN(__message, ...) { \
	fprintf(stderr, "IRT Warning in %s#%d:\n", __FILE__, __LINE__); \
	fprintf(stderr, __message "\n", ##__VA_ARGS__); fflush(stderr); \
}
#define IRT_INFO(__message, ...) { \
	printf(__message, ##__VA_ARGS__); fflush(stdout); \
}
#ifdef IRT_VERBOSE
#define IRT_DEBUG_ONLY(__code__) __code__
#define IRT_DEBUG(__message, ...) { \
	printf("IRT Debug Info (%s#%d): ", __FILE__, __LINE__); \
	printf(__message "\n", ##__VA_ARGS__); fflush(stdout); \
}
#else
#define IRT_DEBUG_ONLY(__code__)
#define IRT_DEBUG(__message, ...)
#endif
#else
#define IRT_DEBUG_PRINTS_OFF
#define IRT_ASSERT(__condition, __errcode, __message, ...) if(__condition);
#define IRT_WARN(__message, ...)
#define IRT_INFO(__message, ...)
#define IRT_DEBUG(__message, ...)
#define IRT_DEBUG_ONLY(__code__)
#endif

#ifdef IRT_VERBOSE
#define IRT_VERBOSE_ONLY(__code) __code
#else
#define IRT_VERBOSE_ONLY(__code)
#endif

void irt_throw_string_error(irt_errcode code, const char* message, ...);
void irt_throw_generic_error(irt_error* error);

const char* irt_errcode_string(irt_errcode code);
void irt_print_error_info(FILE* target, irt_error* error);

void irt_error_handler(int signal);
