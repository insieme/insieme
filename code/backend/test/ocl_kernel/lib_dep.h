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

#include <stdbool.h>
#include <string.h>
#include <stdarg.h>

typedef enum _irt_errcode {
	IRT_ERR_NONE,			// no error
	IRT_ERR_IO,			// I/O error
	IRT_ERR_INIT,			// error related to initialization
	IRT_ERR_INTERNAL,		// internal error caused by runtime system
	IRT_ERR_APP,			// error caused by the user application running on the IRT
	IRT_ERR_OCL			// error caused by the opencl runtime system
} irt_errcode;

struct _irt_error {
	irt_errcode errcode;
	int additional_bytes;
};
// IRT_ASSERT(err_code  == CL_SUCCESS, IRT_ERR_OCL, "Error getting platforms: \"%s\"", _irt_error_string(err_code));
#define IRT_ASSERT(__condition, __errcode, __message, ...) \
if(!(__condition)) { \
	fprintf(stderr, "IRT Assertion failure in %s#%d:\n", __FILE__, __LINE__); \
	printf(__message, ##__VA_ARGS__); \
	printf("\n"); \
}

#define IRT_INFO(__message, ...) { \
	printf(__message, ##__VA_ARGS__); \
}
