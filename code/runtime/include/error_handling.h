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

#define IRT_SIG_ERR SIGUSR1

/* ------------------------------ data structures ----- */

enum _irt_errcode {
	IRT_ERR_NONE,
	IRT_ERR_IO,
	IRT_ERR_INIT,
	IRT_ERR_APP
};

struct _irt_error {
	irt_errcode errcode;
	uint32 additional_bytes;
};


/* ------------------------------ operations ----- */

void irt_throw_string_error(irt_errcode code, const char* message, ...);
void irt_throw_generic_error(irt_error* error);

const char* irt_errcode_string(irt_errcode code);
void irt_print_error_info(FILE* target, irt_error* error);
