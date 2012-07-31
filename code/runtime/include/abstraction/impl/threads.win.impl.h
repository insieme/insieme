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

#include "abstraction/threads.h"

// little helper struct to pass function and parameter to irt_win_thread_func
typedef struct _irt_win_thread_params {
	irt_thread_func *fun;	// function which shall be executed by (new) thread
	void *args;				// parameter for function fun
} irt_win_thread_params;

/*  wrapper function which admits to the required signature for CreateThread
	it simply calls original irt_thread_func function with accoring parameter
*/
DWORD WINAPI _irt_win_thread_func(void* params)
{
	irt_win_thread_params *p = (irt_win_thread_params*)params;
	DWORD ret = 0;
	ret = (DWORD)p->fun(p->args);
	free(params);
	return ret;
}

irt_thread irt_thread_create(irt_thread_func *fun, void *args) {
	irt_win_thread_params *params = (irt_win_thread_params*)malloc(sizeof(irt_win_thread_params));
	params->fun = fun;
	params->args = args;
	return CreateThread(NULL, NULL, _irt_win_thread_func, params, NULL, NULL);
}
