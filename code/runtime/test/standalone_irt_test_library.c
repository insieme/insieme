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

#include <stdio.h>

#define IRT_LIBRARY_MAIN
#include "irt_library.h"

typedef struct {
	int num;
} param_struct;

void loop_body(int64 index, void* data) {
	param_struct *params = (param_struct*)data;
	printf("Loop body called with index %ld, data %d\n", index, params->num);
}

void para_inner(void *data) {
	param_struct *params = (param_struct*)data;
	printf("Hello inner world with number %d\n", params->num);
}

void para(void *data) {
	param_struct *params = (param_struct*)data;
	printf("Hello outer world with number %d\n", params->num);

	param_struct subparams = { 666 };
	irt_joinable j = irt_lib_parallel(2, 2, &para_inner, &subparams, sizeof(param_struct));

	param_struct loopparams = { 37 };
	irt_lib_pfor(0, 10, 2, &loop_body, &loopparams, sizeof(param_struct));
	irt_merge(j);
}

int main(int argc, char **argv) {
	param_struct params = { 42 };
	irt_lib_parallel(4, 4, &para, &params, sizeof(param_struct));
	irt_lib_merge_all();
}
