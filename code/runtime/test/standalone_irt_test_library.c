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
#include <stdio.h>

#define IRT_LIBRARY_MAIN
#include "irt_library.h"

typedef struct { int num; } param_struct;

void loop_body(int64 index, void* data) {
	param_struct* params = (param_struct*)data;
	printf("Loop body called with index %ld, data %d\n", index, params->num);
}

void para_inner(void* data) {
	param_struct* params = (param_struct*)data;
	printf("Hello inner world with number %d\n", params->num);
}

void para(void* data) {
	param_struct* params = (param_struct*)data;
	printf("Hello outer world with number %d\n", params->num);

	param_struct subparams = {666};
	irt_joinable j = irt_lib_parallel(2, 2, &para_inner, &subparams, sizeof(param_struct));

	param_struct loopparams = {37};
	irt_lib_pfor(0, 10, 2, &loop_body, &loopparams, sizeof(param_struct));
	irt_merge(j);
}

int main(int argc, char** argv) {
	param_struct params = {42};
	irt_lib_parallel(4, 4, &para, &params, sizeof(param_struct));
	irt_lib_merge_all();
	return 0;
}
