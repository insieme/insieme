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
 */

/**
 * A simple test case covering some arithmetic.
 */

#include <stdlib.h>

#include "../../input_tests/cba.h"

int main(int argc, char** argv) {

	// create an array
	int** a = (int**)malloc(sizeof(int*) * 10);

	// the indirection vector should be undefined
	cba_expect_undefined_ptr(a[0]);
	cba_expect_undefined_ptr(a[1]);

	for(int i=0; i<10; i++) {
		a[i] = (int*)malloc(sizeof(int) * 10);
	}

	// all values should be undefined
	cba_expect_undefined_int(a[0][0]);
	cba_expect_undefined_int(a[0][1]);
	cba_expect_undefined_int(a[1][0]);
	cba_expect_undefined_int(a[1][1]);

	// set some values
	a[0][1] = 12;
	a[1][0] = 14;

	// check those values
	cba_expect_undefined_int(a[0][0]);
	// TODO: this is a known bug:
	//   since it can not be guaranteed that the above
	//	 loop is updating all elements of the index vector
	//   a[], all of them will also point to the initial,
	//   invalid initialization array, leading to an unknown
	//   result for the arithmetic analysis
//	cba_expect_eq_int(a[0][1], 12);
//	cba_expect_eq_int(a[1][0], 14);
	cba_expect_undefined_int(a[1][1]);

//	cba_dump_statistic();
//	cba_debug();

	return 0;
}
