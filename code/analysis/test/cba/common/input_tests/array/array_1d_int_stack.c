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

#include "../../input_tests/cba.h"

int main(int argc, char** argv) {

	// create an array
	int a[10];

	// check that elements of a are not defined at this point
//	cba_expect_undefined_int(a[0]);
//	cba_expect_undefined_int(a[1]);
//	cba_expect_undefined_int(a[2]);
//	cba_expect_undefined_int(a[4]);
//	cba_expect_undefined_int(a[argc]);

	// fix one value
	a[0] = 12;
//	cba_expect_eq_int(a[0],12);
//	cba_expect_undefined_int(a[1]);
//	cba_expect_undefined_int(a[2]);
//	cba_expect_undefined_int(a[4]);
//	cba_expect_undefined_int(a[argc]);

	// fix some values
	a[1] = 14;
	a[4] = 16;

	// check those values
	cba_expect_eq_int(a[0],12);
//	cba_expect_eq_int(a[1],14);
//	cba_expect_undefined_int(a[2]);
//	cba_expect_eq_int(a[4],16);
//	cba_expect_undefined_int(a[argc]);

//	cba_print_code();
//	cba_dump_solution();
//	cba_dump_json();

	return 0;
}
