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
/**
 * A simple test case covering some arithmetic.
 */

#include <stdlib.h>

#include "../../input_tests/cba.h"

typedef struct {
	int x;
	int y;
	int z;
} A;


int main(int argc, char** argv) {

	A a;
	cba_expect_undefined_int(a.x);
	cba_expect_undefined_int(a.y);
	cba_expect_undefined_int(a.z);

	a.x = 1;
	cba_expect_eq_int(a.x,1);
	cba_expect_undefined_int(a.y);
	cba_expect_undefined_int(a.z);

	a.y = 2;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_undefined_int(a.z);

	a.z = 3;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_eq_int(a.z,3);

	// assign an uncertain value
	a.z = (argc > 2) ? 4 : 5;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_one_of_int(a.z, iset(4,5));


	// reset to something known
	a.z = 6;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_eq_int(a.z,6);

	// update an unknown location
	if (argc > 2) {
		a.x = 7;
	} else {
		a.y = 8;
	}

	cba_expect_one_of_int(a.x, iset(1,7));
	cba_expect_one_of_int(a.y, iset(2,8));
	cba_expect_eq_int(a.z,6);


	// reset the struct
	a.x = 1;
	a.y = 2;
	a.z = 3;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_eq_int(a.z,3);


	// add to the control flow
	if (argc > 2) {
		a.x = 9;
	} else {
		a.y = 9;
	}
	cba_expect_one_of_int(a.x, iset(1,9));
	cba_expect_one_of_int(a.y, iset(2,9));
	cba_expect_eq_int(a.z,3);

	// TODO: known bug - can not create and eliminate a pointer to a field
//	// add uncertainty into assignment
//	*((argc > 2) ? &a.x : &a.y) = 9;
//	cba_expect_one_of_int(a.x, iset(1,9));
//	cba_expect_one_of_int(a.y, iset(1,9));
//	cba_expect_eq_int(a.z,3);

//	cba_dump_solution();
//	cba_print_code();
//	cba_print_code();

	return 0;
}
