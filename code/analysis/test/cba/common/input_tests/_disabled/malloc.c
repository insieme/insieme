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
 * A test case verifying that malloc calls can be properly handled
 */

#include <stdlib.h>

#include "../../input_tests/cba.h"

typedef struct {
	int x;
	int y;
} point;

extern int data;


int main(int argc, char** argv) {

	cba_print_code();

	// test external part
	cba_expect_undefined_int(data);
	data = 2;
	cba_expect_eq_int(data, 2);

	// test an array of scalars
	int* a = (int*)(malloc(sizeof(int) * 5));

	// the reference should be unique
	cba_expect_is_alias(&(a[0]), &(a[0]));

	// check value before / after first usage
	cba_expect_undefined_int(a[2]);
	for(int i = 0; i<5; i++) {
		a[i] = 1;
	}
	cba_print_int(a[2]);


	//cba_dump_equations();

	a[0] = 10;
	a[1] = 12;
	a[2] = 14;
	a[3] = argc;

	cba_expect_eq_int(a[0]+2, a[1]);
	cba_expect_eq_int(a[0]+argc, 10+a[3]);


	// test an array of points
	point* p = (point*)(malloc(sizeof(point) * 3));
	p[0] = (point) { 0, 1 };
	p[1] = (point) { 1, argc };
	p[2] = (point) { argc, 2 };

	cba_expect_eq_int(p[0].y, p[1].x);
	cba_expect_eq_int(p[1].y, p[2].x);

	cba_expect_is_alias(&(p[0]), &(p[0]));

	//cba_dump_equations();
	
	return 0;
}
