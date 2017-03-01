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

int main(int argc, char** argv) {

	int a[3];
	int* p = a;

	a[0] = 1;
	a[1] = 2;
	a[2] = 3;

	// check proper alignment
	cba_expect_is_alias(&a[0],p);
	cba_expect_eq_int(a[0],p[0]);
	cba_expect_eq_int(a[1],p[1]);
	cba_expect_eq_int(a[2],p[2]);

	// move pointer
	++p;
	cba_expect_is_alias(&a[1],p);
	cba_expect_eq_int(a[1],p[0]);
	cba_expect_eq_int(a[2],p[1]);

	// and another time
	p++;
	cba_expect_is_alias(&a[2],p);
	cba_expect_eq_int(a[2],p[0]);


	// jump more than one place
	p = p - 2;
	cba_expect_is_alias(&a[0],p);
	cba_expect_eq_int(a[0],p[0]);
	cba_expect_eq_int(a[1],p[1]);
	cba_expect_eq_int(a[2],p[2]);

	// jump more than one place
	p = p + 2;
	cba_expect_is_alias(&a[2],p);
	cba_expect_eq_int(a[2],p[0]);

//	cba_print_code();

	return 0;
}
