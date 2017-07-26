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

#include <stdlib.h>

#include "../../input_tests/cba.h"

typedef struct {
	int x;
	int y;
	int z;
} A;

int main(void) {
	A a = { 1, 2, 3 };

	cba_expect_eq_int(a.x, 1);
	cba_expect_eq_int(a.y, 2);
	cba_expect_eq_int(a.z, 3);

	A b = { 4, 5 };

	cba_expect_eq_int(b.x, 4);
	cba_expect_eq_int(b.y, 5);
	cba_expect_eq_int(b.z, 0);

	a.x = 15;
	b.z = 42;

	cba_expect_eq_int(a.x, 15);
	cba_expect_eq_int(a.y, 2);
	cba_expect_eq_int(a.z, 3);
	cba_expect_eq_int(b.x, 4);
	cba_expect_eq_int(b.y, 5);
	cba_expect_eq_int(b.z, 42);

	A c;

	cba_expect_undefined_int(c.x);
	cba_expect_undefined_int(c.y);
	cba_expect_undefined_int(c.z);

	A d;

	d = (A){1, 3, 4};

	cba_expect_eq_int(d.x, 1);
	cba_expect_eq_int(d.y, 3);
	cba_expect_eq_int(d.z, 4);

	//cba_dump_solution();
	//cba_dump_json();
	//cba_print_code();

	return 0;
}
