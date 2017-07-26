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

#include "../input_tests/cba.h"

int min(int a, int b) {
	if (a < b) return a;
	return b;
}

int max(int a, int b) {
	return (a == min(a,b)) ? b : a;
}

int main(int argc, char** argv) {

	// let's start with something simple
	cba_expect_eq_int(1,1);

	// a little more challenging
	int a = 10;
	int b = 12;
	cba_expect_eq_int(a,a);
	cba_expect_eq_int(b,b);
	cba_expect_ne_int(a,b);
	cba_expect_eq_int(a+2, b);

	// with some unknown value
	cba_expect_ne_int(a+argc,b+argc);
	cba_expect_eq_int(a+2+argc, b+argc);

	// including function calls
	cba_expect_eq_int(a,min(a,b));
	cba_expect_eq_int(b,max(a,b));

	// simple integer division (modulo always 0)
	cba_expect_eq_int(max(a,b)/2, 6);
	cba_expect_eq_int(min(a,b)%2, 0);

	// even more tricky
	cba_expect_eq_int(a+argc,min(a+argc,b+argc));

	// and after an update
	a = 14;
	cba_expect_eq_int(b+argc,min(a+argc,b+argc));

	return 0;
}
