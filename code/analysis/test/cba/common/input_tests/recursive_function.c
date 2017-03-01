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

#include "../input_tests/cba.h"


int dummy(int x) {
	if (x <= 2) return 1;
	return dummy(x-1);
}


int fib(int x) {
	if (x <= 1) return 1;
	return fib(x-1) + fib(x-2);
}

int odd();

int even(int x) {
	if (x == 0) return 1;
	return odd(x-1);
}

int odd(int x) {
	if (x == 0) return 0;
	return even(x-1);
}

int main(int argc, char** argv) {

//	cba_print_code();
//	cba_dump_json();

	cba_expect_eq_int(1,dummy(-1));
	cba_expect_eq_int(1,dummy(0));
	cba_expect_eq_int(1,dummy(1));
	cba_expect_eq_int(1,dummy(3));

	cba_expect_undefined_int(fib(0));
	cba_expect_undefined_int(fib(1));
	cba_expect_undefined_int(fib(2));

	cba_expect_defined_int(even(0));
	cba_expect_defined_int(odd(1));

	return 0;
}
