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

#include "../../input_tests/cba.h"

int main(int argc, char** argv) {

	int x = 1;

	cba_expect_eq_int(x, 1);

	// if a loop is known to be entered the original value should be eliminated
	for(int i=0; i<1; i++) {
		x = 2;
	}
	cba_expect_eq_int(x, 2);

	// if the loop body is not known to be processed the original value should be preserved
	for(int i=0; i<argc; i++) {
		x = 3;
	}

	cba_expect_may_eq_int(x, 2);
	cba_expect_may_eq_int(x, 3);

	// a omp single is a parallel loop with static range 0..1:1 => should also work
	#pragma omp parallel
	{
		#pragma omp single
		x = 4;
	}

	// x should be 3 now
	cba_expect_eq_int(x, 4);

//	cba_dump_equations();
//	cba_print_code();

	return 0;
}
