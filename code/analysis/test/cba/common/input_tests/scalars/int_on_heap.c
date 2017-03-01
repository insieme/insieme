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

	int* a = (int*)malloc(sizeof(int));
	int* b = (int*)malloc(sizeof(int));

	// --- initial state ---
	cba_expect_undefined_int(*a);
	cba_expect_undefined_int(*b);


	// --- some basic updates ---

	*a = 12;
	cba_expect_eq_int(*a,12);
	cba_expect_undefined_int(*b);

	*b = 14;
	cba_expect_eq_int(*a,12);
	cba_expect_eq_int(*b,14);

	*a = 16;
	cba_expect_eq_int(*a,16);
	cba_expect_eq_int(*b,14);


	// --- uncertain value ---
	*a = (argc > 2) ? 4 : 5;
	cba_expect_one_of_int(*a,iset(4,5));
	cba_expect_eq_int(*b,14);

	*a = 18;
	cba_expect_eq_int(*a,18);
	cba_expect_eq_int(*b,14);


	// --- uncertain target ---
	*((argc > 2) ? a : b) = 20;
	cba_expect_one_of_int(*a,iset(18,20));
	cba_expect_one_of_int(*b,iset(14,20));


	return 0;
}
