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

#include "../../input_tests/cba.h"

struct A {
	static int instances;
	static int step_count;
	A()  { instances++; step_count++; }
	~A() { instances--; step_count++; }
};

int A::instances = 0;
int A::step_count = 0;

int main(int argc, char** argv) {

	cba_expect_eq_int(0,A::instances);
	cba_expect_eq_int(0,A::step_count);

	// create an instance in a closed scope
	{
		A a;
		cba_expect_eq_int(1,A::instances);
		cba_expect_eq_int(1,A::step_count);
	}

	// here it should be 0 again
	cba_expect_eq_int(0,A::instances);
	cba_expect_eq_int(2,A::step_count);


	// create several nested
	A a;
	cba_expect_eq_int(1,A::instances);
	cba_expect_eq_int(3,A::step_count);

	{
		cba_expect_eq_int(1,A::instances);
		cba_expect_eq_int(3,A::step_count);

		A a;

		cba_expect_eq_int(2,A::instances);
		cba_expect_eq_int(4,A::step_count);

		{
			cba_expect_eq_int(2,A::instances);
			cba_expect_eq_int(4,A::step_count);

			A a;

			cba_expect_eq_int(3,A::instances);
			cba_expect_eq_int(5,A::step_count);

			A b;

			cba_expect_eq_int(4,A::instances);
			cba_expect_eq_int(6,A::step_count);
		}

		cba_expect_eq_int(2,A::instances);
		cba_expect_eq_int(8,A::step_count);

	}

	cba_expect_eq_int(1,A::instances);
	cba_expect_eq_int(9,A::step_count);

//	cba_dump_solution();
//	cba_print_code();
//	cba_dump_json();

	return 0;
}
