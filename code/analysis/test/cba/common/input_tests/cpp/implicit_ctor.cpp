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

#include <utility>

#include "../../input_tests/cba.h"

struct A {
	int x;
	A() { x = 1; }
	A(const A& other) : x(other.x) {}	// explicit impl, since Insieme-default is empty
	A(A&& other) : x(other.x) { other.x = 0; };
	A(int x) { this->x = x; }
};

int extract(A a) {
	return a.x;
}

A& reflect(A a) {
	return a;
}

int main() {

	// default initialization
	A a;

	// check that a is initialized
	cba_expect_defined_ptr(&a);

	// check that the default constructor was processed
	cba_expect_eq_int(a.x, 1);

	// check pass-by-value
	cba_expect_eq_int(1,extract(a));

	// a should be untouched
	cba_expect_eq_int(a.x,1);

	// check implicit move constructor call
	cba_expect_eq_int(1,extract(static_cast<A&&>(a)));

	// the original instance should now be altered
	cba_expect_eq_int(a.x,0);

	// check the reference
	cba_expect_defined_ptr(&reflect(a));
	cba_expect_defined_ptr(&reflect(static_cast<A&&>(a)));

	cba_expect_not_alias(&a,&reflect(a));
	cba_expect_not_alias(&a,&reflect(static_cast<A&&>(a)));

	return 0;
}
