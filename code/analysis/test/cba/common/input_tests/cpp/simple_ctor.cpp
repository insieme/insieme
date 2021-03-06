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
	int x;
	A() { x = 1; }
	A(const A& other) : x(other.x) {}	// explicit impl, since Insieme-default is empty
	A(int x) { this->x = x; }
};

int main() {

	// default initialization
	A a;

	// check that a is initialized
	cba_expect_defined_ptr(&a);

	// check that the default constructor was processed
	cba_expect_eq_int(a.x, 1);


	// check a call with parameter
	A b(2);
	cba_expect_eq_int(b.x,2);

	// check the initialization without explicit constructor
	A c = 3;
	cba_expect_eq_int(c.x,3);

	// check a new call
	A* d = new A();
	cba_expect_eq_int(d->x,1);


	// check a new call with parameters
	A* e = new A(3);
	cba_expect_eq_int(e->x,3);


	// check an explicit copy constructor
	A f(b);
	cba_expect_not_alias(&f,&b);
	cba_expect_not_alias(&f.x,&b.x);
	cba_expect_eq_int(f.x,2);

	// check another form of implicit copy constructor
	A g = b;
	cba_expect_not_alias(&g,&b);
	cba_expect_not_alias(&g.x,&b.x);
	cba_expect_eq_int(g.x,2);


	// check creation of an alias
	A& h = b;
	cba_expect_eq_int(h.x,2);

	// h is a alias, not a copy
	cba_expect_is_alias(&h,&b);
	cba_expect_is_alias(&h.x,&b.x);


//	cba_debug();

//	int x = (argc > 2) ? c->x : d->x;
////	cba_expect_one_of_int(x,iset(1,3));
//	cba_expect_defined_int(x);

	return 0;
}
