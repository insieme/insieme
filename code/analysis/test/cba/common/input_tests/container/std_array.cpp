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

#include <array>

#include "array_like_container_test.h"

int main() {

	// all tests delegated
	test<std::array<int,4>>();

	// test special array capabilities
	std::array<int,4> a { 1, 2, 3, 4 };
	std::array<int,4> b { 5, 6, 7, 8 };

//	cba_expect_eq_int(1,a[0]);
//	cba_expect_eq_int(2,a[1]);
//	cba_expect_eq_int(3,a[2]);
//	cba_expect_eq_int(4,a[3]);
//
//	cba_expect_eq_int(5,b[0]);
//	cba_expect_eq_int(6,b[1]);
//	cba_expect_eq_int(7,b[2]);
//	cba_expect_eq_int(8,b[3]);
//
//	// check the at operator
//	cba_expect_eq_int(1,a.at(0));
//	cba_expect_eq_int(2,a.at(1));
//	cba_expect_eq_int(3,a.at(2));
//	cba_expect_eq_int(4,a.at(3));
//
//	cba_expect_eq_int(5,b.at(0));
//	cba_expect_eq_int(6,b.at(1));
//	cba_expect_eq_int(7,b.at(2));
//	cba_expect_eq_int(8,b.at(3));

	// check copy constructor
	std::array<int,4> c = a;

//	cba_expect_eq_int(1,a[0]);
//	cba_expect_eq_int(2,a[1]);
//	cba_expect_eq_int(3,a[2]);
//	cba_expect_eq_int(4,a[3]);
//
//	cba_expect_eq_int(5,b[0]);
//	cba_expect_eq_int(6,b[1]);
//	cba_expect_eq_int(7,b[2]);
//	cba_expect_eq_int(8,b[3]);
//
//	cba_expect_eq_int(1,c[0]);
//	cba_expect_eq_int(2,c[1]);
//	cba_expect_eq_int(3,c[2]);
//	cba_expect_eq_int(4,c[3]);

	cba_expect_not_alias(&a[0],&c[0]);

//	cba_debug();

//	cba_expect_not_alias(&a[1],&c[1]);
//	cba_expect_not_alias(&a[2],&c[2]);
//	cba_expect_not_alias(&a[3],&c[3]);

	// mutate copy (and copy only)
//	c[2] = 9;


//	cba_expect_eq_int(1,a[0]);
//	cba_expect_eq_int(2,a[1]);
//	cba_expect_eq_int(3,a[2]);
//	cba_expect_eq_int(4,a[3]);
//
//	cba_expect_eq_int(5,b[0]);
//	cba_expect_eq_int(6,b[1]);
//	cba_expect_eq_int(7,b[2]);
//	cba_expect_eq_int(8,b[3]);

//	cba_expect_eq_int(1,c[0]);
//	cba_expect_eq_int(2,c[1]);
//	cba_expect_eq_int(9,c[2]);
//	cba_expect_eq_int(4,c[3]);

	// check copy assignment
//	c = b;
//
//	cba_expect_eq_int(1,a[0]);
//	cba_expect_eq_int(2,a[1]);
//	cba_expect_eq_int(3,a[2]);
//	cba_expect_eq_int(4,a[3]);
//
//	cba_expect_eq_int(5,b[0]);
//	cba_expect_eq_int(6,b[1]);
//	cba_expect_eq_int(7,b[2]);
//	cba_expect_eq_int(8,b[3]);
//
//	cba_expect_eq_int(5,c[0]);
//	cba_expect_eq_int(6,c[1]);
//	cba_expect_eq_int(7,c[2]);
//	cba_expect_eq_int(8,c[3]);

//	// check aliases
//	std::array<int,4>& d = a;
//
//	cba_expect_eq_int(1,a[0]);
//	cba_expect_eq_int(2,a[1]);
//	cba_expect_eq_int(3,a[2]);
//	cba_expect_eq_int(4,a[3]);
//
//	cba_expect_eq_int(5,b[0]);
//	cba_expect_eq_int(6,b[1]);
//	cba_expect_eq_int(7,b[2]);
//	cba_expect_eq_int(8,b[3]);
//
//	cba_expect_eq_int(5,c[0]);
//	cba_expect_eq_int(6,c[1]);
//	cba_expect_eq_int(7,c[2]);
//	cba_expect_eq_int(8,c[3]);
//
//	cba_expect_eq_int(1,d[0]);
//	cba_expect_eq_int(2,d[1]);
//	cba_expect_eq_int(3,d[2]);
//	cba_expect_eq_int(4,d[3]);
//
//	d[2] = 9;
//
//	cba_expect_eq_int(1,a[0]);
//	cba_expect_eq_int(2,a[1]);
//	cba_expect_eq_int(9,a[2]);
//	cba_expect_eq_int(4,a[3]);
//
//	cba_expect_eq_int(5,b[0]);
//	cba_expect_eq_int(6,b[1]);
//	cba_expect_eq_int(7,b[2]);
//	cba_expect_eq_int(8,b[3]);
//
//	cba_expect_eq_int(5,c[0]);
//	cba_expect_eq_int(6,c[1]);
//	cba_expect_eq_int(7,c[2]);
//	cba_expect_eq_int(8,c[3]);
//
//	cba_expect_eq_int(1,d[0]);
//	cba_expect_eq_int(2,d[1]);
//	cba_expect_eq_int(9,d[2]);
//	cba_expect_eq_int(4,d[3]);
//
//	cba_expect_is_alias(&a[0],&d[0]);
//	cba_expect_is_alias(&a[1],&d[1]);
//	cba_expect_is_alias(&a[2],&d[2]);
//	cba_expect_is_alias(&a[3],&d[3]);

	// done
	return 0;
}
