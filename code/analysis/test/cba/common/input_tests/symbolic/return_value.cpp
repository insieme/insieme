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

#include "../cba.h"

struct A {
	int x, y;
	A(int a, int b) : x(a), y(b) {}
};

int f() {
	return 12;
}

A g() {
	return {1,2};
}

int main() {

	// check the return value of functions
	cba_expect_symbolic_value("[12]",f());

	// the value of a object type
	cba_expect_symbolic_value("[ref_cast(IMP_A::(ref_temp(type_lit(IMP_A)), 1, 2), type_lit(f), type_lit(f), type_lit(cpp_rref))]",g());

//	// the value of a field of a returned object
//	cba_expect_symbolic_value("[1]",g().x);
//	cba_expect_symbolic_value("[2]",g().y);

//	cba_debug();
//	cba_print_code();

	return 0;
}
