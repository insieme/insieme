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
	A(const A& other) : x(other.x), y(other.y) {}
	A(A&& other) : x(other.x), y(other.y) {}
};

template<typename T>
T copy(const T& value) {
	return value;
}

template<typename T>
T move(T&& value) {
	return value;
}


int main() {

	A a(1,2);

	// just taking the local value should be fine
	cba_expect_symbolic_value("[ref_cast(IMP_A::(ref_temp(type_lit(IMP_A)), 1, 2), type_lit(t), type_lit(f), type_lit(cpp_ref))]",a);

//	cba_expect_symbolic_value("",copy(a));

	cba_expect_symbolic_value("[ref_cast(IMP_A::(ref_temp(type_lit(IMP_A)), ref_cast(ref_cast(IMP_A::(ref_temp(type_lit(IMP_A)), 1, 2), type_lit(f), type_lit(f), type_lit(cpp_rref)), type_lit(t), type_lit(f), type_lit(cpp_ref))), type_lit(f), type_lit(f), type_lit(cpp_rref))]",move(static_cast<A&&>(a)));


//	cba_debug();
//	cba_print_code();

	return 0;
}
