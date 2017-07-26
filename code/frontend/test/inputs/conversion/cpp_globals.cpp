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

// a trivial struct
struct Trivial {
};

Trivial x;

struct A {
	int x;
	A(int y) { x = y; };
};

A a(5);

#pragma test expect_ir(R"INSPIRE(
def struct IMP_A {
    x : int<4>;
    ctor function (v1 : ref<int<4>,f,f,plain>) {
        x = *v1;
    }
};
def struct IMP_Trivial {};
def IMP_main = function () -> int<4> {
    IMP_Trivial::(lit("x" : ref<IMP_Trivial,f,f,plain>));
    IMP_A::(lit("a" : ref<IMP_A,f,f,plain>), 5);
    lit("x" : ref<IMP_Trivial,f,f,plain>);
    lit("a" : ref<IMP_A,f,f,plain>);
    return 0;
};
IMP_main
)INSPIRE")
int main() {
	x;
	a;
	return 0;
}
