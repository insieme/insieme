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

int f(int x = 7) {
	return x;
}
#define F_IR "def IMP_f = function (v0 : ref<int<4>,f,f,plain>) -> int<4> { return *v0; }; "


void g(int x, unsigned y = 4u, char z = 'd') {}
#define G_IR "def IMP_g = function (v0 : ref<int<4>,f,f,plain>, v1 : ref<uint<4>,f,f,plain>, v2 : ref<char,f,f,plain>) -> unit { }; "

struct S {};
void s(S s = S()) {};

int main() {
#pragma test expect_ir(F_IR, "IMP_f(4)")
	f(4);

	#pragma test expect_ir(F_IR, "IMP_f(7)")
	f();

	#pragma test expect_ir(G_IR, R"(IMP_g(4, 19u, lit("'s'":char)))")
	g(4, 19u, 's');

	#pragma test expect_ir(G_IR, R"(IMP_g(5, 12u, lit("'d'":char)))")
	g(5, 12u);

	#pragma test expect_ir(G_IR, R"(IMP_g(7, 4u, lit("'d'":char)))")
	g(7);

	#pragma test expect_ir(R"(
		def struct IMP_S {};
		def IMP_s = function (v0 : ref<IMP_S,f,f,plain>) -> unit { };
		IMP_s(ref_cast(IMP_S::(ref_temp(type_lit(IMP_S))), type_lit(f), type_lit(f), type_lit(cpp_rref)))
	)")
	s();

	return 0;
}
