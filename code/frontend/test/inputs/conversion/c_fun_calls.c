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
int rec(int a) {
	if(a < 1)
		return a;
	return rec(a-1);
}

int i_to_i(int a) {
	return a;
}

int ii_to_i(int a, int b) {
	return b;
}

void qualified_params(const int a, volatile int b) {
}

int main() {

	#define I_TO_I "def IMP_i_to_i = (v1: int<4>) -> int<4> { return v1; };"

	#pragma test expect_ir(I_TO_I,"IMP_i_to_i(1)")
	i_to_i(1);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	i_to_i(2); // use different number, otherwise same node as above -> pragma doesn't work

	#pragma test expect_ir(I_TO_I,"IMP_i_to_i(IMP_i_to_i(1))")
	i_to_i(i_to_i(1));
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	i_to_i(i_to_i(2));

	#define II_TO_I "def IMP_ii_to_i = (v1: int<4>, v2: int<4>) -> int<4> { return v2; };"

	#pragma test expect_ir(II_TO_I,"IMP_ii_to_i(1, 2)")
	ii_to_i(1,2);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(3,4);

	#pragma test expect_ir(II_TO_I,I_TO_I,"IMP_ii_to_i(IMP_i_to_i(1),2)")
	ii_to_i(i_to_i(1),2);
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(i_to_i(3),4);

	#pragma test expect_ir(II_TO_I,I_TO_I,"IMP_ii_to_i(IMP_i_to_i(1),IMP_ii_to_i(IMP_i_to_i(1),2))")
	ii_to_i(i_to_i(1),ii_to_i(i_to_i(1),2));
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	ii_to_i(i_to_i(2),ii_to_i(i_to_i(3),4));

	#pragma test expect_ir("decl IMP_rec: (int<4>)->int<4>; def IMP_rec = (v1: int<4>) -> int<4> { if(v1<1) { return v1; }; return IMP_rec(v1-1); }; { IMP_rec(3); }")
	{ rec(3); }
	#pragma test expect_ir("EXPR_TYPE","int<4>")
	rec(4);

	#pragma test expect_ir("def IMP_qualified_params = function (v0 : ref<int<4>,t,f,plain>, v1 : ref<int<4>,f,t,plain>) -> unit { }; IMP_qualified_params(1,2)")
	qualified_params(1,2);

	return 0;
}
