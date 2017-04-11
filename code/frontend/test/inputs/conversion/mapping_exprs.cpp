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

class Foo {};

void fooEmpty() {
	42;
}

void barOne(int a) {
	43;
}

void barTwo(int a, int b) {
	44;
}

void barThree(int a, int b, int c) {
	45;
}

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"({
		var ref<foo,f,f,plain> v0 = lit("makeFoo" : () -> foo)();
		lit("makeFoo" : () -> foo)();
		lit("singleBar" : (int<4>) -> unit)(1);
		lit("multipleBar" : (int<4>, int<4>) -> unit)(1, 2);
		lit("multipleBar" : (int<4>, int<4>, int<4>) -> unit)(1, 2, 3);
	})")
	{
		Foo f0;
		fooEmpty();
		barOne(1);
		barTwo(1, 2);
		barThree(1, 2, 3);
	}

	return 0;
}
