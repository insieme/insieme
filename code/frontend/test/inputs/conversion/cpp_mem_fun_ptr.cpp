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

struct A {
	void foo() const {};
};

struct B {
	float bla(int a) { return 5.0f; };
};

int main() {
	;

	#pragma test expect_ir(R"(
		def struct IMP_A {
			const function IMP_foo = () -> unit { }
		};
		{
			var ref<ptr<const IMP_A::() -> unit,t,f>,f,f,plain> v0 = ptr_of_function(IMP_A::IMP_foo);
		}
	)")
	{
		auto f = &A::foo;
	}

	#pragma test expect_ir(R"(
		def struct IMP_B {
			function IMP_bla = (v1 : ref<int<4>,f,f,plain>) -> real<4> {
				return 5.0E+0f;
			}
		};
		{
			var ref<ptr<IMP_B::(int<4>) -> real<4>,t,f>,f,f,plain> v0 = ptr_of_function(IMP_B::IMP_bla);
		}
	)")
	{
		auto f = &B::bla;
	}

	return 0;
}
