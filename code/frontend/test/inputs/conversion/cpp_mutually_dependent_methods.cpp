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

// Introduce mutual dependency to method of A while translating type B, coming from A
struct B;

struct A {
	void methA(B* b) { }
	void foo() { }
};

struct B {
	static unsigned test; // ensure global variable is only inserted once
	void methB(A a) {
		a.foo();
	}
};

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"(
		decl struct IMP_B;
		decl struct IMP_A;
		decl IMP_methB:IMP_B::(IMP_A) -> unit;
		decl IMP_foo:IMP_A::() -> unit;
		decl IMP_methA:IMP_A::(ptr<IMP_B>) -> unit;
		def struct IMP_B {
			function IMP_methB = (v1 : ref<IMP_A,f,f,plain>) -> unit {
				v1.IMP_foo();
			}
		};
		def struct IMP_A {
			function IMP_foo = () -> unit { }
			function IMP_methA = (v1 : ref<ptr<IMP_B>,f,f,plain>) -> unit { }
		};
		{
			var ref<IMP_A,f,f,plain> v0 = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
		}
	)")
	{
		A a;
	}

	return 0;
}
