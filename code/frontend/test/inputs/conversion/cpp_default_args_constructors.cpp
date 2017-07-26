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

struct Base {};

struct T1 {
	int i;
	int j;
	T1(int i = 5) : i(i), j(4) {
		3;
	}
};

struct T2 : public Base {
	int i;
	int j;
	T2(int i = 5, bool b = false) : i(i), j(4) {
		3;
	}
};

int main() {

	// Here we test that the frontend also generates a default ctor without any arguments
	// This is necessary, since there might be locations in the code where instances get created without any ctor call involved - not even
	// in the Clang AST (like default construction of member objects, creation of arrays of object types, ...)

	// So what is actually important here is that the generated struct contains a ctor without any parameters, which calls the other one

	#pragma test expect_ir(R"(
		def struct IMP_Base {
		};
		def struct IMP_T2 : [ public IMP_Base ] {
			i : int<4>;
			j : int<4>;
			ctor function (v1 : ref<int<4>,f,f,plain>, v2 : ref<bool,f,f,plain>) {
				IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)));
				<ref<int<4>,f,f,plain>>((this).i) {*v1};
				<ref<int<4>,f,f,plain>>((this).j) {4};
				3;
			}
			ctor function () {
				IMP_T2::(this, 5, false);
			}
		};
		def struct IMP_T1 {
			i : int<4>;
			j : int<4>;
			ctor function (v1 : ref<int<4>,f,f,plain>) {
				<ref<int<4>,f,f,plain>>((this).i) {*v1};
				<ref<int<4>,f,f,plain>>((this).j) {4};
				3;
			}
			ctor function () {
				IMP_T1::(this, 5);
			}
		};
		{
			var ref<IMP_T1,f,f,plain> v0 = IMP_T1::(ref_decl(type_lit(ref<IMP_T1,f,f,plain>)), 5);
			var ref<IMP_T2,f,f,plain> v1 = IMP_T2::(ref_decl(type_lit(ref<IMP_T2,f,f,plain>)), 5, false);
		}
	)")
	{
		T1 t1;
		T2 t2;
	}

	return 0;
}
