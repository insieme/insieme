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

struct Base { };

struct Derived;
Derived&& move(Derived& d) {
	return static_cast<Derived&&>(d);
}

struct Derived : public Base {
	Derived() {}
	Derived(Derived&& other) : Base(move(other)) {}
};

int main() {
	;

	#pragma test expect_ir(R"(
		decl IMP_move : (ref<IMP_Derived,f,f,cpp_ref>) -> ref<IMP_Derived,f,f,cpp_rref>;
		def struct IMP_Base {
		};
		def struct IMP_Derived : [ public IMP_Base ] {
			ctor function () {
				IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)));
			}
			ctor function (v1 : ref<IMP_Derived,f,f,cpp_rref>) {
				IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)), ref_parent_cast(IMP_move(ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(IMP_Base)));
			}
		};
		def IMP_move = function (v0 : ref<IMP_Derived,f,f,cpp_ref>) -> ref<IMP_Derived,f,f,cpp_rref> {
			return ref_cast(v0, type_lit(f), type_lit(f), type_lit(cpp_rref));
		};
		{
			var ref<IMP_Derived,f,f,plain> v0 = IMP_Derived::(ref_decl(type_lit(ref<IMP_Derived,f,f,plain>)));
			var ref<IMP_Derived,f,f,plain> v1 = IMP_move(ref_kind_cast(v0, type_lit(cpp_ref)));
		}
	)")
	{
		Derived d1;
		Derived d2(move(d1));
	}

	return 0;
}
