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

#include <utility>

struct A {
	int i;
	A() = default;
	A(int i) : i(i) {}
};

struct B {
	int i;
	B(int&& i) : i(std::move(i)) {}

	int&& get() {
		return std::move(i);
	}
};

void foo(A&& a) {}

int main() {
	;

	#pragma test expect_ir(R"(
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<int<4>,f,f,cpp_rref> v1 = ref_move_plain(v0);
			var ref<int<4>,f,f,cpp_ref> v2 = ref_kind_cast(v0, type_lit(cpp_ref));
			var ref<int<4>,f,f,cpp_rref> v3 = ref_move_reference(v2);
			var ref<int<4>,f,f,cpp_rref> v4 = ref_move_r_value_reference(v3);
		}
	)")
	{
		int i;
		int&& j = std::move(i);
		int& ir = i;
		int&& jr = std::move(ir);
		int&& jrr = std::move(jr);
	}

	#pragma test expect_ir(R"(
		def struct IMP_A {
			i : int<4>;
			ctor function () = default;
			ctor function (v1 : ref<int<4>,f,f,plain>) {
				<ref<int<4>,f,f,plain>>((this).i) {*v1};
			}
		};
		def IMP_foo = function (v0 : ref<IMP_A,f,f,cpp_rref>) -> unit { };
		{
			var ref<IMP_A,f,f,plain> v0 = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
			IMP_foo(ref_move_plain(v0));
			var ref<IMP_A,f,f,cpp_ref> v1 = ref_kind_cast(v0, type_lit(cpp_ref));
			IMP_foo(ref_move_reference(v1));
		}
	)")
	{
		A a;
		foo(std::move(a));
		A& ar = a;
		foo(std::move(ar));
	}

	#pragma test expect_ir(R"(
		def struct IMP_B {
			i : int<4>;
			ctor function () = delete;
			ctor function (v1 : ref<int<4>,f,f,cpp_rref>) {
				<ref<int<4>,f,f,plain>>((this).i) {*ref_move_r_value_reference(v1)};
			}
			function IMP_get = () -> ref<int<4>,f,f,cpp_rref> {
				return ref_move_plain((this).i);
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			var ref<IMP_B,f,f,plain> v1 = IMP_B::(
					ref_decl(type_lit(ref<IMP_B,f,f,plain>)),
					ref_move_plain(v0)
			);
			v1.IMP_get();
			var ref<IMP_B,f,f,plain> v2 = IMP_B::(
					ref_decl(type_lit(ref<IMP_B,f,f,plain>)),
					ref_kind_cast(ref_temp_init(2), type_lit(cpp_rref))
			);
			v2.IMP_get();
		}
	)")
	{
		int i;
		B b1(std::move(i));
		b1.get();
		B b2(2);
		b2.get();
	}

	return 0;
}
