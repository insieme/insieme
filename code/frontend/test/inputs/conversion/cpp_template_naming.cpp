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

namespace ns {

template<typename T>
struct C {
	T t;
};

template<typename Iter>
void foo(const C<Iter>& c) {

	// This is to test, that the template parameters also get encoded in the name of local classes, since different instantiations have to be different
	struct Local {
		int i;
		C<Iter> c;
	};

	Local{0, c};
}

}

int main() {
	;

	#pragma test expect_ir(R"(
		def struct IMP_ns_colon__colon_C_int {
			t : int<4>;
		};
		def struct IMP_ns_colon__colon_C_bool {
			t : bool;
		};
		def struct IMP_Local {
			i : int<4>;
			c : IMP_ns_colon__colon_C_int;
		};
		def struct IMP_Local_instance1 {
			i : int<4>;
			c : IMP_ns_colon__colon_C_bool;
		};
		def IMP_ns_colon__colon_foo_int_returns_void = function (v0 : ref<IMP_ns_colon__colon_C_int,t,f,cpp_ref>) -> unit {
			<ref<IMP_Local,f,f,plain>>(ref_temp(type_lit(IMP_Local))) {0, v0};
		};
		def IMP_ns_colon__colon_foo_bool_returns_void = function (v0 : ref<IMP_ns_colon__colon_C_bool,t,f,cpp_ref>) -> unit {
			<ref<IMP_Local_instance1,f,f,plain>>(ref_temp(type_lit(IMP_Local_instance1))) {0, v0};
		};
		{
			IMP_ns_colon__colon_foo_int_returns_void(ref_kind_cast(IMP_ns_colon__colon_C_int::(ref_temp(type_lit(IMP_ns_colon__colon_C_int))), type_lit(cpp_ref)));
			IMP_ns_colon__colon_foo_bool_returns_void(ref_kind_cast(IMP_ns_colon__colon_C_bool::(ref_temp(type_lit(IMP_ns_colon__colon_C_bool))), type_lit(cpp_ref)));
		}
	)")
	{
		ns::foo(ns::C<int>());
		ns::foo(ns::C<bool>());
	}

	return 0;
}
