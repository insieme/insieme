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

// this struct is trivially copyable, but it is _not trivial_, since it doesn't have a default constructor
struct S {
	S(int i) {}
};

template<typename Iter>
struct range_spliter {
	static std::pair<Iter,Iter> split() {
		return { {5}, {6} };
	};
};

void constSRefConsumer(const S& i) {}

int main() {

	int magic;
	// This test ensures that the type deduction checks for the trivially copyable property and not for triviality of types.

	#pragma test expect_ir(R"(
		def struct IMP_S {
			ctor function (v1 : ref<int<4>,f,f,plain>) { }
		};
		def IMP_constSRefConsumer = function (v0 : ref<IMP_S,t,f,cpp_ref>) -> unit { };
		def IMP_range_spliter_lt_S_gt__colon__colon_split_returns_std_colon__colon_pair_lt_S_comma__S_gt_ = function () -> IMP_std_colon__colon_pair<ref<IMP_S,f,f,qualified>,ref<IMP_S,f,f,qualified>> {
			return instantiate(lit("target_type" : IMP_std_colon__colon_pair<ref<IMP_S,f,f,qualified>,ref<IMP_S,f,f,qualified>>::(ref<IMP_S,t,f,cpp_ref>, ref<IMP_S,t,f,cpp_ref>)), lit("IMP_std_colon__colon_pair::ctor" : IMP_std_colon__colon_pair<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::(ref<'T_0_0,t,f,cpp_ref>, ref<'T_0_1,t,f,cpp_ref>)))(ref_decl(type_lit(ref<IMP_std_colon__colon_pair<ref<IMP_S,f,f,qualified>,ref<IMP_S,f,f,qualified>>,f,f,plain>)), ref_kind_cast(IMP_S::(ref_temp(type_lit(IMP_S)), 5), type_lit(cpp_ref)), ref_kind_cast(IMP_S::(ref_temp(type_lit(IMP_S)), 6), type_lit(cpp_ref)));
		};
		{
			IMP_constSRefConsumer(ref_kind_cast(ref_member_access(IMP_range_spliter_lt_S_gt__colon__colon_split_returns_std_colon__colon_pair_lt_S_comma__S_gt_() materialize, lit("first"), type_lit(IMP_S)), type_lit(cpp_ref)));
		}
	)")
	{
		constSRefConsumer(range_spliter<S>::split().first);
	}
}
