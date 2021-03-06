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

#include <vector>

int main() {
	;

	#pragma test expect_ir(R"(
		{
			var ref<array<int<4>,3u>,f,f,plain> v0 = <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1, 2, 3};
			var ref<array<int<4>,3u>,f,f,cpp_ref> v1 = ref_kind_cast(v0, type_lit(cpp_ref));
			var ref<ptr<int<4>>,f,f,plain> v2 = ptr_from_array(ref_kind_cast(v1, type_lit(plain)));
			var ref<ptr<int<4>>,f,f,plain> v3 = ptr_add(ptr_from_array(ref_kind_cast(v1, type_lit(plain))), 3l);
			while(ptr_ne(*v2, *v3)) {
				var ref<int<4>,f,f,plain> v4 = *ptr_to_ref(*v2);
				{
					v4;
					ptr_pre_inc(v2);
				}
			}
		}
	)")
	{
		int list[] = {1, 2, 3};
		for(auto element : list) {
			element;
		}
	}

	#pragma test expect_ir(R"(
		{
			var ref<array<int<4>,3u>,f,f,plain> v0 = <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1, 2, 3};
			var ref<array<int<4>,3u>,f,f,cpp_ref> v1 = ref_kind_cast(v0,type_lit(cpp_ref));
			var ref<ptr<int<4>>,f,f,plain> v2 = ptr_from_array(ref_kind_cast(v1, type_lit(plain)));
			var ref<ptr<int<4>>,f,f,plain> v3 = ptr_add(ptr_from_array(ref_kind_cast(v1, type_lit(plain))), 3l);
			while(ptr_ne(*v2, *v3)) {
				var ref<int<4>,f,f,plain> v4 = *ptr_to_ref(*v2);
				{
					{
						if(*v4==2) {
							ptr_pre_inc(v2);
							continue;
						}
						v4;
					}
					ptr_pre_inc(v2);
				}
			}
		}
	)")
	{
		int list[] = {1, 2, 3};
		for(auto element : list) {
			if (element == 2) continue;
			element;
		}
	}

	#pragma test expect_ir(R"(
		{
			var ref<array<int<4>,3u>,f,f,plain> v0 = <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1, 2, 3};
			var ref<array<int<4>,3u>,f,f,cpp_ref> v1 = ref_kind_cast(v0, type_lit(cpp_ref));
			var ref<ptr<int<4>>,f,f,plain> v2 = ptr_from_array(ref_kind_cast(v1, type_lit(plain)));
			var ref<ptr<int<4>>,f,f,plain> v3 = ptr_add(ptr_from_array(ref_kind_cast(v1, type_lit(plain))), 3l);
			while(ptr_ne(*v2, *v3)) {
				var ref<int<4>,f,f,plain> v4 = *ptr_to_ref(*v2);
				{
					{ }
					ptr_pre_inc(v2);
				}
			}
		}
	)")
	{
		int list[] = {1, 2, 3};
		for(auto element : list);
	}

//	#pragma test expect_ir(R"(
//		{
//		}
//	)")
//	{
//		std::vector<int> list;
//		for(auto element : list) {
//			element;
//		}
//	}

	return 0;
}
