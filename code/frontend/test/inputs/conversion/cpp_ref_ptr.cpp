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

void take_ref(int& a) {}
void take_const_ref(const int& a) {}
void take_ptr(int* a) {}


int* g;
int& gen_ref() { return *g; }
int* gen_ptr() { return g; }

typedef int* intPtr;
void take_const_ptr_ref(const intPtr& a) {}
const intPtr& return_const_ptr_ref() {
	int i;
	return &i;
}

int main() {

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<ptr<int<4>>,f,f,plain> v1 = ptr_from_ref(v0);
		var ref<int<4>,f,f,cpp_ref> v2 = ref_kind_cast(v0, type_lit(cpp_ref));
		var ref<int<4>,f,f,plain> v3 = *ptr_to_ref(*v1);
		var ref<ptr<int<4>>,f,f,plain> v4 = ptr_from_ref(ref_cast(v2, type_lit(f), type_lit(f), type_lit(plain)));
		var ref<int<4>,f,f,cpp_ref> v5 = ref_kind_cast(ptr_to_ref(*v1), type_lit(cpp_ref));
	})")
	{
		int i;
		int* i_ptr = &i;
		int& i_ref = i;

		int j = *i_ptr;
		int* j_ptr = &i_ref;
		int& j_ref = *i_ptr;
	}

	#pragma test expect_ir(R"(
		def IMP_take_ref = (a: ref<int<4>,f,f,cpp_ref>) -> unit {};
		def IMP_take_ptr = (a: ptr<int<4>>) -> unit {};
		{
			var ref<int<4>,f,f,plain> v0;
			var ref<ptr<int<4>>,f,f,plain> v1 = ptr_from_ref(v0);
			var ref<int<4>,f,f,cpp_ref> v2 = ref_kind_cast(v0, type_lit(cpp_ref));

			IMP_take_ref(ref_kind_cast(ptr_to_ref(*v1), type_lit(cpp_ref)));
			IMP_take_ptr(ptr_from_ref(ref_cast(v2, type_lit(f), type_lit(f), type_lit(plain))));
		}
	)")
	{
		int i;
		int* i_ptr = &i;
		int& i_ref = i;

		take_ref(*i_ptr);
		take_ptr(&i_ref);
	}

	#pragma test expect_ir(R"(
		def IMP_gen_ref = function () -> ref<int<4>,f,f,cpp_ref> {
			return ref_kind_cast(ptr_to_ref(*lit("g" : ref<ptr<int<4>>,f,f,plain>)), type_lit(cpp_ref));
		};
		def IMP_gen_ptr = function () -> ptr<int<4>> {
			return *lit("g" : ref<ptr<int<4>>,f,f,plain>);
		};
		{
			IMP_gen_ref();
			IMP_gen_ptr();
		}
	)")
	{
		gen_ref();
		gen_ptr();
	}

	#pragma test expect_ir(R"(
		def IMP_take_const_ref = function (v0 : ref<int<4>,t,f,cpp_ref>) -> unit { };
		{
			IMP_take_const_ref(ref_kind_cast(ref_temp_init(5), type_lit(cpp_ref)));
			var ref<int<4>,t,f,cpp_ref> v0 = ref_kind_cast(ref_temp_init(6),type_lit(cpp_ref));
			var ref<int<4>,t,f,cpp_ref> v1 = ref_kind_cast(ref_temp_init(7+8), type_lit(cpp_ref));
		}
	)")
	{
		take_const_ref(5);
		const int& i = 6;
		const int& j = 7 + 8;
	}



	#pragma test expect_ir(R"(
		def IMP_take_const_ptr_ref = function (v0 : ref<ptr<int<4>>,t,f,cpp_ref>) -> unit { };
		def IMP_return_const_ptr_ref = function () -> ref<ptr<int<4>>,t,f,cpp_ref> {
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			return ref_kind_cast(ref_temp_init(ptr_from_ref(v0)), type_lit(cpp_ref));
		};
		{
			var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			IMP_take_const_ptr_ref(ref_kind_cast(ref_temp_init(ptr_from_ref(v0)), type_lit(cpp_ref)));
			IMP_return_const_ptr_ref();
		}
	)")
	{
		int i;
		take_const_ptr_ref(&i);
		return_const_ptr_ref();
	}

	return 0;
}
