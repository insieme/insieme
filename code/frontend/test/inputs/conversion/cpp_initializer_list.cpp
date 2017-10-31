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

#include <initializer_list>

#define int_IR R"(
	decl struct IMP_std_colon__colon_initializer_list_int;
	decl IMP_size_returns_size_type:const IMP_std_colon__colon_initializer_list_int::() -> uint<8>;
	def struct IMP_std_colon__colon_initializer_list_int {
		_M_array : ptr<int<4>,t,f>;
		_M_len : uint<8>;
		_M_original : bool;
		ctor function () {
			(this)._M_len = 0ul;
			(this)._M_original = true;
		}
		ctor function (v1 : ref<IMP_std_colon__colon_initializer_list_int,t,f,cpp_ref>) {
			(this)._M_array = *v1._M_array;
			(this)._M_len = *v1._M_len;
			(this)._M_original = false;
		}
		ctor function (v1 : ref<IMP_std_colon__colon_initializer_list_int,f,f,cpp_rref>) {
			(this)._M_array = *v1._M_array;
			(this)._M_len = *v1._M_len;
			(this)._M_original = false;
		}
		ctor function (v1 : ref<int<4>,t,f,cpp_ref>) {
			(this)._M_array = ptr_cast(ptr_from_array(<ref<array<int<4>,1u>,f,f,plain>>(ref_new(type_lit(array<int<4>,1u>))) {v1}), type_lit(t), type_lit(f));
			(this)._M_len = 1ul;
			(this)._M_original = true;
		}
		ctor function (v1 : ref<int<4>,t,f,cpp_ref>, v2 : ref<int<4>,t,f,cpp_ref>) {
			(this)._M_array = ptr_cast(ptr_from_array(<ref<array<int<4>,2u>,f,f,plain>>(ref_new(type_lit(array<int<4>,2u>))) {v1, v2}), type_lit(t), type_lit(f));
			(this)._M_len = 2ul;
			(this)._M_original = true;
		}
		ctor function (v1 : ref<int<4>,t,f,cpp_ref>, v2 : ref<int<4>,t,f,cpp_ref>, v3 : ref<int<4>,t,f,cpp_ref>) {
			(this)._M_array = ptr_cast(ptr_from_array(<ref<array<int<4>,3u>,f,f,plain>>(ref_new(type_lit(array<int<4>,3u>))) {v1, v2, v3}), type_lit(t), type_lit(f));
			(this)._M_len = 3ul;
			(this)._M_original = true;
		}
		dtor function () {
			if(*(this)._M_original) {
				if(*(this)._M_len!=0ul) {
					ref_delete(ptr_to_array(ptr_const_cast(*(this)._M_array, type_lit(f))));
				}
			}
		}
		function IMP__operator_assign_ = (v1 : ref<IMP_std_colon__colon_initializer_list_int,t,f,cpp_ref>) -> ref<IMP_std_colon__colon_initializer_list_int,f,f,cpp_ref> {
			(this)._M_array = *v1._M_array;
			(this)._M_len = *v1._M_len;
			(this)._M_original = false;
			return this;
		}
		function IMP__operator_assign_ = (v1 : ref<IMP_std_colon__colon_initializer_list_int,f,f,cpp_rref>) -> ref<IMP_std_colon__colon_initializer_list_int,f,f,cpp_ref> {
			(this)._M_array = *v1._M_array;
			(this)._M_len = *v1._M_len;
			(this)._M_original = false;
			return this;
		}
		const function IMP_begin_returns_const_iterator = () -> ptr<int<4>,t,f> {
			return *(this)._M_array;
		}
		const function IMP_end_returns_const_iterator = () -> ptr<int<4>,t,f> {
			return ptr_add(this.IMP_begin_returns_const_iterator(), num_cast(this.IMP_size_returns_size_type(), type_lit(int<8>)));
		}
		const function IMP_size_returns_size_type = () -> uint<8> {
			return *(this)._M_len;
		}
	};
)"

struct S {
	int i1;
	S(const std::initializer_list<int>& initList) {
		for(int i : initList) {
			i1 = i;
		}
	}
};

void f(std::initializer_list<int> l) {}

struct TestObj {
	TestObj() { 5; }
	TestObj(const TestObj& other) { 5; }
	TestObj(TestObj&& other) { 5; }
	~TestObj() { 5; }
};

struct T2 {
	T2(std::initializer_list<TestObj> l) {}
};

int main() {
	;

	#pragma test expect_ir(int_IR, R"(
		{
			var ref<IMP_std_colon__colon_initializer_list_int,f,f,plain> v0 = ref_cast(IMP_std_colon__colon_initializer_list_int::(ref_temp(type_lit(IMP_std_colon__colon_initializer_list_int)), 1, 2, 3), type_lit(t), type_lit(f), type_lit(cpp_ref));
		}
	)")
	{
		std::initializer_list<int> a = {1,2,3};
	}

	#pragma test expect_ir(int_IR, R"(
		def struct IMP_S {
			i1 : int<4>;
			ctor function (v1 : ref<IMP_std_colon__colon_initializer_list_int,t,f,cpp_ref>) {
				var ref<IMP_std_colon__colon_initializer_list_int,t,f,cpp_ref> v2 = v1;
				var ref<ptr<int<4>,t,f>,f,f,plain> v3 = v2.IMP_begin_returns_const_iterator();
				var ref<ptr<int<4>,t,f>,f,f,plain> v4 = v2.IMP_end_returns_const_iterator();
				while(ptr_ne(*v3, *v4)) {
					var ref<int<4>,f,f,plain> v5 = *ptr_to_ref(*v3);
					{
						(this).i1 = *v5;
						ptr_pre_inc(v3);
					}
				}
			}
		};
		{
			IMP_S::(ref_temp(type_lit(IMP_S)), ref_kind_cast(IMP_std_colon__colon_initializer_list_int::(ref_temp(type_lit(IMP_std_colon__colon_initializer_list_int)), 42, 43), type_lit(cpp_ref))).i1;
		}
	)")
	{
		S{42,43}.i1;
	}

	#pragma test expect_ir(int_IR, R"(
		def IMP_f = function (v0 : ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>) -> unit { };
		{
			IMP_f(ref_cast(IMP_std_colon__colon_initializer_list_int::(ref_temp(type_lit(IMP_std_colon__colon_initializer_list_int)), 5), type_lit(t), type_lit(f), type_lit(cpp_ref)));
		}
	)")
	{
		f({ 5 });
	}

	#pragma test expect_ir(R"(
		decl struct IMP_TestObj;
		decl IMP_size_returns_size_type:const IMP_std_colon__colon_initializer_list_struct_space_TestObj::() -> uint<8>;
		decl IMP_end_returns_const_iterator:const IMP_std_colon__colon_initializer_list_struct_space_TestObj::() -> ptr<IMP_TestObj,t,f>;
		decl IMP_begin_returns_const_iterator:const IMP_std_colon__colon_initializer_list_struct_space_TestObj::() -> ptr<IMP_TestObj,t,f>;
		def struct IMP_std_colon__colon_initializer_list_struct_space_TestObj {
			_M_array : ptr<IMP_TestObj,t,f>;
			_M_len : uint<8>;
			_M_original : bool;
			ctor function () {
				(this)._M_len = 0ul;
				(this)._M_original = true;
			}
			ctor function (v1 : ref<IMP_std_colon__colon_initializer_list_struct_space_TestObj,t,f,cpp_ref>) {
				(this)._M_array = *v1._M_array;
				(this)._M_len = *v1._M_len;
				(this)._M_original = false;
			}
			ctor function (v1 : ref<IMP_std_colon__colon_initializer_list_struct_space_TestObj,f,f,cpp_rref>) {
				(this)._M_array = *v1._M_array;
				(this)._M_len = *v1._M_len;
				(this)._M_original = false;
			}
			ctor function (v1 : ref<IMP_TestObj,t,f,cpp_ref>, v2 : ref<IMP_TestObj,t,f,cpp_ref>) {
				(this)._M_array = ptr_cast(ptr_from_array(<ref<array<IMP_TestObj,2u>,f,f,plain>>(ref_new(type_lit(array<IMP_TestObj,2u>))) {v1, v2}), type_lit(t), type_lit(f));
				(this)._M_len = 2ul;
				(this)._M_original = true;
			}
			dtor function () {
				if(*(this)._M_original) {
					if(*(this)._M_len!=0ul) {
						ref_delete(ptr_to_array(ptr_const_cast(*(this)._M_array, type_lit(f))));
					}
				}
			}
			function IMP__operator_assign_ = (v1 : ref<IMP_std_colon__colon_initializer_list_struct_space_TestObj,t,f,cpp_ref>) -> ref<IMP_std_colon__colon_initializer_list_struct_space_TestObj,f,f,cpp_ref> {
				(this)._M_array = *v1._M_array;
				(this)._M_len = *v1._M_len;
				(this)._M_original = false;
				return this;
			}
			function IMP__operator_assign_ = (v1 : ref<IMP_std_colon__colon_initializer_list_struct_space_TestObj,f,f,cpp_rref>) -> ref<IMP_std_colon__colon_initializer_list_struct_space_TestObj,f,f,cpp_ref> {
				(this)._M_array = *v1._M_array;
				(this)._M_len = *v1._M_len;
				(this)._M_original = false;
				return this;
			}
			const function IMP_begin_returns_const_iterator = () -> ptr<IMP_TestObj,t,f> {
				return *(this)._M_array;
			}
			const function IMP_end_returns_const_iterator = () -> ptr<IMP_TestObj,t,f> {
				return ptr_add(this.IMP_begin_returns_const_iterator(), num_cast(this.IMP_size_returns_size_type(), type_lit(int<8>)));
			}
			const function IMP_size_returns_size_type = () -> uint<8> {
				return *(this)._M_len;
			}
		};
		def struct IMP_T2 {
			ctor function (v1 : ref<IMP_std_colon__colon_initializer_list_struct_space_TestObj,f,f,plain>) { }
		};
		def struct IMP_TestObj {
			ctor function () {
				5;
			}
			ctor function (v1 : ref<IMP_TestObj,t,f,cpp_ref>) {
				5;
			}
			ctor function (v1 : ref<IMP_TestObj,f,f,cpp_rref>) {
				5;
			}
			dtor function () {
				5;
			}
		};
		{
			var ref<IMP_TestObj,f,f,plain> v0 = IMP_TestObj::(ref_decl(type_lit(ref<IMP_TestObj,f,f,plain>)));
			IMP_T2::(ref_temp(type_lit(IMP_T2)), ref_cast(IMP_std_colon__colon_initializer_list_struct_space_TestObj::(ref_temp(type_lit(IMP_std_colon__colon_initializer_list_struct_space_TestObj)), ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref))), type_lit(t), type_lit(f), type_lit(cpp_ref)));
		}
	)")
	{
		TestObj obj;
		T2({ obj, obj });
	}

	return 0;
}
