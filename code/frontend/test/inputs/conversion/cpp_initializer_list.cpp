/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
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
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

#include <initializer_list>


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

	#pragma test expect_ir(R"(
		decl struct IMP_std_colon__colon_initializer_list_int;
		decl IMP_size_returns_size_type:const IMP_std_colon__colon_initializer_list_int::() -> uint<8>;
		def struct IMP_std_colon__colon_initializer_list_int {
			_M_array : ptr<int<4>,t,f>;
			_M_len : uint<8>;
			ctor function () {
				(this)._M_len = 0ul;
			}
			ctor function (v1 : ref<ptr<int<4>,t,f>,f,f,plain>, v2 : ref<uint<8>,f,f,plain>) {
				var uint<inf> v3 = num_cast(*v2, type_lit(uint<inf>));
				(this)._M_array = ptr_const_cast(ptr_from_array(ref_new(type_lit(array<int<4>,#v3>))), type_lit(t));
				(this)._M_len = *v2;
				for( int<8> v4 = 0l .. num_cast(*v2, type_lit(int<8>)) : 1l) {
					ptr_subscript(ptr_const_cast(*(this)._M_array, type_lit(f)), v4) = *ptr_subscript(*v1, v4);
				}
			}
			dtor function () {
				if(*(this)._M_len!=0ul) {
					ref_delete(ptr_to_ref(ptr_const_cast(*(this)._M_array, type_lit(f))));
				}
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
		{
			var ref<IMP_std_colon__colon_initializer_list_int,f,f,plain> v0 = ref_cast(IMP_std_colon__colon_initializer_list_int::(ref_decl(type_lit(ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>)), ptr_from_array(<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {1, 2, 3}), num_cast(3u, type_lit(uint<8>))), type_lit(t), type_lit(f), type_lit(cpp_ref));
		}
	)")
	{
		std::initializer_list<int> a = {1,2,3};
	}

	#pragma test expect_ir(R"(
		decl struct IMP_std_colon__colon_initializer_list_int;
		decl IMP_size_returns_size_type:const IMP_std_colon__colon_initializer_list_int::() -> uint<8>;
		def struct IMP_std_colon__colon_initializer_list_int {
			_M_array : ptr<int<4>,t,f>;
			_M_len : uint<8>;
			ctor function () {
				(this)._M_len = 0ul;
			}
			ctor function (v1 : ref<ptr<int<4>,t,f>,f,f,plain>, v2 : ref<uint<8>,f,f,plain>) {
				var uint<inf> v3 = num_cast(*v2, type_lit(uint<inf>));
				(this)._M_array = ptr_const_cast(ptr_from_array(ref_new(type_lit(array<int<4>,#v3>))), type_lit(t));
				(this)._M_len = *v2;
				for( int<8> v4 = 0l .. num_cast(*v2, type_lit(int<8>)) : 1l) {
					ptr_subscript(ptr_const_cast(*(this)._M_array, type_lit(f)), v4) = *ptr_subscript(*v1, v4);
				}
			}
			dtor function () {
				if(*(this)._M_len!=0ul) {
					ref_delete(ptr_to_ref(ptr_const_cast(*(this)._M_array, type_lit(f))));
				}
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
			IMP_S::(ref_temp(type_lit(IMP_S)), ref_kind_cast(IMP_std_colon__colon_initializer_list_int::(ref_temp(type_lit(IMP_std_colon__colon_initializer_list_int)), ptr_from_array(<ref<array<int<4>,2>,f,f,plain>>(ref_temp(type_lit(array<int<4>,2>))) {42, 43}), num_cast(2u, type_lit(uint<8>))), type_lit(cpp_ref))).i1;
		}
	)")
	{
		S{42,43}.i1;
	}

	#pragma test expect_ir(R"(
		decl struct IMP_std_colon__colon_initializer_list_int;
		decl IMP_size_returns_size_type:const IMP_std_colon__colon_initializer_list_int::() -> uint<8>;
		def struct IMP_std_colon__colon_initializer_list_int {
			_M_array : ptr<int<4>,t,f>;
			_M_len : uint<8>;
			ctor function () {
				(this)._M_len = 0ul;
			}
			ctor function (v1 : ref<ptr<int<4>,t,f>,f,f,plain>, v2 : ref<uint<8>,f,f,plain>) {
				var uint<inf> v3 = num_cast(*v2, type_lit(uint<inf>));
				(this)._M_array = ptr_const_cast(ptr_from_array(ref_new(type_lit(array<int<4>,#v3>))), type_lit(t));
				(this)._M_len = *v2;
				for( int<8> v4 = 0l .. num_cast(*v2, type_lit(int<8>)) : 1l) {
					ptr_subscript(ptr_const_cast(*(this)._M_array, type_lit(f)), v4) = *ptr_subscript(*v1, v4);
				}
			}
			dtor function () {
				if(*(this)._M_len!=0ul) {
					ref_delete(ptr_to_ref(ptr_const_cast(*(this)._M_array, type_lit(f))));
				}
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
		def IMP_f = function (v0 : ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>) -> unit { };
		{
			IMP_f(ref_cast(IMP_std_colon__colon_initializer_list_int::(ref_decl(type_lit(ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>)), ptr_from_array(<ref<array<int<4>,1>,f,f,plain>>(ref_temp(type_lit(array<int<4>,1>))) {5}), num_cast(1u, type_lit(uint<8>))), type_lit(t), type_lit(f), type_lit(cpp_ref)));
		}
	)")
	{
		f({ 5 });
	}

	#pragma test expect_ir(R"(
		decl struct IMP_std_colon__colon_initializer_list_struct_TestObj;
		decl IMP_size_returns_size_type:const IMP_std_colon__colon_initializer_list_struct_TestObj::() -> uint<8>;
		decl IMP_end_returns_const_iterator:const IMP_std_colon__colon_initializer_list_struct_TestObj::() -> ptr<IMP_TestObj,t,f>;
		decl IMP_begin_returns_const_iterator:const IMP_std_colon__colon_initializer_list_struct_TestObj::() -> ptr<IMP_TestObj,t,f>;
		def struct IMP_std_colon__colon_initializer_list_struct_TestObj {
			_M_array : ptr<IMP_TestObj,t,f>;
			_M_len : uint<8>;
			ctor function () {
				(this)._M_len = 0ul;
			}
			ctor function (v1 : ref<ptr<IMP_TestObj,t,f>,f,f,plain>, v2 : ref<uint<8>,f,f,plain>) {
				var uint<inf> v3 = num_cast(*v2, type_lit(uint<inf>));
				(this)._M_array = ptr_const_cast(ptr_from_array(ref_new(type_lit(array<IMP_TestObj,#v3>))), type_lit(t));
				(this)._M_len = *v2;
				for( int<8> v4 = 0l .. num_cast(*v2, type_lit(int<8>)) : 1l) {
					ref_assign(ptr_subscript(ptr_const_cast(*(this)._M_array, type_lit(f)), v4), ref_cast(ptr_subscript(*v1, v4), type_lit(t), type_lit(f), type_lit(cpp_ref)));
				}
			}
			dtor function () {
				if(*(this)._M_len!=0ul) {
					ref_delete(ptr_to_ref(ptr_const_cast(*(this)._M_array, type_lit(f))));
				}
			}
		};
		def struct IMP_T2 {
			ctor function (v1 : ref<IMP_std_colon__colon_initializer_list_struct_TestObj,f,f,plain>) { }
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
			IMP_T2::(ref_temp(type_lit(IMP_T2)), ref_cast(IMP_std_colon__colon_initializer_list_struct_TestObj::(ref_decl(type_lit(ref<IMP_std_colon__colon_initializer_list_struct_TestObj,f,f,plain>)), ptr_from_array(<ref<array<IMP_TestObj,2>,f,f,plain>>(ref_temp(type_lit(array<IMP_TestObj,2>))) {ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref))}), num_cast(2u, type_lit(uint<8>))), type_lit(t), type_lit(f), type_lit(cpp_ref)));
		}
	)")
	{
		TestObj obj;
		T2({ obj, obj });
	}

	return 0;
}
