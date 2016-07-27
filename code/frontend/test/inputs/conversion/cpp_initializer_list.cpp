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

#define int_IR R"(
	decl struct IMP_std_colon__colon_initializer_list_int;
	decl fun000 : (ref<ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>,f,f,plain>) -> bool;
	decl IMP_std_colon__colon_initializer_list_int::_M_array : ptr<int<4>,t,f>;
	decl IMP_std_colon__colon_initializer_list_int::_M_len : uint<8>;
	decl IMP_std_colon__colon_initializer_list_int::_M_original : bool;
	decl ctor:IMP_std_colon__colon_initializer_list_int::();
	decl ctor:IMP_std_colon__colon_initializer_list_int::(ref<IMP_std_colon__colon_initializer_list_int,t,f,cpp_ref>);
	decl ctor:IMP_std_colon__colon_initializer_list_int::(ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>);
	decl ctor:IMP_std_colon__colon_initializer_list_int::(ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>);
	decl dtor:~IMP_std_colon__colon_initializer_list_int::();
	decl IMP_begin_returns_const_iterator:const IMP_std_colon__colon_initializer_list_int::() -> ptr<int<4>,t,f>;
	decl IMP_end_returns_const_iterator:const IMP_std_colon__colon_initializer_list_int::() -> ptr<int<4>,t,f>;
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
		ctor function (v1 : ref<int<4>,t,f,cpp_ref>) {
			(this)._M_array = ptr_cast(ptr_from_array(<ref<array<int<4>,1>,f,f,plain>>(ref_new(type_lit(array<int<4>,1>))) {v1}), type_lit(t), type_lit(f));
			(this)._M_len = 1ul;
			(this)._M_original = true;
		}
		ctor function (v1 : ref<int<4>,t,f,cpp_ref>, v2 : ref<int<4>,t,f,cpp_ref>) {
			(this)._M_array = ptr_cast(ptr_from_array(<ref<array<int<4>,2>,f,f,plain>>(ref_new(type_lit(array<int<4>,2>))) {v1, v2}), type_lit(t), type_lit(f));
			(this)._M_len = 2ul;
			(this)._M_original = true;
		}
		ctor function (v1 : ref<int<4>,t,f,cpp_ref>, v2 : ref<int<4>,t,f,cpp_ref>, v3 : ref<int<4>,t,f,cpp_ref>) {
			(this)._M_array = ptr_cast(ptr_from_array(<ref<array<int<4>,3>,f,f,plain>>(ref_new(type_lit(array<int<4>,3>))) {v1, v2, v3}), type_lit(t), type_lit(f));
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
			var ref<IMP_std_colon__colon_initializer_list_int,f,f,plain> v0 = IMP_std_colon__colon_initializer_list_int::(ref_decl(type_lit(ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>)), 1, 2, 3);
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
			IMP_f(IMP_std_colon__colon_initializer_list_int::(ref_decl(type_lit(ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>)), 5));
		}
	)")
	{
		f({ 5 });
	}

	#pragma test expect_ir(R"(
		decl struct IMP_std_colon__colon_initializer_list_struct_TestObj;
		decl struct IMP_T2;
		decl struct IMP_TestObj;
		decl IMP_std_colon__colon_initializer_list_struct_TestObj::_M_array : ptr<IMP_TestObj,t,f>;
		decl IMP_std_colon__colon_initializer_list_struct_TestObj::_M_len : uint<8>;
		decl IMP_std_colon__colon_initializer_list_struct_TestObj::_M_original : bool;
		decl ctor:IMP_std_colon__colon_initializer_list_struct_TestObj::();
		decl ctor:IMP_std_colon__colon_initializer_list_struct_TestObj::(ref<IMP_std_colon__colon_initializer_list_struct_TestObj,t,f,cpp_ref>);
		decl ctor:IMP_std_colon__colon_initializer_list_struct_TestObj::(ref<IMP_TestObj,t,f,cpp_ref>, ref<IMP_TestObj,t,f,cpp_ref>);
		decl dtor:~IMP_std_colon__colon_initializer_list_struct_TestObj::();
		decl ctor:IMP_T2::(IMP_std_colon__colon_initializer_list_struct_TestObj);
		decl ctor:IMP_TestObj::();
		decl ctor:IMP_TestObj::(ref<IMP_TestObj,t,f,cpp_ref>);
		decl ctor:IMP_TestObj::(ref<IMP_TestObj,f,f,cpp_rref>);
		decl dtor:~IMP_TestObj::();
		decl IMP_size_returns_size_type:const IMP_std_colon__colon_initializer_list_struct_TestObj::() -> uint<8>;
		decl IMP_end_returns_const_iterator:const IMP_std_colon__colon_initializer_list_struct_TestObj::() -> ptr<IMP_TestObj,t,f>;
		decl IMP_begin_returns_const_iterator:const IMP_std_colon__colon_initializer_list_struct_TestObj::() -> ptr<IMP_TestObj,t,f>;
		def struct IMP_std_colon__colon_initializer_list_struct_TestObj {
			_M_array : ptr<IMP_TestObj,t,f>;
			_M_len : uint<8>;
			_M_original : bool;
			ctor function () {
				(this)._M_len = 0ul;
				(this)._M_original = true;
			}
			ctor function (v1 : ref<IMP_std_colon__colon_initializer_list_struct_TestObj,t,f,cpp_ref>) {
				(this)._M_array = *v1._M_array;
				(this)._M_len = *v1._M_len;
				(this)._M_original = false;
			}
			ctor function (v1 : ref<IMP_TestObj,t,f,cpp_ref>, v2 : ref<IMP_TestObj,t,f,cpp_ref>) {
				(this)._M_array = ptr_cast(ptr_from_array(<ref<array<IMP_TestObj,2>,f,f,plain>>(ref_new(type_lit(array<IMP_TestObj,2>))) {v1, v2}), type_lit(t), type_lit(f));
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
			IMP_T2::(ref_temp(type_lit(IMP_T2)), IMP_std_colon__colon_initializer_list_struct_TestObj::(ref_decl(type_lit(ref<IMP_std_colon__colon_initializer_list_struct_TestObj,f,f,plain>)), ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref))));
		}
	)")
	{
		TestObj obj;
		T2({ obj, obj });
	}

	return 0;
}
