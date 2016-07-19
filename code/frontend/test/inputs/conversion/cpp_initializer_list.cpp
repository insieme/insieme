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

#define S_IR R"(
decl struct IMP_std_colon__colon_initializer_list_int;
decl IMP_std_colon__colon_initializer_list_int::_M_array:ptr<int<4>,t,f>;
decl IMP_std_colon__colon_initializer_list_int::_M_len:uint<8>;
decl ctor:IMP_std_colon__colon_initializer_list_int::(ptr<int<4>,t,f>, uint<8>);
decl IMP_begin_returns_const_iterator : const IMP_std_colon__colon_initializer_list_int::() -> (ref<array<int<4>,inf>,t,f,plain>,int<8>);
decl IMP_end_returns_const_iterator : const IMP_std_colon__colon_initializer_list_int::() -> (ref<array<int<4>,inf>,t,f,plain>,int<8>);
decl IMP_size_returns_size_type : const IMP_std_colon__colon_initializer_list_int::() -> uint<8>;
def struct IMP_std_colon__colon_initializer_list_int {
    _M_array : ptr<int<4>,t,f>;
    _M_len : uint<8>;
    ctor function (v1 : ref<ptr<int<4>,t,f>,f,f,plain>, v2 : ref<uint<8>,f,f,plain>) {
        var uint<inf> v3 = num_cast(*v2, type_lit(uint<inf>));
        (this)._M_array = ptr_from_array(ref_const_cast(ref_new(type_lit(array<int<4>,#v3>)), type_lit(t)));
        (this)._M_len = *v2;
        for( uint<8> v4 = 0ul .. *v2 : 1ul) {
            ref_const_cast(ptr_subscript(*(this)._M_array, num_cast(v4, type_lit(int<8>))), type_lit(f)) = *ptr_subscript(*v1, num_cast(v4, type_lit(int<8>)));
        }
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


int main() {
	;

	// this IR test pragma is not correct yet. just committed for testing purposes.
	#pragma test expect_ir(S_IR, R"({ var ref<IMP_std_colon__colon_initializer_list_int,f,f,plain> v0 =  IMP_std_colon__colon_initializer_list_int::(ref_decl(type_lit(ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>)), ptr_from_array(<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {1,2,3}), num_cast(3u, type_lit(uint<8>))); })")
	{
		std::initializer_list<int> a = {1,2,3};
	}

	#pragma test expect_ir(R"(
		decl struct IMP_std_colon__colon_initializer_list_int;
		decl struct IMP_S;
		decl IMP_std_colon__colon_initializer_list_int::_M_array : ptr<int<4>,t,f>;
		decl IMP_std_colon__colon_initializer_list_int::_M_len : uint<8>;
		decl ctor:IMP_std_colon__colon_initializer_list_int::(ptr<int<4>,t,f>, uint<8>);
		decl IMP_begin_returns_const_iterator:const IMP_std_colon__colon_initializer_list_int::() -> ptr<int<4>,t,f>;
		decl IMP_end_returns_const_iterator:const IMP_std_colon__colon_initializer_list_int::() -> ptr<int<4>,t,f>;
		decl IMP_size_returns_size_type:const IMP_std_colon__colon_initializer_list_int::() -> uint<8>;
		decl IMP_S::i1 : int<4>;
		decl ctor:IMP_S::(ref<IMP_std_colon__colon_initializer_list_int,t,f,cpp_ref>);
		def struct IMP_std_colon__colon_initializer_list_int {
			_M_array : ptr<int<4>,t,f>;
			_M_len : uint<8>;
			ctor function (v1 : ref<ptr<int<4>,t,f>,f,f,plain>, v2 : ref<uint<8>,f,f,plain>) {
				var uint<inf> v3 = num_cast(*v2, type_lit(uint<inf>));
				(this)._M_array = ptr_from_array(ref_const_cast(ref_new(type_lit(array<int<4>,#v3>)), type_lit(t)));
				(this)._M_len = *v2;
				for( uint<8> v4 = 0ul .. *v2 : 1ul) {
					ref_const_cast(ptr_subscript(*(this)._M_array, num_cast(v4, type_lit(int<8>))), type_lit(f)) = *ptr_subscript(*v1, num_cast(v4, type_lit(int<8>)));
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

	return 0;
}
