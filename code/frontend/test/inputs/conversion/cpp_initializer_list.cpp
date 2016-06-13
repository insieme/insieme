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
    ctor function (v1 : ref<ptr<int<4>,t,f>,f,f,plain>, v2 : ref<uint<8>,f,f,plain>) { }
};
)"

int main() {
	//this IR test pragma is not correct yet. just committed for testing purposes.
	#pragma test expect_ir(S_IR, R"({ var ref<IMP_std_colon__colon_initializer_list_int,f,f,plain> v0 =  IMP_std_colon__colon_initializer_list_int::(ref_decl(type_lit(ref<IMP_std_colon__colon_initializer_list_int,f,f,plain>)), ptr_from_array(<ref<array<int<4>,3>,f,f,plain>>(ref_temp(type_lit(array<int<4>,3>))) {1,2,3}), num_cast(3u, type_lit(uint<8>))); })")
	{
		std::initializer_list<int> a = {1,2,3};
	}

	return 0;
}
