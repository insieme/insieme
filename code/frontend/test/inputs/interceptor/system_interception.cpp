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

#include <sys/time.h>

#include <iostream>
#include <vector>
#include <future>

int main() {
	//simple C function with struct param
	#pragma test expect_ir(R"({
		var ref<IMP_timeval> v0 = lit("IMP_timeval::ctor" : IMP_timeval::())(v0);
		lit("IMP_gettimeofday": (ptr<IMP_timeval>, ptr<IMP_timezone>) -> int<4>)(ptr_from_ref(v0), ptr_null(type_lit(IMP_timezone), type_lit(f), type_lit(f)));
	})")
	{
		struct timeval t;
		gettimeofday(&t, nullptr);
	}

	//cpp type and member function
	#pragma test expect_ir(R"({
		var ref<IMP_std_colon__colon_vector_int_std_colon__colon_allocator_lt_int_gt_> v0 =
			lit("IMP_std_colon__colon_vector_int_std_colon__colon_allocator_lt_int_gt_::ctor"
				: IMP_std_colon__colon_vector_int_std_colon__colon_allocator_lt_int_gt_::())(v0);
		lit("IMP_std_colon__colon_vector_int_std_colon__colon_allocator_lt_int_gt_::IMP_push_back" :
			IMP_std_colon__colon_vector_int_std_colon__colon_allocator_lt_int_gt_::(ref<int<4>,f,f,cpp_rref>) -> unit) (v0, 0);
	})")
	{
		std::vector<int> v;
		v.push_back(0);
	}

	//cpp global and function
	#pragma test expect_ir(R"({
		lit("IMP_std_colon__colon__operator_lshift__struct_std_colon__colon_char_traits_lt_char_gt__returns_basic_ostream_lt_char_comma__struct_std_colon__colon_char_traits_lt_char_gt___gt___ampersand_" :
			(ref<IMP_std_colon__colon_basic_ostream_char_struct_std_colon__colon_char_traits_lt_char_gt_,f,f,cpp_ref>, ptr<char,t,f>) -> ref<IMP_std_colon__colon_basic_ostream_char_struct_std_colon__colon_char_traits_lt_char_gt_,f,f,cpp_ref>) (
				ref_kind_cast(lit("IMP_std_colon__colon_cout":ref<IMP_std_colon__colon_basic_ostream_char_struct_std_colon__colon_char_traits_lt_char_gt_>), type_lit(cpp_ref)), ptr_from_array("Test")) materialize;
	})")
	{
		std::cout << "Test";
		//std::operator<<<std::char_traits<char>>(std::cout, "Test");
	}

	//cpp global enum
	#pragma test expect_ir(R"({
		var ref<IMP_std_colon__colon_launch,f,f,plain> v0 = lit("IMP_std_colon__colon_launch_colon__colon_async":ref<IMP_std_colon__colon_launch,f,f,plain>);
	})")
	{
		auto a = std::launch::async;
	}
}
