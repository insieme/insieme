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
 *
 */
#include <sys/time.h>

#include <iostream>
#include <vector>
#include <map>
#include <future>

struct Trivial {};
struct OtherTrivial {}; // should ONLY be used once in the remaining test code

struct InitListTest {
	std::vector<int> v;
	InitListTest() : v() {};
};

int main() {

	// simple C function with struct param
	#pragma test expect_ir(R"({
		var ref<IMP_timeval> v0 = lit("IMP_timeval::ctor" : IMP_timeval::())(ref_decl(type_lit(ref<IMP_timeval,f,f,plain>)));
		lit("IMP_gettimeofday": (ptr<IMP_timeval>, ptr<IMP_timezone>) -> int<4>)(ptr_from_ref(v0), ptr_null(type_lit(IMP_timezone), type_lit(f), type_lit(f)));
	})")
	{
		struct timeval t;
		gettimeofday(&t, nullptr);
	}

	// cpp type and member function
	#pragma test expect_ir(R"({
		var ref<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_std_colon__colon_vector::ctor" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::())(ref_decl(type_lit(ref<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
		lit("IMP_std_colon__colon_vector::IMP_push_back" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::(ref<'T_0_0,f,f,cpp_rref>) -> unit)(v0, 0);
		var ref<IMP_std_colon__colon_vector<ref<real<8>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<real<8>,f,f,qualified>>,f,f,qualified>>,f,f,plain> v1 = lit("IMP_std_colon__colon_vector::ctor" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::())(ref_decl(type_lit(ref<IMP_std_colon__colon_vector<ref<real<8>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<real<8>,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
		lit("IMP_std_colon__colon_vector::IMP_push_back" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::(ref<'T_0_0,f,f,cpp_rref>) -> unit)(v1, 1.0E+0);
	})")
	{
		std::vector<int> v1;
		v1.push_back(0);
		std::vector<double> v2;
		v2.push_back(1.0);
	}

	// cpp global and function
	#pragma test expect_ir(R"({
		type_instantiation(type_lit((ref<IMP_std_colon__colon_basic_ostream<ref<char,f,f,qualified>,ref<IMP_std_colon__colon_char_traits<ref<char,f,f,qualified>>,f,f,qualified>>,f,f,cpp_ref>, ptr<char,t,f>) -> ref<IMP_std_colon__colon_basic_ostream<ref<char,f,f,qualified>,ref<IMP_std_colon__colon_char_traits<ref<char,f,f,qualified>>,f,f,qualified>>,f,f,cpp_ref>), lit("IMP_std_colon__colon__operator_lshift_" : (ref<IMP_basic_ostream<char,'T_0_0>,f,f,cpp_ref>, ptr<char,t,f>) -> ref<IMP_basic_ostream<char,'T_0_0>,f,f,cpp_ref>))(ref_kind_cast(lit("IMP_std_colon__colon_cout" : ref<IMP_std_colon__colon_basic_ostream<ref<char,f,f,qualified>,ref<IMP_std_colon__colon_char_traits<ref<char,f,f,qualified>>,f,f,qualified>>,f,f,plain>), type_lit(cpp_ref)), ptr_from_array(lit(""Test"" : ref<array<char,5>,t,f,plain>))) materialize ;
	})")
	{
		std::cout << "Test";
	}

	#pragma test expect_ir(R"({
		lit("IMP_std_colon__colon_basic_ostream::IMP__operator_lshift_" : IMP_std_colon__colon_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::(int<4>) -> ref<IMP_std_colon__colon_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>,f,f,cpp_ref>)(lit("IMP_std_colon__colon_cout" : ref<IMP_std_colon__colon_basic_ostream<ref<char,f,f,qualified>,ref<IMP_std_colon__colon_char_traits<ref<char,f,f,qualified>>,f,f,qualified>>,f,f,plain>), 1) materialize;
	})")
	{
		std::cout << 1;
	}

	// cpp global enum
	#pragma test expect_ir("REGEX_S", R"(.*gen_eq\(IMP_std_colon__colon_launch_colon__colon_async, IMP_std_colon__colon_launch_colon__colon_deferred\).*)")
	{
		std::launch::async == std::launch::deferred;
	}

	// non-intercepted type in intercepted template
	#pragma test expect_ir(R"(
		decl struct IMP_Trivial;
		def struct IMP_Trivial { };
		{
			var ref<IMP_std_colon__colon_vector<ref<IMP_Trivial,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<IMP_Trivial,f,f,qualified>>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_std_colon__colon_vector::ctor" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::())(ref_decl(type_lit(ref<IMP_std_colon__colon_vector<ref<IMP_Trivial,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<IMP_Trivial,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
		}
	)")
	{
		std::vector<Trivial> vectorOfTrivial;
	}

	// check naming of const method
	#pragma test expect_ir(R"({
		var ref<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_std_colon__colon_vector::ctor" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::())(ref_decl(type_lit(ref<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
		lit("IMP_std_colon__colon_vector::IMP_size" : const IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::() -> uint<8>)(v0);
	})")
	{
		std::vector<int> a;
		a.size();
	}

	// check partial template specialization
	#pragma test expect_ir(R"({
		var ref<IMP_std_colon__colon_vector<ref<bool,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<bool,f,f,qualified>>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_std_colon__colon_vector::ctor" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::())(ref_decl(type_lit(ref<IMP_std_colon__colon_vector<ref<bool,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<bool,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
	})")
	{
		std::vector<bool> b;
	}

	// check backend definition dependency generation for user code struct
	#pragma test expect_ir(R"(
		decl struct IMP_OtherTrivial;
		def struct IMP_OtherTrivial { };
		{
			type_instantiation(type_lit(<ref<IMP_OtherTrivial,f,f,qualified>>() -> IMP_std_colon__colon_shared_ptr<ref<IMP_OtherTrivial,f,f,qualified>>), lit("IMP_std_colon__colon_make_shared" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>, 'V_T_0_1...>('V_T_0_1...) -> IMP_shared_ptr<'T_0_0>))() materialize;
		}
	)")
	{
		std::make_shared<OtherTrivial>();
	}

	#pragma test expect_ir(R"({
		var ref<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_std_colon__colon_vector::ctor" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::())(ref_decl(type_lit(ref<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
		var ref<IMP___gnu_cxx_colon__colon___normal_iterator<ref<ptr<int<4>>,f,f,qualified>,ref<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,qualified>>,f,f,plain> v1 = ref_cast(type_instantiation(type_lit(IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>::() -> IMP___gnu_cxx_colon__colon___normal_iterator<ref<ptr<int<4>>,f,f,qualified>,ref<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,qualified>>), lit("IMP_std_colon__colon_vector::IMP_begin" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::() -> IMP___normal_iterator<'IMP_typename_space__Base_colon__colon_pointer,IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>>))(v0) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
	})")
	{
		std::vector<int> a;
		std::vector<int>::iterator it = a.begin();
	}

	#pragma test expect_ir(R"(
		def struct IMP_InitListTest {
			v : IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>;
			ctor function () {
				lit("IMP_std_colon__colon_vector::ctor" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::())((this).v);
			}
		};
		{
			var ref<IMP_InitListTest,f,f,plain> v0 = IMP_InitListTest::(ref_decl(type_lit(ref<IMP_InitListTest,f,f,plain>)));
		}
	)")
	{
		InitListTest il;
	}

	#pragma test expect_ir(R"(
		 var ref<IMP_std_colon__colon_pair<ref<int<4>,f,f,qualified>,ref<bool,f,f,qualified>>,f,f,plain> v0 =
			ref_cast(type_instantiation(type_lit((ref<int<4>,f,f,cpp_rref>, ref<bool,f,f,cpp_rref>) -> IMP_std_colon__colon_pair<ref<int<4>,f,f,qualified>,ref<bool,f,f,qualified>>),
				lit("IMP_std_colon__colon_make_pair" : (ref<'T_0_0,f,f,cpp_rref>, ref<'T_0_1,f,f,cpp_rref>) -> IMP_pair<'IMP_typename_space___decay_and_strip_lt__T1_gt__colon__colon___type,'IMP_typename_space___decay_and_strip_lt__T2_gt__colon__colon___type>))(12, true) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
	)")
	std::pair<int,bool> x = std::make_pair(12, true);

	// std::function
	#pragma test expect_ir(R"({
		var ref<IMP_std_colon__colon_function<ref<(int<4>) -> int<4>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_std_colon__colon_function::ctor" : IMP_std_colon__colon_function<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_std_colon__colon_function<ref<(int<4>) -> int<4>,f,f,qualified>>,f,f,plain>)));
		type_instantiation(type_lit(const IMP_std_colon__colon_function<ref<(int<4>)->int<4>,f,f,qualified>>::(int<4>) -> int<4>), lit("IMP_std_colon__colon_function::IMP__operator_call_" : const IMP_std_colon__colon_function<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::('V_T_0_1...) -> '__std_fun_ret_type))(v0, 1);
	})")
	{
		std::function<int (int)> x;
		x(1);
	}

	// method call on rvalue
	#pragma test expect_ir(R"({
		var ref<IMP_std_colon__colon_map<ref<int<4>,f,f,qualified>,ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_less<ref<int<4>,f,f,qualified>>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<IMP_std_colon__colon_pair<ref<int<4>,t,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_std_colon__colon_map::ctor" : IMP_std_colon__colon_map<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>,ref<'T_0_2,'T_0_2_a,'T_0_2_b,'T_0_2_c>,ref<'T_0_3,'T_0_3_a,'T_0_3_b,'T_0_3_c>>::())(ref_decl(type_lit(ref<IMP_std_colon__colon_map<ref<int<4>,f,f,qualified>,ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_less<ref<int<4>,f,f,qualified>>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<IMP_std_colon__colon_pair<ref<int<4>,t,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
		lit("IMP_std_colon__colon__Rb_tree_iterator::IMP__operator_neq_" : const IMP_std_colon__colon__Rb_tree_iterator<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::(ref<IMP_std_colon__colon__Rb_tree_iterator<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>,t,f,cpp_ref>) -> bool)(type_instantiation(type_lit(IMP_std_colon__colon_map<ref<int<4>,f,f,qualified>,ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_less<ref<int<4>,f,f,qualified>>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<IMP_std_colon__colon_pair<ref<int<4>,t,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,qualified>>::(ref<int<4>,t,f,cpp_ref>) -> IMP_std_colon__colon__Rb_tree_iterator<ref<IMP_std_colon__colon_pair<ref<int<4>,t,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,qualified>>), lit("IMP_std_colon__colon_map::IMP_find" : IMP_std_colon__colon_map<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>,ref<'T_0_2,'T_0_2_a,'T_0_2_b,'T_0_2_c>,ref<'T_0_3,'T_0_3_a,'T_0_3_b,'T_0_3_c>>::(ref<'T_0_0,t,f,cpp_ref>) -> 'IMP_typename_space__Rep_type_colon__colon_iterator))(v0, 1) materialize , ref_kind_cast(type_instantiation(type_lit(IMP_std_colon__colon_map<ref<int<4>,f,f,qualified>,ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_less<ref<int<4>,f,f,qualified>>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<IMP_std_colon__colon_pair<ref<int<4>,t,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,qualified>>::(ref<int<4>,t,f,cpp_ref>) -> IMP_std_colon__colon__Rb_tree_iterator<ref<IMP_std_colon__colon_pair<ref<int<4>,t,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,qualified>>), lit("IMP_std_colon__colon_map::IMP_find" : IMP_std_colon__colon_map<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>,ref<'T_0_2,'T_0_2_a,'T_0_2_b,'T_0_2_c>,ref<'T_0_3,'T_0_3_a,'T_0_3_b,'T_0_3_c>>::(ref<'T_0_0,t,f,cpp_ref>) -> 'IMP_typename_space__Rep_type_colon__colon_iterator))(v0, 2) materialize , type_lit(cpp_ref)));
	})")
	{
		std::map<int,int> m;
		m.find(1) != m.find(2);
	}

	// new/delete of intercepted type
	#pragma test expect_ir(R"({
		var ref<ptr<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>>,f,f,plain> v0 = ptr_from_ref(lit("IMP_std_colon__colon_vector::ctor" : IMP_std_colon__colon_vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::())(ref_new(type_lit(IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>))));
		ref_delete(lit("IMP_std_colon__colon_vector::dtor" : ~IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>::())(ptr_to_ref(*v0)));
	})")
	{
		std::vector<int> *x = new std::vector<int>;
		delete x;
	}

	// array new/delete of intercepted type
	#pragma test expect_ir(R"({
		var ref<ptr<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>>,f,f,plain> v0 = ptr_from_array(<ref<array<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,5>,f,f,plain>>(ref_new(type_lit(array<IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>,5>))) {});
		ref_delete(ptr_to_array(*v0));
	})")
	{
		std::vector<int> *arr = new std::vector<int>[5];
		delete [] arr;
	}
}
