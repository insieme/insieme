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
struct SimplestConstructor { };

struct SlightlyLessSimpleConstructor {
	int i;
	SlightlyLessSimpleConstructor(int a) { i = a; }
};

struct NonTrivial {
	virtual ~NonTrivial() { 5; }
};

struct SelfDestructing {
	void foo() {
		delete this;
	}
};

#define SimplestConstructor_IR R"( def struct IMP_SimplestConstructor { }; )"

int main() {
	;

	// Base types ----------------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_ref(ref_new(type_lit(int<4>)));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int* i = new int;
		delete i;
	}

	// different from above!
	#pragma test expect_ir(R"({
		var ref<ptr<int<4>>,f,f,plain> v0 = ptr_from_ref(ref_new_init(0));
		ref_delete(ptr_to_ref(*v0));
	})")
	{
		int* i = new int();
		delete i;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_ref(ref_new_init(42));
		ref_delete(ptr_to_ref(*i));
	})")
	{
		int* i = new int{42};
		delete i;
	}

	// Base type arrays ----------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,f,f>,f,f,plain> i = ptr_from_array(<ref<array<int<4>,50u>,f,f,plain>>(ref_new(type_lit(array<int<4>,50u>))) {});
		ref_delete(ptr_to_array(*i));
	})")
	{
		int* arri = new int[50];
		delete [] arri;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>>,f,f,plain> v0 = ptr_from_array(<ref<array<int<4>,50u>,f,f,plain>>(ref_new(type_lit(array<int<4>,50u>))) {1, 2, 3});
		ref_delete(ptr_to_array(*v0));
	})")
	{
		int* arri = new int[50]{1, 2, 3};
		delete [] arri;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<array<int<4>,3u>>,f,f,plain> v0 = ptr_from_array(<ref<array<array<int<4>,3u>,50u>,f,f,plain>>(ref_new(type_lit(array<array<int<4>,3u>,50u>))) {});
		ref_delete(ptr_to_array(*v0));
	})")
	{
		int (*arri)[3] = new int[50][3];
		delete [] arri;
	}

	// Class types ---------------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(SimplestConstructor_IR, R"({
		var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v0 = ptr_from_ref(IMP_SimplestConstructor::(ref_new(type_lit(IMP_SimplestConstructor))));
		ref_delete(IMP_SimplestConstructor::~(ptr_to_ref(*v0)));
	})")
	{
		SimplestConstructor *simple = new SimplestConstructor;
		delete simple;
	}

	#pragma test expect_ir(SimplestConstructor_IR, R"({
		var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v0 = ptr_from_array(<ref<array<IMP_SimplestConstructor,3u>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,3u>))) {});
		ref_delete(ptr_to_array(*v0));
	})")
	{
		SimplestConstructor* arrsimple = new SimplestConstructor[3];
		delete [] arrsimple;
	}

	#pragma test expect_ir(SimplestConstructor_IR, R"({
		var ref<IMP_SimplestConstructor,f,f,plain> v0 = IMP_SimplestConstructor::(ref_decl(type_lit(ref<IMP_SimplestConstructor,f,f,plain>)));
		var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v1 = ptr_from_array(<ref<array<IMP_SimplestConstructor,3u>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,3u>))) {ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref))});
		ref_delete(ptr_to_array(*v1));
	})")
	{
		SimplestConstructor sc;
		SimplestConstructor* arrsimple = new SimplestConstructor[3]{sc,sc};
		delete [] arrsimple;
	}

	#pragma test expect_ir(R"(
		def struct IMP_SlightlyLessSimpleConstructor {
			i : int<4>;
			ctor function (v1 : ref<int<4>,f,f,plain>) {
				(this).i = *v1;
			}
		};
		{
			var ref<ptr<IMP_SlightlyLessSimpleConstructor>,f,f,plain> v0 = ptr_from_ref(IMP_SlightlyLessSimpleConstructor::(ref_new(type_lit(IMP_SlightlyLessSimpleConstructor)), 42));
			ref_delete(IMP_SlightlyLessSimpleConstructor::~(ptr_to_ref(*v0)));
		}
	)")
	{
		SlightlyLessSimpleConstructor *less = new SlightlyLessSimpleConstructor(42);
		delete less;
	}

	// Variable size arrays ------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"(
		def new_arr_fun = function (v0 : ref<uint<inf>,f,f,plain>) -> ptr<int<4>> {
			var uint<inf> v1 = *v0;
			return ptr_from_array(<ref<array<int<4>,#v1>,f,f,plain>>(ref_new(type_lit(array<int<4>,#v1>))) {});
		};
		{
			var ref<int<4>,f,f,plain> v0 = 50;
			var ref<ptr<int<4>>,f,f,plain> v1 = new_arr_fun(num_cast(*v0+5, type_lit(uint<inf>)));
			ref_delete(ptr_to_array(*v1));
		}
	)")
	{
		int x = 50;
		int* arri = new int[x+5];
		delete [] arri;
	}

	#pragma test expect_ir(R"(
		def new_arr_fun = function (v0 : ref<uint<inf>,f,f,plain>, v1 : ref<int<4>,t,f,cpp_ref>, v2 : ref<int<4>,t,f,cpp_ref>, v3 : ref<int<4>,t,f,cpp_ref>) -> ptr<int<4>> {
			var uint<inf> v4 = *v0;
			return ptr_from_array(<ref<array<int<4>,#v4>,f,f,plain>>(ref_new(type_lit(array<int<4>,#v4>))) {v1, v2, v3});
		};
		{
			var ref<int<4>,f,f,plain> v0 = 50;
			var ref<ptr<int<4>>,f,f,plain> v1 = new_arr_fun(num_cast(*v0+5, type_lit(uint<inf>)), 1, 2, 3);
			ref_delete(ptr_to_array(*v1));
		}
	)")
	{
		int x = 50;
		int* arri = new int[x+5]{1,2,3};
		delete [] arri;
	}

	#pragma test expect_ir(SimplestConstructor_IR, R"(
		def new_arr_fun = function (v0 : ref<uint<inf>,f,f,plain>) -> ptr<IMP_SimplestConstructor> {
			var uint<inf> v1 = *v0;
			return ptr_from_array(<ref<array<IMP_SimplestConstructor,#v1>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,#v1>))) {});
		};
		{
			var ref<int<4>,f,f,plain> v0 = 30;
			var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v1 = new_arr_fun(num_cast(*v0+5, type_lit(uint<inf>)));
			ref_delete(ptr_to_array(*v1));
		}
	)")
	{
		int x = 30;
		SimplestConstructor* arri = new SimplestConstructor[x+5];
		delete [] arri;
	}

	#pragma test expect_ir(SimplestConstructor_IR, R"(
		def new_arr_fun = function (v0 : ref<uint<inf>,f,f,plain>, v1 : ref<IMP_SimplestConstructor,t,f,cpp_ref>, v2 : ref<IMP_SimplestConstructor,t,f,cpp_ref>) -> ptr<IMP_SimplestConstructor> {
			var uint<inf> v3 = *v0;
			return ptr_from_array(<ref<array<IMP_SimplestConstructor,#v3>,f,f,plain>>(ref_new(type_lit(array<IMP_SimplestConstructor,#v3>))) {v1, v2});
		};
		{
			var ref<int<4>,f,f,plain> v0 = 50;
			var ref<IMP_SimplestConstructor,f,f,plain> v1 = IMP_SimplestConstructor::(ref_decl(type_lit(ref<IMP_SimplestConstructor,f,f,plain>)));
			var ref<IMP_SimplestConstructor,f,f,plain> v2 = IMP_SimplestConstructor::(ref_decl(type_lit(ref<IMP_SimplestConstructor,f,f,plain>)));
			var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v3 = new_arr_fun(num_cast(*v0+5, type_lit(uint<inf>)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v2, type_lit(t), type_lit(f), type_lit(cpp_ref)));
			ref_delete(ptr_to_array(*v3));
		}
	)")
	{
		int x = 50;
		SimplestConstructor sc1, sc2;
		SimplestConstructor* arri = new SimplestConstructor[x+5]{sc1,sc2};
		delete [] arri;
	}

	#pragma test expect_ir(R"(
		def struct IMP_NonTrivial {
				dtor virtual function () {
						5;
				}
		};
		def new_arr_fun = function (v0 : ref<uint<inf>,f,f,plain>, v1 : ref<IMP_NonTrivial,t,f,cpp_ref>, v2 : ref<IMP_NonTrivial,t,f,cpp_ref>) -> ptr<IMP_NonTrivial> {
				var uint<inf> v3 = *v0;
				return ptr_from_array(<ref<array<IMP_NonTrivial,#v3>,f,f,plain>>(ref_new(type_lit(array<IMP_NonTrivial,#v3>))) {v1, v2});
		};
		{
				var ref<int<4>,f,f,plain> v0 = 50;
				var ref<IMP_NonTrivial,f,f,plain> v1 = IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
				var ref<ptr<IMP_NonTrivial>,f,f,plain> v2 = new_arr_fun(num_cast(*v0+5, type_lit(uint<inf>)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
				ref_delete(ptr_to_array(*v2));
		}
	)")
	{
		int x = 50;
		NonTrivial nt;
		NonTrivial* arri = new NonTrivial[x+5]{nt, nt};
		delete [] arri;
	}

	// Destructor call within class method ----------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"(
		def struct IMP_SelfDestructing {
			function IMP_foo = () -> unit {
				ref_delete(IMP_SelfDestructing::~(this));
			}
		};
		{
			var ref<IMP_SelfDestructing,f,f,plain> v0 = IMP_SelfDestructing::(ref_decl(type_lit(ref<IMP_SelfDestructing,f,f,plain>)));
		}
	)")
	{
		SelfDestructing sd;
	}

	return 0;
}
