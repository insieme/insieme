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

#include <new>
#include <cstdlib>

struct SimplestConstructor { };

struct SlightlyLessSimpleConstructor {
	int i;
	SlightlyLessSimpleConstructor(int a) { i = a; }
};

struct NonTrivial {
	virtual ~NonTrivial() { 5; }
};

int main() {
	;

	// Base types ----------------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<ptr<int<4>>,f,f,plain> v1 = ptr_reinterpret(ptr_reinterpret(ptr_from_ref(v0), type_lit(unit)), type_lit(int<4>));
	})")
	{
		int place;
		int* val = new (&place) int;
	}

	// different from above!
	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<ptr<int<4>>,f,f,plain> v1 = cxx_placement_new(ptr_reinterpret(ptr_from_ref(v0), type_lit(unit)), 0);
	})")
	{
		int place;
		int* i = new (&place) int();
	}

	// different from above!
	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<ptr<int<4>>,f,f,plain> v1 = cxx_placement_new(ptr_reinterpret(ptr_from_ref(v0), type_lit(unit)), 42);
	})")
	{
		int place;
		int* i = new (&place) int{42};
	}

	//// Class types ---------------------------------------------------------------------------------------------------------------------------------------------

	#pragma test expect_ir(R"(
		def struct IMP_SimplestConstructor {
		};
		{
			var ref<IMP_SimplestConstructor,f,f,plain> v0 = IMP_SimplestConstructor::(ref_decl(type_lit(ref<IMP_SimplestConstructor,f,f,plain>)));
			var ref<ptr<IMP_SimplestConstructor>,f,f,plain> v1 = ptr_from_ref(IMP_SimplestConstructor::(ptr_to_ref(ptr_reinterpret(ptr_reinterpret(ptr_from_ref(v0), type_lit(unit)), type_lit(IMP_SimplestConstructor)))));
		}
	)")
	{
		SimplestConstructor place;
		SimplestConstructor *simple = new (&place) SimplestConstructor();
	}

	#pragma test expect_ir(R"(
		def struct IMP_SlightlyLessSimpleConstructor {
			i : int<4>;
			ctor function (v1 : ref<int<4>,f,f,plain>) {
				(this).i = *v1;
			}
		};
		{
			var ref<IMP_SlightlyLessSimpleConstructor,f,f,plain> v0 = IMP_SlightlyLessSimpleConstructor::(ref_decl(type_lit(ref<IMP_SlightlyLessSimpleConstructor,f,f,plain>)), 41);
			var ref<ptr<IMP_SlightlyLessSimpleConstructor>,f,f,plain> v1 = ptr_from_ref(IMP_SlightlyLessSimpleConstructor::(ptr_to_ref(ptr_reinterpret(ptr_reinterpret(ptr_from_ref(v0), type_lit(unit)), type_lit(IMP_SlightlyLessSimpleConstructor))), 42));
		}
	)")
	{
		SlightlyLessSimpleConstructor place(41);
		SlightlyLessSimpleConstructor *less = new (&place) SlightlyLessSimpleConstructor(42);
	}

	#pragma test expect_ir(R"(
		def struct IMP_NonTrivial {
			dtor virtual function () {
				5;
			}
		};
		{
			var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
			var ref<ptr<IMP_NonTrivial>,f,f,plain> v1 = ptr_from_ref(IMP_NonTrivial::(ptr_to_ref(ptr_reinterpret(ptr_reinterpret(ptr_from_ref(v0), type_lit(unit)), type_lit(IMP_NonTrivial)))));
		}
	)")
	{
		NonTrivial place;
		NonTrivial *less = new (&place) NonTrivial();
	}

	return 0;
}
