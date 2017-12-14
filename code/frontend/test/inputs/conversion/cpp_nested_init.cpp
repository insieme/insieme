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

struct Inner {
	int i;
};

struct Outer {
	int o;
	Inner i;
};

struct StructWithCtor {
	int i;
	StructWithCtor() : i(0) {}
	StructWithCtor(int i) : i(i) {}
};

struct StructWithArray {
	int i;
	int j[2];
};

int main() {

	// ----------------- primitive arrays --

	// no nesting
	#pragma test expect_ir(R"(
		{
			var ref<array<int<4>,5u>,f,f,plain> v0 = <ref<array<int<4>,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5u>,f,f,plain>))) {};
			var ref<array<int<4>,5u>,f,f,plain> v1 = <ref<array<int<4>,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5u>,f,f,plain>))) {0};
			var ref<array<int<4>,5u>,f,f,plain> v2 = <ref<array<int<4>,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,5u>,f,f,plain>))) {0, 1, 2, 3, 4};
		}
	)")
	{
		int emptyInit[5] = {};
		int shortInit[5] = {0};
		int fullInit[5]  = {0, 1, 2, 3, 4};
	}

	// one nesting level
	#pragma test expect_ir(R"(
		{
			var ref<array<array<int<4>,3u>,2u>,f,f,plain> v0 = <ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f,plain>))) {};
			var ref<array<array<int<4>,3u>,2u>,f,f,plain> v1 = <ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f,plain>))) {<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1, 2, 3}};
			var ref<array<array<int<4>,3u>,2u>,f,f,plain> v2 = <ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f,plain>))) {<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1}, <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {4}};
			var ref<array<array<int<4>,3u>,2u>,f,f,plain> v3 = <ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f,plain>))) {<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1, 2, 3}, <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {4, 5, 6}};
		}
	)")
	{
		int emptyInit[2][3]   = {};
		int shortInit[2][3]   = {{1,2,3}};
		int partialInit[2][3] = {{1}, {4}};
		int fullInit[2][3]    = {{1,2,3}, {4,5,6}};
	}

	// two nesting levels
	#pragma test expect_ir(R"(
		{
			var ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain> v0 = <ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain>))) {};
			var ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain> v1 = <ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain>))) {<ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f,plain>))) {<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1, 2, 3}}};
			var ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain> v2 = <ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain>))) {<ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f,plain>))) {<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1}, <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {4}}};
			var ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain> v3 = <ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<array<int<4>,3u>,2u>,1u>,f,f,plain>))) {<ref<array<array<int<4>,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<int<4>,3u>,2u>,f,f,plain>))) {<ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {1, 2, 3}, <ref<array<int<4>,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3u>,f,f,plain>))) {4, 5, 6}}};
		}
	)")
	{
		int emptyInit[1][2][3]   = {};
		int shortInit[1][2][3]   = {{{1,2,3}}};
		int partialInit[1][2][3] = {{{1}, {4}}};
		int fullInit[1][2][3]    = {{{1,2,3}, {4,5,6}}};
	}


	// ----------------- arrays of object type --

	// no nesting
	#pragma test expect_ir(R"(
		def struct IMP_Inner {
			i : int<4>;
		};
		{
			var ref<array<IMP_Inner,5u>,f,f,plain> v0 = <ref<array<IMP_Inner,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_Inner,5u>,f,f,plain>))) {};
			var ref<array<IMP_Inner,5u>,f,f,plain> v1 = <ref<array<IMP_Inner,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_Inner,5u>,f,f,plain>))) {<ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {0}};
			var ref<array<IMP_Inner,5u>,f,f,plain> v2 = <ref<array<IMP_Inner,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_Inner,5u>,f,f,plain>))) {<ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {0}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {1}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {2}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {3}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {4}};
		}
	)")
	{
		Inner emptyInit[5] = {};
		Inner shortInit[5] = {0};
		Inner fullInit[5]  = {0, 1, 2, 3, 4};
	}

	// some nesting
	#pragma test expect_ir(R"(
		def struct IMP_Inner {
			i : int<4>;
		};
		{
			var ref<array<array<IMP_Inner,3u>,2u>,f,f,plain> v0 = <ref<array<array<IMP_Inner,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<IMP_Inner,3u>,2u>,f,f,plain>))) {};
			var ref<array<array<IMP_Inner,3u>,2u>,f,f,plain> v1 = <ref<array<array<IMP_Inner,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<IMP_Inner,3u>,2u>,f,f,plain>))) {<ref<array<IMP_Inner,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_Inner,3u>,f,f,plain>))) {<ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {1}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {2}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {3}}, <ref<array<IMP_Inner,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_Inner,3u>,f,f,plain>))) {<ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {4}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {5}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {6}}};
			var ref<array<array<array<IMP_Inner,3u>,2u>,1u>,f,f,plain> v2 = <ref<array<array<array<IMP_Inner,3u>,2u>,1u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<array<IMP_Inner,3u>,2u>,1u>,f,f,plain>))) {};
			var ref<array<array<array<IMP_Inner,3u>,2u>,1u>,f,f,plain> v3 = <ref<array<array<array<IMP_Inner,3u>,2u>,1u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<array<IMP_Inner,3u>,2u>,1u>,f,f,plain>))) {<ref<array<array<IMP_Inner,3u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<IMP_Inner,3u>,2u>,f,f,plain>))) {<ref<array<IMP_Inner,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_Inner,3u>,f,f,plain>))) {<ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {1}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {2}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {3}}, <ref<array<IMP_Inner,3u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_Inner,3u>,f,f,plain>))) {<ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {4}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {5}, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {6}}}};
		}
	)")
	{
		Inner emptyInit[2][3]       = {};
		Inner fullInit[2][3]        = {{1,2,3}, {4,5,6}};
		Inner twoEmptyInit[1][2][3] = {};
		Inner twoFullInit[1][2][3]  = {{{1,2,3}, {4,5,6}}};
	}


	// ----------------- object types --

	// no ctors
	#pragma test expect_ir(R"(
		def struct IMP_Inner {
			i : int<4>;
		};
		def struct IMP_Outer {
			o : int<4>;
			i : IMP_Inner;
		};
		{
			var ref<IMP_Inner,f,f,plain> v0 = IMP_Inner::(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>)));
			var ref<IMP_Inner,f,f,plain> v1 = <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {0};
			var ref<IMP_Inner,f,f,plain> v2 = <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {1};
			var ref<IMP_Outer,f,f,plain> v3 = IMP_Outer::(ref_decl(type_lit(ref<IMP_Outer,f,f,plain>)));
			var ref<IMP_Outer,f,f,plain> v4 = <ref<IMP_Outer,f,f,plain>>(ref_decl(type_lit(ref<IMP_Outer,f,f,plain>))) {0, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {0}};
			var ref<IMP_Outer,f,f,plain> v5 = <ref<IMP_Outer,f,f,plain>>(ref_decl(type_lit(ref<IMP_Outer,f,f,plain>))) {1, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {0}};
			var ref<IMP_Outer,f,f,plain> v6 = <ref<IMP_Outer,f,f,plain>>(ref_decl(type_lit(ref<IMP_Outer,f,f,plain>))) {1, <ref<IMP_Inner,f,f,plain>>(ref_decl(type_lit(ref<IMP_Inner,f,f,plain>))) {2}};
		}
	)")
	{
		Inner innerDefaultInit;
		Inner innerEmptyInit{};
		Inner innerFullInit{1};
		Outer outerDefaultInit;
		Outer outerEmptyInit{};
		Outer outerShortInit{1};
		Outer outerFullInit{1,{2}};
	}

	// with ctors
	#pragma test expect_ir(R"(
		def struct IMP_StructWithCtor {
			i : int<4>;
			ctor function () {
				<ref<int<4>,f,f,plain>>((this).i) {0};
			}
			ctor function (v1 : ref<int<4>,f,f,plain>) {
				<ref<int<4>,f,f,plain>>((this).i) {*v1};
			}
		};
		{
			var ref<IMP_StructWithCtor,f,f,plain> v0 = IMP_StructWithCtor::(ref_decl(type_lit(ref<IMP_StructWithCtor,f,f,plain>)));
			var ref<IMP_StructWithCtor,f,f,plain> v1 = IMP_StructWithCtor::(ref_decl(type_lit(ref<IMP_StructWithCtor,f,f,plain>)), 1);
			var ref<array<IMP_StructWithCtor,5u>,f,f,plain> v2 = <ref<array<IMP_StructWithCtor,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_StructWithCtor,5u>,f,f,plain>))) {};
			var ref<array<IMP_StructWithCtor,5u>,f,f,plain> v3 = <ref<array<IMP_StructWithCtor,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_StructWithCtor,5u>,f,f,plain>))) {ref_cast(IMP_StructWithCtor::(ref_temp(type_lit(IMP_StructWithCtor)), 1), type_lit(f), type_lit(f), type_lit(cpp_rref))};
			var ref<array<IMP_StructWithCtor,5u>,f,f,plain> v4 = <ref<array<IMP_StructWithCtor,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_StructWithCtor,5u>,f,f,plain>))) {ref_cast(IMP_StructWithCtor::(ref_temp(type_lit(IMP_StructWithCtor)), 1), type_lit(f), type_lit(f), type_lit(cpp_rref)), ref_cast(IMP_StructWithCtor::(ref_temp(type_lit(IMP_StructWithCtor)), 2), type_lit(f), type_lit(f), type_lit(cpp_rref)), ref_cast(IMP_StructWithCtor::(ref_temp(type_lit(IMP_StructWithCtor)), 3), type_lit(f), type_lit(f), type_lit(cpp_rref)), ref_cast(IMP_StructWithCtor::(ref_temp(type_lit(IMP_StructWithCtor)), 4), type_lit(f), type_lit(f), type_lit(cpp_rref)), ref_cast(IMP_StructWithCtor::(ref_temp(type_lit(IMP_StructWithCtor)), 5), type_lit(f), type_lit(f), type_lit(cpp_rref))};
		}
	)")
	{
		StructWithCtor defaultInit;
		StructWithCtor simpleInit{1};
		StructWithCtor arrayEmptyInit[5] = {};
		StructWithCtor arrayShortInit[5] = {1};
		StructWithCtor arrayFullInit[5]  = {1, 2, 3, 4, 5};
	}

	// with array members
	#pragma test expect_ir(R"(
		def struct IMP_StructWithArray {
			i : int<4>;
			j : array<int<4>,2u>;
		};
		{
			var ref<IMP_StructWithArray,f,f,plain> v0 = IMP_StructWithArray::(ref_decl(type_lit(ref<IMP_StructWithArray,f,f,plain>)));
			var ref<IMP_StructWithArray,f,f,plain> v1 = <ref<IMP_StructWithArray,f,f,plain>>(ref_decl(type_lit(ref<IMP_StructWithArray,f,f,plain>))) {1, <ref<array<int<4>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,2u>,f,f,plain>))) {0}};
			var ref<IMP_StructWithArray,f,f,plain> v2 = <ref<IMP_StructWithArray,f,f,plain>>(ref_decl(type_lit(ref<IMP_StructWithArray,f,f,plain>))) {1, <ref<array<int<4>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,2u>,f,f,plain>))) {2, 3}};
			var ref<array<IMP_StructWithArray,5u>,f,f,plain> v3 = <ref<array<IMP_StructWithArray,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_StructWithArray,5u>,f,f,plain>))) {};
			var ref<array<IMP_StructWithArray,5u>,f,f,plain> v4 = <ref<array<IMP_StructWithArray,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_StructWithArray,5u>,f,f,plain>))) {<ref<IMP_StructWithArray,f,f,plain>>(ref_decl(type_lit(ref<IMP_StructWithArray,f,f,plain>))) {1, <ref<array<int<4>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,2u>,f,f,plain>))) {0}}};
			var ref<array<IMP_StructWithArray,5u>,f,f,plain> v5 = <ref<array<IMP_StructWithArray,5u>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_StructWithArray,5u>,f,f,plain>))) {<ref<IMP_StructWithArray,f,f,plain>>(ref_decl(type_lit(ref<IMP_StructWithArray,f,f,plain>))) {1, <ref<array<int<4>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,2u>,f,f,plain>))) {2, 3}}};
			var ref<ptr<IMP_StructWithArray>,f,f,plain> v6 = ref_decl(type_lit(ref<ptr<IMP_StructWithArray>,f,f,plain>));
			v6 = ptr_from_array(<ref<array<IMP_StructWithArray,2u>,f,f,plain>>(ref_temp(type_lit(array<IMP_StructWithArray,2u>))) {<ref<IMP_StructWithArray,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(IMP_StructWithArray)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {1, <ref<array<int<4>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,2u>,f,f,plain>))) {2, 3}}, <ref<IMP_StructWithArray,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(IMP_StructWithArray)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {4, <ref<array<int<4>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,2u>,f,f,plain>))) {5, 6}}});
		}
	)")
	{
		StructWithArray defaultInit;
		StructWithArray shortInit{1};
		StructWithArray fullInit{1, {2, 3}};
		StructWithArray arrayEmptyInit[5] = {};
		StructWithArray arrayShortPartialInit[5] = {1};
		StructWithArray arrayShortInit[5] = {{1, {2, 3}}};
		StructWithArray* arrayPtr;
		arrayPtr = (StructWithArray[]) {
			(StructWithArray) {1, {2, 3}},
			(StructWithArray) {4, {5, 6}},
		};
	}

	return 0;
}
