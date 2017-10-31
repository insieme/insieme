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

template<int num>
struct NonTypeTemplateClass {
	int x = num;
};
template<>
struct NonTypeTemplateClass<0> {
};
template<>
struct NonTypeTemplateClass<1> {
};


template<typename A, int num>
struct MixedTemplateClass {
	A x = num;
};
template<typename A>
struct MixedTemplateClass<A, 0> {
};
template<typename A>
struct MixedTemplateClass<A, 1> {
};

template<typename A, typename B>
struct PartiallySpecializedTemplateClass {
};
template<typename A>
struct PartiallySpecializedTemplateClass<A, int> {
};
template<typename A>
struct PartiallySpecializedTemplateClass<A, float> {
};

int main() {
	;

	#pragma test expect_ir(R"(
		def struct IMP_MixedTemplateClass_int_2 {
			x : int<4>;
		};
		def struct IMP_MixedTemplateClass_int_1 {
		};
		def struct IMP_MixedTemplateClass_int_0 {
		};
		{
			var ref<IMP_MixedTemplateClass_int_0,f,f,plain> v0 = IMP_MixedTemplateClass_int_0::(ref_decl(type_lit(ref<IMP_MixedTemplateClass_int_0,f,f,plain>)));
			var ref<IMP_MixedTemplateClass_int_1,f,f,plain> v1 = IMP_MixedTemplateClass_int_1::(ref_decl(type_lit(ref<IMP_MixedTemplateClass_int_1,f,f,plain>)));
			var ref<IMP_MixedTemplateClass_int_2,f,f,plain> v2 = IMP_MixedTemplateClass_int_2::(ref_decl(type_lit(ref<IMP_MixedTemplateClass_int_2,f,f,plain>)));
		}
	)")
	{
		MixedTemplateClass<int, 0> mtp0;
		MixedTemplateClass<int, 1> mtp1;
		MixedTemplateClass<int, 2> mtp2;
	}

	#pragma test expect_ir(R"(
		def struct IMP_PartiallySpecializedTemplateClass_int_float {
		};
		def struct IMP_PartiallySpecializedTemplateClass_int_double {
		};
		def struct IMP_PartiallySpecializedTemplateClass_int_int {
		};
		{
			var ref<IMP_PartiallySpecializedTemplateClass_int_int,f,f,plain> v0 = IMP_PartiallySpecializedTemplateClass_int_int::(ref_decl(type_lit(ref<IMP_PartiallySpecializedTemplateClass_int_int,f,f,plain>)));
			var ref<IMP_PartiallySpecializedTemplateClass_int_float,f,f,plain> v1 = IMP_PartiallySpecializedTemplateClass_int_float::(ref_decl(type_lit(ref<IMP_PartiallySpecializedTemplateClass_int_float,f,f,plain>)));
			var ref<IMP_PartiallySpecializedTemplateClass_int_double,f,f,plain> v2 = IMP_PartiallySpecializedTemplateClass_int_double::(ref_decl(type_lit(ref<IMP_PartiallySpecializedTemplateClass_int_double,f,f,plain>)));
		}
	)")
	{
		PartiallySpecializedTemplateClass<int, int> pstcInt;
		PartiallySpecializedTemplateClass<int, float> pstcFloat;
		PartiallySpecializedTemplateClass<int, double> pstcDouble;
	}

	return 0;
}
