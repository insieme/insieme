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

#include "template_interception.h"

int main() {
	int magic;

	#pragma test expect_ir(R"({
		var ref<IMP_ConvertibleToTemplateClass,f,f,plain> v0 = lit("IMP_ConvertibleToTemplateClass::ctor" : IMP_ConvertibleToTemplateClass::())(ref_decl(type_lit(ref<IMP_ConvertibleToTemplateClass,f,f,plain>)));
		var ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain> v1 = ref_cast(lit("IMP_ConvertibleToTemplateClass::IMP__conversion_operator_TemplateClass_lt_int_gt_" : IMP_ConvertibleToTemplateClass::() -> IMP_TemplateClass<ref<int<4>,f,f,qualified>>)(v0) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
		var ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain> v2 = ref_cast(lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<int<4>,f,f,qualified>>::(ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,cpp_rref>))(ref_temp(type_lit(IMP_TemplateClass<ref<int<4>,f,f,qualified>>)), ref_kind_cast(lit("IMP_ConvertibleToTemplateClass::IMP__conversion_operator_TemplateClass_lt_int_gt_" : IMP_ConvertibleToTemplateClass::() -> IMP_TemplateClass<ref<int<4>,f,f,qualified>>)(v0) materialize , type_lit(cpp_rref))), type_lit(f), type_lit(f), type_lit(cpp_rref));
		var ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain> v3 = ref_cast(lit("IMP_ConvertibleToTemplateClass::IMP__conversion_operator_TemplateClass_lt_int_gt_" : IMP_ConvertibleToTemplateClass::() -> IMP_TemplateClass<ref<int<4>,f,f,qualified>>)(v0) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
		var ref<IMP_TemplateClass<ref<real<4>,f,f,qualified>>,f,f,plain> v4 = ref_cast(lit("IMP_ConvertibleToTemplateClass::IMP__conversion_operator_TemplateClass_lt_float_gt_" : IMP_ConvertibleToTemplateClass::() -> IMP_TemplateClass<ref<real<4>,f,f,qualified>>)(v0) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
	})")
	{
		ConvertibleToTemplateClass convertible;
		TemplateClass<int> explicitCallConverted = convertible.operator TemplateClass<int>();
		TemplateClass<int> explicitlyConverted = (TemplateClass<int>)convertible;
		TemplateClass<int> implicitlyConverted = convertible;
		TemplateClass<float> implicitlyConvertedFloat = convertible;
	}

	#pragma test expect_ir(R"({
		var ref<IMP_ConvertibleToTemplateClass,f,f,plain> v0 = lit("IMP_ConvertibleToTemplateClass::ctor" : IMP_ConvertibleToTemplateClass::())(ref_decl(type_lit(ref<IMP_ConvertibleToTemplateClass,f,f,plain>)));
		var ref<IMP_TemplateClass<ref<int<4>,t,f,qualified>>,f,f,plain> v1 = ref_cast(lit("IMP_ConvertibleToTemplateClass::IMP__conversion_operator_TemplateClass_lt_const_space_int_gt_" : IMP_ConvertibleToTemplateClass::() -> IMP_TemplateClass<ref<int<4>,t,f,qualified>>)(v0) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
	})")
	{
		ConvertibleToTemplateClass convertible;
		TemplateClass<const int> implicitlyConvertedFloat = convertible;
	}
}
