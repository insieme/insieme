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
#include "template_interception.h"


// Integer template arguments //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void intTypeParams() {
	#pragma test expect_ir(R"({
		var ref<IMP_IntTemplateClass<5>,f,f,plain> v0 = lit("IMP_IntTemplateClass::ctor" : IMP_IntTemplateClass<'T_0_0>::())(ref_decl(type_lit(ref<IMP_IntTemplateClass<5>,f,f,plain>)));
		var ref<IMP_IntTemplateClass<42>,f,f,plain> v1 = lit("IMP_IntTemplateClass::ctor" : IMP_IntTemplateClass<'T_0_0>::())(ref_decl(type_lit(ref<IMP_IntTemplateClass<42>,f,f,plain>)));
	})")
	{
		IntTemplateClass<5> fiveInstance;
		IntTemplateClass<42> answerInstance;
	}
}


int main() {

	intTypeParams();

	// Functions with templates ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(type_instantiation(type_lit(<ref<int<4>,f,f,qualified>>() -> int<4>), lit("IMP_templateFunRet" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>() -> 'T_0_0))())")
	templateFunRet<int>();
	#pragma test expect_ir(R"(type_instantiation(type_lit(<ref<real<4>,f,f,qualified>>() -> real<4>), lit("IMP_templateFunRet" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>() -> 'T_0_0))())")
	templateFunRet<float>();
	#pragma test expect_ir(R"(type_instantiation(type_lit(<ref<int<4>,f,f,qualified>, ref<uint<4>,f,f,qualified>>(uint<4>) -> int<4>), lit("IMP_templateFunRetParam" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>, ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>('T_0_1) -> 'T_0_0))(7u))")
	templateFunRetParam<int>(7u);
	#pragma test expect_ir(R"(type_instantiation(type_lit(<ref<int<4>,f,f,qualified>, ref<uint<4>,f,f,qualified>>(uint<4>) -> int<4>), lit("IMP_templateFunRetParam" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>, ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>('T_0_1) -> 'T_0_0))(6u))")
	templateFunRetParam<int,unsigned>(6u);


	#pragma test expect_ir(R"( type_instantiation(type_lit((int<4>) -> int<4>), lit("IMP_templateFun" : ('T_0_0) -> 'T_0_0))(1) )")
	templateFun(1);
	#pragma test expect_ir(R"( type_instantiation(type_lit((real<8>) -> real<8>), lit("IMP_templateFun" : ('T_0_0) -> 'T_0_0))(2.0E+0) )")
	templateFun(2.0);
	#pragma test expect_ir(R"( type_instantiation(type_lit((uint<16>) -> uint<16>), lit("IMP_templateFun" : ('T_0_0) -> 'T_0_0))(3ull) )")
	templateFun(3ull);
	#pragma test expect_ir(R"( type_instantiation(type_lit(<ref<uint<8>,f,f,qualified>>(uint<8>) -> uint<8>), lit("IMP_templateFun" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>('T_0_0) -> 'T_0_0))(num_cast(4, type_lit(uint<8>))) )")
	templateFun<unsigned long>(4);

	// Dependent name
	#pragma test expect_ir(R"(type_instantiation(type_lit(<ref<IMP_Typer,f,f,qualified>>(int<4>) -> unit), lit("IMP_dependentNameFun" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>('IMP_typename_space_T_colon__colon_Bla) -> unit))(5))")
	dependentNameFun<Typer>(5);

	// Class with method dependent on class instantiation type

	#pragma test expect_ir(R"({
		var ref<IMP_TemplateWithMethod<ref<int<4>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_TemplateWithMethod::ctor" : IMP_TemplateWithMethod<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateWithMethod<ref<int<4>,f,f,qualified>>,f,f,plain>)));
		lit("IMP_TemplateWithMethod::IMP_get" : IMP_TemplateWithMethod<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::() -> 'T_0_0)(v0);
		var ref<IMP_TemplateWithMethod<ref<real<4>,f,f,qualified>>,f,f,plain> v1 = lit("IMP_TemplateWithMethod::ctor" : IMP_TemplateWithMethod<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateWithMethod<ref<real<4>,f,f,qualified>>,f,f,plain>)));
		lit("IMP_TemplateWithMethod::IMP_get" : IMP_TemplateWithMethod<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::() -> 'T_0_0)(v1);
	})")
	{
		TemplateWithMethod<int> a;
		a.get();
		TemplateWithMethod<float> b;
		b.get();
	}

	// Class with template method

	#pragma test expect_ir(R"({
		var ref<IMP_ClassWithTemplateMethod,f,f,plain> v0 = lit("IMP_ClassWithTemplateMethod::ctor" : IMP_ClassWithTemplateMethod::())(ref_decl(type_lit(ref<IMP_ClassWithTemplateMethod,f,f,plain>)));
		type_instantiation(type_lit(<ref<int<4>,f,f,qualified>>IMP_ClassWithTemplateMethod::() -> int<4>), lit("IMP_ClassWithTemplateMethod::IMP_get" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>IMP_ClassWithTemplateMethod::() -> 'T_0_0))(v0);
		type_instantiation(type_lit(<ref<real<4>,f,f,qualified>>IMP_ClassWithTemplateMethod::() -> real<4>), lit("IMP_ClassWithTemplateMethod::IMP_get" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>IMP_ClassWithTemplateMethod::() -> 'T_0_0))(v0);
	})")
	{
		ClassWithTemplateMethod a;
		a.get<int>();
		a.get<float>();
	}


	// Classes with templates //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"({
		var ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain>)));
		var ref<IMP_TemplateClass<ref<real<8>,f,f,qualified>>,f,f,plain> v1 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<real<8>,f,f,qualified>>,f,f,plain>)));
		var ref<IMP_TemplateClass<ref<bool,f,f,qualified>>,f,f,plain> v2 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<bool,f,f,qualified>>,f,f,plain>)));
	})")
	{
		TemplateClass<int> intInstance;
		TemplateClass<double> doubleInstance;
		TemplateClass<bool> boolInstance;
	}

	// Template template arguments /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	// simple instantiation of template template (implicit)

	#pragma test expect_ir(R"({
		var ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain>)));
		type_instantiation(type_lit((ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,cpp_ref>, int<4>) -> unit), lit("IMP_templateTemplateFun" : (ref<IMP_Container<'T_0_1>,f,f,cpp_ref>, 'T_0_1) -> unit))(ref_kind_cast(v0, type_lit(cpp_ref)), 0);
	})")
	{
		TemplateClass<int> c;
		templateTemplateFun(c, 0);
	}

	// simple instantiation of template template (explicit - otherwise the same as above)

	#pragma test expect_ir(R"({
		1;
		var ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain>)));
		type_instantiation(type_lit(<IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>, ref<int<4>,f,f,qualified>>(ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,cpp_ref>, int<4>) -> unit), lit("IMP_templateTemplateFun" : <'T_T_0_0<ref<'T_1_0,'T_1_0_a,'T_1_0_b,'T_1_0_c>>, ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>(ref<IMP_Container<'T_0_1>,f,f,cpp_ref>, 'T_0_1) -> unit))(ref_kind_cast(v0, type_lit(cpp_ref)), 0);
	})")
	{
		1; TemplateClass<int> c;
		templateTemplateFun<TemplateClass, int>(c, 0);
	}

	// nested templated types for template template (implicit)
	#pragma test expect_ir(R"({
		var ref<IMP_TemplateClass<ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
		var ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain> v1 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain>)));
		type_instantiation(type_lit((ref<IMP_TemplateClass<ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,cpp_ref>, IMP_TemplateClass<ref<int<4>,f,f,qualified>>) -> unit), lit("IMP_templateTemplateFun" : (ref<IMP_Container<'T_0_1>,f,f,cpp_ref>, 'T_0_1) -> unit))(ref_kind_cast(v0, type_lit(cpp_ref)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{
		TemplateClass<TemplateClass<int> > c;
		TemplateClass<int> c1;
		templateTemplateFun(c, c1);
	}

	// nested templated types for template template (explicit - otherwise the same as above)
	#pragma test expect_ir(R"({
		1;
		var ref<IMP_TemplateClass<ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,plain>)));
		var ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain> v1 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,plain>)));
		type_instantiation(type_lit(<IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>, ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,qualified>>(ref<IMP_TemplateClass<ref<IMP_TemplateClass<ref<int<4>,f,f,qualified>>,f,f,qualified>>,f,f,cpp_ref>, IMP_TemplateClass<ref<int<4>,f,f,qualified>>) -> unit), lit("IMP_templateTemplateFun" : <'T_T_0_0<ref<'T_1_0,'T_1_0_a,'T_1_0_b,'T_1_0_c>>, ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>(ref<IMP_Container<'T_0_1>,f,f,cpp_ref>, 'T_0_1) -> unit))(ref_kind_cast(v0, type_lit(cpp_ref)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{
		1; TemplateClass<TemplateClass<int> > c;
		TemplateClass<int> c1;
		templateTemplateFun<TemplateClass, TemplateClass<int> >(c, c1);
	}

	//#pragma test expect_ir(R"({
	//	var ref<IMP_TemplateTemplateClass<IMP_TemplateClass<'T_0_0>,int<4>>,f,f,plain> v0 = lit("IMP_TemplateTemplateClass::ctor" : IMP_TemplateTemplateClass<'T_T_0_0<'T_1_0>,'T_0_1>::())(v0) materialize ;
	//})")
	//{
	//	TemplateTemplateClass<TemplateClass, int> tt;
	//}

	// Function pointer ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(type_instantiation(type_lit((ptr<(real<4>) -> int<4>,t,f>) -> unit), lit("IMP_templateFunPointerParam" : (ptr<('T_0_1) -> 'T_0_0,t,f>) -> unit))(ptr_of_function(type_instantiation(type_lit(<ref<int<4>,f,f,qualified>, ref<real<4>,f,f,qualified>>(real<4>) -> int<4>), lit("IMP_templateFunRetParam" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>, ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>('T_0_1) -> 'T_0_0)))))")
	templateFunPointerParam(templateFunRetParam<int,float>);

	#pragma test expect_ir(R"(lit("IMP_specificFunPointerParam" : (ptr<(real<4>) -> int<4>,t,f>) -> unit)(ptr_of_function(type_instantiation(type_lit((real<4>) -> int<4>), lit("IMP_templateFunRetParam" : ('T_0_1) -> 'T_0_0)))))")
	specificFunPointerParam(templateFunRetParam);

	#pragma test expect_ir(R"(type_instantiation(type_lit(<ref<int<4>,f,f,qualified>>(ptr<(IMP_TemplateClass<ref<int<4>,f,f,qualified>>) -> unit,t,f>) -> unit), lit("IMP_dependentFunPointerParam" : <ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>(ptr<(IMP_TemplateClass<'T_0_0>) -> unit,t,f>) -> unit))(ptr_of_function(type_instantiation(type_lit((IMP_TemplateClass<ref<int<4>,f,f,qualified>>) -> unit), lit("IMP_modifier" : (IMP_TemplateClass<'T_0_0>) -> unit)))))")
	dependentFunPointerParam<int>(modifier);

	#pragma test expect_ir(R"({
		var ref<IMP_basic_ostream<ref<char,f,f,qualified>>,f,f,plain> v0 = lit("IMP_basic_ostream::ctor" : IMP_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())(ref_decl(type_lit(ref<IMP_basic_ostream<ref<char,f,f,qualified>>,f,f,plain>)));
		var ref<IMP_basic_ostream<ref<char,f,f,qualified>>,f,f,cpp_ref> v1 = v0;
		lit("IMP_basic_ostream::IMP_op" : IMP_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::(ptr<(ref<IMP_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>,f,f,cpp_ref>) -> unit,t,f>) -> ref<IMP_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>,f,f,cpp_ref>)(v0, ptr_of_function(type_instantiation(type_lit((ref<IMP_basic_ostream<ref<char,f,f,qualified>>,f,f,cpp_ref>) -> unit), lit("IMP_endl" : (ref<IMP_basic_ostream<'T_0_0>,f,f,cpp_ref>) -> unit)))) materialize;
		lit("IMP_basic_ostream::IMP_op" : IMP_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::(ptr<(ref<IMP_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>,f,f,cpp_ref>) -> unit,t,f>) -> ref<IMP_basic_ostream<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>,f,f,cpp_ref>)(v1, ptr_of_function(type_instantiation(type_lit((ref<IMP_basic_ostream<ref<char,f,f,qualified>>,f,f,cpp_ref>) -> unit), lit("IMP_endl" : (ref<IMP_basic_ostream<'T_0_0>,f,f,cpp_ref>) -> unit)))) materialize;
	})")
	{
		ostream os;
		ostream& osr = os;
		os.op(endl);
		osr.op(endl);
	}

	// Variadic templates //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(type_instantiation(type_lit((int<4>) -> int<4>), lit("IMP_variadicTemplateFun" : ('T_0_0) -> 'T_0_0))(0))")
	variadicTemplateFun(0);

	#pragma test expect_ir(R"(type_instantiation(type_lit((int<4>, int<4>) -> int<4>), lit("IMP_variadicTemplateFun" : ('T_0_0, 'V_T_0_1...) -> 'T_0_0))(0, 1))")
	variadicTemplateFun(0, 1);

	#pragma test expect_ir(R"(type_instantiation(type_lit((int<4>, int<4>, real<8>, uint<4>) -> int<4>), lit("IMP_variadicTemplateFun" : ('T_0_0, 'V_T_0_1...) -> 'T_0_0))(0, 1, 2.0E+0, 90u))")
	variadicTemplateFun(0, 1, 2.0, 90u);

	#pragma test expect_ir(R"({
	var ref<IMP_VariadicClass<ref<int<4>,f,f,qualified>,ref<real<8>,f,f,qualified>>,f,f,plain> v0 = lit("IMP_VariadicClass::ctor" : IMP_VariadicClass<'V_T_0_0...>::())(ref_decl(type_lit(ref<IMP_VariadicClass<ref<int<4>,f,f,qualified>,ref<real<8>,f,f,qualified>>,f,f,plain>))) materialize ;
	})")
	{
		VariadicClass<int, double> a;
	}

	// Variadic template template //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(type_instantiation(type_lit(<IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>>() -> unit), lit("IMP_variadicTemplateTemplateFun" : <'V_T_T_0_0...<>>() -> unit))())")
	variadicTemplateTemplateFun<TemplateClass>();

	#pragma test expect_ir(R"(type_instantiation(type_lit(<IMP_TemplateWithMethod<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>, IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>>() -> unit), lit("IMP_variadicTemplateTemplateFun" : <'V_T_T_0_0...<>>() -> unit))())")
	variadicTemplateTemplateFun<TemplateWithMethod, TemplateClass>();

	// Global template instance ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	auto minint = __numeric_traits_integer<int>::__min;
}
