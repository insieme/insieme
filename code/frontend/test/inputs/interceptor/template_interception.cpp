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

#include "template_interception.h"

int main() {

	#pragma test expect_ir(R"(type_instantiation(type_lit(<int<4>>() -> int<4>), lit("IMP_templateFunRet" : <'T_0_0>() -> 'T_0_0))())")
	templateFunRet<int>();

	#pragma test expect_ir(R"(type_instantiation(type_lit(<real<4>>() -> real<4>), lit("IMP_templateFunRet" : <'T_0_0>() -> 'T_0_0))())")
	templateFunRet<float>();

	#pragma test expect_ir(R"(type_instantiation(type_lit(<int<4>,uint<4>>(uint<4>) -> int<4>), lit("IMP_templateFunRetParam" : <'T_0_0, 'T_0_1>('T_0_1) -> 'T_0_0))(7u))")
	templateFunRetParam<int>(7u);

	#pragma test expect_ir(R"(type_instantiation(type_lit(<int<4>,uint<4>>(uint<4>) -> int<4>), lit("IMP_templateFunRetParam" : <'T_0_0, 'T_0_1>('T_0_1) -> 'T_0_0))(6u))")
	templateFunRetParam<int,unsigned>(6u);

	// Functions with templates //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(type_instantiation(type_lit(<int<4>>(int<4>) -> int<4>), lit("IMP_templateFun" : <'T_0_0>('T_0_0) -> 'T_0_0))(1))")
	templateFun(1);
	#pragma test expect_ir(R"(type_instantiation(type_lit(<real<8>>(real<8>) -> real<8>), lit("IMP_templateFun" : <'T_0_0>('T_0_0) -> 'T_0_0))(lit("2.0E+0":real<8>)))")
	templateFun(2.0);
	#pragma test expect_ir(R"(type_instantiation(type_lit(<uint<16>>(uint<16>) -> uint<16>), lit("IMP_templateFun" : <'T_0_0>('T_0_0) -> 'T_0_0))(lit("3":uint<16>)))")
	templateFun(3ull);
	#pragma test expect_ir(R"(type_instantiation(type_lit(<uint<8>>(uint<8>) -> uint<8>), lit("IMP_templateFun" : <'T_0_0>('T_0_0) -> 'T_0_0))(num_cast(4, type_lit(uint<8>))))")
	templateFun<unsigned long>(4);

	// Class with method dependent on class instantiation type
	#pragma test expect_ir(R"({
		var ref<IMP_TemplateWithMethod<int<4>>,f,f,plain> v0 = lit("IMP_TemplateWithMethod::ctor" : IMP_TemplateWithMethod<'T_0_0>::())(v0);
		lit("IMP_TemplateWithMethod::IMP_get" : IMP_TemplateWithMethod<'T_0_0>::() -> 'T_0_0)(v0);
		var ref<IMP_TemplateWithMethod<real<4>>,f,f,plain> v1 = lit("IMP_TemplateWithMethod::ctor" : IMP_TemplateWithMethod<'T_0_0>::())(v1);
		lit("IMP_TemplateWithMethod::IMP_get" : IMP_TemplateWithMethod<'T_0_0>::() -> 'T_0_0)(v1);
	})")
	{
		TemplateWithMethod<int> a;
		a.get();
		TemplateWithMethod<float> b;
		b.get();
	}

	// Class with template method
	#pragma test expect_ir(R"({
		var ref<IMP_ClassWithTemplateMethod,f,f,plain> v0 = lit("IMP_ClassWithTemplateMethod::ctor" : IMP_ClassWithTemplateMethod::())(v0);
		type_instantiation(type_lit(<int<4>>IMP_ClassWithTemplateMethod::() -> int<4>), lit("IMP_ClassWithTemplateMethod::IMP_get" : <'T_0_0>IMP_ClassWithTemplateMethod::() -> 'T_0_0))(v0);
		type_instantiation(type_lit(<real<4>>IMP_ClassWithTemplateMethod::() -> real<4>), lit("IMP_ClassWithTemplateMethod::IMP_get" : <'T_0_0>IMP_ClassWithTemplateMethod::() -> 'T_0_0))(v0);
	})")
	{
		ClassWithTemplateMethod a;
		a.get<int>();
		a.get<float>();
	}


	// Classes with templates //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(var ref<IMP_TemplateClass<int<4>>,f,f,plain> v0 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<'T_0_0>::())(v0);)")
	TemplateClass<int> intInstance;
	#pragma test expect_ir(R"(var ref<IMP_TemplateClass<real<8>>,f,f,plain> v0 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<'T_0_0>::())(v0);)")
	TemplateClass<double> doubleInstance;
	#pragma test expect_ir(R"(var ref<IMP_TemplateClass<bool>,f,f,plain> v0 = lit("IMP_TemplateClass::ctor" : IMP_TemplateClass<'T_0_0>::())(v0);)")
	TemplateClass<bool> boolInstance;

	// Integer template arguments /////////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(var ref<IMP_IntTemplateClass<5>,f,f,plain> v0 = lit("IMP_IntTemplateClass::ctor" : IMP_IntTemplateClass<'T_0_0>::())(v0);)")
	IntTemplateClass<5> fiveInstance;
	#pragma test expect_ir(R"(var ref<IMP_IntTemplateClass<42>,f,f,plain> v0 = lit("IMP_IntTemplateClass::ctor" : IMP_IntTemplateClass<'T_0_0>::())(v0);)")
	IntTemplateClass<42> answerInstance;

	//// Template template arguments //////////////////////////////////////////////////////////////

	////simple instantiation of template template (implicit)
	//#pragma test expect_ir(R"({
	//	var ref<IMP_TemplateClass_int,f,f,plain> v0 = lit("IMP_TemplateClass_int::ctor" : IMP_TemplateClass_int::())(v0);
	//	lit("IMP_templateTemplateFun_TemplateClass_int_returns_void"
	//			: (ref<IMP_TemplateClass_int,f,f,cpp_ref>, int<4>) -> unit)(ref_kind_cast(v0, type_lit(cpp_ref)), 0);
	//})")
	//{
	//	TemplateClass<int> c;
	//	templateTemplateFun(c, 0);
	//}

	////simple instantiation of template template (explicit - otherwise the same as above)
	//#pragma test expect_ir(R"({
	//	var ref<IMP_TemplateClass_int,f,f,plain> v0 = lit("IMP_TemplateClass_int::ctor" : IMP_TemplateClass_int::())(v0);
	//	lit("IMP_templateTemplateFun_TemplateClass_int_returns_void"
	//			: (ref<IMP_TemplateClass_int,f,f,cpp_ref>, int<4>) -> unit)(ref_kind_cast(v0, type_lit(cpp_ref)), 0);
	//})")
	//{
	//	TemplateClass<int> c;
	//	templateTemplateFun<TemplateClass, int>(c, 0);
	//}

	////nested templated types for template template (implicit)
	//#pragma test expect_ir(R"({
	//	var ref<IMP_TemplateClass_TemplateClass_lt_int_gt_,f,f,plain> v0 = lit("IMP_TemplateClass_TemplateClass_lt_int_gt_::ctor" : IMP_TemplateClass_TemplateClass_lt_int_gt_::())(v0);
	//	var ref<IMP_TemplateClass_int,f,f,plain> v1 = lit("IMP_TemplateClass_int::ctor" : IMP_TemplateClass_int::())(v1);
	//	lit("IMP_templateTemplateFun_TemplateClass_TemplateClass_lt_int_gt__returns_void"
	//			: (ref<IMP_TemplateClass_TemplateClass_lt_int_gt_,f,f,cpp_ref>, IMP_TemplateClass_int) -> unit)(
	//					ref_kind_cast(v0, type_lit(cpp_ref)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	//})")
	//{
	//	TemplateClass<TemplateClass<int> > c;
	//	TemplateClass<int> c1;
	//	templateTemplateFun(c, c1);
	//}

	////nested templated types for template template (explicit - otherwise the same as above)
	//#pragma test expect_ir(R"({
	//	var ref<IMP_TemplateClass_TemplateClass_lt_int_gt_,f,f,plain> v0 = lit("IMP_TemplateClass_TemplateClass_lt_int_gt_::ctor" : IMP_TemplateClass_TemplateClass_lt_int_gt_::())(v0);
	//	var ref<IMP_TemplateClass_int,f,f,plain> v1 = lit("IMP_TemplateClass_int::ctor" : IMP_TemplateClass_int::())(v1);
	//	lit("IMP_templateTemplateFun_TemplateClass_TemplateClass_lt_int_gt__returns_void"
	//			: (ref<IMP_TemplateClass_TemplateClass_lt_int_gt_,f,f,cpp_ref>, IMP_TemplateClass_int) -> unit)(
	//					ref_kind_cast(v0, type_lit(cpp_ref)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	//})")
	//{
	//	TemplateClass<TemplateClass<int> > c;
	//	TemplateClass<int> c1;
	//	templateTemplateFun<TemplateClass, TemplateClass<int> >(c, c1);
	//}


	//// Variadic templates //////////////////////////////////////////////////////////////

	//#pragma test expect_ir(R"(lit("IMP_variadicTemplateFun_int_returns_int" : (int<4>) -> int<4>)(0))")
	//variadicTemplateFun(0);

	//#pragma test expect_ir(R"(lit("IMP_variadicTemplateFun_int_pack_begin_int_pack_end_returns_int" : (int<4>, int<4>) -> int<4>)(0, 1))")
	//variadicTemplateFun(0, 1);
}
