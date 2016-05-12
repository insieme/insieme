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

#include "interceptor_header.h"

void intercept_simpleFunc() {

	#pragma test expect_ir(R"( lit("IMP_ns_colon__colon_simpleFunc" : (int<4>) -> int<4>)(1) )")
	ns::simpleFunc(1);

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 0;
		lit("IMP_ns_colon__colon_simpleFunc" : (int<4>) -> int<4>)(*v0);
	})")
	{
		int a = 0;
		ns::simpleFunc(a);
	}
}

void intercept_memFunc() {
	#pragma test expect_ir(R"(
		var ref<IMP_ns_colon__colon_S> v0 = lit("IMP_ns_colon__colon_S::ctor" : IMP_ns_colon__colon_S::())(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>)));
	)")
	ns::S s1;

	#pragma test expect_ir(R"({
		var ref<IMP_ns_colon__colon_S,f,f,plain> v0 = lit("IMP_ns_colon__colon_S::ctor" : IMP_ns_colon__colon_S::())(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>)));
		lit("IMP_ns_colon__colon_S::IMP_memberFunc" : IMP_ns_colon__colon_S::(int<4>) -> int<4>)(v0, 1);
	})")
	{
		ns::S s2;
		s2.memberFunc(1);
	}
}

void intercept_memFunc2() {
	using namespace ns;
	int magic; // do not remove

	#pragma test expect_ir(R"(
		var ref<IMP_ns_colon__colon_S> v0 = lit("IMP_ns_colon__colon_S::ctor" : IMP_ns_colon__colon_S::())(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>)));
	)")
	S s1;

	#pragma test expect_ir(R"({
		var ref<IMP_ns_colon__colon_S,f,f,plain> v0 = lit("IMP_ns_colon__colon_S::ctor" : IMP_ns_colon__colon_S::())(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>)));
		lit("IMP_ns_colon__colon_S::IMP_memberFunc" : IMP_ns_colon__colon_S::(int<4>) -> int<4>)(v0, 1);
	})")
	{
		S s2;
		s2.memberFunc(1);
	}
}

void intercept_fieldAccess() {
	#pragma test expect_ir(R"({
		var ref<IMP_ns_colon__colon_S,f,f,plain> v0 = lit("IMP_ns_colon__colon_S::ctor" : IMP_ns_colon__colon_S::())(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>)));
		ref_member_access(v0,lit("a"),type_lit(int<4>)) = *ref_member_access(v0,lit("b"),type_lit(int<4>));
	})")
	{
		ns::S s2;
		s2.a = s2.b;
	}
}

void intercept_new() {
	#pragma test expect_ir(R"({
		ptr_from_ref(lit("IMP_ns_colon__colon_S::ctor" : IMP_ns_colon__colon_S::())(ref_new(type_lit(IMP_ns_colon__colon_S))));
	})")
	{
		new ns::S();
	}
}

void intercept_materialize() {

	#pragma test expect_ir(R"(lit("IMP_refFunTest" : () -> ref<int<4>,f,f,cpp_ref>)() materialize)")
	refFunTest();

	#pragma test expect_ir(R"( {
		var ref<IMP_RefOpTest,f,f,plain> v0 = lit("IMP_RefOpTest::ctor" : IMP_RefOpTest::())(ref_decl(type_lit(ref<IMP_RefOpTest,f,f,plain>)));
		var ref<IMP_RefOpTest,f,f,plain> v1 = lit("IMP_RefOpTest::ctor" : IMP_RefOpTest::())(ref_decl(type_lit(ref<IMP_RefOpTest,f,f,plain>)));
		lit("IMP_RefOpTest::IMP__operator_plus_" : IMP_RefOpTest::(ref<IMP_RefOpTest,t,f,cpp_ref>) -> ref<IMP_RefOpTest,f,f,cpp_ref>)
			(lit("IMP_RefOpTest::IMP__operator_plus_" : IMP_RefOpTest::(ref<IMP_RefOpTest,t,f,cpp_ref>) -> ref<IMP_RefOpTest,f,f,cpp_ref>)
				(v0, ref_kind_cast(v1, type_lit(cpp_ref))) materialize , ref_kind_cast(v1, type_lit(cpp_ref))) materialize ;
	} )")
	{
		RefOpTest a, b;
		a + b + b;
	}

	#pragma test expect_ir(R"( {
		var ref<IMP_RefMethTest,f,f,plain> v0 = lit("IMP_RefMethTest::ctor" : IMP_RefMethTest::())(ref_decl(type_lit(ref<IMP_RefMethTest,f,f,plain>)));
		lit("IMP_RefMethTest::IMP_meth" : IMP_RefMethTest::() -> ref<IMP_RefMethTest,f,f,cpp_ref>)(v0) materialize ;
	} )")
	{
		RefMethTest a;
		a.meth();
	}
}

int main() {
	intercept_simpleFunc();
	intercept_memFunc();
	intercept_memFunc2();
	intercept_fieldAccess();
	intercept_new();
	intercept_materialize();
};
