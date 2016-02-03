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

struct Simplest {
};

#define SIMPLEST_IR R"(
def struct IMP_Simplest {
};)"

struct AddOpTest {
	AddOpTest operator+(const AddOpTest& rhs) {
		return AddOpTest();
	}
};

#define ADDOPTEST_IR R"(
def struct IMP_AddOpTest {
	function IMP__operator_plus_ = (v0 : ref<IMP_AddOpTest,t,f,cpp_ref>) -> IMP_AddOpTest { 
		return ref_cast(IMP_AddOpTest::(ref_temp(type_lit(IMP_AddOpTest))), type_lit(f), type_lit(f), type_lit(cpp_rref));
	}
};)"

struct AddOpExternTest {
};

AddOpExternTest operator+(const AddOpExternTest& lhs, const AddOpExternTest& rhs) {
	return AddOpExternTest();
}

#define ADDOPEXTERNTEST_IR R"(
def struct IMP_AddOpExternTest {
};
def IMP__operator_plus_ = function (v0 : ref<IMP_AddOpExternTest,t,f,cpp_ref>, v1 : ref<IMP_AddOpExternTest,t,f,cpp_ref>) -> IMP_AddOpExternTest {
	return ref_cast(IMP_AddOpExternTest::(ref_temp(type_lit(IMP_AddOpExternTest))), type_lit(f), type_lit(f), type_lit(cpp_rref));
};
)"

int main() {
	; // this is required because of the clang compound source location bug
	
	// call copy assignment
	#pragma test expect_ir(SIMPLEST_IR,R"( { 
		var ref<IMP_Simplest,f,f,plain> v0 = IMP_Simplest::(v0);
		var ref<IMP_Simplest,f,f,plain> v1 = IMP_Simplest::(v1);
		v0.IMP__operator_assign_(ref_kind_cast(v1, type_lit(cpp_ref)));
	} )")
	{ 
		Simplest a, b;
		a = b;
	}
	
	// call move assignment
	#pragma test expect_ir(SIMPLEST_IR,R"( {
		var ref<IMP_Simplest,f,f,plain> v0 = IMP_Simplest::(v0);
		v0.IMP__operator_assign_(ref_kind_cast(IMP_Simplest::(ref_temp(type_lit(IMP_Simplest))), type_lit(cpp_rref)));
	} )")
	{
		Simplest a;
		a = Simplest();
	}

	// addition	
	#pragma test expect_ir(ADDOPTEST_IR,R"( {
		var ref<IMP_AddOpTest,f,f,plain> v0 = IMP_AddOpTest::(v0);
		var ref<IMP_AddOpTest,f,f,plain> v1 = IMP_AddOpTest::(v1);
		v0.IMP__operator_assign_(v0.IMP__operator_plus_(ref_kind_cast(v1, type_lit(cpp_ref))) : ref<IMP_AddOpTest,f,f,cpp_rref>);
	} )")
	{
		AddOpTest a, b;
		a = a+b;
	}
	
	// external addition	
	#pragma test expect_ir(ADDOPEXTERNTEST_IR,R"( {
		var ref<IMP_AddOpExternTest,f,f,plain> v0 = IMP_AddOpExternTest::(v0);
		var ref<IMP_AddOpExternTest,f,f,plain> v1 = IMP_AddOpExternTest::(v1);
		v0.IMP__operator_assign_(IMP__operator_plus_(ref_kind_cast(v0, type_lit(cpp_ref)), ref_kind_cast(v1, type_lit(cpp_ref))) : ref<IMP_AddOpExternTest,f,f,cpp_rref>);
	} )")
	{
		AddOpExternTest a, b;
		a = a+b;
	}

	return 0;
}
