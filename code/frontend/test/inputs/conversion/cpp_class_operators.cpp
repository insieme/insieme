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

struct RefOpTest {
	RefOpTest& operator+(const RefOpTest& rhs) {
		return *this;
	}
};

#define REFOPTEST_IR R"(
def struct IMP_RefOpTest {
	function IMP__operator_plus_ = (v0 : ref<IMP_RefOpTest,t,f,cpp_ref>) -> ref<IMP_RefOpTest,f,f,cpp_ref> {
		return this;
	}
};)"

struct S {
	int a;
	~S() {
		5;
	}
};

void j(S& s) {
	s = {5};
}

int main() {
	; // this is required because of the clang compound source location bug

	// call copy assignment
	#pragma test expect_ir(SIMPLEST_IR,R"( {
		var ref<IMP_Simplest,f,f,plain> v0 = IMP_Simplest::(ref_decl(type_lit(ref<IMP_Simplest,f,f,plain>)));
		var ref<IMP_Simplest,f,f,plain> v1 = IMP_Simplest::(ref_decl(type_lit(ref<IMP_Simplest,f,f,plain>)));
		v0.IMP__operator_assign_(ref_kind_cast(v1, type_lit(cpp_ref))) materialize;
	} )")
	{
		Simplest a, b;
		a = b;
	}

	// call move assignment
	#pragma test expect_ir(SIMPLEST_IR,R"( {
		var ref<IMP_Simplest,f,f,plain> v0 = IMP_Simplest::(ref_decl(type_lit(ref<IMP_Simplest,f,f,plain>)));
		v0.IMP__operator_assign_(ref_kind_cast(IMP_Simplest::(ref_temp(type_lit(IMP_Simplest))), type_lit(cpp_rref))) materialize;
	} )")
	{
		Simplest a;
		a = Simplest();
	}

	// addition
	#pragma test expect_ir(ADDOPTEST_IR,R"( {
		var ref<IMP_AddOpTest,f,f,plain> v0 = IMP_AddOpTest::(ref_decl(type_lit(ref<IMP_AddOpTest,f,f,plain>)));
		var ref<IMP_AddOpTest,f,f,plain> v1 = IMP_AddOpTest::(ref_decl(type_lit(ref<IMP_AddOpTest,f,f,plain>)));
		v0.IMP__operator_assign_(ref_kind_cast(v0.IMP__operator_plus_(ref_kind_cast(v1, type_lit(cpp_ref))) materialize, type_lit(cpp_rref))) materialize;
	} )")
	{
		AddOpTest a, b;
		a = a+b;
	}

	// external addition
	#pragma test expect_ir(ADDOPEXTERNTEST_IR,R"( {
		var ref<IMP_AddOpExternTest,f,f,plain> v0 = IMP_AddOpExternTest::(ref_decl(type_lit(ref<IMP_AddOpExternTest,f,f,plain>)));
		var ref<IMP_AddOpExternTest,f,f,plain> v1 = IMP_AddOpExternTest::(ref_decl(type_lit(ref<IMP_AddOpExternTest,f,f,plain>)));
		v0.IMP__operator_assign_(ref_kind_cast(IMP__operator_plus_(ref_kind_cast(v0, type_lit(cpp_ref)), ref_kind_cast(v1, type_lit(cpp_ref))) materialize, type_lit(cpp_rref))) materialize;
	} )")
	{
		AddOpExternTest a, b;
		a = a+b;
	}

	#pragma test expect_ir(REFOPTEST_IR,R"( {
		var ref<IMP_RefOpTest,f,f,plain> v0 = IMP_RefOpTest::(ref_decl(type_lit(ref<IMP_RefOpTest,f,f,plain>)));
		var ref<IMP_RefOpTest,f,f,plain> v1 = IMP_RefOpTest::(ref_decl(type_lit(ref<IMP_RefOpTest,f,f,plain>)));
		v0.IMP__operator_plus_(ref_kind_cast(v1, type_lit(cpp_ref))) materialize .IMP__operator_plus_(ref_kind_cast(v1, type_lit(cpp_ref))) materialize ;
	} )")
	{
		RefOpTest a, b;
		a + b + b;
	}

	#pragma test expect_ir(R"(
		def struct IMP_S {
			a : int<4>;
			dtor function () {
				5;
			}
		};
		def IMP_j = function (v0 : ref<IMP_S,f,f,cpp_ref>) -> unit {
			ref_kind_cast(v0, type_lit(plain)).IMP__operator_assign_(ref_kind_cast(<ref<IMP_S,f,f,plain>>(ref_temp(type_lit(IMP_S))) {5}, type_lit(cpp_ref))) materialize ;
		};
		{
			var ref<IMP_S,f,f,plain> v0 = IMP_S::(ref_decl(type_lit(ref<IMP_S,f,f,plain>)));
			IMP_j(ref_kind_cast(v0, type_lit(cpp_ref)));
		}
	)")
	{
		S a;
		j(a);
	}

	return 0;
}
