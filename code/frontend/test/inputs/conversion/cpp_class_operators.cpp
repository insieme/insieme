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
		return ref_kind_cast(this, type_lit(cpp_ref));
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

	// call copy assignment
	#pragma test expect_ir(SIMPLEST_IR,R"( {
		var ref<IMP_Simplest,f,f,plain> v0 = IMP_Simplest::(ref_decl(type_lit(ref<IMP_Simplest,f,f,plain>)));
		var ref<IMP_Simplest,f,f,plain> v1 = IMP_Simplest::(ref_decl(type_lit(ref<IMP_Simplest,f,f,plain>)));
		v0.IMP__operator_assign_(ref_kind_cast(v1, type_lit(cpp_ref)));
	} )")
	{
		Simplest a, b;
		a = b;
	}

	// call move assignment
	#pragma test expect_ir(SIMPLEST_IR,R"( {
		var ref<IMP_Simplest,f,f,plain> v0 = IMP_Simplest::(ref_decl(type_lit(ref<IMP_Simplest,f,f,plain>)));
		v0.IMP__operator_assign_(ref_kind_cast(IMP_Simplest::(ref_temp(type_lit(IMP_Simplest))), type_lit(cpp_rref)));
	} )")
	{
		Simplest a;
		a = Simplest();
	}

	// addition
	#pragma test expect_ir(ADDOPTEST_IR,R"( {
		var ref<IMP_AddOpTest,f,f,plain> v0 = IMP_AddOpTest::(ref_decl(type_lit(ref<IMP_AddOpTest,f,f,plain>)));
		var ref<IMP_AddOpTest,f,f,plain> v1 = IMP_AddOpTest::(ref_decl(type_lit(ref<IMP_AddOpTest,f,f,plain>)));
		v0.IMP__operator_assign_(ref_kind_cast(v0.IMP__operator_plus_(ref_kind_cast(v1, type_lit(cpp_ref))) materialize, type_lit(cpp_rref)));
	} )")
	{
		AddOpTest a, b;
		a = a+b;
	}

	// external addition
	#pragma test expect_ir(ADDOPEXTERNTEST_IR,R"( {
		var ref<IMP_AddOpExternTest,f,f,plain> v0 = IMP_AddOpExternTest::(ref_decl(type_lit(ref<IMP_AddOpExternTest,f,f,plain>)));
		var ref<IMP_AddOpExternTest,f,f,plain> v1 = IMP_AddOpExternTest::(ref_decl(type_lit(ref<IMP_AddOpExternTest,f,f,plain>)));
		v0.IMP__operator_assign_(ref_kind_cast(IMP__operator_plus_(ref_kind_cast(v0, type_lit(cpp_ref)), ref_kind_cast(v1, type_lit(cpp_ref))) materialize, type_lit(cpp_rref)));
	} )")
	{
		AddOpExternTest a, b;
		a = a+b;
	}

	#pragma test expect_ir(REFOPTEST_IR,R"( {
		var ref<IMP_RefOpTest,f,f,plain> v0 = IMP_RefOpTest::(ref_decl(type_lit(ref<IMP_RefOpTest,f,f,plain>)));
		var ref<IMP_RefOpTest,f,f,plain> v1 = IMP_RefOpTest::(ref_decl(type_lit(ref<IMP_RefOpTest,f,f,plain>)));
		ref_kind_cast(
				v0.IMP__operator_plus_(ref_kind_cast(v1, type_lit(cpp_ref))),
				type_lit(plain)
		).IMP__operator_plus_(ref_kind_cast(v1, type_lit(cpp_ref)));
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
			ref_kind_cast(v0, type_lit(plain)).IMP__operator_assign_(ref_kind_cast(<ref<IMP_S,f,f,plain>>(ref_temp(type_lit(IMP_S))) {5}, type_lit(cpp_ref)));
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
