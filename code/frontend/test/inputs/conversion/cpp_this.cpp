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

struct ThisTest {
	ThisTest* getThis() { return this; }
};

#define STRUCT_THISTEST R"(def struct IMP_ThisTest {
	lambda IMP_getThis = () -> ptr<IMP_ThisTest> { return ptr_from_ref(this); }
};)"

struct ThisTest2 {
	float v;
	float fA() { return this->v; }
	float fB() { return v; }
};
#define STRUCT_THISTEST2 R"(def struct IMP_ThisTest2 {
	v : real<4>;
	lambda IMP_fA = () -> real<4> { return *v; }
	lambda IMP_fB = () -> real<4> { return *v; }
};)"

struct RefMember {
	const int& mem;
	RefMember() : mem(0) { mem; }
};

struct A {
	int x;
	int& y;
};

struct B {
	void m() {}
	bool operator !() { return false; }
};

B makeB() {
	return {};
}

int main() {

	#pragma test expect_ir(STRUCT_THISTEST,R"({
		var ref<IMP_ThisTest,f,f,plain> v0 = IMP_ThisTest::(ref_decl(type_lit(ref<IMP_ThisTest,f,f,plain>)));
		v0.IMP_getThis();
	})")
	{
		ThisTest tt;
		tt.getThis();
	}

	#pragma test expect_ir(STRUCT_THISTEST2,R"({
		var ref<IMP_ThisTest2,f,f,plain> v0 = IMP_ThisTest2::(ref_decl(type_lit(ref<IMP_ThisTest2,f,f,plain>)));
	})")
	{
		ThisTest2 tt2;
	}

	#pragma test expect_ir(R"(
		def struct IMP_RefMember {
			mem : ref<int<4>,t,f,cpp_ref>;
			ctor function () {
				<ref<int<4>,t,f,cpp_ref>>(*(this).mem) {ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref))};
				(this).mem;
			}
		};
		{
			var ref<IMP_RefMember,f,f,plain> v0 = IMP_RefMember::(ref_decl(type_lit(ref<IMP_RefMember,f,f,plain>)));
		}
	)")
	{
		RefMember r;
	}

	#pragma test expect_ir(R"(
		def struct IMP_A {
			x : int<4>;
			y : ref<int<4>,f,f,cpp_ref>;
		};
		{
			var ref<int<4>,f,f,plain> v0 = 0;
			var ref<IMP_A,t,f,plain> v1 = <ref<IMP_A,t,f,plain>>(ref_decl(type_lit(ref<IMP_A,t,f,plain>))) {1, ref_kind_cast(v0, type_lit(cpp_ref))};
			v1.x;
			v1.y;
		}
	)")
	{
		int i = 0;
		const A a{1,i};
		a.x;
		a.y;
	}

	// method call on rvalue
	#pragma test expect_ir(R"(
		def struct IMP_B {
			function IMP_m = () -> unit { }
			function IMP__operator_not_ = () -> bool {
				return false;
			}
		};
		def IMP_makeB = function () -> IMP_B {
			return <ref<IMP_B,f,f,plain>>(ref_decl(type_lit(ref<IMP_B,f,f,plain>))) {};
		};
		{
			IMP_makeB() materialize .IMP_m();
		}
	)")
	{
		makeB().m();
	}

	// operator call on rvalue
	#pragma test expect_ir(R"(
		def struct IMP_B {
			function IMP_m = () -> unit { }
			function IMP__operator_not_ = () -> bool {
				return false;
			}
		};
		def IMP_makeB = function () -> IMP_B {
			return <ref<IMP_B,f,f,plain>>(ref_decl(type_lit(ref<IMP_B,f,f,plain>))) {};
		};
		{
			IMP_makeB() materialize .IMP__operator_not_();
		}
	)")
	{
		!makeB();
	}

	return 0;
}
