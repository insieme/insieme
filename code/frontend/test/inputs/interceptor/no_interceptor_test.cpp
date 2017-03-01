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
#include "interceptor_header.h"

#define SIMPLE_FUNC "def IMP_ns_colon__colon_simpleFunc = (v0 : int<4>) -> int<4> { return v0; };"
#define STRUCT_S "def struct IMP_ns_colon__colon_S { a : int<4>; b : int<4>; c : int<4>; lambda IMP_memberFunc = (v1 : int<4>) -> int<4> { return v1; } };"

void intercept_simpleFunc() {

	#pragma test expect_ir(SIMPLE_FUNC, R"( IMP_ns_colon__colon_simpleFunc(1) )")
	ns::simpleFunc(1);

	#pragma test expect_ir(SIMPLE_FUNC, R"({
		var ref<int<4>,f,f,plain> v0 = 0;
		IMP_ns_colon__colon_simpleFunc(*v0);
	})")
	{
		int a = 0;
		ns::simpleFunc(a);
	}
}

void intercept_memFunc() {
	#pragma test expect_ir(STRUCT_S, R"( var ref<IMP_ns_colon__colon_S> v0 = IMP_ns_colon__colon_S::(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>))); )")
	ns::S s1;

	#pragma test expect_ir(STRUCT_S, R"({
		var ref<IMP_ns_colon__colon_S> v0 = IMP_ns_colon__colon_S::(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>)));
		v0.IMP_memberFunc(1);
	})")
	{
		ns::S s2;
		s2.memberFunc(1);
	}
}

void intercept_memFunc2() {
	using namespace ns;
	int magic; // do not remove

	#pragma test expect_ir(STRUCT_S, R"( var ref<IMP_ns_colon__colon_S> v0 = IMP_ns_colon__colon_S::(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>))); )")
	S s1;

	#pragma test expect_ir(STRUCT_S, R"({
		var ref<IMP_ns_colon__colon_S> v0 = IMP_ns_colon__colon_S::(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>)));
		v0.IMP_memberFunc(1);
	})")
	{
		S s2;
		s2.memberFunc(1);
	}
}

void intercept_fieldAccess() {
	#pragma test expect_ir(STRUCT_S, R"({
		var ref<IMP_ns_colon__colon_S> v0 = IMP_ns_colon__colon_S::(ref_decl(type_lit(ref<IMP_ns_colon__colon_S,f,f,plain>)));
		v0.a = *v0.b;
	})")
	{
		ns::S s2;
		s2.a = s2.b;
	}
}

void intercept_new() {
	#pragma test expect_ir(STRUCT_S, R"({ ptr_from_ref(IMP_ns_colon__colon_S::(ref_new(type_lit(IMP_ns_colon__colon_S)))); })")
	{
		new ns::S();
	}
}

int main() {
	intercept_simpleFunc();
	intercept_memFunc();
	intercept_memFunc2();
	intercept_fieldAccess();
	intercept_new();
};
