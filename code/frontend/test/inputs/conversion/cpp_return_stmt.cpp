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
// -- Scalar tests
// ================================================================================================|

int scalar_int() {
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> a; return *a in ref<int<4>>; }")
	{
		int a;
		return a;
	}
}
const int& scalar_ref_int() {
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0;  return v0 in ref<int<4>,t,f,cpp_ref>; } ")
	{
		int a;
		return a;
	}
}

// upgrade cast on return
float scalar_float() {
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> a; return num_cast(*a, type_lit(real<4>)) in ref<real<4>>; }")
	{
		int a;
		return a;
	}
}

typedef int* intPtr;
intPtr pointer_int() {
	#pragma test expect_ir("return ptr_null(type_lit(int<4>), type_lit(f), type_lit(f)) in ref<ptr<int<4>>>;")
	return 0;
}

typedef const int* constIntPtr;
constIntPtr const_pointer_int() {
	#pragma test expect_ir("return ptr_null(type_lit(int<4>), type_lit(t), type_lit(f)) in ref<ptr<int<4>,t,f>>;")
	return 0;
}

constIntPtr nonconst_to_const_pointer_int() {
	intPtr i = 0;
	return i;
}

intPtr& reference_pointer_int() {
	#pragma test expect_ir("{var ref<ptr<int<4>>,f,f,plain> v0; return v0 in ref<ptr<int<4>,f,f>,f,f,cpp_ref>; } ")
	{
		intPtr a;
		return a;
	}
}

const intPtr& const_reference_pointer_int() {
	#pragma test expect_ir("{var ref<ptr<int<4>>,f,f,plain> v0; return v0 in ref<ptr<int<4>,f,f>,t,f,cpp_ref>; } ")
	{
		intPtr a;
		return a;
	}
}

constIntPtr& reference_const_pointer_int() {
	#pragma test expect_ir("{var ref<ptr<int<4>,t,f>,f,f,plain> v0; return v0 in ref<ptr<int<4>,t,f>,f,f,cpp_ref>; } ")
	{
		constIntPtr a;
		return a;
	}
}

const constIntPtr& const_reference_const_pointer_int() {
	#pragma test expect_ir("{var ref<ptr<int<4>,t,f>,f,f,plain> v0; return v0 in ref<ptr<int<4>,t,f>,t,f,cpp_ref>; } ")
	{
		constIntPtr a;
		return a;
	}
}

void recursiveVoid(int a) {
	if(a<=0) return;
	return recursiveVoid(a-1);
}

class Trivial {
	int x;
public:
	Trivial() {}
	Trivial(int x) : x(x) {}
};
int g_a;
class NonTrivial {
public:
	NonTrivial() {}
	NonTrivial(int x) {
		g_a = x;
	}
	~NonTrivial() {
		g_a++;
	}
};

Trivial returnTrivial() {
	return Trivial();
}
Trivial returnTrivialInit() {
	return {};
}
Trivial returnTrivialInitWithParam() {
	return { 5 };
}

NonTrivial returnNonTrivial() {
	return NonTrivial();
}
NonTrivial returnNonTrivialInit() {
	return {};
}
NonTrivial returnNonTrivialInitWithParam() {
	return { 7 };
}

int main() {
	{} // help pragmas to find their way

	{
		#pragma test expect_ir("EXPR_TYPE", "int<4>")
		scalar_int();
		#pragma test expect_ir("EXPR_TYPE", "real<4>")
		scalar_float();
		#pragma test expect_ir("EXPR_TYPE", "ref<int<4>,t,f>")
		scalar_ref_int();
	}

	{
		#pragma test expect_ir("EXPR_TYPE", "ptr<int<4>>")
		pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ptr<int<4>, t,f>")
		const_pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ptr<int<4>, t,f>")
		nonconst_to_const_pointer_int();
	}

	{
		#pragma test expect_ir("EXPR_TYPE", "ref<ptr<int<4>>>")
		reference_pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ref<ptr<int<4>>,t,f>")
		const_reference_pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ref<ptr<int<4>,t,f>,f,f>")
		reference_const_pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ref<ptr<int<4>,t,f>,t,f>")
		const_reference_const_pointer_int();
	}

	#pragma test expect_ir(R"(
		decl IMP_recursiveVoid : (int<4>) -> unit;
		def IMP_recursiveVoid = function (v0 : ref<int<4>,f,f,plain>) -> unit {
			if(*v0<=0) {
				return unit;
			}
			IMP_recursiveVoid(*v0-1);
			return unit;
		};
		IMP_recursiveVoid(7)
	)")
	recursiveVoid(7);

	#pragma test expect_ir(R"(
		def struct IMP_Trivial {
			x : int<4>;
			ctor function () { }
			ctor function (v1 : ref<int<4>,f,f,plain>) {
				<ref<int<4>,f,f,plain>>((this).x) {*v1};
			}
		};
		def IMP_returnTrivial = function () -> IMP_Trivial {
			return ref_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))), type_lit(f), type_lit(f), type_lit(cpp_rref));
		};
		def IMP_returnTrivialInit = function () -> IMP_Trivial {
			return IMP_Trivial::(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>)));
		};
		def IMP_returnTrivialInitWithParam = function () -> IMP_Trivial {
			return IMP_Trivial::(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>)), 5);
		};
		{
			IMP_returnTrivial();
			IMP_returnTrivialInit();
			IMP_returnTrivialInitWithParam();
		}
	)")
	{
		returnTrivial();
		returnTrivialInit();
		returnTrivialInitWithParam();
	}
	#pragma test expect_ir(R"(
		def struct IMP_NonTrivial {
			ctor function () { }
			ctor function (v1 : ref<int<4>,f,f,plain>) {
				lit("g_a" : ref<int<4>,f,f,plain>) = *v1;
			}
			dtor function () {
				gen_post_inc(lit("g_a" : ref<int<4>,f,f,plain>));
			}
		};
		def IMP_returnNonTrivial = function () -> IMP_NonTrivial {
			return ref_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))), type_lit(t), type_lit(f), type_lit(cpp_ref));
		};
		def IMP_returnNonTrivialInit = function () -> IMP_NonTrivial {
			return IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
		};
		def IMP_returnNonTrivialInitWithParam = function () -> IMP_NonTrivial {
			return IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)), 7);
		};
		{
			IMP_returnNonTrivial() materialize ;
			IMP_returnNonTrivialInit() materialize ;
			IMP_returnNonTrivialInitWithParam() materialize ;
		}
	)")
	{
		returnNonTrivial();
		returnNonTrivialInit();
		returnNonTrivialInitWithParam();
	}
}
