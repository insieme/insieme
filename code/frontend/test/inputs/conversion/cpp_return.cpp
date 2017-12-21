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

int x;
int x1() {
	return x;
}
const int& x2() {
	return x;
};
int&& x3() {
	return static_cast<int&&>(x);
};
const int&& x4() {
	return static_cast<int&&>(x);
};

#define X_FUNS R"(
decl IMP_x1 : () -> int<4>;
decl IMP_x2 : () -> ref<int<4>,t,f,cpp_ref>;
decl IMP_x3 : () -> ref<int<4>,f,f,cpp_rref>;
decl IMP_x4 : () -> ref<int<4>,t,f,cpp_rref>;
def IMP_x1 = function () -> int<4> {
    return *lit("x":ref<int<4>>);
};
def IMP_x2 = function () -> ref<int<4>,t,f,cpp_ref> {
    return ref_kind_cast(lit("x" : ref<int<4>,f,f,plain>), type_lit(cpp_ref));
};
def IMP_x3 = function () -> ref<int<4>,f,f,cpp_rref> {
    return ref_cast(lit("x":ref<int<4>>), type_lit(f), type_lit(f), type_lit(cpp_rref));
};
def IMP_x4 = function () -> ref<int<4>,t,f,cpp_rref> {
    return ref_cast(lit("x":ref<int<4>>), type_lit(f), type_lit(f), type_lit(cpp_rref)) in ref<int<4>,t,f,cpp_rref>;
};
)"

struct Trivial {};
Trivial y;
Trivial y1() {
	return y;
}
const Trivial& y2() {
	return y;
};
Trivial&& y3() {
	return static_cast<Trivial&&>(y);
};
const Trivial&& y4() {
	return static_cast<Trivial&&>(y);
};

#define Y_FUNS R"(
def struct IMP_Trivial { };
def IMP_y1 = function () -> IMP_Trivial {
    return ref_cast(lit("y":ref<IMP_Trivial>), type_lit(t), type_lit(f), type_lit(cpp_ref));
};
def IMP_y2 = function () -> ref<IMP_Trivial,t,f,cpp_ref> {
    return ref_kind_cast(lit("y" : ref<IMP_Trivial,f,f,plain>), type_lit(cpp_ref));
};
def IMP_y3 = function () -> ref<IMP_Trivial,f,f,cpp_rref> {
    return ref_cast(lit("y":ref<IMP_Trivial>), type_lit(f), type_lit(f), type_lit(cpp_rref));
};
def IMP_y4 = function () -> ref<IMP_Trivial,t,f,cpp_rref> {
    return ref_cast(lit("y":ref<IMP_Trivial>), type_lit(f), type_lit(f), type_lit(cpp_rref));
};
)"

struct NonTrivial {
	~NonTrivial() { 42; }
};
NonTrivial z;
NonTrivial z1() {
	return z;
}
const NonTrivial& z2() {
	return z;
};
NonTrivial&& z3() {
	return static_cast<NonTrivial&&>(z);
};
const NonTrivial&& z4() {
	return static_cast<NonTrivial&&>(z);
};

#define Z_FUNS R"(
def struct IMP_NonTrivial {
	dtor function () { 42; }
};
def IMP_z1 = function () -> IMP_NonTrivial {
    return ref_cast(lit("z":ref<IMP_NonTrivial>), type_lit(t), type_lit(f), type_lit(cpp_ref));
};
def IMP_z2 = function () -> ref<IMP_NonTrivial,t,f,cpp_ref> {
    return ref_kind_cast(lit("z" : ref<IMP_NonTrivial,f,f,plain>), type_lit(cpp_ref));
};
def IMP_z3 = function () -> ref<IMP_NonTrivial,f,f,cpp_rref> {
    return ref_cast(lit("z":ref<IMP_NonTrivial>), type_lit(f), type_lit(f), type_lit(cpp_rref));
};
def IMP_z4 = function () -> ref<IMP_NonTrivial,t,f,cpp_rref> {
    return ref_cast(lit("z":ref<IMP_NonTrivial>), type_lit(f), type_lit(f), type_lit(cpp_rref));
};
)"

struct VarReturn { int i; int j; };
VarReturn v;
const VarReturn& v1() {
	return VarReturn();
}
VarReturn* v2() {
	return new VarReturn();
}
const VarReturn& v3() {
	return VarReturn(v);
}
const VarReturn& v4() {
	return VarReturn(VarReturn());
}
VarReturn* v5() {
	return new VarReturn(v);
}
VarReturn* v6() {
	return new VarReturn(VarReturn());
}
VarReturn&& v7() {
	return (VarReturn){};
}
VarReturn&& v8() {
	return (VarReturn){1};
}
VarReturn&& v9() {
	return (VarReturn){1, 2};
}
VarReturn&& v10() {
	return (VarReturn){{0}};
}


void intTest() {
	#pragma test expect_ir(X_FUNS, R"({
		var ref<int<4>,f,f,plain> v0 = IMP_x1();
		var ref<int<4>,f,f,plain> v1 = *IMP_x2() materialize;
		var ref<int<4>,f,f,plain> v2 = *IMP_x3();
		var ref<int<4>,f,f,plain> v3 = *IMP_x4();
	})")
	{
		int a = x1();
		int b = x2();
		int c = x3();
		int d = x4();
	}

	#pragma test expect_ir(X_FUNS, R"({
		var ref<int<4>,t,f,cpp_ref> v0 = ref_kind_cast(IMP_x1() materialize , type_lit(cpp_ref));
		var ref<int<4>,t,f,cpp_ref> v1 = ref_kind_cast(IMP_x2() materialize , type_lit(cpp_ref));
		var ref<int<4>,t,f,cpp_ref> v2 = ref_kind_cast(IMP_x3(), type_lit(cpp_ref));
		var ref<int<4>,t,f,cpp_ref> v3 = ref_kind_cast(IMP_x4(), type_lit(cpp_ref));
	})")
	{
		const int& a = x1();
		const int& b = x2();
		const int& c = x3();
		const int& d = x4();
	}

	#pragma test expect_ir(X_FUNS, R"({
		var ref<int<4>,f,f,cpp_rref> v0 = ref_kind_cast(IMP_x1() materialize , type_lit(cpp_rref));
		var ref<int<4>,f,f,cpp_rref> v2 = IMP_x3();
	})")
	{
		int&& a = x1();
		// int&& b = x2(); // NOT ALLOWED
		int&& c = x3();
		// int&& d = x4(); // NOT ALLOWED
	}

	#pragma test expect_ir(X_FUNS, R"({
		var ref<int<4>,t,f,cpp_rref> v0 = ref_kind_cast(IMP_x1() materialize , type_lit(cpp_rref));
		var ref<int<4>,t,f,cpp_rref> v2 = IMP_x3();
		var ref<int<4>,t,f,cpp_rref> v3 = IMP_x4();
	})")
	{
		const int&& a = x1();
		// const int&& b = x2(); // NOT ALLOWED
		const int&& c = x3();
		const int&& d = x4();
	}
}

void trivialTest() {
	#pragma test expect_ir(Y_FUNS, R"({
		var ref<IMP_Trivial,f,f,plain> v0 = ref_cast(IMP_y1() materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
		var ref<IMP_Trivial,f,f,plain> v1 = ref_cast(IMP_y2() materialize , type_lit(t), type_lit(f), type_lit(cpp_ref));
		var ref<IMP_Trivial,f,f,plain> v2 = IMP_y3();
		var ref<IMP_Trivial,f,f,plain> v3 = ref_cast(IMP_y4(), type_lit(t), type_lit(f), type_lit(cpp_ref));
	})")
	{
		Trivial a = y1();
		Trivial b = y2();
		Trivial c = y3();
		Trivial d = y4();
	}
	#pragma test expect_ir(Y_FUNS, R"({
		var ref<IMP_Trivial,t,f,cpp_ref> v0 = ref_kind_cast(IMP_y1() materialize , type_lit(cpp_ref));
		var ref<IMP_Trivial,t,f,cpp_ref> v1 = ref_kind_cast(IMP_y2() materialize , type_lit(cpp_ref));
		var ref<IMP_Trivial,t,f,cpp_ref> v2 = ref_kind_cast(IMP_y3(), type_lit(cpp_ref));
		var ref<IMP_Trivial,t,f,cpp_ref> v3 = ref_kind_cast(IMP_y4(), type_lit(cpp_ref));
	})")
	{
		const Trivial& a = y1();
		const Trivial& b = y2();
		const Trivial& c = y3();
		const Trivial& d = y4();
	}
	#pragma test expect_ir(Y_FUNS, R"({
		var ref<IMP_Trivial,f,f,cpp_rref> v0 = ref_kind_cast(IMP_y1() materialize , type_lit(cpp_rref));
		var ref<IMP_Trivial,f,f,cpp_rref> v1 = IMP_y3();
	})")
	{
		Trivial&& a = y1();
		// Trivial&& b = y2(); // NOT ALLOWED
		Trivial&& c = y3();
		// Trivial&& d = y4(); // NOT ALLOWED
	}
	#pragma test expect_ir(Y_FUNS, R"({
		var ref<IMP_Trivial,t,f,cpp_rref> v0 = ref_kind_cast(IMP_y1() materialize , type_lit(cpp_rref));
		var ref<IMP_Trivial,t,f,cpp_rref> v1 = IMP_y3();
		var ref<IMP_Trivial,t,f,cpp_rref> v2 = IMP_y4();
	})")
	{
		const Trivial&& a = y1();
		// const Trivial&& b = y2(); // NOT ALLOWED
		const Trivial&& c = y3();
		const Trivial&& d = y4();
	}
}

void nonTrivialTest() {
	#pragma test expect_ir(Z_FUNS, R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = ref_cast(IMP_z1() materialize , type_lit(t), type_lit(f), type_lit(cpp_ref));
		var ref<IMP_NonTrivial,f,f,plain> v1 = ref_cast(IMP_z2() materialize , type_lit(t), type_lit(f), type_lit(cpp_ref));
		var ref<IMP_NonTrivial,f,f,plain> v2 = ref_cast(IMP_z3(), type_lit(t), type_lit(f), type_lit(cpp_ref));
		var ref<IMP_NonTrivial,f,f,plain> v3 = ref_cast(IMP_z4(), type_lit(t), type_lit(f), type_lit(cpp_ref));
	})")
	{
		NonTrivial a = z1();
		NonTrivial b = z2();
		NonTrivial c = z3();
		NonTrivial d = z4();
	}
	#pragma test expect_ir(Z_FUNS, R"({
		var ref<IMP_NonTrivial,t,f,cpp_ref> v0 = ref_kind_cast(IMP_z1() materialize , type_lit(cpp_ref));
		var ref<IMP_NonTrivial,t,f,cpp_ref> v1 = ref_kind_cast(IMP_z2() materialize , type_lit(cpp_ref));
		var ref<IMP_NonTrivial,t,f,cpp_ref> v2 = ref_kind_cast(IMP_z3(), type_lit(cpp_ref));
		var ref<IMP_NonTrivial,t,f,cpp_ref> v3 = ref_kind_cast(IMP_z4(), type_lit(cpp_ref));
	})")
	{
		const NonTrivial& a = z1();
		const NonTrivial& b = z2();
		const NonTrivial& c = z3();
		const NonTrivial& d = z4();
	}
	#pragma test expect_ir(Z_FUNS, R"({
		var ref<IMP_NonTrivial,f,f,cpp_rref> v0 = ref_kind_cast(IMP_z1() materialize , type_lit(cpp_rref));
		var ref<IMP_NonTrivial,f,f,cpp_rref> v1 = IMP_z3();
	})")
	{
		NonTrivial&& a = z1();
		// NonTrivial&& b = z2(); // NOT ALLOWED
		NonTrivial&& c = z3();
		// NonTrivial&& d = z4(); // NOT ALLOWED
	}
	#pragma test expect_ir(Z_FUNS, R"({
		var ref<IMP_NonTrivial,t,f,cpp_rref> v0 = ref_kind_cast(IMP_z1() materialize , type_lit(cpp_rref));
		var ref<IMP_NonTrivial,t,f,cpp_rref> v1 = IMP_z3();
		var ref<IMP_NonTrivial,t,f,cpp_rref> v2 = IMP_z4();
	})")
	{
		const NonTrivial&& a = z1();
		// const NonTrivial&& b = z2(); // NOT ALLOWED
		const NonTrivial&& c = z3();
		const NonTrivial&& d = z4();
	}
}

void returnVariableTest() {

	#pragma test expect_ir(R"(
		def struct IMP_VarReturn {
				i : int<4>;
				j : int<4>;
		};
		def IMP_v1 = function () -> ref<IMP_VarReturn,t,f,cpp_ref> {
				return ref_kind_cast(IMP_VarReturn::(ref_temp(type_lit(IMP_VarReturn))), type_lit(cpp_ref));
		};
		def IMP_v2 = function () -> ptr<IMP_VarReturn> {
				return ptr_from_ref(IMP_VarReturn::(ref_new(type_lit(IMP_VarReturn))));
		};
		def IMP_v3 = function () -> ref<IMP_VarReturn,t,f,cpp_ref> {
				return ref_kind_cast(IMP_VarReturn::(ref_temp(type_lit(IMP_VarReturn)), ref_kind_cast(lit("v" : ref<IMP_VarReturn,f,f,plain>), type_lit(cpp_ref))), type_lit(cpp_ref));
		};
		def IMP_v4 = function () -> ref<IMP_VarReturn,t,f,cpp_ref> {
				return ref_kind_cast(IMP_VarReturn::(ref_temp(type_lit(IMP_VarReturn)), ref_kind_cast(IMP_VarReturn::(ref_temp(type_lit(IMP_VarReturn))), type_lit(cpp_rref))), type_lit(cpp_ref));
		};
		def IMP_v5 = function () -> ptr<IMP_VarReturn> {
				return ptr_from_ref(IMP_VarReturn::(ref_new(type_lit(IMP_VarReturn)), ref_kind_cast(lit("v":ref<IMP_VarReturn>), type_lit(cpp_ref))));
		};
		def IMP_v6 = function () -> ptr<IMP_VarReturn> {
				return ptr_from_ref(IMP_VarReturn::(ref_new(type_lit(IMP_VarReturn)), ref_kind_cast(IMP_VarReturn::(ref_temp(type_lit(IMP_VarReturn))), type_lit(cpp_rref))));
		};
		def IMP_v7 = function () -> ref<IMP_VarReturn,f,f,cpp_rref> {
				return ref_kind_cast(<ref<IMP_VarReturn,f,f,plain>>(ref_temp(type_lit(IMP_VarReturn))) {0, 0}, type_lit(cpp_rref));
		};
		def IMP_v8 = function () -> ref<IMP_VarReturn,f,f,cpp_rref> {
				return ref_kind_cast(<ref<IMP_VarReturn,f,f,plain>>(ref_temp(type_lit(IMP_VarReturn))) {1, 0}, type_lit(cpp_rref));
		};
		def IMP_v9 = function () -> ref<IMP_VarReturn,f,f,cpp_rref> {
				return ref_kind_cast(<ref<IMP_VarReturn,f,f,plain>>(ref_temp(type_lit(IMP_VarReturn))) {1, 2}, type_lit(cpp_rref));
		};
		def IMP_v10 = function () -> ref<IMP_VarReturn,f,f,cpp_rref> {
				return ref_kind_cast(<ref<IMP_VarReturn,f,f,plain>>(ref_temp(type_lit(IMP_VarReturn))) {0, 0}, type_lit(cpp_rref));
		};
		{
				IMP_v1() materialize ;
				IMP_v2();
				IMP_v3() materialize ;
				IMP_v4() materialize ;
				IMP_v5();
				IMP_v6();
				IMP_v7();
				IMP_v8();
				IMP_v9();
				IMP_v10();
		}
	)")
	{
		v1();
		v2();
		v3();
		v4();
		v5();
		v6();
		v7();
		v8();
		v9();
		v10();
	}
}

int main() {
	intTest();
	trivialTest();
	nonTrivialTest();
	returnVariableTest();
}
