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

// a trivial struct
struct Trivial {
	int i;
};

// a conversion function
template<typename T>
void consume(T) {}

template<typename T>
T produce() { return T(); }

template<typename R, typename A>
R to(A x) {
	return x;
}

#define STRUCT_TRIVIAL "def struct IMP_Trivial { i : int<4>; }; "
#define CONSUME_TRIVIAL "def IMP_consume_struct_space_Trivial_returns_void = function (v0 : ref<IMP_Trivial,f,f,plain>) -> unit { }; "
#define PRODUCE_TRIVIAL "def IMP_produce_struct_space_Trivial_returns_struct_space_Trivial = function () -> IMP_Trivial { return ref_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))), type_lit(f), type_lit(f), type_lit(cpp_rref)); }; "

#define CONSUME_TRIVIAL_REF "def IMP_consume_struct_space_Trivial_space__ampersand__returns_void = function (v0 : ref<IMP_Trivial,f,f,cpp_ref>) -> unit { }; "
#define CONSUME_TRIVIAL_RREF "def IMP_consume_struct_space_Trivial_space__ampersand__ampersand__returns_void = function (v0 : ref<IMP_Trivial,f,f,cpp_rref>) -> unit { }; "

#define CONSUME_TRIVIAL_CONST_REF "def IMP_consume_const_space_struct_space_Trivial_space__ampersand__returns_void = function (v0 : ref<IMP_Trivial,t,f,cpp_ref>) -> unit { };"
#define CONSUME_TRIVIAL_CONST_RREF "def IMP_consume_const_space_struct_space_Trivial_space__ampersand__ampersand__returns_void = function (v0 : ref<IMP_Trivial,t,f,cpp_rref>) -> unit { }; "

void validateTrivial() {

	using T = Trivial;

	// produce only
	#pragma test expect_ir(STRUCT_TRIVIAL,PRODUCE_TRIVIAL, R"(IMP_produce_struct_space_Trivial_returns_struct_space_Trivial())")
	produce<T>();

	// by value: ===============================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL, R"({
		var ref<IMP_Trivial> v0 = IMP_Trivial::(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>)));
		IMP_consume_struct_space_Trivial_returns_void(ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ T t; consume<T>(t); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL, R"(
		{ IMP_consume_struct_space_Trivial_returns_void(ref_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))), type_lit(f), type_lit(f), type_lit(cpp_rref))); }
	)")
	{ consume<T>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL, R"(
		{ IMP_consume_struct_space_Trivial_returns_void(<ref<IMP_Trivial>>(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>))){12}); }
	)")
	{ consume<T>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL,PRODUCE_TRIVIAL R"(
		{ IMP_consume_struct_space_Trivial_returns_void(ref_cast(IMP_produce_struct_space_Trivial_returns_struct_space_Trivial() materialize, type_lit(f), type_lit(f), type_lit(cpp_rref))); }
	)")
	{ consume<T>(produce<T>()); }

	// pass reference
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>)));
		var ref<IMP_Trivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_struct_space_Trivial_returns_void(ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ T t, &r = t; consume<T>(r); }

	// by reference: ===========================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_REF,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>)));
		IMP_consume_struct_space_Trivial_space__ampersand__returns_void(ref_kind_cast(v0,type_lit(cpp_ref)));
	})")
	{ T t; consume<T&>(t); }

	//{ consume<T&>(T()); }			    // pass temporary   - NOT ALLOWED
	//{ consume<T&>({12}); }			// pass temporary   - NOT ALLOWED
	//{ consume<T&>(produce<T>()); }	// pass x-value     - NOT ALLOWED

	// pass reference
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_REF,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>)));
		var ref<IMP_Trivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_struct_space_Trivial_space__ampersand__returns_void(v1);
	})")
	{ T t, &r = t; consume<T&>(r); }

	// by r-value reference: ===================================================================================================================================

	//{ T t; consume<T&&>(t); }		// pass l-value         - NOT ALLOWED

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_RREF,R"(
		{ IMP_consume_struct_space_Trivial_space__ampersand__ampersand__returns_void(ref_kind_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))),type_lit(cpp_rref))); }
	)")
	{ consume<T&&>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_RREF,R"(
		{ IMP_consume_struct_space_Trivial_space__ampersand__ampersand__returns_void(ref_kind_cast(<ref<IMP_Trivial,f,f,plain>>(ref_temp(type_lit(IMP_Trivial))) {12}, type_lit(cpp_rref))); }
	)")
	{ consume<T&&>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_RREF,PRODUCE_TRIVIAL R"(
		{ IMP_consume_struct_space_Trivial_space__ampersand__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_space_Trivial_returns_struct_space_Trivial() materialize, type_lit(cpp_rref))); }
	)")
	{ consume<T&&>(produce<T>()); }

	//{ T t, &r = t; consume<T&&>(r); } // pass reference   - NOT ALLOWED

	// by constant reference: ==================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>)));
		IMP_consume_const_space_struct_space_Trivial_space__ampersand__returns_void(ref_kind_cast(v0,type_lit(cpp_ref)));
	})")
	{ T t; consume<const T&>(t); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,R"(
		{ IMP_consume_const_space_struct_space_Trivial_space__ampersand__returns_void(ref_kind_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))),type_lit(cpp_ref))); }
	)")
	{ consume<const T&>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,R"(
		{ IMP_consume_const_space_struct_space_Trivial_space__ampersand__returns_void(ref_kind_cast(<ref<IMP_Trivial,f,f,plain>>(ref_temp(type_lit(IMP_Trivial))) {12}, type_lit(cpp_ref))); }
	)")
	{ consume<const T&>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,PRODUCE_TRIVIAL R"(
		{ IMP_consume_const_space_struct_space_Trivial_space__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_space_Trivial_returns_struct_space_Trivial() materialize, type_lit(cpp_ref))); }
	)")
	{ consume<const T&>(produce<T>()); }

	// pass reference
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(ref_decl(type_lit(ref<IMP_Trivial,f,f,plain>)));
		var ref<IMP_Trivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_const_space_struct_space_Trivial_space__ampersand__returns_void(v1);
	})")
	{ T t, &r = t; consume<const T&>(r); }

	// by constant r-value reference: ==========================================================================================================================

	//{ T t; consume<const T&&>(t); }	// pass l-value          - NOT ALLOWED

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_RREF,R"(
		{ IMP_consume_const_space_struct_space_Trivial_space__ampersand__ampersand__returns_void(ref_kind_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))),type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>(T()); }		// pass temporary

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_RREF,R"(
		{ IMP_consume_const_space_struct_space_Trivial_space__ampersand__ampersand__returns_void(ref_kind_cast(<ref<IMP_Trivial,f,f,plain>>(ref_temp(type_lit(IMP_Trivial))) {12}, type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>({12}); }		// pass temporary

	// pass x-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_RREF,PRODUCE_TRIVIAL R"(
		{ IMP_consume_const_space_struct_space_Trivial_space__ampersand__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_space_Trivial_returns_struct_space_Trivial() materialize, type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>(produce<T>()); }	// pass x-value

	//{ T t, &r = t; consume<const T&&>(r); } // pass reference  - NOT ALLOWED
}


// a non-trivial struct
struct NonTrivial {
	int i;
	~NonTrivial() { 5; }
};

#define STRUCT_NON_TRIVIAL "def struct IMP_NonTrivial { i : int<4>; dtor() { 5; } }; "
#define CONSUME_NON_TRIVIAL "def IMP_consume_struct_space_NonTrivial_returns_void = function (v0 : ref<IMP_NonTrivial,f,f,plain>) -> unit { }; "
#define PRODUCE_NON_TRIVIAL "def IMP_produce_struct_space_NonTrivial_returns_struct_space_NonTrivial = function () -> IMP_NonTrivial { return ref_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))), type_lit(t), type_lit(f), type_lit(cpp_ref)); }; "

#define CONSUME_NON_TRIVIAL_REF "def IMP_consume_struct_space_NonTrivial_space__ampersand__returns_void = function (v0 : ref<IMP_NonTrivial,f,f,cpp_ref>) -> unit { }; "
#define CONSUME_NON_TRIVIAL_RREF "def IMP_consume_struct_space_NonTrivial_space__ampersand__ampersand__returns_void = function (v0 : ref<IMP_NonTrivial,f,f,cpp_rref>) -> unit { }; "

#define CONSUME_NON_TRIVIAL_CONST_REF "def IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__returns_void = function (v0 : ref<IMP_NonTrivial,t,f,cpp_ref>) -> unit { };"
#define CONSUME_NON_TRIVIAL_CONST_RREF "def IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__ampersand__returns_void = function (v0 : ref<IMP_NonTrivial,t,f,cpp_rref>) -> unit { }; "

void validateNonTrivial() {

	using T = NonTrivial;

	// by value: ===============================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"({
		var ref<IMP_NonTrivial> v0 = IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
		IMP_consume_struct_space_NonTrivial_returns_void(ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ T t; consume<T>(t); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"({
		IMP_consume_struct_space_NonTrivial_returns_void(ref_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))), type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ consume<T>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"({
		 IMP_consume_struct_space_NonTrivial_returns_void(<ref<IMP_NonTrivial,f,f,plain>>(ref_decl(type_lit(ref<IMP_NonTrivial>))) {12});
	})")
	{ consume<T>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL,PRODUCE_NON_TRIVIAL, R"({
		IMP_consume_struct_space_NonTrivial_returns_void(ref_cast(IMP_produce_struct_space_NonTrivial_returns_struct_space_NonTrivial() materialize , type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ consume<T>(produce<T>()); }

	// pass reference
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
		var ref<IMP_NonTrivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_struct_space_NonTrivial_returns_void(ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ T t, &r = t; consume<T>(r); }


	// by reference: ===========================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_REF,R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
		IMP_consume_struct_space_NonTrivial_space__ampersand__returns_void(ref_kind_cast(v0,type_lit(cpp_ref)));
	})")
	{ T t; consume<T&>(t); }

	//{ consume<T&>(T()); }			    // pass temporary   - NOT ALLOWED
	//{ consume<T&>({12}); }			// pass temporary   - NOT ALLOWED
	//{ consume<T&>(produce<T>()); }	// pass x-value     - NOT ALLOWED

	// pass reference
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_REF,R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
		var ref<IMP_NonTrivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_struct_space_NonTrivial_space__ampersand__returns_void(v1);
	})")
	{ T t, &r = t; consume<T&>(r); }

	// by r-value reference: ===================================================================================================================================

	//{ T t; consume<T&&>(t); }		// pass l-value         - NOT ALLOWED

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_RREF,R"(
		{ IMP_consume_struct_space_NonTrivial_space__ampersand__ampersand__returns_void(ref_kind_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))),type_lit(cpp_rref))); }
	)")
	{ consume<T&&>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_RREF,R"(
		{ IMP_consume_struct_space_NonTrivial_space__ampersand__ampersand__returns_void(ref_kind_cast(<ref<IMP_NonTrivial,f,f,plain>>(ref_temp(type_lit(IMP_NonTrivial))) {12}, type_lit(cpp_rref))); }
	)")
	{ consume<T&&>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_RREF,PRODUCE_NON_TRIVIAL R"(
		{ IMP_consume_struct_space_NonTrivial_space__ampersand__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_space_NonTrivial_returns_struct_space_NonTrivial() materialize, type_lit(cpp_rref))); }
	)")
	{ consume<T&&>(produce<T>()); }

	//{ T t, &r = t; consume<T&&>(r); } // pass reference   - NOT ALLOWED

	// by constant reference: ==================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
		IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__returns_void(ref_kind_cast(v0,type_lit(cpp_ref)));
	})")
	{ T t; consume<const T&>(t); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,R"(
		{ IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__returns_void(ref_kind_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))),type_lit(cpp_ref))); }
	)")
	{ consume<const T&>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,R"(
		{ IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__returns_void(ref_kind_cast(<ref<IMP_NonTrivial,f,f,plain>>(ref_temp(type_lit(IMP_NonTrivial))) {12}, type_lit(cpp_ref))); }
	)")
	{ consume<const T&>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,PRODUCE_NON_TRIVIAL R"(
		{ IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_space_NonTrivial_returns_struct_space_NonTrivial() materialize, type_lit(cpp_ref))); }
	)")
	{ consume<const T&>(produce<T>()); }

	// pass reference
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(ref_decl(type_lit(ref<IMP_NonTrivial,f,f,plain>)));
		var ref<IMP_NonTrivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__returns_void(v1);
	})")
	{ T t, &r = t; consume<const T&>(r); }

	// by constant r-value reference: ==========================================================================================================================

	//{ T t; consume<const T&&>(t); }	// pass l-value          - NOT ALLOWED

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_RREF,R"(
		{ IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__ampersand__returns_void(ref_kind_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))),type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>(T()); }		// pass temporary

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_RREF,R"(
		{ IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__ampersand__returns_void(ref_kind_cast(<ref<IMP_NonTrivial,f,f,plain>>(ref_temp(type_lit(IMP_NonTrivial))) {12}, type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>({12}); }		// pass temporary

	// pass x-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_RREF,PRODUCE_NON_TRIVIAL R"(
		{ IMP_consume_const_space_struct_space_NonTrivial_space__ampersand__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_space_NonTrivial_returns_struct_space_NonTrivial() materialize, type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>(produce<T>()); }	// pass x-value

	//{ T t, &r = t; consume<const T&&>(r); } // pass reference  - NOT ALLOWED
}

struct WithSingleArgConstructor {
	int x;
	WithSingleArgConstructor(int x) : x(x) {}
};

WithSingleArgConstructor setX(WithSingleArgConstructor p) {
	auto p1 = p;
	p1.x = 10;
	return p1;
}

void validateSpecial() {
	;
	#pragma test expect_ir(R"(
		def struct IMP_WithSingleArgConstructor {
			x : int<4>;
			ctor function (v1 : ref<int<4>,f,f,plain>) {
				<ref<int<4>,f,f,plain>>((this).x) {*v1};
			}
		};
		def IMP_setX = function (v0 : ref<IMP_WithSingleArgConstructor,f,f,plain>) -> IMP_WithSingleArgConstructor {
			var ref<IMP_WithSingleArgConstructor,f,f,plain> v1 = ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref));
			v1.x = 10;
			return ref_cast(v1, type_lit(f), type_lit(f), type_lit(cpp_rref));
		};
		{
			var ref<IMP_WithSingleArgConstructor,f,f,plain> v0 = ref_cast(IMP_setX(IMP_WithSingleArgConstructor::(ref_decl(type_lit(ref<IMP_WithSingleArgConstructor,f,f,plain>)), 0)) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		WithSingleArgConstructor a = setX({0});
	}
}

int main() {

	validateTrivial();
	validateNonTrivial();
	validateSpecial();

	return 0;
}
