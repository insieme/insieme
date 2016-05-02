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
#define CONSUME_TRIVIAL "def IMP_consume_struct_Trivial_returns_void = function (v0 : ref<IMP_Trivial,f,f,plain>) -> unit { }; "
#define PRODUCE_TRIVIAL "def IMP_produce_struct_Trivial_returns_struct_Trivial = function () -> IMP_Trivial { return ref_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))), type_lit(f), type_lit(f), type_lit(cpp_rref)); }; "

#define CONSUME_TRIVIAL_REF "def IMP_consume_struct_Trivial__ampersand__returns_void = function (v0 : ref<IMP_Trivial,f,f,cpp_ref>) -> unit { }; "
#define CONSUME_TRIVIAL_RREF "def IMP_consume_struct_Trivial__ampersand__ampersand__returns_void = function (v0 : ref<IMP_Trivial,f,f,cpp_rref>) -> unit { }; "

#define CONSUME_TRIVIAL_CONST_REF "def IMP_consume_const_struct_Trivial__ampersand__returns_void = function (v0 : ref<IMP_Trivial,t,f,cpp_ref>) -> unit { };"
#define CONSUME_TRIVIAL_CONST_RREF "def IMP_consume_const_struct_Trivial__ampersand__ampersand__returns_void = function (v0 : ref<IMP_Trivial,t,f,cpp_rref>) -> unit { }; "

void validateTrivial() {
	; // this is required because of the clang compound source location bug

	using T = Trivial;

	// produce only
	#pragma test expect_ir(STRUCT_TRIVIAL,PRODUCE_TRIVIAL, R"(IMP_produce_struct_Trivial_returns_struct_Trivial())")
	produce<T>();

	// by value: ===============================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL, R"({
		var ref<IMP_Trivial> v0 = IMP_Trivial::(v0);
		IMP_consume_struct_Trivial_returns_void(ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ T t; consume<T>(t); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL, R"(
		{ IMP_consume_struct_Trivial_returns_void(ref_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))), type_lit(f), type_lit(f), type_lit(cpp_rref))); }
	)")
	{ consume<T>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL, R"(
		{ IMP_consume_struct_Trivial_returns_void(*<ref<IMP_Trivial>> {12}); }
	)")
	{ consume<T>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL,PRODUCE_TRIVIAL R"(
		{ IMP_consume_struct_Trivial_returns_void(ref_cast(IMP_produce_struct_Trivial_returns_struct_Trivial() materialize, type_lit(f), type_lit(f), type_lit(cpp_rref))); }
	)")
	{ consume<T>(produce<T>()); }

	// pass reference
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(v0);
		var ref<IMP_Trivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_struct_Trivial_returns_void(ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ T t, &r = t; consume<T>(r); }

	// by reference: ===========================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_REF,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(v0);
		IMP_consume_struct_Trivial__ampersand__returns_void(ref_kind_cast(v0,type_lit(cpp_ref)));
	})")
	{ T t; consume<T&>(t); }

	//{ consume<T&>(T()); }			    // pass temporary   - NOT ALLOWED
	//{ consume<T&>({12}); }			// pass temporary   - NOT ALLOWED
	//{ consume<T&>(produce<T>()); }	// pass x-value     - NOT ALLOWED

	// pass reference
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_REF,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(v0);
		var ref<IMP_Trivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_struct_Trivial__ampersand__returns_void(v1);
	})")
	{ T t, &r = t; consume<T&>(r); }

	// by r-value reference: ===================================================================================================================================

	//{ T t; consume<T&&>(t); }		// pass l-value         - NOT ALLOWED

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_RREF,R"(
		{ IMP_consume_struct_Trivial__ampersand__ampersand__returns_void(ref_kind_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))),type_lit(cpp_rref))); }
	)")
	{ consume<T&&>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_RREF,R"(
		{ IMP_consume_struct_Trivial__ampersand__ampersand__returns_void(ref_kind_cast(<ref<IMP_Trivial,f,f,plain>>(ref_temp(type_lit(IMP_Trivial))) {12}, type_lit(cpp_rref))); }
	)")
	{ consume<T&&>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_RREF,PRODUCE_TRIVIAL R"(
		{ IMP_consume_struct_Trivial__ampersand__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_Trivial_returns_struct_Trivial() materialize, type_lit(cpp_rref))); }
	)")
	{ consume<T&&>(produce<T>()); }

	//{ T t, &r = t; consume<T&&>(r); } // pass reference   - NOT ALLOWED

	// by constant reference: ==================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(v0);
		IMP_consume_const_struct_Trivial__ampersand__returns_void(ref_kind_cast(v0,type_lit(cpp_ref)));
	})")
	{ T t; consume<const T&>(t); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,R"(
		{ IMP_consume_const_struct_Trivial__ampersand__returns_void(ref_kind_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))),type_lit(cpp_ref))); }
	)")
	{ consume<const T&>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,R"(
		{ IMP_consume_const_struct_Trivial__ampersand__returns_void(ref_kind_cast(<ref<IMP_Trivial,f,f,plain>>(ref_temp(type_lit(IMP_Trivial))) {12}, type_lit(cpp_ref))); }
	)")
	{ consume<const T&>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,PRODUCE_TRIVIAL R"(
		{ IMP_consume_const_struct_Trivial__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_Trivial_returns_struct_Trivial() materialize, type_lit(cpp_ref))); }
	)")
	{ consume<const T&>(produce<T>()); }

	// pass reference
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_REF,R"({
		var ref<IMP_Trivial,f,f,plain> v0 = IMP_Trivial::(v0);
		var ref<IMP_Trivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_const_struct_Trivial__ampersand__returns_void(v1);
	})")
	{ T t, &r = t; consume<const T&>(r); }

	// by constant r-value reference: ==========================================================================================================================

	//{ T t; consume<const T&&>(t); }	// pass l-value          - NOT ALLOWED

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_RREF,R"(
		{ IMP_consume_const_struct_Trivial__ampersand__ampersand__returns_void(ref_kind_cast(IMP_Trivial::(ref_temp(type_lit(IMP_Trivial))),type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>(T()); }		// pass temporary

	// pass temporary
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_RREF,R"(
		{ IMP_consume_const_struct_Trivial__ampersand__ampersand__returns_void(ref_kind_cast(<ref<IMP_Trivial,f,f,plain>>(ref_temp(type_lit(IMP_Trivial))) {12}, type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>({12}); }		// pass temporary

	// pass x-value
	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL_CONST_RREF,PRODUCE_TRIVIAL R"(
		{ IMP_consume_const_struct_Trivial__ampersand__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_Trivial_returns_struct_Trivial() materialize, type_lit(cpp_rref))); }
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
#define CONSUME_NON_TRIVIAL "def IMP_consume_struct_NonTrivial_returns_void = function (v0 : ref<IMP_NonTrivial,f,f,plain>) -> unit { }; "
#define PRODUCE_NON_TRIVIAL "def IMP_produce_struct_NonTrivial_returns_struct_NonTrivial = function () -> IMP_NonTrivial { return ref_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))), type_lit(t), type_lit(f), type_lit(cpp_ref)); }; "

#define CONSUME_NON_TRIVIAL_REF "def IMP_consume_struct_NonTrivial__ampersand__returns_void = function (v0 : ref<IMP_NonTrivial,f,f,cpp_ref>) -> unit { }; "
#define CONSUME_NON_TRIVIAL_RREF "def IMP_consume_struct_NonTrivial__ampersand__ampersand__returns_void = function (v0 : ref<IMP_NonTrivial,f,f,cpp_rref>) -> unit { }; "

#define CONSUME_NON_TRIVIAL_CONST_REF "def IMP_consume_const_struct_NonTrivial__ampersand__returns_void = function (v0 : ref<IMP_NonTrivial,t,f,cpp_ref>) -> unit { };"
#define CONSUME_NON_TRIVIAL_CONST_RREF "def IMP_consume_const_struct_NonTrivial__ampersand__ampersand__returns_void = function (v0 : ref<IMP_NonTrivial,t,f,cpp_rref>) -> unit { }; "

void validateNonTrivial() {
	; // this is required because of the clang compound source location bug

	using T = NonTrivial;

	// by value: ===============================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"({
		var ref<IMP_NonTrivial> v0 = IMP_NonTrivial::(v0);
		IMP_consume_struct_NonTrivial_returns_void(ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ T t; consume<T>(t); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"({
		IMP_consume_struct_NonTrivial_returns_void(ref_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))), type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ consume<T>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"({
		 IMP_consume_struct_NonTrivial_returns_void(<ref<IMP_NonTrivial,f,f,plain>>(ref_temp(type_lit(IMP_NonTrivial))) {12});
	})")
	{ consume<T>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL,PRODUCE_NON_TRIVIAL, R"({
		IMP_consume_struct_NonTrivial_returns_void(ref_cast(IMP_produce_struct_NonTrivial_returns_struct_NonTrivial() materialize , type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ consume<T>(produce<T>()); }

	// pass reference
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(v0);
		var ref<IMP_NonTrivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_struct_NonTrivial_returns_void(ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref)));
	})")
	{ T t, &r = t; consume<T>(r); }


	// by reference: ===========================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_REF,R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(v0);
		IMP_consume_struct_NonTrivial__ampersand__returns_void(ref_kind_cast(v0,type_lit(cpp_ref)));
	})")
	{ T t; consume<T&>(t); }

	//{ consume<T&>(T()); }			    // pass temporary   - NOT ALLOWED
	//{ consume<T&>({12}); }			// pass temporary   - NOT ALLOWED
	//{ consume<T&>(produce<T>()); }	// pass x-value     - NOT ALLOWED

	// pass reference
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_REF,R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(v0);
		var ref<IMP_NonTrivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_struct_NonTrivial__ampersand__returns_void(v1);
	})")
	{ T t, &r = t; consume<T&>(r); }

	// by r-value reference: ===================================================================================================================================

	//{ T t; consume<T&&>(t); }		// pass l-value         - NOT ALLOWED

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_RREF,R"(
		{ IMP_consume_struct_NonTrivial__ampersand__ampersand__returns_void(ref_kind_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))),type_lit(cpp_rref))); }
	)")
	{ consume<T&&>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_RREF,R"(
		{ IMP_consume_struct_NonTrivial__ampersand__ampersand__returns_void(ref_kind_cast(<ref<IMP_NonTrivial,f,f,plain>>(ref_temp(type_lit(IMP_NonTrivial))) {12}, type_lit(cpp_rref))); }
	)")
	{ consume<T&&>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_RREF,PRODUCE_NON_TRIVIAL R"(
		{ IMP_consume_struct_NonTrivial__ampersand__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_NonTrivial_returns_struct_NonTrivial() materialize, type_lit(cpp_rref))); }
	)")
	{ consume<T&&>(produce<T>()); }

	//{ T t, &r = t; consume<T&&>(r); } // pass reference   - NOT ALLOWED

	// by constant reference: ==================================================================================================================================

	// pass l-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(v0);
		IMP_consume_const_struct_NonTrivial__ampersand__returns_void(ref_kind_cast(v0,type_lit(cpp_ref)));
	})")
	{ T t; consume<const T&>(t); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,R"(
		{ IMP_consume_const_struct_NonTrivial__ampersand__returns_void(ref_kind_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))),type_lit(cpp_ref))); }
	)")
	{ consume<const T&>(T()); }

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,R"(
		{ IMP_consume_const_struct_NonTrivial__ampersand__returns_void(ref_kind_cast(<ref<IMP_NonTrivial,f,f,plain>>(ref_temp(type_lit(IMP_NonTrivial))) {12}, type_lit(cpp_ref))); }
	)")
	{ consume<const T&>({12}); }

	// pass x-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,PRODUCE_NON_TRIVIAL R"(
		{ IMP_consume_const_struct_NonTrivial__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_NonTrivial_returns_struct_NonTrivial() materialize, type_lit(cpp_ref))); }
	)")
	{ consume<const T&>(produce<T>()); }

	// pass reference
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_REF,R"({
		var ref<IMP_NonTrivial,f,f,plain> v0 = IMP_NonTrivial::(v0);
		var ref<IMP_NonTrivial,f,f,cpp_ref> v1 = v0;
		IMP_consume_const_struct_NonTrivial__ampersand__returns_void(v1);
	})")
	{ T t, &r = t; consume<const T&>(r); }

	// by constant r-value reference: ==========================================================================================================================

	//{ T t; consume<const T&&>(t); }	// pass l-value          - NOT ALLOWED

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_RREF,R"(
		{ IMP_consume_const_struct_NonTrivial__ampersand__ampersand__returns_void(ref_kind_cast(IMP_NonTrivial::(ref_temp(type_lit(IMP_NonTrivial))),type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>(T()); }		// pass temporary

	// pass temporary
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_RREF,R"(
		{ IMP_consume_const_struct_NonTrivial__ampersand__ampersand__returns_void(ref_kind_cast(<ref<IMP_NonTrivial,f,f,plain>>(ref_temp(type_lit(IMP_NonTrivial))) {12}, type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>({12}); }		// pass temporary

	// pass x-value
	#pragma test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL_CONST_RREF,PRODUCE_NON_TRIVIAL R"(
		{ IMP_consume_const_struct_NonTrivial__ampersand__ampersand__returns_void(ref_kind_cast(IMP_produce_struct_NonTrivial_returns_struct_NonTrivial() materialize, type_lit(cpp_rref))); }
	)")
	{ consume<const T&&>(produce<T>()); }	// pass x-value

	//{ T t, &r = t; consume<const T&&>(r); } // pass reference  - NOT ALLOWED
}

int main() {

	validateTrivial();
	validateNonTrivial();

	return 0;
}
