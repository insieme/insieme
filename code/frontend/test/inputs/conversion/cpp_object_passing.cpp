/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

// a non-trivial struct
struct NonTrivial {
	int i;
	~NonTrivial() {}
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

#define STRUCT_TRIVIAL "def struct IMP_Trivial { i : int<4>; };"
#define CONSUME_TRIVIAL "def IMP_consume_struct_Trivial_returns_void = function (v0 : ref<IMP_Trivial,f,f,plain>) -> unit { };"

#define STRUCT_NON_TRIVIAL "def struct IMP_NonTrivial { i : int<4>; };"
#define CONSUME_NON_TRIVIAL "def IMP_consume_struct_NonTrivial_returns_void = function (v0 : ref<IMP_NonTrivial,f,f,plain>) -> unit { };"


void space() {};

void validateTrivial() {
	; // this is required because of the clang compound source location bug

	using T = Trivial;

	// -------- arguments --------

	// by value:

	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL, R"(
		{ var ref<IMP_Trivial> v0 = IMP_Trivial::(ref_var(type_lit(IMP_Trivial))); IMP_consume_struct_Trivial_returns_void(*v0); }
	)")
	{ T t; consume<T>(t); }		// pass l-value

	#pragma test expect_ir(STRUCT_TRIVIAL,CONSUME_TRIVIAL, R"(
		{ IMP_consume_struct_Trivial_returns_void(*IMP_Trivial::(ref_var(type_lit(IMP_Trivial)))); }
	)")
	{ consume<T>(T()); }	// pass temporary
/*
	{ consume<T>({12}); }	// pass temporary

	{ consume<T>(produce<T>()); }	// pass x-value


	// by reference:

	{ T t; consume<T&>(t); }		// pass l-value

//	{ consume<T&>(T()); }			// pass temporary

//	{ consume<T&>({12}); }			// pass temporary

//	{ consume<T&>(produce<T>()); }	// pass x-value


	// by r-value reference:

//	{ T t; consume<T&&>(t); }		// pass l-value

	{ consume<T&&>(T()); }			// pass temporary

	{ consume<T&&>({12}); }			// pass temporary

	{ consume<T&&>(produce<T>()); }	// pass x-value


	// by constant reference:

	{ T t; consume<const T&>(t); }		// pass l-value

	{ consume<const T&>(T()); }			// pass temporary

	{ consume<const T&>({12}); }		// pass temporary

	{ consume<const T&>(produce<T>()); }	// pass x-value


	// by constant r-value reference:

//	{ T t;  consume<const T&&>(t); }	// pass l-value

	{ consume<const T&&>(T()); }		// pass temporary

	{ consume<const T&&>({12}); }		// pass temporary

	{ consume<const T&&>(produce<T>()); }	// pass x-value



	// --------- return values ----------

	// by value:

	{ T x = produce<T>(); }				// return r-value, capture value

//	{ T& x = produce<T>(); }			// return r-value, capture by reference

	{ const T& x = produce<T>(); }		// return r-value, capture by constant reference

	{ T&& x = produce<T>(); }			// return r-value, capture by r-value reference

	{ const T&& x = produce<T>(); }		// return r-value, capture by r-value reference

	// -- life time extension --

//	{ T& x = produce<T>(); space(); }				// life-time extension of value

	{ const T& x = produce<T>(); space(); }			// life-time extension of value

	{ T&& x = produce<T>(); space(); }				// life-time extension of value

	{ const T&& x = produce<T>(); space(); }		// life-time extension of value


	// by reference:

	{ T t; T x = to<T&,T&>(t); }			// return reference, capture value

	{ T t; T& x = to<T&,T&>(t); }			// return reference, capture by reference

	{ T t; const T& x = to<T&,T&>(t); }		// return reference, capture by constant reference

//	{ T t; T&& x = to<T&,T&>(t); }			// return reference, capture by r-value reference

	// -- life time extension --

	{ T t; T& x = to<T&,T&>(t); space(); }				// life-time extension of referenced value ?

	{ T t; const T& x = to<T&,T&>(t); space(); }		// life-time extension of referenced value ?

//	{ T t; T&& x = to<T&,T&>(t); space(); }				// life-time extension of referenced value ?

//	{ T t; const T&& x = to<T&,T&>(t); space(); }		// life-time extension of referenced value ?



	// by r-value reference: -- none is possible?

//	{ T x = to<T&&,T&&>(produce<T>()); }			// return r-value reference, capture value

//	{ T& x = to<T&&,T&&>(produce<T>()); }			// return r-value reference, capture by reference

//	{ const T& x = to<T&&,T&&>(produce<T>()); }		// return r-value reference, capture by constant reference

//	{ T&& x = to<T&&,T&&>(produce<T>()); }			// return r-value reference, capture by r-value reference

	// -- life time extension --

//	{ T& x = to<T&&,T&&>(produce<T>()); space(); }				// life-time extension of value

//	{ const T& x = to<T&&,T&&>(produce<T>()); space(); }		// life-time extension of value

//	{ T&& x = to<T&&,T&&>(produce<T>()); space(); }				// life-time extension of value

//	{ const T&& x = to<T&&,T&&>(produce<T>()); space(); }		// life-time extension of value


	// by constant reference:

	{ T t; T x = to<const T&,const T&>(t); }			// return reference, capture value

//	{ T t; T& x = to<const T&,const T&>(t); }			// return reference, capture by reference

	{ T t; const T& x = to<const T&,const T&>(t); }		// return reference, capture by constant reference

//	{ T t; T&& x = to<const T&,const T&>(t); }			// return reference, capture by r-value reference

	// -- life time extension --

//	{ T t; T& x = to<const T&,const T&>(t); space(); }				// life-time extension of referenced value ?

	{ T t; const T& x = to<const T&,const T&>(t); space(); }		// life-time extension of referenced value ?

//	{ T t; T&& x = to<const T&,const T&>(t); space(); }				// life-time extension of value

//	{ T t; const T&& x = to<const T&,const T&>(t); space(); }		// life-time extension of value



	// by constant r-value reference: (non of those work)

//	{ T x = to<const T&&,const T&&>(produce<T>()); }			// return reference, capture value

//	{ T& x = to<const T&&,const T&&>(produce<T>()); }			// return reference, capture by reference

//	{ const T& x = to<const T&&,const T&&>(produce<T>()); }		// return reference, capture by constant reference

//	{ T&& x = to<const T&&,const T&&>(produce<T>()); }			// return reference, capture by r-value reference

	// -- life time extension --

//	{ T& x = to<const T&&,const T&&>(produce<T>()); space(); }				// life-time extension of referenced value ?

//	{ const T& x = to<const T&&,const T&&>(produce<T>()); space(); }		// life-time extension of referenced value ?

//	{ T&& x = to<const T&&,const T&&>(produce<T>()); space(); }				// life-time extension of value

//	{ const T&& x = to<const T&&,const T&&>(produce<T>()); space(); }		// life-time extension of value
*/
}


/*
void validateNonTrivial() {
	; // this is required because of the clang compound source location bug

	using T = NonTrivial;

	#pragma future test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"(
		{ var ref<IMP_NonTrivial> v0 = IMP_NonTrivial::(ref_var(type_lit(IMP_NonTrivial))); IMP_consume_struct_NonTrivial_returns_void(v0); }
	)")
	{ T t; consume<T>(t); }		// pass l-value

	#pragma future test expect_ir(STRUCT_NON_TRIVIAL,CONSUME_NON_TRIVIAL, R"(
		{ IMP_consume_struct_NonTrivial_returns_void(*IMP_NonTrivial::(ref_var(type_lit(IMP_NonTrivial)))); }
	)")
	{ consume<T>(T()); }	// pass temporary

	{ consume<T>({12}); }	// pass temporary

	{ consume<T>(produce<T>()); }	// pass x-value


	// by reference:

	{ T t; consume<T&>(t); }		// pass l-value

//	{ consume<T&>(T()); }			// pass temporary

//	{ consume<T&>({12}); }			// pass temporary

//	{ consume<T&>(produce<T>()); }	// pass x-value


	// by r-value reference:

//	{ T t; consume<T&&>(t); }		// pass l-value

	{ consume<T&&>(T()); }			// pass temporary

	{ consume<T&&>({12}); }			// pass temporary

	{ consume<T&&>(produce<T>()); }	// pass x-value


	// by constant reference:

	{ T t; consume<const T&>(t); }		// pass l-value

	{ consume<const T&>(T()); }			// pass temporary

	{ consume<const T&>({12}); }		// pass temporary

	{ consume<const T&>(produce<T>()); }	// pass x-value


	// by constant r-value reference:

//	{ T t;  consume<const T&&>(t); }	// pass l-value

	{ consume<const T&&>(T()); }		// pass temporary

	{ consume<const T&&>({12}); }		// pass temporary

	{ consume<const T&&>(produce<T>()); }	// pass x-value



	// --------- return values ----------

	// by value:

	{ T x = produce<T>(); }				// return r-value, capture value

//	{ T& x = produce<T>(); }			// return r-value, capture by reference

	{ const T& x = produce<T>(); }		// return r-value, capture by constant reference

	{ T&& x = produce<T>(); }			// return r-value, capture by r-value reference

	{ const T&& x = produce<T>(); }		// return r-value, capture by r-value reference

	// -- life time extension --

//	{ T& x = produce<T>(); space(); }				// life-time extension of value

	{ const T& x = produce<T>(); space(); }			// life-time extension of value

	{ T&& x = produce<T>(); space(); }				// life-time extension of value

	{ const T&& x = produce<T>(); space(); }		// life-time extension of value


	// by reference:

	{ T t; T x = to<T&,T&>(t); }			// return reference, capture value

	{ T t; T& x = to<T&,T&>(t); }			// return reference, capture by reference

	{ T t; const T& x = to<T&,T&>(t); }		// return reference, capture by constant reference

//	{ T t; T&& x = to<T&,T&>(t); }			// return reference, capture by r-value reference

	// -- life time extension --

	{ T t; T& x = to<T&,T&>(t); space(); }				// life-time extension of referenced value ?

	{ T t; const T& x = to<T&,T&>(t); space(); }		// life-time extension of referenced value ?

//	{ T t; T&& x = to<T&,T&>(t); space(); }				// life-time extension of referenced value ?

//	{ T t; const T&& x = to<T&,T&>(t); space(); }		// life-time extension of referenced value ?



	// by r-value reference: -- none is possible?

//	{ T x = to<T&&,T&&>(produce<T>()); }			// return r-value reference, capture value

//	{ T& x = to<T&&,T&&>(produce<T>()); }			// return r-value reference, capture by reference

//	{ const T& x = to<T&&,T&&>(produce<T>()); }		// return r-value reference, capture by constant reference

//	{ T&& x = to<T&&,T&&>(produce<T>()); }			// return r-value reference, capture by r-value reference

// -- life time extension --

//	{ T& x = to<T&&,T&&>(produce<T>()); space(); }				// life-time extension of value

//	{ const T& x = to<T&&,T&&>(produce<T>()); space(); }		// life-time extension of value

//	{ T&& x = to<T&&,T&&>(produce<T>()); space(); }				// life-time extension of value

//	{ const T&& x = to<T&&,T&&>(produce<T>()); space(); }		// life-time extension of value


	// by constant reference:

	{ T t; T x = to<const T&,const T&>(t); }			// return reference, capture value

//	{ T t; T& x = to<const T&,const T&>(t); }			// return reference, capture by reference

	{ T t; const T& x = to<const T&,const T&>(t); }		// return reference, capture by constant reference

//	{ T t; T&& x = to<const T&,const T&>(t); }			// return reference, capture by r-value reference

	// -- life time extension --

//	{ T t; T& x = to<const T&,const T&>(t); space(); }				// life-time extension of referenced value ?

	{ T t; const T& x = to<const T&,const T&>(t); space(); }		// life-time extension of referenced value ?

//	{ T t; T&& x = to<const T&,const T&>(t); space(); }				// life-time extension of value

//	{ T t; const T&& x = to<const T&,const T&>(t); space(); }		// life-time extension of value



	// by constant r-value reference: (non of those work)

//	{ T x = to<const T&&,const T&&>(produce<T>()); }			// return reference, capture value

//	{ T& x = to<const T&&,const T&&>(produce<T>()); }			// return reference, capture by reference

//	{ const T& x = to<const T&&,const T&&>(produce<T>()); }		// return reference, capture by constant reference

//	{ T&& x = to<const T&&,const T&&>(produce<T>()); }			// return reference, capture by r-value reference

	// -- life time extension --

//	{ T& x = to<const T&&,const T&&>(produce<T>()); space(); }				// life-time extension of referenced value ?

//	{ const T& x = to<const T&&,const T&&>(produce<T>()); space(); }		// life-time extension of referenced value ?

//	{ T&& x = to<const T&&,const T&&>(produce<T>()); space(); }				// life-time extension of value

//	{ const T&& x = to<const T&&,const T&&>(produce<T>()); space(); }		// life-time extension of value

}
*/

int main() {

	validateTrivial();
//	validateNonTrivial();

	return 0;
}
