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

// static methods ==============================================================================================================================================

struct X {
	static void a() {
		b();
	}
	static void b() {
	}
};

struct S {
	static void a() {}
};

struct Y {
	static void a() {
		Y y;
		y.b();
	}
	static void b() {
	}
};

// static members ==============================================================================================================================================

struct M {
	static int a;
};

int M::a = 42;

struct M2 {
	static void b() {
		a = 1;
	}
	static int a;
};


#pragma test expect_ir("REGEX_S", R"(.* <ref<int<4>,f,f,plain>>\(IMP_M_colon__colon_a\) \{42\}; .*)")
int main() {
	; // this is required because of the clang compound source location bug

	// static methods ==========================================================================================================================================

	#pragma test expect_ir(R"(
		decl IMP_X_colon__colon_b : () -> unit;
		def IMP_X_colon__colon_a = function () -> unit {
			IMP_X_colon__colon_b();
		};
		def IMP_X_colon__colon_b = function () -> unit { };
		{
			IMP_X_colon__colon_a();
		}
	)")
	{
		X::a();
	}

	#pragma test expect_ir(R"(
		def struct IMP_S {
		};
		def IMP_S_colon__colon_a = function () -> unit { };
		{
			var ref<IMP_S,f,f,plain> v0 = IMP_S::(ref_decl(type_lit(ref<IMP_S,f,f,plain>)));
			IMP_S_colon__colon_a();
		}
	)")
	{
		S s;
		s.a();
	}

	#pragma test expect_ir(R"(
		decl IMP_Y_colon__colon_b : () -> unit;
		def struct IMP_Y {
		};
		def IMP_Y_colon__colon_a = function () -> unit {
			var ref<IMP_Y,f,f,plain> v0 = IMP_Y::(ref_decl(type_lit(ref<IMP_Y,f,f,plain>)));
			IMP_Y_colon__colon_b();
		};
		def IMP_Y_colon__colon_b = function () -> unit { };
		{
			IMP_Y_colon__colon_a();
		}
	)")
	{
		Y::a();
	}


	// static members ==========================================================================================================================================

	#pragma test expect_ir(R"({ lit("IMP_M_colon__colon_a" : ref<int<4>,f,f,plain>) = 1; })")
	{
		M::a = 1;
	}

	M2 m2;
	m2.b();

	return 0;
}
