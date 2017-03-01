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
