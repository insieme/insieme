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

struct NoDefaultCtor {
	NoDefaultCtor(int i) {}
};

struct DefaultedDefaultCtor {
	DefaultedDefaultCtor() = default;
	DefaultedDefaultCtor(int i) {}
};

struct DeletedDefaultCtor {
	DeletedDefaultCtor() = delete;
};

struct DefaultedCopyCtor {
	DefaultedCopyCtor(const DefaultedCopyCtor& other) = default;
};

struct DeletedCopyCtor {
	DeletedCopyCtor(const DeletedCopyCtor& other) = delete;
};

struct DefaultedMoveCtor {
	DefaultedMoveCtor(DefaultedMoveCtor&& other) = default;
};

struct DeletedMoveCtor {
	DeletedMoveCtor(DeletedMoveCtor&& other) = delete;
};

struct DefaultedDtor {
	~DefaultedDtor() = default;
};

struct DeletedDtor {
	~DeletedDtor() = delete;
};

struct CustomDtor {
	~CustomDtor() {}
};

struct DefaultedCopyAssignment {
	DefaultedCopyAssignment& operator=(const DefaultedCopyAssignment& rhs) = default;
};

struct DeletedCopyAssignment {
	DeletedCopyAssignment& operator=(const DeletedCopyAssignment& rhs) = delete;
};

struct DefaultedMoveAssignment {
	DefaultedMoveAssignment& operator=(DefaultedMoveAssignment&& rhs) = default;
};

struct DeletedMoveAssignment {
	DeletedMoveAssignment& operator=(DeletedMoveAssignment&& rhs) = delete;
};

struct WildMix {
	WildMix() = default;
	WildMix(const WildMix& other) = delete;
	WildMix(WildMix&& other) {};
	~WildMix() {}
	WildMix& operator=(const WildMix& rhs) = default;
	WildMix& operator=(WildMix&& rhs) = delete;
};

int global;
struct BasedOnFields1 {
	int i;
};
struct BasedOnFields2 {
	int i = 5;
};
struct BasedOnFields3 {
	int& i;
};
struct BasedOnFields4 {
	int& i = global;
};

int main() {

	// verifies that the frontend and the parser handle defaulted and deleted members equally

	#pragma test expect_ir(R"(
		def struct IMP_NoDefaultCtor {
			ctor function (v1 : ref<int<4>,f,f,plain>) { }
		};
		def struct IMP_DefaultedDefaultCtor {
			ctor function () = default;
			ctor function (v1 : ref<int<4>,f,f,plain>) { }
		};
		def struct IMP_DeletedDefaultCtor {
			ctor function () = delete;
		};
		def struct IMP_DefaultedCopyCtor {
			ctor function (other : ref<IMP_DefaultedCopyCtor,t,f,cpp_ref>) = default;
		};
		def struct IMP_DeletedCopyCtor {
			ctor function (other : ref<IMP_DeletedCopyCtor,t,f,cpp_ref>) = delete;
		};
		def struct IMP_DefaultedMoveCtor {
			ctor function (other : ref<IMP_DefaultedMoveCtor,f,f,cpp_rref>) = default;
		};
		def struct IMP_DeletedMoveCtor {
			ctor function (other : ref<IMP_DeletedMoveCtor,f,f,cpp_rref>) = delete;
		};
		def struct IMP_DefaultedDtor {
			dtor function () = default;
		};
		def struct IMP_DeletedDtor {
			dtor function () = delete;
		};
		def struct IMP_CustomDtor {
			dtor function () {}
		};
		def struct IMP_DefaultedCopyAssignment {
			function IMP__operator_assign_ = (rhs : ref<IMP_DefaultedCopyAssignment,t,f,cpp_ref>) -> ref<IMP_DefaultedCopyAssignment,f,f,cpp_ref> = default;
		};
		def struct IMP_DeletedCopyAssignment {
			function IMP__operator_assign_ = (rhs : ref<IMP_DeletedCopyAssignment,t,f,cpp_ref>) -> ref<IMP_DeletedCopyAssignment,f,f,cpp_ref> = delete;
		};
		def struct IMP_DefaultedMoveAssignment {
			function IMP__operator_assign_ = (rhs : ref<IMP_DefaultedMoveAssignment,f,f,cpp_rref>) -> ref<IMP_DefaultedMoveAssignment,f,f,cpp_ref> = default;
		};
		def struct IMP_DeletedMoveAssignment {
			function IMP__operator_assign_ = (rhs : ref<IMP_DeletedMoveAssignment,f,f,cpp_rref>) -> ref<IMP_DeletedMoveAssignment,f,f,cpp_ref> = default;
		};
		def struct IMP_WildMix {
			ctor function () = default;
			ctor function (other : ref<IMP_WildMix,t,f,cpp_ref>) = delete;
			ctor function (other : ref<IMP_WildMix,f,f,cpp_rref>) { }
			dtor function () { }
			function IMP__operator_assign_ = (rhs : ref<IMP_WildMix,t,f,cpp_ref>) -> ref<IMP_WildMix,f,f,cpp_ref> = default;
			function IMP__operator_assign_ = (rhs : ref<IMP_WildMix,f,f,cpp_rref>) -> ref<IMP_WildMix,f,f,cpp_ref> = delete;
		};
		{
			var ref<ptr<IMP_NoDefaultCtor>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<IMP_NoDefaultCtor>,f,f,plain>));
			var ref<ptr<IMP_DefaultedDefaultCtor>,f,f,plain> v1 = ref_decl(type_lit(ref<ptr<IMP_DefaultedDefaultCtor>,f,f,plain>));
			var ref<ptr<IMP_DeletedDefaultCtor>,f,f,plain> v2 = ref_decl(type_lit(ref<ptr<IMP_DeletedDefaultCtor>,f,f,plain>));
			var ref<ptr<IMP_DefaultedCopyCtor>,f,f,plain> v3 = ref_decl(type_lit(ref<ptr<IMP_DefaultedCopyCtor>,f,f,plain>));
			var ref<ptr<IMP_DeletedCopyCtor>,f,f,plain> v4 = ref_decl(type_lit(ref<ptr<IMP_DeletedCopyCtor>,f,f,plain>));
			var ref<ptr<IMP_DefaultedMoveCtor>,f,f,plain> v5 = ref_decl(type_lit(ref<ptr<IMP_DefaultedMoveCtor>,f,f,plain>));
			var ref<ptr<IMP_DeletedMoveCtor>,f,f,plain> v6 = ref_decl(type_lit(ref<ptr<IMP_DeletedMoveCtor>,f,f,plain>));
			var ref<ptr<IMP_DefaultedDtor>,f,f,plain> v7 = ref_decl(type_lit(ref<ptr<IMP_DefaultedDtor>,f,f,plain>));
			var ref<ptr<IMP_DeletedDtor>,f,f,plain> v8 = ref_decl(type_lit(ref<ptr<IMP_DeletedDtor>,f,f,plain>));
			var ref<ptr<IMP_CustomDtor>,f,f,plain> v9 = ref_decl(type_lit(ref<ptr<IMP_CustomDtor>,f,f,plain>));
			var ref<ptr<IMP_DefaultedCopyAssignment>,f,f,plain> v10 = ref_decl(type_lit(ref<ptr<IMP_DefaultedCopyAssignment>,f,f,plain>));
			var ref<ptr<IMP_DeletedCopyAssignment>,f,f,plain> v11 = ref_decl(type_lit(ref<ptr<IMP_DeletedCopyAssignment>,f,f,plain>));
			var ref<ptr<IMP_DefaultedMoveAssignment>,f,f,plain> v12 = ref_decl(type_lit(ref<ptr<IMP_DefaultedMoveAssignment>,f,f,plain>));
			var ref<ptr<IMP_DeletedMoveAssignment>,f,f,plain> v13 = ref_decl(type_lit(ref<ptr<IMP_DeletedMoveAssignment>,f,f,plain>));
			var ref<ptr<IMP_WildMix>,f,f,plain> v14 = ref_decl(type_lit(ref<ptr<IMP_WildMix>,f,f,plain>));
		}
	)")
	{
		NoDefaultCtor* c0;
		DefaultedDefaultCtor* c1;
		DeletedDefaultCtor* c2;
		DefaultedCopyCtor* c3;
		DeletedCopyCtor* c4;
		DefaultedMoveCtor* c5;
		DeletedMoveCtor* c6;
		DefaultedDtor* c7;
		DeletedDtor* c8;
		CustomDtor* c9;
		DefaultedCopyAssignment* c10;
		DeletedCopyAssignment* c11;
		DefaultedMoveAssignment* c12;
		DeletedMoveAssignment* c13;
		WildMix* w;
	}


	// Verifies that certain members get removed if we have reference fields. Also checks correct initialization of reference fields in the default ctor

	#pragma test expect_ir(R"(
		def struct IMP_BasedOnFields1 {
			i : int<4>;
		};
		def struct IMP_BasedOnFields2 {
			i : int<4>;
			ctor function() {
				lit(""INSIEME_DEFAULTED"":ref<array<char,18u>,t,f>);
				<ref<int<4>,f,f,plain>>((this).i) {5};
			}
		};
		def struct IMP_BasedOnFields3 {
			i : ref<int<4>,f,f,cpp_ref>;
			ctor function () = delete;
			ctor function (other : ref<IMP_BasedOnFields3,t,f,cpp_ref>) = default;
			ctor function (other : ref<IMP_BasedOnFields3,f,f,cpp_rref>) = default;
			function IMP__operator_assign_ = (rhs : ref<IMP_BasedOnFields3,t,f,cpp_ref>) -> ref<IMP_BasedOnFields3,f,f,cpp_ref> = delete;
			function IMP__operator_assign_ = (rhs : ref<IMP_BasedOnFields3,f,f,cpp_rref>) -> ref<IMP_BasedOnFields3,f,f,cpp_ref> = delete;
		};
		def struct IMP_BasedOnFields4 {
			i : ref<int<4>,f,f,cpp_ref>;
			ctor function() {
				lit(""INSIEME_DEFAULTED"":ref<array<char,18u>,t,f>);
				<ref<ref<int<4>,f,f,cpp_ref>,f,f,plain>>((this).i) {ref_kind_cast(lit("global" : ref<int<4>,f,f,plain>), type_lit(cpp_ref))};
			}
			ctor function (other : ref<IMP_BasedOnFields4,t,f,cpp_ref>) = default;
			ctor function (other : ref<IMP_BasedOnFields4,f,f,cpp_rref>) = default;
			function IMP__operator_assign_ = (rhs : ref<IMP_BasedOnFields4,t,f,cpp_ref>) -> ref<IMP_BasedOnFields4,f,f,cpp_ref> = delete;
			function IMP__operator_assign_ = (rhs : ref<IMP_BasedOnFields4,f,f,cpp_rref>) -> ref<IMP_BasedOnFields4,f,f,cpp_ref> = delete;
		};
		{
			var ref<ptr<IMP_BasedOnFields1>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<IMP_BasedOnFields1>,f,f,plain>));
			var ref<ptr<IMP_BasedOnFields2>,f,f,plain> v1 = ref_decl(type_lit(ref<ptr<IMP_BasedOnFields2>,f,f,plain>));
			var ref<ptr<IMP_BasedOnFields3>,f,f,plain> v2 = ref_decl(type_lit(ref<ptr<IMP_BasedOnFields3>,f,f,plain>));
			var ref<ptr<IMP_BasedOnFields4>,f,f,plain> v3 = ref_decl(type_lit(ref<ptr<IMP_BasedOnFields4>,f,f,plain>));
		}
	)")
	{
		BasedOnFields1* c1;
		BasedOnFields2* c2;
		BasedOnFields3* c3;
		BasedOnFields4* c4;
	}

	return 0;
}
