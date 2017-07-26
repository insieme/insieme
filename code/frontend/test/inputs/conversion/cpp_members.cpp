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

struct NonTrivial {
	NonTrivial() = delete;
	NonTrivial(int i) {}
	void foo() {}
};

struct S {
	int i;
	NonTrivial n;
	S() : i(), n(1) {}
};

struct T {
	int i;
	NonTrivial n;
	S s;
	T() : i(), n(1), s() {}
};

S generateS() { return {}; }

T generateT() { return {}; }

void consumeValue(NonTrivial n) {}

void consumeConstReference(const NonTrivial& n) {}

#define STRUCTS_IR R"(
	def struct IMP_NonTrivial {
		ctor function () = delete;
		ctor function (v1 : ref<int<4>,f,f,plain>) { }
		function IMP_foo = () -> unit { }
	};
	def struct IMP_S {
		i : int<4>;
		n : IMP_NonTrivial;
		ctor function () {
			<ref<int<4>,f,f,plain>>((this).i) {0};
			IMP_NonTrivial::((this).n, 1);
		}
	};
	def struct IMP_T {
		i : int<4>;
		n : IMP_NonTrivial;
		s : IMP_S;
		ctor function () {
			<ref<int<4>,f,f,plain>>((this).i) {0};
			IMP_NonTrivial::((this).n, 1);
			IMP_S::((this).s);
		}
	};
)"

#define GENERATE_STRUCTS_IR R"(
	def IMP_generateS = function () -> IMP_S {
		return IMP_S::(ref_decl(type_lit(ref<IMP_S,f,f,plain>)));
	};
	def IMP_generateT = function () -> IMP_T {
		return IMP_T::(ref_decl(type_lit(ref<IMP_T,f,f,plain>)));
	};
)"

int main() {

	// ========================================================================================================================================== values
	#pragma test expect_ir(STRUCTS_IR, R"(
		{
			var ref<IMP_S,f,f,plain> v0 = IMP_S::(ref_decl(type_lit(ref<IMP_S,f,f,plain>)));
			v0.i;
			v0.n;
			var ref<IMP_T,f,f,plain> v1 = IMP_T::(ref_decl(type_lit(ref<IMP_T,f,f,plain>)));
			v1.i;
			v1.n;
			v1.s;
			v1.s.i;
			v1.s.n;
		}
	)")
	{
		S s;
		s.i;
		s.n;

		T t;
		t.i;
		t.n;
		t.s;
		t.s.i;
		t.s.n;
	}

	// ======================================================================================================================================== pointers
	#pragma test expect_ir(STRUCTS_IR, R"(
		{
			var ref<ptr<IMP_S>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<IMP_S>,f,f,plain>));
			ptr_to_ref(*v0).i;
			ptr_to_ref(*v0).n;
			var ref<ptr<IMP_T>,f,f,plain> v1 = ref_decl(type_lit(ref<ptr<IMP_T>,f,f,plain>));
			ptr_to_ref(*v1).i;
			ptr_to_ref(*v1).n;
			ptr_to_ref(*v1).s;
			ptr_to_ref(*v1).s.i;
			ptr_to_ref(*v1).s.n;
		}
	)")
	{
		S* s;
		s->i;
		s->n;

		T* t;
		t->i;
		t->n;
		t->s;
		t->s.i;
		t->s.n;
	}

	// ============================================================================================================================== r-value references
	#pragma test expect_ir(STRUCTS_IR, GENERATE_STRUCTS_IR, R"(
		{
			IMP_generateS() materialize .i;
			IMP_generateS() materialize .n;
			IMP_generateT() materialize .i;
			IMP_generateT() materialize .n;
			IMP_generateT() materialize .s;
			IMP_generateT() materialize .s.i;
			IMP_generateT() materialize .s.n;
		}
	)")
	{
		generateS().i;
		generateS().n;

		generateT().i;
		generateT().n;
		generateT().s;
		generateT().s.i;
		generateT().s.n;
	}

	// =================================================================================================== r-value references used in another expression
	#pragma test expect_ir(STRUCTS_IR, GENERATE_STRUCTS_IR, R"(
		{
			*IMP_generateS() materialize .i+5;
			IMP_generateS() materialize .n.IMP_foo();
			*IMP_generateT() materialize .i+6;
			IMP_generateT() materialize .n.IMP_foo();
			*IMP_generateT() materialize .s.i+7;
			IMP_generateT() materialize .s.n.IMP_foo();
		}
	)")
	{
		generateS().i + 5;
		generateS().n.foo();

		generateT().i + 6;
		generateT().n.foo();
		generateT().s.i + 7;
		generateT().s.n.foo();
	}

	// ========================================================================================================================================= consume
	#pragma test expect_ir(STRUCTS_IR, GENERATE_STRUCTS_IR, R"(
		def IMP_consumeValue = function (v0 : ref<IMP_NonTrivial,f,f,plain>) -> unit { };
		def IMP_consumeConstReference = function (v0 : ref<IMP_NonTrivial,t,f,cpp_ref>) -> unit { };
		{
			var ref<IMP_S,f,f,plain> v0 = IMP_S::(ref_decl(type_lit(ref<IMP_S,f,f,plain>)));
			IMP_consumeValue(ref_cast(v0.n, type_lit(t), type_lit(f), type_lit(cpp_ref)));
			IMP_consumeConstReference(ref_kind_cast(v0.n, type_lit(cpp_ref)));
			IMP_consumeValue(ref_cast(IMP_generateS() materialize .n, type_lit(f), type_lit(f), type_lit(cpp_rref)));
			IMP_consumeConstReference(ref_kind_cast(IMP_generateS() materialize .n, type_lit(cpp_ref)));
		}
	)")
	{
		S s;
		consumeValue(s.n);
		consumeConstReference(s.n);

		consumeValue(generateS().n);
		consumeConstReference(generateS().n);
	}

	return 0;
}
