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

typedef struct S {
	int i;
} S;

typedef struct T {
	int i;
	S s;
} T;

S generate_S() { return (S){0}; };

T generate_T() { return (T){}; };

void consume_int(int i) {}

#define STRUCTS_IR " \
	def struct IMP_S { \
		i : int<4>; \
	}; \
	def struct IMP_T { \
		i : int<4>; \
		s : IMP_S; \
	};"

#define GENERATE_STRUCTS_IR " \
	def IMP_generate_S = function () -> IMP_S { \
		return *<ref<IMP_S,f,f,plain>>(ref_temp(type_lit(IMP_S))) {0}; \
	}; \
	def IMP_generate_T = function () -> IMP_T { \
		return *<ref<IMP_T,f,f,plain>>(ref_temp(type_lit(IMP_T))) {0, *<ref<IMP_S,f,f,plain>>(ref_temp(type_lit(IMP_S))) {0}}; \
	};"

int main() {

	// ========================================================================================================================================== values
	#pragma test expect_ir(STRUCTS_IR, R"(
		{
			var ref<IMP_S,f,f,plain> v0 = ref_decl(type_lit(ref<IMP_S,f,f,plain>));
			v0.i;
			var ref<IMP_T,f,f,plain> v1 = ref_decl(type_lit(ref<IMP_T,f,f,plain>));
			v1.i;
			v1.s;
			v1.s.i;
		}
	)")
	{
		S s;
		s.i;

		T t;
		t.i;
		t.s;
		t.s.i;
	}

	// ======================================================================================================================================== pointers
	#pragma test expect_ir(STRUCTS_IR, R"(
		{
			var ref<ptr<IMP_S>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<IMP_S>,f,f,plain>));
			ptr_to_ref(*v0).i;
			var ref<ptr<IMP_T>,f,f,plain> v1 = ref_decl(type_lit(ref<ptr<IMP_T>,f,f,plain>));
			ptr_to_ref(*v1).i;
			ptr_to_ref(*v1).s;
			ptr_to_ref(*v1).s.i;
		}
	)")
	{
		S* s;
		s->i;

		T* t;
		t->i;
		t->s;
		t->s.i;
	}

	// ============================================================================================================================== r-value references
	#pragma test expect_ir(STRUCTS_IR, GENERATE_STRUCTS_IR, R"(
		{
			IMP_generate_S() materialize .i;
			IMP_generate_T() materialize .i;
			IMP_generate_T() materialize .s;
			IMP_generate_T() materialize .s.i;
		}
	)")
	{
		generate_S().i;

		generate_T().i;
		generate_T().s;
		generate_T().s.i;
	}

	// =================================================================================================== r-value references used in another expression
	#pragma test expect_ir(STRUCTS_IR, GENERATE_STRUCTS_IR, R"(
		{
			*IMP_generate_S() materialize .i+5;
			*IMP_generate_T() materialize .i+6;
			*IMP_generate_T() materialize .s.i+7;
		}
	)")
	{
		generate_S().i + 5;
		generate_T().i + 6;
		generate_T().s.i + 7;
	}

	// ========================================================================================================================================= consume
	#pragma test expect_ir(STRUCTS_IR, GENERATE_STRUCTS_IR, R"(
		def IMP_consume_int = function (v0 : ref<int<4>,f,f,plain>) -> unit { };
		{
			var ref<IMP_S,f,f,plain> v0 = ref_decl(type_lit(ref<IMP_S,f,f,plain>));
			IMP_consume_int(*v0.i);
			var ref<IMP_T,f,f,plain> v1 = ref_decl(type_lit(ref<IMP_T,f,f,plain>));
			IMP_consume_int(*v1.i);
			IMP_consume_int(*v1.s.i);
			IMP_consume_int(*IMP_generate_S() materialize .i);
			IMP_consume_int(*IMP_generate_T() materialize .i);
			IMP_consume_int(*IMP_generate_T() materialize .s.i);
		}
	)")
	{
		S s;
		consume_int(s.i);

		T t;
		consume_int(t.i);
		consume_int(t.s.i);

		consume_int(generate_S().i);
		consume_int(generate_T().i);
		consume_int(generate_T().s.i);
	}

	return 0;
}
