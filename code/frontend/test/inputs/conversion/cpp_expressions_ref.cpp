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
int main() {
	//===-------------------------------------------------------------------------------------------------------------------------------- UNARY OPERATORS ---===

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		ptr_from_ref(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		&b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,t,f,cpp_ref> v1 = v0;
		ptr_from_ref(ref_cast(v1, type_lit(t), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		const int& b = a;
		&b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		0-*v1;
	})")
	{
		int a;
		int& b = a;
		-b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,t,f,cpp_ref> v1 = v0;
		0-*v1;
	})")
	{
		int a;
		const int& b = a;
		-b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		gen_pre_inc(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		++b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		gen_post_inc(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		b++;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		gen_pre_dec(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		--b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		gen_post_dec(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		b--;
	}

	//===------------------------------------------------------------------------------------------------------------------------------- BINARY OPERATORS ---===

	// MATH //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		*v0+*v1;
	})")
	{
		int a;
		int& b = a;
		a + b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,t,f,plain> v0 = 1;
		var ref<int<4>,t,f,cpp_ref> v1 = v0;
		*v0+*v1;
	})")
	{
		const int a = 1;
		const int& b = a;
		a + b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		*v1+*v0;
	})")
	{
		int a;
		int& b = a;
		b + a;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		*v1+*v2;
	})")
	{
		int a;
		int& b = a;
		int& c = a;
		b + c;
	}

	// COMPARISON /////////////////////////////////////////////////////////

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		*v0==*v1;
	})")
	{
		int a;
		int& b = a;
		a == b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		*v0<=*v1;
	})")
	{
		int a;
		int& b = a;
		a <= b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		*v0<*v1;
	})")
	{
		int a;
		int& b = a;
		a < b;
	}

	// WITH DIFFERENT TYPES ///////////////////////////////////////////////////

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<real<4>,f,f,plain> v1 = ref_decl(type_lit(ref<real<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		var ref<real<4>,f,f,cpp_ref> v3 = v1;
		num_cast(*v2, type_lit(real<4>))+*v3;
	})")
	{
		int a;
		float b;
		int& ra = a;
		float& rb = b;

		ra + rb;
	}

	// POINTER ///////////////////////////////////////////////////////

	typedef int* intPtr;

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<int<4>>,f,f,plain>));
		var ref<ptr<int<4>>,f,f,cpp_ref> v1 = v0;
		ptr_to_ref(*v1);
	})")
	{
		intPtr a;
		intPtr& b = a;
		*b;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<int<4>>,f,f,plain>));
		var ref<ptr<int<4>>,t,f,cpp_ref> v1 = v0;
		ptr_to_ref(*v1);
	})")
	{
		intPtr a;
		const intPtr& b = a;
		*b;
	}

	typedef const int* constIntPtr;

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>,t,f>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<int<4>,t,f>,f,f,plain>));
		var ref<ptr<int<4>,t,f>,t,f,cpp_ref> v1 = v0;
		ptr_to_ref(*v1);
	})")
	{
		constIntPtr a;
		const constIntPtr& b = a;
		*b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<ptr<int<4>>,f,f,plain> v1 = ref_decl(type_lit(ref<ptr<int<4>>,f,f,plain>));
		var ref<ptr<int<4>>,f,f,cpp_ref> v2 = v1;
		v2 = ptr_from_ref(v0);
	})")
	{
		int v;
		intPtr a;
		intPtr& b = a;
		b = &v;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<int<4>>,f,f,plain>));
		var ref<ptr<int<4>>,f,f,cpp_ref> v1 = v0;
		ptr_gt(*v1, *v1);
		ptr_lt(*v1, *v1);
		ptr_le(*v1, *v1);
		ptr_ge(*v1, *v1);
	})")
	{
		intPtr x;
		intPtr& a = x;
		a > a;
		a < a;
		a <= a;
		a >= a;
	}

	// COMPOUND //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"(
	using "ext.compound_ops";
	{
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		comp_assign_add(v1, 1);
	})")
	{
		int b;
		int& a = b;
		a += 1;
	}

	#pragma test expect_ir(R"(
	using "ext.compound_ops";
	{
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		comp_assign_subtract(v1, 1);
	})")
	{
		int b;
		int& a = b;
		a -= 1;
	}

	// ASSIGNMENT //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		v1 = *v0;
	})")
	{
		int b;
		int& a = b;
		a = b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		v1 = 1;
	})")
	{
		int b;
		int& a = b;
		a = 1;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		v1 = *v2;
	})")
	{
		int b;
		int& a = b;
		int& c = b;
		a = c;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		v0 = *cxx_style_assignment(v1, *v2);
	})")
	{
		int b;
		int& a = b;
		int& c = b;
		b = a = c;
	}

	//===----------------------------------------------------------------------------------------------------------------------------------------- SIZEOF ---===

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		v1 = num_cast(sizeof(type_lit(int<4>)), type_lit(int<4>));
	})")
	{
		int a;
		int& i = a;
		i = sizeof(i);
	}
}
