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

int main() {
	//===-------------------------------------------------------------------------------------------------------------------------------- UNARY OPERATORS ---===

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		ptr_from_ref(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		&b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,t,f,cpp_ref> v1 = v0;
		ptr_from_ref(ref_cast(v1, type_lit(t), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		const int& b = a;
		&b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		0-*v1;
	})")
	{
		int a;
		int& b = a;
		-b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,t,f,cpp_ref> v1 = v0;
		0-*v1;
	})")
	{
		int a;
		const int& b = a;
		-b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		gen_pre_inc(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		++b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		gen_post_inc(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		b++;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		gen_pre_dec(ref_cast(v1, type_lit(f), type_lit(f), type_lit(plain)));
	})")
	{
		int a;
		int& b = a;
		--b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
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
		var ref<int<4>,f,f,plain> v0 = v0;
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
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		*v1+*v0;
	})")
	{
		int a;
		int& b = a;
		b + a;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
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
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		*v0==*v1;
	})")
	{
		int a;
		int& b = a;
		a == b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		*v0<=*v1;
	})")
	{
		int a;
		int& b = a;
		a <= b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
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
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<real<4>,f,f,plain> v1 = v1;
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
		var ref<ptr<int<4>>,f,f,plain> v0 = v0;
		var ref<ptr<int<4>>,f,f,cpp_ref> v1 = v0;
		ptr_to_ref(*v1);
	})")
	{
		intPtr a;
		intPtr& b = a;
		*b;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<int<4>>,f,f,plain> v0 = v0;
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
		var ref<ptr<int<4>,t,f>,f,f,plain> v0 = v0;
		var ref<ptr<int<4>,t,f>,t,f,cpp_ref> v1 = v0;
		ptr_to_ref(*v1);
	})")
	{
		constIntPtr a;
		const constIntPtr& b = a;
		*b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<ptr<int<4>>,f,f,plain> v1 = v1;
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
		var ref<ptr<int<4>>,f,f,plain> v0 = v0;
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

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		v1 = *v1+1;
	})")
	{
		int b;
		int& a = b;
		a += 1;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		v1 = *v1-1;
	})")
	{
		int b;
		int& a = b;
		a -= 1;
	}

	// ASSIGNMENT //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		v1 = *v0;
	})")
	{
		int b;
		int& a = b;
		a = b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		v1 = 1;
	})")
	{
		int b;
		int& a = b;
		a = 1;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = v0;
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
		var ref<int<4>,f,f,plain> v0 = v0;
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
		var ref<int<4>,f,f,plain> v0 = v0;
		var ref<int<4>,f,f,cpp_ref> v1 = v0;
		v1 = num_cast(sizeof(type_lit(int<4>)), type_lit(int<4>));
	})")
	{
		int a;
		int& i = a;
		i = sizeof(i);
	}
}
