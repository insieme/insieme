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

// -- Scalar tests
// ================================================================================================|

int scalar_int() {
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> a; return *a in ref<int<4>>; }")
	{
		int a;
		return a;
	}
}
const int& scalar_ref_int() {
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0;  return v0 in ref<int<4>,t,f,cpp_ref>; } ")
	{
		int a;
		return a;
	}
}

// upgrade cast on return
float scalar_float() {
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> a; return num_cast(*a, type_lit(real<4>)) in ref<real<4>>; }")
	{
		int a;
		return a;
	}
}

typedef int* intPtr;
intPtr pointer_int() {
	#pragma test expect_ir("return ptr_null(type_lit(int<4>), type_lit(f), type_lit(f)) in ref<ptr<int<4>>>;")
	return 0;
}

typedef const int* constIntPtr;
constIntPtr const_pointer_int() {
	#pragma test expect_ir("return ptr_null(type_lit(int<4>), type_lit(t), type_lit(f)) in ref<ptr<int<4>,t,f>>;")
	return 0;
}

intPtr& reference_pointer_int() {
	#pragma test expect_ir("{var ref<ptr<int<4>>,f,f,plain> v0; return v0 in ref<ptr<int<4>,f,f>,f,f,cpp_ref>; } ")
	{
		intPtr a;
		return a;
	}
}

const intPtr& const_reference_pointer_int() {
	#pragma test expect_ir("{var ref<ptr<int<4>>,f,f,plain> v0; return v0 in ref<ptr<int<4>,f,f>,t,f,cpp_ref>; } ")
	{
		intPtr a;
		return a;
	}
}

constIntPtr& reference_const_pointer_int() {
	#pragma test expect_ir("{var ref<ptr<int<4>,t,f>,f,f,plain> v0; return v0 in ref<ptr<int<4>,t,f>,f,f,cpp_ref>; } ")
	{
		constIntPtr a;
		return a;
	}
}

const constIntPtr& const_reference_const_pointer_int() {
	#pragma test expect_ir("{var ref<ptr<int<4>,t,f>,f,f,plain> v0; return v0 in ref<ptr<int<4>,t,f>,t,f,cpp_ref>; } ")
	{
		constIntPtr a;
		return a;
	}
}

void recursiveVoid(int a) {
	if(a<=0) return;
	return recursiveVoid(a-1);
}

int main() {
	{} // help pragmas to find their way

	{
		#pragma test expect_ir("EXPR_TYPE", "int<4>")
		scalar_int();
		#pragma test expect_ir("EXPR_TYPE", "real<4>")
		scalar_float();
		#pragma test expect_ir("EXPR_TYPE", "ref<int<4>,t,f>")
		scalar_ref_int();
	}

	{
		#pragma test expect_ir("EXPR_TYPE", "ptr<int<4>>")
		pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ptr<int<4>, t,f>")
		const_pointer_int();
	}

	{
		#pragma test expect_ir("EXPR_TYPE", "ref<ptr<int<4>>>")
		reference_pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ref<ptr<int<4>>,t,f>")
		const_reference_pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ref<ptr<int<4>,t,f>,f,f>")
		reference_const_pointer_int();

		#pragma test expect_ir("EXPR_TYPE", "ref<ptr<int<4>,t,f>,t,f>")
		const_reference_const_pointer_int();
	}

	#pragma test expect_ir(R"(
		decl IMP_recursiveVoid : (int<4>) -> unit;
		def IMP_recursiveVoid = function (v0 : ref<int<4>,f,f,plain>) -> unit {
			if(*v0<=0) {
				return unit;
			}
			IMP_recursiveVoid(*v0-1);
			return unit;
		};
		IMP_recursiveVoid(7)
	)")
	recursiveVoid(7);
}
