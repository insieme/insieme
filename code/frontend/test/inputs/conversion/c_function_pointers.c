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

int foo(int a) { return a; }

int main() {	

	// TYPES //////////////////////////////////////////////////////////////

	#pragma test expect_ir("var ref<ptr<(real<4>)->int<4>,t,f>,f,f> v0;")
	int(*ifFuncPtr)(float);

	#pragma test expect_ir("var ref<ptr<()->unit,t,f>,f,f> v0;")
	void(*vvFuncPtr)(void);

	// EXPRESSIONS //////////////////////////////////////////////////////////////
	
	#define FOO_FUN "alias foo_type = (int<4>) -> int<4>; def IMP_foo : (a: int<4>)->int<4> { return a; }; alias foo_ptr_type = ptr<foo_type,t,f>; "
	{ }

	#pragma test expect_ir(FOO_FUN "{ ptr_of_function(IMP_foo); 1; }")
	{
		&foo;
		1; // to make INSPIRE different from next case
	}
	
	#pragma test expect_ir(FOO_FUN "{ ptr_of_function(IMP_foo); 2; }")
	{
		foo;
		2; // to make INSPIRE different from previous case
	}
	
	#pragma test expect_ir(FOO_FUN "{ var ref<foo_ptr_type,f,f> v0; !ptr_ne(*v0, ptr_null(type_lit(foo_type), type_lit(t), type_lit(f))); }")
	{
		int (*ptr)(int);
		!ptr;
	}
	
	#pragma test expect_ir(FOO_FUN "{ var ref<foo_ptr_type,f,f> v0; ptr_deref(*v0)(5); }")
	{
		int (*ptr)(int);
		ptr(5);
	}
	
	#pragma test expect_ir(FOO_FUN "{ var ref<foo_ptr_type,f,f> v0; c_style_assignment(v0, ptr_of_function(IMP_foo)); }")
	{
		int (*ptr)(int);
		ptr = foo;
	}
	
	#pragma test expect_ir(FOO_FUN "{ var ref<foo_ptr_type,f,f> v0; c_style_assignment(v0, ptr_of_function(IMP_foo)); }")
	{
		int (*ptr)(int);
		ptr = &foo;
	}

	#pragma test expect_ir(FOO_FUN "{ ptr_deref(ptr_of_function(IMP_foo))(5); }")
	{
		(*(&foo))(5);
	}

	// INIT EXPRESSIONS ///////////////////////////////////////////////////////////
	
	#pragma test expect_ir(FOO_FUN "{ var ref<foo_ptr_type,f,f> v0 = ref_var_init(ptr_of_function(IMP_foo)); }")
	{
		int (*ptr)(int) = foo;
	}

	#pragma test expect_ir(FOO_FUN "{ var ref<foo_ptr_type,f,f> v0 = ref_var_init(ptr_of_function(IMP_foo)); }")
	{
		int (*ptr)(int) = &foo;
	}

	// CASTS //////////////////////////////////////////////////////////////

	#pragma test expect_ir("{ var ref<ptr<(real<4>)->int<4>,t,f>,f,f> v0; ptr_reinterpret(*v0, type_lit((int<4>)->real<4>)); }")
	{
		int(*ifFuncPtr)(float);
		(float(*)(int))ifFuncPtr;
	}
}
