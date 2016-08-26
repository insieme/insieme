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

	//===-------------------------------------------------------------------------------------------------------------------------------- NULL TO POINTER ---===

	#pragma test expect_ir("ptr_null(type_lit(int<4>), type_lit(f), type_lit(f))")
	(int*)0;

	#pragma test expect_ir("ptr_null(type_lit(real<4>), type_lit(t), type_lit(f))")
	(const float*)0;

	#pragma test expect_ir("ptr_null(type_lit(real<4>), type_lit(f), type_lit(f))")
	(float* const)0;

	#pragma test expect_ir("ptr_null(type_lit(int<4>), type_lit(t), type_lit(f))")
	(const int* const)0;

	//===---------------------------------------------------------------------------------------------------------------------------------- NUMERIC CASTS ---===

	// IntegralToFloating
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<real<4>,f,f> v1 = num_cast(*v0,type_lit(real<4>)); }")
	{
		int x;
		float y = x;
	}
	
	// FloatingToIntegral
	#pragma test expect_ir("{ var ref<real<4>,f,f> v0; var ref<int<4>,f,f> v1 = num_cast(*v0,type_lit(int<4>)); }")
	{
		float x;
		int y = x;
	}
	
	// FloatingCast
	#pragma test expect_ir("{ var ref<real<4>,f,f> v0; var ref<real<8>,f,f> v1 = num_cast(*v0,type_lit(real<8>)); }")
	{
		float x;
		double y = x;
	}

	// IntegralCast
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; var ref<uint<4>,f,f> v1 = num_cast(*v0,type_lit(uint<4>)); }")
	{
		int x;
		unsigned y = x;
	}
	#pragma test expect_ir("{ var ref<uint<8>,f,f> v0; var ref<int<4>,f,f> v1 = num_cast(*v0,type_lit(int<4>)); }")
	{
		unsigned long x;
		int y = x;
	}
	#pragma test expect_ir("{ var ref<char,f,f> v0; var ref<int<1>,f,f> v1 = num_cast(*v0,type_lit(int<1>)); }")
	{
		char x;
		signed char y = x;
	}
	
	//===---------------------------------------------------------------------------------------------------------------------------------- POINTER CASTS ---===
	
	// implicit type change
	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; var ref<ptr<int<4>,f,f>,f,f> v1 = ptr_reinterpret(*v0, type_lit(int<4>)); }")
	{
		void* x;
		int* y = x;
	}
	
	// explicit type change
	#pragma test expect_ir("{ var ref<ptr<unit,f,f>,f,f> v0; var ref<ptr<int<4>,f,f>,f,f> v1 = ptr_reinterpret(*v0, type_lit(int<4>)); }")
	{
		void* x;
		int* y = (int*)x;
	}

	// implicit qualifier change
	#pragma test expect_ir("{ var ref<ptr<unit,t,f>,f,f> v0; var ref<ptr<unit,f,f>,f,f> v1 = ptr_cast(*v0, type_lit(f), type_lit(f)); }")
	{
		const void* x;
		void* y = x;
	}
	
	// explicit qualifier change
	#pragma test expect_ir("{ var ref<ptr<unit,t,f>,f,f> v0; var ref<ptr<unit,f,f>,f,f> v1 = ptr_cast(*v0, type_lit(f), type_lit(f)); }")
	{
		const void* x;
		void* y = (void*)x;
	}
	
	// implicit type + qualifier change
	#pragma test expect_ir("{ var ref<ptr<unit,t,f>,f,f> v0; var ref<ptr<int<4>,f,f>,f,f> v1 = ptr_cast(ptr_reinterpret(*v0, type_lit(int<4>)), type_lit(f), type_lit(f)); }")
	{
		const void* x;
		int* y = x;
	}
	
	// explicit type + qualifier change
	#pragma test expect_ir("{ var ref<ptr<unit,t,f>,f,f> v0; var ref<ptr<int<4>,f,f>,f,f> v1 = ptr_cast(ptr_reinterpret(*v0, type_lit(int<4>)), type_lit(f), type_lit(f)); }")
	{
		const void* x;
		int* y = (int*)x;
	}
	
	// implicit int to pointer
	#pragma test expect_ir("var ref<ptr<unit,f,f>,f,f> v0 = ptr_from_integral(5, type_lit(ptr<unit,f,f>));")
	void* a = 5;

	// explicit int to pointer
	#pragma test expect_ir("var ref<ptr<unit,f,f>,f,f> v0 = ptr_from_integral(5, type_lit(ptr<unit,f,f>));")
	void* a2 = (void*)5;
	
	// implicit pointer to int
	#pragma test expect_ir("var ref<int<4>,f,f> v0 = ptr_to_integral(ptr_from_integral(5, type_lit(ptr<unit,f,f>)), type_lit(int<4>));")
	int ifromp = (void*)5;

	// explicit pointer to int
	#pragma test expect_ir("var ref<int<4>,f,f> v0 = ptr_to_integral(ptr_from_integral(5, type_lit(ptr<unit,f,f>)), type_lit(int<4>));")
	int ifromp2 = (int)(void*)5;
	
	// implicit int to volatile pointer
	#pragma test expect_ir("var ref<ptr<unit,f,t>,f,f> v0 = ptr_from_integral(5, type_lit(ptr<unit,f,t>));")
	volatile void* vpointerfromint = 5;
	
	// implicit uint to pointer
	#pragma test expect_ir("var ref<ptr<unit,f,f>,f,f> v0 = ptr_from_integral(5u, type_lit(ptr<unit,f,f>));")
	void* pointerfromuint = 5u;
	
	// implicit pointer to uint
	#pragma test expect_ir("var ref<uint<4>,f,f> v0 = ptr_to_integral(ptr_from_integral(5, type_lit(ptr<unit,f,f>)), type_lit(uint<4>));")
	unsigned uintfrompointer = (void*)5;

	//===------------------------------------------------------------------------------------------------------------------------------------- MISC CASTS ---===

	// NoOp - casting between identical types
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; v0; }")
	{
		int x;
		(int)x;
	}

	// void - ignores the return value
	#pragma test expect_ir("unit_consume(1)")
	(void)1;

	//===----------------------------------------------------------------------------------------------------------------------------------- TO BOOL CASTS---===

	// char to bool (C99's native boolean type is _Bool)
	#pragma test expect_ir(R"({ var ref<char,f,f> v0; *v0!=lit("'\0'":char); })")
	{
		char x;
		(_Bool)x;
	}

	// integral to bool
	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; *v0!=0; }")
	{
		int x;
		(_Bool)x;
	}

	// integral to bool
	#pragma test expect_ir("{ var ref<uint<8>,f,f> v0; *v0!=0ul; }")
	{
		unsigned long x;
		(_Bool)x;
	}

	// float to bool
	#pragma test expect_ir("{ var ref<real<4>,f,f> v0; *v0!=0.0f; }")
	{
		float x;
		(_Bool)x;
	}

	// pointer to bool
	#pragma test expect_ir("{ var ref<ptr<real<4>,f,f>,f,f> v0; ptr_ne(*v0, ptr_null(type_lit(real<4>), type_lit(f), type_lit(f))); }")
	{
		float* x;
		(_Bool)x;
	}

}
