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

int main() {		

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = ref_var_init(0); for(int<4> v1 = 0 .. 10 : 1) { v1; }; }}")
	{
		for(int i = 0; i < 10; i++) {
			i;
		}
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = ref_var_init(2); for(int<4> v1 = 2 .. 5 : 5) { v1; }; }}")
	{
		for(int k = 2; k < 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = ref_var_init(2); for(int<4> v1 = 2 .. 5+1 : 5) { v1; }; }}")
	{
		for(int k = 2; k <= 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir(R"({{ var ref<uint<4>,f,f> v0 = ref_var_init(2u); for(uint<4> v1 = 2u .. 5u : 1u) { v1; }; } } )")
	{
		for(unsigned k = 2u; k < 5u; k++) k;
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = ref_var_init(2); for(int<4> v1 = 2 .. 5 : 1) { }; }}")
	{
		for(int k = 2; k < 5; k+=1);
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = ref_var_init(2); for(int<4> v1 = 2 .. 5 : 1) { }; }}")
	{
		for(int k = 2; k < 5; k+=1) { }
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f,plain> v0 =  ref_var_init(2); while( *v0<5) { if( *v0==3) { c_style_assignment(v0,*v0+1); continue; }; c_style_assignment(v0,*v0+1); } }}")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3){ continue; } }
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f,plain> v0 =  ref_var_init(2); while( *v0<5) { if( *v0==3) { break; }; c_style_assignment(v0,*v0+1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3) break; }
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f,plain> v0 =  ref_var_init(2); while( *v0<5) { if( *v0==3) { return 0; }; c_style_assignment(v0,*v0+1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3) return 0; }
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f,plain> v0 =  ref_var_init(2); while( *v0<5) { gen_post_inc(v0); c_style_assignment(v0,*v0+1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { k++; }
	}

	// check that we are doing nothing wrong here
	// (update once whileToFor is smarter!)	
	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = ref_var_init(2); while(*v0>5) { c_style_assignment(v0, *v0+1); }; }}")
	{
		for(int k = 2; k > 5; k+=1) { }
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f,plain> v0; { c_style_assignment(v0, 0); while( *v0<5) { gen_post_inc(v0); } } { var ref<int<4>,f,f,plain> v1 = ref_var_init(0); for( int<4> v2 = 0 .. 5 : 1) { } } var ref<int<4>,f,f,plain> v3 = ref_var_init(*v0); }")
	{
		int i;
		for(i=0; i<5; i++) { }
		for(int k = 0; k<5; k++) { }
		int z=i;
	}

	return 0;
}
