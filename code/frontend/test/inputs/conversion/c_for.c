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

int g_bla = 10;

int main() {

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = 0; for(int<4> v1 = 0 .. 10 : 1) { v1; }; }}")
	{
		for(int i = 0; i < 10; i++) {
			i;
		}
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = 2; for(int<4> v1 = 2 .. 5 : 5) { v1; }; }}")
	{
		for(int k = 2; k < 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = 2; for(int<4> v1 = 2 .. 5+1 : 5) { v1; }; }}")
	{
		for(int k = 2; k <= 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir(R"({{ var ref<uint<4>,f,f> v0 = 2u; for(uint<4> v1 = 2u .. 5u : 1u) { v1; }; } } )")
	{
		for(unsigned k = 2u; k < 5u; k++) k;
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = 2; for(int<4> v1 = 2 .. 5 : 1) { }; }}")
	{
		for(int k = 2; k < 5; k+=1);
	}

	#pragma test expect_ir("{{ var ref<int<4>,f,f> v0 = 2; for(int<4> v1 = 2 .. 5 : 1) { }; }}")
	{
		for(int k = 2; k < 5; k+=1) { }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f,plain> v0 =  2; while( *v0<5) { if( *v0==3) { *comp_assign_add(v0, 1); continue; }; *comp_assign_add(v0, 1); } }}")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3){ continue; } }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f,plain> v0 =  2; while( *v0<5) { if( *v0==3) { break; }; *comp_assign_add(v0, 1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3) break; }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f,plain> v0 =  2; while( *v0<5) { if( *v0==3) { return 0; }; *comp_assign_add(v0, 1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3) return 0; }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f,plain> v0 =  2; while( *v0<5) { gen_post_inc(v0); *comp_assign_add(v0, 1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { k++; }
	}

	// check that we are doing nothing wrong here
	// (update once whileToFor is smarter!)
	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f> v0 = 2; while(*v0>5) { *comp_assign_add(v0, 1); }; }}")
	{
		for(int k = 2; k > 5; k+=1) { }
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f,plain> v0; { v0 = 0; for( int<4> v1 = 0 .. 5 : 1) { }; v0 = 5+((0-5)%1+1)%1; } { var ref<int<4>,f,f,plain> v1 = 0; for( int<4> v2 = 0 .. 5 : 1) { } } var ref<int<4>,f,f,plain> v3 = *v0; }")
	{
		int i;
		for(i=0; i<5; i++) { }

		for(int k = 0; k<5; k++) { }
		int z=i;
	}

	// should not be converted into for loop, because we do
	// not inspect the modification of the global literal yet.
	#pragma test expect_ir("{{ var ref<int<4>, f, f, plain> v0 =  0; while(*v0<*lit(\"g_bla\": ref<int<4>,f,f>)) { gen_post_dec(lit(\"g_bla\": ref<int<4>,f,f>)); gen_pre_inc(v0); }; }}")
	{
		for (int i = 0; i < g_bla; ++i) {
			g_bla--;
		}
	}

	// should not be converted into for loop, because we do
	// not inspect the modification of the global literal yet.
	#pragma test expect_ir("{ var ref<int<4>,f,f,plain> v1 = 10; var ref<int<4>,f,f,plain> v2 = 0; { var ref<int<4>,f,f,plain> v3 = 0; while(*v3<*lit(\"g_bla\": ref<int<4>,f,f>)) { gen_post_inc(v2); gen_pre_inc(v3); } } }")
	{
		int l_bla1 = 10;
		int l_bla2 = 0;
		for (int i = 0; i < g_bla; ++i) {
			l_bla2++;
		}

	}

	/* FOLLOWING TEST SNIPPETS ARE CHECKING THE
	CORRECT CONVERSION OF VARIOUS (NEARLY ALL)
	POSSIBLE WAYS TO WRITE A FOR STMT */
	/******* FORMAL SYNTAX TEST *********/
	//this is how a for statement should look like according to the cppreference
	//attr(optional) for ( init-statement condition(optional) ; iteration_expression(optional) ) statement

	//for () statement
	#pragma test expect_ir("{{while(true) {break;};};}")
	{
		for(;;) { break; }
	}

	//for () statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 0; {while(true) { gen_post_inc(v0); if(int_eq(ref_deref(v0), 10)) {break;} else {};};};}")
	{
		int i=0;
		for(;;) {
			i++;
			if(i==10) break;
		}
	}

	//for ( init-statement; ; ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 0; { v0 = 8; while(true) { gen_post_inc(v0); if(int_eq(ref_deref(v0), 10)) {break;} else {};};};}")
	{
		int i=0;
		for(i=8;;) {
			i++;
			if(i==10) break;
		}
	}

	//for ( ; condition; ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 0; { while(*v0<10) { gen_post_inc(v0); };};}")
	{
		int i=0;
		for(;i<10;) {
			i++;
		}
	}

	//for ( ; ; iteration_expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 0; { while(true) { if(int_eq(ref_deref(v0), 10)) {break;} else {}; gen_pre_inc(v0); };};}")
	{
		int i=0;
		for(;;++i) {
			if(i==10) break;
		}
	}

	//for ( init-statement; ; iteration_expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 0; { v0 = 7; while(true) { if(int_eq(ref_deref(v0), 10)) {break;} else {}; gen_post_inc(v0); };};}")
	{
		int i=0;
		for(i=7;;i++) {
			if(i==10) break;
		}
	}

	//for ( init-statement; condition ; ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 0; { v0 = 7; for( int<4> v1 = 7 .. 10 : 1) { }; v0 = 10+((7-10)%1+1)%1;};}")
	{
		int i=0;
		for(i=7;i<10;) {
			i++;
		}
	}

	//for ( ; condition ; iteration_expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { while(*v0<10) { v0 = *v0; gen_pre_inc(v0);};};}")
	{
		int i=8;
		for(;i<10;++i) { i=i; }
	}

	//for ( init-statement; condition ; iteration_expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { v0 = 5; for( int<4> v1 = 5 .. 10 : 1) { var ref<int<4>,f,f,plain> v2 = v1;};v0 = 10+((5-10)%1+1)%1;};}")
	{
		int i=8;
		for(i=5;i<10;++i) { int j=i; }
	}

	//for ( init-statement; condition ; iteration_expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { v0 = 10; while(*v0>5) { var ref<int<4>,f,f,plain> v1 = *v0; gen_pre_dec(v0);};};}")
	{
		int i=8;
		for(i=10;i>5;--i) { int j=i; }
	}

	//for ( init-statement; condition ; iteration_expression ) statement
	#pragma test expect_ir("using \"ext.compound_ops\"; {var ref<int<4>,f,f,plain> v0 = -10; { v0 = 5;  while(*v0<20) { var ref<int<4>,f,f,plain> v1 = *v0; *comp_assign_multiply(v0, 2);};};}")
	{
		int i=-10;
		for(i=5;i<20;i*=2) { int j=i; }
	}

	/******* INFORMAL SYNTAX TEST *********/
	//attr(optional) for ( declaration-or-expression(optional) ; declaration-or-expression(optional) ; expression(optional) ) statement

	//for ( expression; expression; ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 0; { 5;  while(20!=0) { gen_post_inc(v0); if(int_eq(ref_deref(v0), 3)) {break;} else {}; };};}")
	{
		int i=0;
		for(5;20;) {
			i++;
			if(i==3) break;
		}
	}

	//for ( expression; expression; expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 0; { 5;  while(20!=0) { if(int_eq(ref_deref(v0), 3)) {break;} else {}; gen_pre_inc(v0); };};}")
	{
		int i=0;
		for(5;20;++i) {
			if(i==3) break;
		}
	}

	//for ( expression; declaration; ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { 5;  while(*ref_temp_init(*v0%10)!=0) { gen_post_inc(v0); var ref<int<4>,f,f,plain> v1 = *v0; };};}")
	{
		int i=8;
		for(5;(int){i%10};) {
			i++;
			int j=i;
		}
	}

	//for ( expression; declaration; expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { 5;  while(*ref_temp_init(*v0%10)!=0) { var ref<int<4>,f,f,plain> v1 = *v0; gen_post_inc(v0); };};}")
	{
		int i=8;
		for(5;(int){i%10};i++) {
			int j=i;
		}
	}

	//for ( declaration; expression; ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { var ref<int<4>,f,f,plain> v1 = 5; for( int<4> v2 = 5 .. 7 : 1) { var ref<int<4>,f,f,plain> v3 = v2; }; };}")
	{
		int i=8;
		for(int i=5;i<7;) {
			int j=i;
			i++;
		}
	}

	//for ( declaration; expression; expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { var ref<int<4>,f,f,plain> v1 = 5; for( int<4> v2 = 5 .. 7 : 1) { var ref<int<4>,f,f,plain> v3 = v2; }; };}")
	{
		int i=8;
		for(int i=5;i<7;++i) {
			int j=i;
		}
	}

	//for ( declaration; declaration; ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { var ref<int<4>,f,f,plain> v1 = 5; while(*ref_temp_init(*v1%10)!=0) { gen_post_inc(v1); var ref<int<4>,f,f,plain> v2 = *v1; }; };}")
	{
		int i=8;
		for(int i=5;(int){i%10};) {
			i++;
			int j=i;
		}
	}

	//for ( declaration; declaration; expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { var ref<int<4>,f,f,plain> v1 = 5; while(*ref_temp_init(*v1%10)!=0) { var ref<int<4>,f,f,plain> v2 = *v1; gen_pre_inc(v1); }; };}")
	{
		int i=8;
		for(int i=5;(int){i%10};++i) {
			int j=i;
		}
	}


	return 0;
}
