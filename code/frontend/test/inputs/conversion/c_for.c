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


int g_bla = 10;

#define N 10
double g_volume[N][N][N][N];

void fooValue(int i) {}
void fooPointer(int* i) {}

int main() {

	int magic;

	#pragma test expect_ir("{{ for(int<4> v1 = 0 .. 10 : 1) { v1; }; }}")
	{
		for(int i = 0; i < 10; i++) {
			i;
		}
	}

	#pragma test expect_ir("{{ for(int<4> v1 = 2 .. 5 : 5) { v1; }; }}")
	{
		for(int k = 2; k < 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir("{{ for(int<4> v1 = 2 .. 5+1 : 5) { v1; }; }}")
	{
		for(int k = 2; k <= 5; k+=5) {
			k;
		}
	}

	#pragma test expect_ir("{{ for(int<4> v1 = 2 .. 5+1 : 0 - -5) { v1; }; }}")
	{
		for(int k = 2; k <= 5; k-=-5) {
			k;
		}
	}

	#pragma test expect_ir(R"({{ for(uint<4> v1 = 2u .. 5u : 1u) { v1; }; } } )")
	{
		for(unsigned k = 2u; k < 5u; k++) k;
	}

	#pragma test expect_ir("{{ for(int<4> v1 = 2 .. 5 : 1) { }; }}")
	{
		for(int k = 2; k < 5; k+=1);
	}

	#pragma test expect_ir("{{ for(int<4> v1 = 2 .. 5 : 1) { }; }}")
	{
		for(int k = 2; k < 5; k+=1) { }
	}

	// ensure that loop variable manipulation in loop is detected
	#pragma test expect_ir(R"({
		{
			var ref<int<4>, f, f, plain> v0 = 2;
			while(*v0<5) {
				gen_post_inc(v0);
				gen_post_inc(v0);
			}
		}
	})")
	{
		for(int k = 2; k < 5; k++) { k++; }
	}

	// check continue
	#pragma test expect_ir(R"({
		{
			for( int<4> v0 = 2 .. 5 : 1) {
				if(v0==3) {
					continue;
				}
			}
		}
	})")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3){ continue; } }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f,plain> v0 = 2; while( *v0<5) { if( *v0==3) { break; }; comp_assign_add(v0, 1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3) break; }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f,plain> v0 = 2; while( *v0<5) { if( *v0==3) { return 0 in ref<int<4>>; }; comp_assign_add(v0, 1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { if(k==3) return 0; }
	}

	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f,plain> v0 = 2; while( *v0<5) { gen_post_inc(v0); comp_assign_add(v0, 1); }; }; }")
	{
		for(int k = 2; k < 5; k+=1) { k++; }
	}

	// check that we are doing nothing wrong here
	// (update once whileToFor is smarter!)
	#pragma test expect_ir("using \"ext.compound_ops\"; {{ var ref<int<4>,f,f> v0 = 2; while(*v0>5) { comp_assign_add(v0, 1); }; }}")
	{
		for(int k = 2; k > 5; k+=1) { }
	}

	#pragma test expect_ir("{ var ref<int<4>,f,f,plain> v0; { v0 = 0; for( int<4> v1 = 0 .. 5 : 1) { }; v0 = 5+((0-5)%1+1)%1; } { for( int<4> v2 = 0 .. 5 : 1) { } } var ref<int<4>,f,f,plain> v3 = *v0; }")
	{
		int i;
		for(i=0; i<5; i++) { }

		for(int k = 0; k<5; k++) { }
		int z=i;
	}

	// should not be converted into for loop, because we do
	// not inspect the modification of the global literal yet.
	#pragma test expect_ir("{ { var ref<int<4>,f,f,plain> v0 = 0; while(*v0<*lit(\"g_bla\": ref<int<4>,f,f>)) { gen_post_dec(lit(\"g_bla\": ref<int<4>,f,f>)); gen_pre_inc(v0); }; }}")
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

	// passing the iterator as value is ok, passing it as pointer will cause the while loop not to be transformed to a for loop
	#pragma test expect_ir(R"(
		def IMP_fooValue = function (v0 : ref<int<4>,f,f,plain>) -> unit { };
		{
			{
				for( int<4> v0 = 0 .. 10 : 1) {
					IMP_fooValue(v0);
				}
			}
		}
	)")
	{
		for(int i = 0; i < 10; i++) {
			fooValue(i);
		}
	}

	#pragma test expect_ir(R"(
		def IMP_fooPointer = function (v0 : ref<ptr<int<4>>,f,f,plain>) -> unit { };
		{
			{
				var ref<int<4>,f,f,plain> v0 = 0;
				while(*v0<10) {
					IMP_fooPointer(ptr_from_ref(v0));
					gen_post_inc(v0);
				}
			}
		}
	)")
	{
		for(int i = 0; i < 10; i++) {
			fooPointer(&i);
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
	#pragma test expect_ir("using \"ext.compound_ops\"; {var ref<int<4>,f,f,plain> v0 = -10; { v0 = 5;  while(*v0<20) { var ref<int<4>,f,f,plain> v1 = *v0; comp_assign_multiply(v0, 2);};};}")
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
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { for( int<4> v2 = 5 .. 7 : 1) { var ref<int<4>,f,f,plain> v3 = v2; }; };}")
	{
		int i=8;
		for(int i=5;i<7;) {
			int j=i;
			i++;
		}
	}

	//for ( declaration; expression; expression ) statement
	#pragma test expect_ir("{var ref<int<4>,f,f,plain> v0 = 8; { for( int<4> v2 = 5 .. 7 : 1) { var ref<int<4>,f,f,plain> v3 = v2; }; };}")
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

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	// TODO: replace cvar, not deref(cvar) -- issues with binds expecting refs

	//#pragma test expect_ir(R"({
	//	var ref<int<4>,f,f,plain> v0 = 0;
	//	for( int<4> v1 = 0 .. 10 : 1) {
	//		ptr_subscript(ptr_from_array(ptr_subscript(ptr_from_array(ptr_subscript(ptr_from_array(ptr_subscript(ptr_from_array(lit("g_volume" : ref<array<array<array<array<real<8>,10>,10>,10>,10>,f,f,plain>)), 0)), v1)), 0)), 0) = v1==10/2?1.0E+0:0.0E+0;
	//	};
	//})")
	//for(int i=0; i<N; ++i) {
	//	g_volume[0][i][0][0] = (i == N/2) ? 1.0 : 0.0;
	//}

	//#pragma test expect_ir(R"({})")
	//for(int i=0; i<N; ++i) {
	//	g_volume[0][i][0][0] = (1 && i == N/2) ? 1.0 : 0.0;
	//}

	//#pragma test expect_ir(R"({})")
	//for(int i=0; i<N; ++i) {
	//	for(int j=0; j<N; ++j) {
	//		g_volume[0][i][j][0] = (i == N/2 && j == N/2) ? 1.0 : 0.0;
	//	}
	//}

	//#pragma test expect_ir(R"({})")
	//for(int i=0; i<N; ++i) {
	//	for(int j=0; j<N; ++j) {
	//		for(int k=0; k<N; ++k) {
	//			g_volume[0][i][j][k] = (i == N/2 && j == N/2 && k == N/2) ? 1.0 : 0.0;
	//		}
	//	}
	//}

	return 0;
}
