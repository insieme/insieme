/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

---

// start code fragment :: fundef_codefragment_printf //
int printf([[unhandled_simple_type: array<ref<char>,1>]], ...);

// start code fragment :: fundef_codefragment_timer_clear //
int timer_clear();

// start code fragment :: fundef_codefragment_randlc //
double randlc([[unhandled_simple_type: array<ref<real<8>>,1>]] X, [[unhandled_simple_type: array<ref<real<8>>,1>]] A){
	int KS = 0;
	double R23 = 0.0;
	double R46 = 0.0;
	double T23 = 0.0;
	double T46 = 0.0;
	double T1 = 0.0;
	double T2 = 0.0;
	double T3 = 0.0;
	double T4 = 0.0;
	double A1 = 0.0;
	double A2 = 0.0;
	double X1 = 0.0;
	double X2 = 0.0;
	double Z = 0.0;
	int i = 0;
	int j = 0;
	if((KS == 0)) {
		(R23 = 1.0);
		(R46 = 1.0);
		(T23 = 1.0);
		(T46 = 1.0);
		{
			for(int unnamed_var_132 = 1; unnamed_var_132 < 23; unnamed_var_132 += 1) {
				(R23 = (0.50 * R23));
				(T23 = (2.0 * T23));
			};
			(i = 23);
		};
		{
			for(int unnamed_var_133 = 1; unnamed_var_133 < 46; unnamed_var_133 += 1) {
				(R46 = (0.50 * R46));
				(T46 = (2.0 * T46));
			};
			(i = 46);
		};
		(KS = 1);
	} else ;
	(T1 = (R23 * A[0]));
	(j = ((int)(T1)));
	(A1 = ((double)(j)));
	(A2 = (A[0] - (T23 * A1)));
	(T1 = (R23 * X[0]));
	(j = ((int)(T1)));
	(X1 = ((double)(j)));
	(X2 = (X[0] - (T23 * X1)));
	(T1 = ((A1 * X2) + (A2 * X1)));
	(j = ((int)((R23 * T1))));
	(T2 = ((double)(j)));
	(Z = (T1 - (T23 * T2)));
	(T3 = ((T23 * Z) + (A2 * X2)));
	(j = ((int)((R46 * T3))));
	(T4 = ((double)(j)));
	(X[0] = (T3 - (T46 * T4)));
	return (R46 * X[0]);;
}

// start code fragment :: fundef_codefragment_randlc //
double randlc([[unhandled_simple_type: array<ref<real<8>>,1>]] X, [[unhandled_simple_type: array<ref<real<8>>,1>]] A){
	int KS = 0;
	double R23 = 0.0;
	double R46 = 0.0;
	double T23 = 0.0;
	double T46 = 0.0;
	double T1 = 0.0;
	double T2 = 0.0;
	double T3 = 0.0;
	double T4 = 0.0;
	double A1 = 0.0;
	double A2 = 0.0;
	double X1 = 0.0;
	double X2 = 0.0;
	double Z = 0.0;
	int i = 0;
	int j = 0;
	if((KS == 0)) {
		(R23 = 1.0);
		(R46 = 1.0);
		(T23 = 1.0);
		(T46 = 1.0);
		{
			for(int unnamed_var_134 = 1; unnamed_var_134 < 23; unnamed_var_134 += 1) {
				(R23 = (0.50 * R23));
				(T23 = (2.0 * T23));
			};
			(i = 23);
		};
		{
			for(int unnamed_var_135 = 1; unnamed_var_135 < 46; unnamed_var_135 += 1) {
				(R46 = (0.50 * R46));
				(T46 = (2.0 * T46));
			};
			(i = 46);
		};
		(KS = 1);
	} else ;
	(T1 = (R23 * A[0]));
	(j = ((int)(T1)));
	(A1 = ((double)(j)));
	(A2 = (A[0] - (T23 * A1)));
	(T1 = (R23 * X[0]));
	(j = ((int)(T1)));
	(X1 = ((double)(j)));
	(X2 = (X[0] - (T23 * X1)));
	(T1 = ((A1 * X2) + (A2 * X1)));
	(j = ((int)((R23 * T1))));
	(T2 = ((double)(j)));
	(Z = (T1 - (T23 * T2)));
	(T3 = ((T23 * Z) + (A2 * X2)));
	(j = ((int)((R46 * T3))));
	(T4 = ((double)(j)));
	(X[0] = (T3 - (T46 * T4)));
	return (R46 * X[0]);;
}

// start code fragment :: fundef_codefragment_randlc //
double randlc([[unhandled_simple_type: array<ref<real<8>>,1>]] X, [[unhandled_simple_type: array<ref<real<8>>,1>]] A){
	int KS = 0;
	double R23 = 0.0;
	double R46 = 0.0;
	double T23 = 0.0;
	double T46 = 0.0;
	double T1 = 0.0;
	double T2 = 0.0;
	double T3 = 0.0;
	double T4 = 0.0;
	double A1 = 0.0;
	double A2 = 0.0;
	double X1 = 0.0;
	double X2 = 0.0;
	double Z = 0.0;
	int i = 0;
	int j = 0;
	if((KS == 0)) {
		(R23 = 1.0);
		(R46 = 1.0);
		(T23 = 1.0);
		(T46 = 1.0);
		{
			for(int unnamed_var_136 = 1; unnamed_var_136 < 23; unnamed_var_136 += 1) {
				(R23 = (0.50 * R23));
				(T23 = (2.0 * T23));
			};
			(i = 23);
		};
		{
			for(int unnamed_var_137 = 1; unnamed_var_137 < 46; unnamed_var_137 += 1) {
				(R46 = (0.50 * R46));
				(T46 = (2.0 * T46));
			};
			(i = 46);
		};
		(KS = 1);
	} else ;
	(T1 = (R23 * A[0]));
	(j = ((int)(T1)));
	(A1 = ((double)(j)));
	(A2 = (A[0] - (T23 * A1)));
	(T1 = (R23 * X[0]));
	(j = ((int)(T1)));
	(X1 = ((double)(j)));
	(X2 = (X[0] - (T23 * X1)));
	(T1 = ((A1 * X2) + (A2 * X1)));
	(j = ((int)((R23 * T1))));
	(T2 = ((double)(j)));
	(Z = (T1 - (T23 * T2)));
	(T3 = ((T23 * Z) + (A2 * X2)));
	(j = ((int)((R46 * T3))));
	(T4 = ((double)(j)));
	(X[0] = (T3 - (T46 * T4)));
	return (R46 * X[0]);;
}

// start code fragment :: fundef_codefragment_randlc //
double randlc([[unhandled_simple_type: array<ref<real<8>>,1>]] X, [[unhandled_simple_type: array<ref<real<8>>,1>]] A){
	int KS = 0;
	double R23 = 0.0;
	double R46 = 0.0;
	double T23 = 0.0;
	double T46 = 0.0;
	double T1 = 0.0;
	double T2 = 0.0;
	double T3 = 0.0;
	double T4 = 0.0;
	double A1 = 0.0;
	double A2 = 0.0;
	double X1 = 0.0;
	double X2 = 0.0;
	double Z = 0.0;
	int i = 0;
	int j = 0;
	if((KS == 0)) {
		(R23 = 1.0);
		(R46 = 1.0);
		(T23 = 1.0);
		(T46 = 1.0);
		{
			for(int unnamed_var_138 = 1; unnamed_var_138 < 23; unnamed_var_138 += 1) {
				(R23 = (0.50 * R23));
				(T23 = (2.0 * T23));
			};
			(i = 23);
		};
		{
			for(int unnamed_var_139 = 1; unnamed_var_139 < 46; unnamed_var_139 += 1) {
				(R46 = (0.50 * R46));
				(T46 = (2.0 * T46));
			};
			(i = 46);
		};
		(KS = 1);
	} else ;
	(T1 = (R23 * A[0]));
	(j = ((int)(T1)));
	(A1 = ((double)(j)));
	(A2 = (A[0] - (T23 * A1)));
	(T1 = (R23 * X[0]));
	(j = ((int)(T1)));
	(X1 = ((double)(j)));
	(X2 = (X[0] - (T23 * X1)));
	(T1 = ((A1 * X2) + (A2 * X1)));
	(j = ((int)((R23 * T1))));
	(T2 = ((double)(j)));
	(Z = (T1 - (T23 * T2)));
	(T3 = ((T23 * Z) + (A2 * X2)));
	(j = ((int)((R46 * T3))));
	(T4 = ((double)(j)));
	(X[0] = (T3 - (T46 * T4)));
	return (R46 * X[0]);;
}

// start code fragment :: fundef_codefragment_create_seq //
void create_seq(double seed, double a){
	double x = 0.0;
	int i = 0;
	int j = 0;
	int k = 0;
	(k = ((1 << 11) / 4));
	{
		for(int unnamed_var_140 = 0; unnamed_var_140 < (1 << 16); unnamed_var_140 += 1) {
			(x = randlc(seed, a));
			(x = (x + randlc(seed, a)));
			(x = (x + randlc(seed, a)));
			(x = (x + randlc(seed, a)));
			(key_array[unnamed_var_140] = ((int)((((double)(k)) * x))));
		};
		(i = (1 << 16));
	};
}

// start code fragment :: fundef_codefragment_int.add //
int<a> int.add(int<a>, int<a>);

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_0 //
int __insieme_unnamed_fun_0(int*[65536] unnamed_var_145, int* unnamed_var_146, int*[2048] unnamed_var_147){
	int unnamed_var_144 = unnamed_var_147[unnamed_var_145[unnamed_var_146]];
	(unnamed_var_147[unnamed_var_145[unnamed_var_146]] = int.add(unnamed_var_147[unnamed_var_145[unnamed_var_146]], ((int)(1))));
	return unnamed_var_144;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_1 //
int __insieme_unnamed_fun_1(int* unnamed_var_153){
	int unnamed_var_152 = unnamed_var_153;
	(unnamed_var_153 = int.add(unnamed_var_153, ((int)(1))));
	return unnamed_var_152;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_2 //
int __insieme_unnamed_fun_2(int* unnamed_var_155){
	int unnamed_var_154 = unnamed_var_155;
	(unnamed_var_155 = int.add(unnamed_var_155, ((int)(1))));
	return unnamed_var_154;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_3 //
int __insieme_unnamed_fun_3(int* unnamed_var_157){
	int unnamed_var_156 = unnamed_var_157;
	(unnamed_var_157 = int.add(unnamed_var_157, ((int)(1))));
	return unnamed_var_156;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_4 //
int __insieme_unnamed_fun_4(int* unnamed_var_159){
	int unnamed_var_158 = unnamed_var_159;
	(unnamed_var_159 = int.add(unnamed_var_159, ((int)(1))));
	return unnamed_var_158;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_5 //
int __insieme_unnamed_fun_5(int* unnamed_var_161){
	int unnamed_var_160 = unnamed_var_161;
	(unnamed_var_161 = int.add(unnamed_var_161, ((int)(1))));
	return unnamed_var_160;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_6 //
int __insieme_unnamed_fun_6(int* unnamed_var_163){
	int unnamed_var_162 = unnamed_var_163;
	(unnamed_var_163 = int.add(unnamed_var_163, ((int)(1))));
	return unnamed_var_162;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_7 //
int __insieme_unnamed_fun_7(int* unnamed_var_165){
	int unnamed_var_164 = unnamed_var_165;
	(unnamed_var_165 = int.add(unnamed_var_165, ((int)(1))));
	return unnamed_var_164;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_8 //
int __insieme_unnamed_fun_8(int* unnamed_var_167){
	int unnamed_var_166 = unnamed_var_167;
	(unnamed_var_167 = int.add(unnamed_var_167, ((int)(1))));
	return unnamed_var_166;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_9 //
int __insieme_unnamed_fun_9(int* unnamed_var_169){
	int unnamed_var_168 = unnamed_var_169;
	(unnamed_var_169 = int.add(unnamed_var_169, ((int)(1))));
	return unnamed_var_168;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_10 //
int __insieme_unnamed_fun_10(int* unnamed_var_171){
	int unnamed_var_170 = unnamed_var_171;
	(unnamed_var_171 = int.add(unnamed_var_171, ((int)(1))));
	return unnamed_var_170;;
}

// start code fragment :: fundef_codefragment_rank //
void rank(int iteration){
	int i = 0;
	int j = 0;
	int k = 0;
	int l = 0;
	int m = 0;
	int shift = (11 - 9);
	int key = 0;
	int min_key_val = 0;
	int max_key_val = 0;
	int prv_buff1[2048];
	{
		(key_array[iteration] = iteration);
		(key_array[(iteration + 10)] = ((1 << 11) - iteration));
		{
			for(int unnamed_var_141 = 0; unnamed_var_141 < 5; unnamed_var_141 += 1) (partial_verify_vals[unnamed_var_141] = key_array[test_index_array[unnamed_var_141]]);
			(i = 5);
		};
		{
			for(int unnamed_var_142 = 0; unnamed_var_142 < (1 << 11); unnamed_var_142 += 1) (key_buff1[unnamed_var_142] = 0);
			(i = (1 << 11));
		};
	};
	{
		for(int unnamed_var_143 = 0; unnamed_var_143 < (1 << 11); unnamed_var_143 += 1) (prv_buff1[unnamed_var_143] = 0);
		(i = (1 << 11));
	};
	{
		for(int unnamed_var_148 = 0; unnamed_var_148 < (1 << 16); unnamed_var_148 += 1) {
			(key_buff2[unnamed_var_148] = key_array[unnamed_var_148]);
			__insieme_unnamed_fun_0(key_buff2, unnamed_var_148, prv_buff1);
		};
		(i = (1 << 16));
	};
	{
		for(int unnamed_var_149 = 0; unnamed_var_149 < ((1 << 11) - 1); unnamed_var_149 += 1) (prv_buff1[(unnamed_var_149 + 1)] = (prv_buff1[(unnamed_var_149 + 1)] + prv_buff1[unnamed_var_149]));
		(i = ((1 << 11) - 1));
	};
	{
		{
			for(int unnamed_var_150 = 0; unnamed_var_150 < (1 << 11); unnamed_var_150 += 1) (key_buff1[unnamed_var_150] = (key_buff1[unnamed_var_150] + prv_buff1[unnamed_var_150]));
			(i = (1 << 11));
		};
	};
	{
		{
			for(int unnamed_var_172 = 0; unnamed_var_172 < 5; unnamed_var_172 += 1) {
				(k = partial_verify_vals[unnamed_var_172]);
				if(((0 <= k) && (k <= ((1 << 16) - 1)))) {
					int<a> unnamed_var_151 = ((int<a>)('S'));
					break;
					break;
					break;
					break;
					break;
					switch(unnamed_var_151) {
					'C':
						if((unnamed_var_172 <= 2)) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] + iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_1(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] - iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_2(passed_verification);
						}break;
					'B':
						if((((unnamed_var_172 == 1) || (unnamed_var_172 == 2)) || (unnamed_var_172 == 4))) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] + iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_3(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] - iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_4(passed_verification);
						}break;
					'A':
						if((unnamed_var_172 <= 2)) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] + (iteration - 1)))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_5(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] - (iteration - 1)))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_6(passed_verification);
						}break;
					'W':
						if((unnamed_var_172 < 2)) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] + (iteration - 2)))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_7(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] - iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_8(passed_verification);
						}break;
					'S':
						if((unnamed_var_172 <= 2)) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] + iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_9(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_172] - iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_172);
							} else __insieme_unnamed_fun_10(passed_verification);
						}break;
					};
				} else ;
			};
			(i = 5);
		};
		if((iteration == 10)) (key_buff_ptr_global = (([[unhandled_simple_type: array<ref<int<4>>,1>]])(key_buff1))) else ;
	};
}

// start code fragment :: fundef_codefragment_timer_start //
int timer_start();

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_11 //
int __insieme_unnamed_fun_11(int*[65536] unnamed_var_177, int* unnamed_var_178, int*[2048] unnamed_var_179){
	int unnamed_var_176 = unnamed_var_179[unnamed_var_177[unnamed_var_178]];
	(unnamed_var_179[unnamed_var_177[unnamed_var_178]] = int.add(unnamed_var_179[unnamed_var_177[unnamed_var_178]], ((int)(1))));
	return unnamed_var_176;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_12 //
int __insieme_unnamed_fun_12(int* unnamed_var_185){
	int unnamed_var_184 = unnamed_var_185;
	(unnamed_var_185 = int.add(unnamed_var_185, ((int)(1))));
	return unnamed_var_184;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_13 //
int __insieme_unnamed_fun_13(int* unnamed_var_187){
	int unnamed_var_186 = unnamed_var_187;
	(unnamed_var_187 = int.add(unnamed_var_187, ((int)(1))));
	return unnamed_var_186;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_14 //
int __insieme_unnamed_fun_14(int* unnamed_var_189){
	int unnamed_var_188 = unnamed_var_189;
	(unnamed_var_189 = int.add(unnamed_var_189, ((int)(1))));
	return unnamed_var_188;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_15 //
int __insieme_unnamed_fun_15(int* unnamed_var_191){
	int unnamed_var_190 = unnamed_var_191;
	(unnamed_var_191 = int.add(unnamed_var_191, ((int)(1))));
	return unnamed_var_190;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_16 //
int __insieme_unnamed_fun_16(int* unnamed_var_193){
	int unnamed_var_192 = unnamed_var_193;
	(unnamed_var_193 = int.add(unnamed_var_193, ((int)(1))));
	return unnamed_var_192;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_17 //
int __insieme_unnamed_fun_17(int* unnamed_var_195){
	int unnamed_var_194 = unnamed_var_195;
	(unnamed_var_195 = int.add(unnamed_var_195, ((int)(1))));
	return unnamed_var_194;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_18 //
int __insieme_unnamed_fun_18(int* unnamed_var_197){
	int unnamed_var_196 = unnamed_var_197;
	(unnamed_var_197 = int.add(unnamed_var_197, ((int)(1))));
	return unnamed_var_196;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_19 //
int __insieme_unnamed_fun_19(int* unnamed_var_199){
	int unnamed_var_198 = unnamed_var_199;
	(unnamed_var_199 = int.add(unnamed_var_199, ((int)(1))));
	return unnamed_var_198;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_20 //
int __insieme_unnamed_fun_20(int* unnamed_var_201){
	int unnamed_var_200 = unnamed_var_201;
	(unnamed_var_201 = int.add(unnamed_var_201, ((int)(1))));
	return unnamed_var_200;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_21 //
int __insieme_unnamed_fun_21(int* unnamed_var_203){
	int unnamed_var_202 = unnamed_var_203;
	(unnamed_var_203 = int.add(unnamed_var_203, ((int)(1))));
	return unnamed_var_202;;
}

// start code fragment :: fundef_codefragment_rank //
void rank(int iteration){
	int i = 0;
	int j = 0;
	int k = 0;
	int l = 0;
	int m = 0;
	int shift = (11 - 9);
	int key = 0;
	int min_key_val = 0;
	int max_key_val = 0;
	int prv_buff1[2048];
	{
		(key_array[iteration] = iteration);
		(key_array[(iteration + 10)] = ((1 << 11) - iteration));
		{
			for(int unnamed_var_173 = 0; unnamed_var_173 < 5; unnamed_var_173 += 1) (partial_verify_vals[unnamed_var_173] = key_array[test_index_array[unnamed_var_173]]);
			(i = 5);
		};
		{
			for(int unnamed_var_174 = 0; unnamed_var_174 < (1 << 11); unnamed_var_174 += 1) (key_buff1[unnamed_var_174] = 0);
			(i = (1 << 11));
		};
	};
	{
		for(int unnamed_var_175 = 0; unnamed_var_175 < (1 << 11); unnamed_var_175 += 1) (prv_buff1[unnamed_var_175] = 0);
		(i = (1 << 11));
	};
	{
		for(int unnamed_var_180 = 0; unnamed_var_180 < (1 << 16); unnamed_var_180 += 1) {
			(key_buff2[unnamed_var_180] = key_array[unnamed_var_180]);
			__insieme_unnamed_fun_11(key_buff2, unnamed_var_180, prv_buff1);
		};
		(i = (1 << 16));
	};
	{
		for(int unnamed_var_181 = 0; unnamed_var_181 < ((1 << 11) - 1); unnamed_var_181 += 1) (prv_buff1[(unnamed_var_181 + 1)] = (prv_buff1[(unnamed_var_181 + 1)] + prv_buff1[unnamed_var_181]));
		(i = ((1 << 11) - 1));
	};
	{
		{
			for(int unnamed_var_182 = 0; unnamed_var_182 < (1 << 11); unnamed_var_182 += 1) (key_buff1[unnamed_var_182] = (key_buff1[unnamed_var_182] + prv_buff1[unnamed_var_182]));
			(i = (1 << 11));
		};
	};
	{
		{
			for(int unnamed_var_204 = 0; unnamed_var_204 < 5; unnamed_var_204 += 1) {
				(k = partial_verify_vals[unnamed_var_204]);
				if(((0 <= k) && (k <= ((1 << 16) - 1)))) {
					int<a> unnamed_var_183 = ((int<a>)('S'));
					break;
					break;
					break;
					break;
					break;
					switch(unnamed_var_183) {
					'C':
						if((unnamed_var_204 <= 2)) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] + iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_12(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] - iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_13(passed_verification);
						}break;
					'B':
						if((((unnamed_var_204 == 1) || (unnamed_var_204 == 2)) || (unnamed_var_204 == 4))) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] + iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_14(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] - iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_15(passed_verification);
						}break;
					'A':
						if((unnamed_var_204 <= 2)) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] + (iteration - 1)))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_16(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] - (iteration - 1)))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_17(passed_verification);
						}break;
					'W':
						if((unnamed_var_204 < 2)) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] + (iteration - 2)))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_18(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] - iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_19(passed_verification);
						}break;
					'S':
						if((unnamed_var_204 <= 2)) {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] + iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_20(passed_verification);
						} else {
							if((key_buff1[(k - 1)] != (test_rank_array[unnamed_var_204] - iteration))) {
								printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Failed partial verification: ")))), iteration, unnamed_var_204);
							} else __insieme_unnamed_fun_21(passed_verification);
						}break;
					};
				} else ;
			};
			(i = 5);
		};
		if((iteration == 10)) (key_buff_ptr_global = (([[unhandled_simple_type: array<ref<int<4>>,1>]])(key_buff1))) else ;
	};
}

// start code fragment :: fundef_codefragment_timer_stop //
int timer_stop();

// start code fragment :: fundef_codefragment_timer_read //
int timer_read();

// start code fragment :: fundef_codefragment_int.sub //
int<a> int.sub(int<a>, int<a>);

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_22 //
int __insieme_unnamed_fun_22(int* unnamed_var_206, int*[65536] unnamed_var_207, [[unhandled_simple_type: array<ref<int<4>>,1>]]* unnamed_var_208){
	(unnamed_var_208[unnamed_var_207[unnamed_var_206]] = int.sub(unnamed_var_208[unnamed_var_207[unnamed_var_206]], ((int)(1))));
	unnamed_var_208[unnamed_var_207[unnamed_var_206]];
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_23 //
int __insieme_unnamed_fun_23(int* unnamed_var_211){
	int unnamed_var_210 = unnamed_var_211;
	(unnamed_var_211 = int.add(unnamed_var_211, ((int)(1))));
	return unnamed_var_210;;
}

// start code fragment :: fundef_codefragment___insieme_unnamed_fun_24 //
int __insieme_unnamed_fun_24(int* unnamed_var_214){
	int unnamed_var_213 = unnamed_var_214;
	(unnamed_var_214 = int.add(unnamed_var_214, ((int)(1))));
	return unnamed_var_213;;
}

// start code fragment :: fundef_codefragment_full_verify //
void full_verify(){
	int i = 0;
	int j = 0;
	int k = 0;
	int m = 0;
	int unique_keys = 0;
	{
		for(int unnamed_var_209 = 0; unnamed_var_209 < (1 << 16); unnamed_var_209 += 1) (key_array[__insieme_unnamed_fun_22(unnamed_var_209, key_buff2, key_buff_ptr_global)] = key_buff2[unnamed_var_209]);
		(i = (1 << 16));
	};
	(j = 0);
	{
		for(int unnamed_var_212 = 1; unnamed_var_212 < (1 << 16); unnamed_var_212 += 1) if((key_array[(unnamed_var_212 - 1)] > key_array[unnamed_var_212])) __insieme_unnamed_fun_23(j) else ;
		(i = (1 << 16));
	};
	if((j != 0)) {
		printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("Full_verify: number of keys out of sort: %d\n")))), j);
	} else __insieme_unnamed_fun_24(passed_verification);
}

// start code fragment :: fundef_codefragment_c_print_results //
int c_print_results();

// start code fragment :: fundef_codefragment_main //
int main(int argc, [[unhandled_simple_type: array<ref<array<ref<char>,1>>,1>]] argv){
	int i = 0;
	int iteration = 0;
	int itemp = 0;
	int nthreads = 1;
	double timecounter = 0.0;
	double maxtime = 0.0;
	{
		for(int unnamed_var_131 = 0; unnamed_var_131 < 5; unnamed_var_131 += 1) {
			int<a> unnamed_var_120 = ((int<a>)('S'));
			(test_rank_array[unnamed_var_131] = S_test_rank_array[unnamed_var_131]);
			break;
			(test_rank_array[unnamed_var_131] = A_test_rank_array[unnamed_var_131]);
			break;
			(test_rank_array[unnamed_var_131] = W_test_rank_array[unnamed_var_131]);
			break;
			(test_rank_array[unnamed_var_131] = B_test_rank_array[unnamed_var_131]);
			break;
			(test_rank_array[unnamed_var_131] = C_test_rank_array[unnamed_var_131]);
			break;
			switch(unnamed_var_120) {
			'C':
				(test_index_array[unnamed_var_131] = C_test_index_array[unnamed_var_131])break;
			'B':
				(test_index_array[unnamed_var_131] = B_test_index_array[unnamed_var_131])break;
			'W':
				(test_index_array[unnamed_var_131] = W_test_index_array[unnamed_var_131])break;
			'A':
				(test_index_array[unnamed_var_131] = A_test_index_array[unnamed_var_131])break;
			'S':
				(test_index_array[unnamed_var_131] = S_test_index_array[unnamed_var_131])break;
			};
		};
		(i = 5);
	};
	;
	printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("\n\n NAS Parallel Benchmarks 2.3 OpenMP C version")))), );
	printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])(" Size:  %d  (class %c)\n")))), (1 << 16), 'S');
	printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])(" Iterations:   %d\n")))), 10);
	timer_clear(0);
	create_seq(314159265.00, 1220703125.00);
	rank(1);
	(passed_verification = 0);
	if(('S' != 'S')) printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("\n   iteration\n")))), ) else ;
	timer_start(0);
	{
		for(int unnamed_var_205 = 1; unnamed_var_205 < 10; unnamed_var_205 += 1) {
			if(('S' != 'S')) printf((([[unhandled_simple_type: array<ref<char>,1>]])((([[unhandled_simple_type: array<ref<char>,1>]])("        %d\n")))), unnamed_var_205) else ;
			rank(unnamed_var_205);
		};
		(iteration = 10);
	};
	timer_stop(0);
	(timecounter = ((double)(timer_read(0))));
	full_verify();
	if((passed_verification != ((5 * 10) + 1))) (passed_verification = 0) else ;
	c_print_results((([[unhandled_simple_type: array<ref<char>,1>]])("IS")), 'S', (1 << 16), 0, 0, 10, nthreads, timecounter, ((((double)((10 * (1 << 16)))) / timecounter) / 1000000.), (([[unhandled_simple_type: array<ref<char>,1>]])("keys ranked")), passed_verification, (([[unhandled_simple_type: array<ref<char>,1>]])("2.3")), (([[unhandled_simple_type: array<ref<char>,1>]])("03 Nov 2010")), (([[unhandled_simple_type: array<ref<char>,1>]])("cc")), (([[unhandled_simple_type: array<ref<char>,1>]])("cc")), (([[unhandled_simple_type: array<ref<char>,1>]])("-lm")), (([[unhandled_simple_type: array<ref<char>,1>]])("-I../common")), (([[unhandled_simple_type: array<ref<char>,1>]])("-O3 ")), (([[unhandled_simple_type: array<ref<char>,1>]])("-lm")), (([[unhandled_simple_type: array<ref<char>,1>]])("randlc")));
}

// start code fragment :: unnamed //
main