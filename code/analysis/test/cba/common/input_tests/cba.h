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

/**
 * A header file forming the interface for the CBA test cases.
 */

#pragma once

#include <assert.h>

#define bool int
#define true (1)
#define false (0)


// alias tests
void cba_expect_is_alias(void* a, void* b)  { assert(a==b); };
void cba_expect_not_alias(void* a, void* b) { assert(a!=b); };
void cba_expect_may_alias(void* a, void* b) {};

// boolean analysis
void cba_expect_true(bool a)         { assert(a); };
void cba_expect_false(bool a)        { assert(!a); };
void cba_expect_may_be_true(bool a)  {};
void cba_expect_may_be_false(bool a) {};

// integer tests
void cba_expect_undefined_int(int a)     {};                  // = is universe
void cba_expect_defined_int(int a)       {};                  // = is not empty and not universe
void cba_expect_single_int(int a)        {};                  // = is a single value
void cba_expect_eq_int(int a, int b)     { assert(a==b); };
void cba_expect_ne_int(int a, int b)     { assert(a!=b); };
void cba_expect_may_eq_int(int a, int b) {};

typedef struct {} _iset;
#define iset_size(...) (sizeof((int[]){__VA_ARGS__})/sizeof(int))
#define iset(...) (_iset*)(int[]){ iset_size(__VA_ARGS__), __VA_ARGS__ }
void cba_expect_one_of_int(int a, _iset* b) {
	int* x = (int*)b;
	int l = *x;
	x++;
	for(int i=0; i<l; i++) {
		if(a == x[i]) return;
	}
	assert(false);
};


// pointer tests
void cba_expect_undefined_ptr(void* a) {};                // = is universe
void cba_expect_defined_ptr(void* a) { assert(a); };      // = is not empty and not universe
void cba_expect_single_ptr(void* a) {};                   // = is a single target
void cba_expect_not_single_ptr(void* a) {};               // = is not a single target

void cba_expect_null_ptr(void* a)     { assert(!a); };    // = is null
void cba_expect_not_null_ptr(void* a) { assert(a); };     // = not null
void cba_expect_maybe_null_ptr(void* a) {};               // = maybe null

void cba_expect_extern_ptr(void* a) {};                   // = is undefined
void cba_expect_not_extern_ptr(void* a) {};               // = not undefined
void cba_expect_maybe_extern_ptr(void* a) {};             // = maybe undefined


typedef struct {} _pset;
#define pset(...) (_pset*)(void*[]){ __VA_ARGS__ }
void cba_expect_one_of_ptr(int a, _pset* b) {};


#ifdef __cplusplus

	// symbolic value
	template<typename T>
	void cba_expect_symbolic_value(const char*, T);

#endif

// debugging
void cba_print_code() {};
void cba_print_int(int a) {};
void cba_dump_json() {};
void cba_dump_statistic() {};
void cba_dump_solution() {};

#define cba_debug() cba_print_code(); cba_dump_json(); cba_dump_solution();


