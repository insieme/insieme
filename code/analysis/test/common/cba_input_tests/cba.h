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
void cba_expect_undefined_int(int a) {};			// = is universe
void cba_expect_defined_int(int a) {};				// = is not empty and not universe
void cba_expect_single_int(int a) {};				// = is a single value
void cba_expect_eq_int(int a, int b) { assert(a==b); };
void cba_expect_ne_int(int a, int b) { assert(a!=b); };
void cba_expect_may_eq_int(int a, int b) {};

typedef struct {} _iset;
#define iset(...) (_iset*)(int[]){ __VA_ARGS__ }
void cba_expect_one_of_int(int a, _iset* b) {};


// pointer tests
void cba_expect_undefined_ptr(void* a) {};			      // = is universe
void cba_expect_defined_ptr(void* a) { assert(a); };	  // = is not empty and not universe
void cba_expect_single_ptr(void* a) {};	                  // = is a single target
void cba_expect_not_single_ptr(void* a) {};               // = is not a single target

void cba_expect_null_ptr(void* a)     { assert(!a); };				// = is null
void cba_expect_not_null_ptr(void* a) { assert(a); };			// = not null
void cba_expect_maybe_null_ptr(void* a) {};		// = maybe null

void cba_expect_extern_ptr(void* a) {};			// = is undefined
void cba_expect_not_extern_ptr(void* a) {};		// = not undefined
void cba_expect_maybe_extern_ptr(void* a) {};		// = maybe undefined


typedef struct {} _pset;
#define pset(...) (_pset*)(void*[]){ __VA_ARGS__ }
void cba_expect_one_of_ptr(int a, _pset* b) {};


// debugging
void cba_print_code() {};
void cba_print_int(int a) {};
void cba_dump_json() {};
void cba_dump_statistic() {};
void cba_dump_solution() {};

//void cba_dump_execution_net();
//void cba_dump_state_graph();
//void cba_dump_thread_regions();
//void cba_dump_sync_points();
//void cba_dump_thread_list();

//void cba_expect_num_threads(int);
//void cba_expect_execution_net_num_places(int);
//void cba_expect_execution_net_num_transitions(int);

// boolean tests (mapped to integer tests, since in C everything is an int)
//#define cba_expect_true(_c)             cba_expect_eq_int((_c!=0), 1)
//#define cba_expect_false(_c)            cba_expect_eq_int((_c==0), 1)
//#define cba_expect_may_be_true(_c)      cba_expect_may_eq_int((_c!=0), 1)
//#define cba_expect_may_be_false(_c)     cba_expect_may_eq_int((_c==0), 1)
