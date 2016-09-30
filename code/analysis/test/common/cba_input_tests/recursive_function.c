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
 * A simple test case covering some arithmetic.
 */

#include "cba.h"


int dummy(int x) {
	if (x <= 2) return 1;
	return dummy(x-1);
}


int fib(int x) {
	if (x <= 1) return 1;
	return fib(x-1) + fib(x-2);
}

int odd();

int even(int x) {
	if (x == 0) return 1;
	return odd(x-1);
}

int odd(int x) {
	if (x == 0) return 0;
	return even(x-1);
}

int main(int argc, char** argv) {

//	cba_print_code();
//	cba_dump_json();

	cba_expect_eq_int(1,dummy(-1));
	cba_expect_eq_int(1,dummy(0));
	cba_expect_eq_int(1,dummy(1));
	cba_expect_eq_int(1,dummy(3));

	cba_expect_undefined_int(fib(0));
	cba_expect_undefined_int(fib(1));
	cba_expect_undefined_int(fib(2));

	cba_expect_defined_int(even(0));
	cba_expect_defined_int(odd(1));

	return 0;
}
