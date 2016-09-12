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

int min(int a, int b) {
	if (a < b) return a;
	return b;
}

int max(int a, int b) {
	return (a == min(a,b)) ? b : a;
}

int main(int argc, char** argv) {

	// let's start with something simple
	cba_expect_eq_int(1,1);

	// a little more challenging
	int a = 10;
	int b = 12;
	cba_expect_eq_int(a,a);
	cba_expect_eq_int(b,b);
	cba_expect_ne_int(a,b);
	cba_expect_eq_int(a+2, b);

	// with some unknown value
	cba_expect_ne_int(a+argc,b+argc);
	cba_expect_eq_int(a+2+argc, b+argc);

	// including function calls
	cba_expect_eq_int(a,min(a,b));
	cba_expect_eq_int(b,max(a,b));

	// simple integer division (modulo always 0)
	cba_expect_eq_int(max(a,b)/2, 6);
	cba_expect_eq_int(min(a,b)%2, 0);

	// even more tricky
	cba_expect_eq_int(a+argc,min(a+argc,b+argc));

	// and after an update
	a = 14;
	cba_expect_eq_int(b+argc,min(a+argc,b+argc));

	return 0;
}
