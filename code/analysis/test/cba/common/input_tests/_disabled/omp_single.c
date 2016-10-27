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

#include "../../input_tests/cba.h"

int main(int argc, char** argv) {

	int x = 1;

	cba_expect_eq_int(x, 1);

	// if a loop is known to be entered the original value should be eliminated
	for(int i=0; i<1; i++) {
		x = 2;
	}
	cba_expect_eq_int(x, 2);

	// if the loop body is not known to be processed the original value should be preserved
	for(int i=0; i<argc; i++) {
		x = 3;
	}

	cba_expect_may_eq_int(x, 2);
	cba_expect_may_eq_int(x, 3);

	// a omp single is a parallel loop with static range 0..1:1 => should also work
	#pragma omp parallel
	{
		#pragma omp single
		x = 4;
	}

	// x should be 3 now
	cba_expect_eq_int(x, 4);

//	cba_dump_equations();
//	cba_print_code();

	return 0;
}
