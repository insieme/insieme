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

#include <stdlib.h>

#include "cba.h"

int main(int argc, char** argv) {

	int* a = (int*)malloc(sizeof(int));
	int* b = (int*)malloc(sizeof(int));

	// --- initial state ---
	cba_expect_undefined_int(*a);
	cba_expect_undefined_int(*b);


	// --- some basic updates ---

	*a = 12;
	cba_expect_eq_int(*a,12);
	cba_expect_undefined_int(*b);

	*b = 14;
	cba_expect_eq_int(*a,12);
	cba_expect_eq_int(*b,14);

	*a = 16;
	cba_expect_eq_int(*a,16);
	cba_expect_eq_int(*b,14);


	// --- uncertain value ---
	*a = (argc > 2) ? 4 : 5;
	cba_expect_one_of_int(*a,iset(4,5));
	cba_expect_eq_int(*b,14);

	*a = 18;
	cba_expect_eq_int(*a,18);
	cba_expect_eq_int(*b,14);


	// --- uncertain target ---
	*((argc > 2) ? a : b) = 20;
	cba_expect_one_of_int(*a,iset(18,20));
	cba_expect_one_of_int(*b,iset(14,20));


	return 0;
}
