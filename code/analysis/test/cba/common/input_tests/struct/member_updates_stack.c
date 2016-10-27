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

#include "../../input_tests/cba.h"

typedef struct {
	int x;
	int y;
	int z;
} A;


int main(int argc, char** argv) {

	A a;
	cba_expect_undefined_int(a.x);
	cba_expect_undefined_int(a.y);
	cba_expect_undefined_int(a.z);

	a.x = 1;
	cba_expect_eq_int(a.x,1);
	cba_expect_undefined_int(a.y);
	cba_expect_undefined_int(a.z);

	a.y = 2;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_undefined_int(a.z);

	a.z = 3;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_eq_int(a.z,3);

	// assign an uncertain value
	a.z = (argc > 2) ? 4 : 5;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_one_of_int(a.z, iset(4,5));


	// reset to something known
	a.z = 6;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_eq_int(a.z,6);

	// update an unknown location
	if (argc > 2) {
		a.x = 7;
	} else {
		a.y = 8;
	}

	cba_expect_one_of_int(a.x, iset(1,7));
	cba_expect_one_of_int(a.y, iset(2,8));
	cba_expect_eq_int(a.z,6);


	// reset the struct
	a.x = 1;
	a.y = 2;
	a.z = 3;
	cba_expect_eq_int(a.x,1);
	cba_expect_eq_int(a.y,2);
	cba_expect_eq_int(a.z,3);


	// add to the control flow
	if (argc > 2) {
		a.x = 9;
	} else {
		a.y = 9;
	}
	cba_expect_one_of_int(a.x, iset(1,9));
	cba_expect_one_of_int(a.y, iset(2,9));
	cba_expect_eq_int(a.z,3);

	// TODO: known bug - can not create and eliminate a pointer to a field
//	// add uncertainty into assignment
//	*((argc > 2) ? &a.x : &a.y) = 9;
//	cba_expect_one_of_int(a.x, iset(1,9));
//	cba_expect_one_of_int(a.y, iset(1,9));
//	cba_expect_eq_int(a.z,3);

//	cba_dump_solution();
//	cba_print_code();
//	cba_print_code();

	return 0;
}
