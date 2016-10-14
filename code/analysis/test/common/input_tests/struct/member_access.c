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

#include <stdlib.h>

#include "../../input_tests/cba.h"

typedef struct {
	int x;
	int y;
	int z;
} A;

int main(void) {
	A a = { 1, 2, 3 };

	cba_expect_eq_int(a.x, 1);
	cba_expect_eq_int(a.y, 2);
	cba_expect_eq_int(a.z, 3);

	A b = { 4, 5 };

	cba_expect_eq_int(b.x, 4);
	cba_expect_eq_int(b.y, 5);
	cba_expect_eq_int(b.z, 0);

	a.x = 15;
	b.z = 42;

	cba_expect_eq_int(a.x, 15);
	cba_expect_eq_int(a.y, 2);
	cba_expect_eq_int(a.z, 3);
	cba_expect_eq_int(b.x, 4);
	cba_expect_eq_int(b.y, 5);
	cba_expect_eq_int(b.z, 42);

	A c;

	cba_expect_ne_int(c.x, 0);
	cba_expect_ne_int(c.y, 0);
	cba_expect_ne_int(c.z, 0);

	A d;

	d = (A){1, 3, 4};

	cba_expect_eq_int(d.x, 1);
	cba_expect_eq_int(d.y, 3);
	cba_expect_eq_int(d.z, 4);

	//cba_dump_solution();
	//cba_dump_json();
	//cba_print_code();

	return 0;
}
