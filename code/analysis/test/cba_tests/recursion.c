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

int fib(int x) {
	if (x == 0) return 0;
	if (x == 1) return 1;

	int a = spawn fib(x-1);
	int b = spawn fib(x-2);

	sync;
	return a + b;
}


int main(int argc, char** argv) {

	fib(1);
	fib(2);
	fib(3);
	fib(4);

	// check the execution net
//	cba_print_code();
//	cba_dump_thread_regions();
	cba_expect_execution_net_num_places(90);
//	cba_dump_execution_net();
//	cba_dump_thread_list();
//	cba_dump_sync_points();
//	cba_dump_equations();

	return 0;
}
