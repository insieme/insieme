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


#pragma insieme mark
int simpleRegion() {

	int a = 0, n = 0;

	/*We need target clause because region directive without clauses is ignored*/
	#pragma omp region target(accelerator)
	{
		int x = 3;
	}

	return 0;
}

#pragma insieme mark
int objective() {

	int a = 0, n = 0;

	#pragma omp region objective(0.1*E+0.3*P+0.6*T+0*Q)
	{
		int x = 3;
	}

	#pragma omp task objective(:E<10)
	{
		int x = 4;
	}

	#pragma omp parallel objective(0.1*E+0.2*P+0.7*T+0*Q:T<3;P>22)
	{
		int x = 5;
	}

	return 0;
}

#pragma insieme mark
int param() {

	int a = 0, n = 0;
    int A[3];

	#pragma omp region param(a, range(0:10:2)) 
	{
		int x = a;
	}

    #pragma omp parallel param(a, enum(A:3)) 
	{
		int x = a;
	}

	return 0;
}

#pragma insieme mark
int target() {

	int a = 0, n = 0;
    int A[3];

	#pragma omp region target(accelerator) 
	{
		int x = a;
	}

    #pragma omp parallel target(accelerator: 0,1 ... 8: 12)
	{
		int x = a;
	}

	return 0;
}

#pragma insieme mark
int firstLocal() {

	int a = 5;

	#pragma omp region firstlocal(a)
	{
		int x = a + 3;
	}

	return 0;
}
