/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#define bool int
#define true 1
#define false 0

#define N 10

typedef struct _solution {
	struct _solution* head;
	int pos;
} solution;


bool contains(solution* solution, int pos) {
	if(!solution) { return false; }
	return solution->pos == pos || contains(solution->head, pos);
}


///////////////////////////////////////////////////////////////////
//
double solve_rec(solution* partial, int level) {
	// terminal case
	if(level == 0) { return 0.0; }

	// fix current position
	double res[N];
	solution tmps[N];

	for(int i = 0; i < N; i++) {
	#pragma omp task firstprivate(i)
		if(!contains(partial, i)) {
			//		tmps[i] = (solution){partial,i};
			res[i] = solve_rec(&tmps[i], level - 1);
		}
	}
	return 0.0;
}


int main() {
	solution* map;
	solve_rec(map, 10);
}
