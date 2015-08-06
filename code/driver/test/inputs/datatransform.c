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

#include <stdlib.h>

struct Aos {
	int a;
	float b;
	double c;
};

int main() {
#pragma insieme data_transform "0"
	struct Aos* aos = (struct Aos*)malloc(sizeof(struct Aos) * 100);
	
	for(int i = 0; i < 100; ++i) {
		struct Aos init;
		init.a = 0;
		init.b = 1.0f;
		init.c = 2.0;
		
		aos[i] = init;
	}
	
	for(int i = 0; i < 100; ++i) {
		int a1;
		a1 = aos[i].a;
//		double c1 = aos[i].c; probably translated wrong by the frontend (compositeMemberAccess) and therefore not handled
		aos[i].b = (float)i;
	}
	
	struct Aos* aos1 = &(aos[15]);
	struct Aos* aos2 = aos + 3;
	free(aos);
}
