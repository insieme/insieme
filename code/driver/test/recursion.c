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

//typedef int bool;
#define bool int

#define true 1
#define false 0

extern int printf(char *, ...);

// example - simple recursive function
unsigned fac(unsigned x) {
	return (x>1)?(x * fac(x-1)):1;
}

// example - mutually recursive function
bool even(unsigned x);
bool odd(unsigned x);

bool even(unsigned x) {
	return (x==0)?true:odd(x-1);
}

bool odd(unsigned x) {
	return (x==0)?false:even(x-1);
}

// example - nested recursive function
unsigned ack(unsigned n, unsigned m) {
	if (n == 0) return m + 1;
	if (m == 0) return ack(n-1, 1);
	return ack(n-1, ack(n, m-1));
}


// using the given functions
int main(int argc, char* argv[]) {
	int x = 10;
	printf("x=%d\n", x);
	printf("fac(x)=%d\n", fac(x));
	printf("fac(x+1)=%d\n", fac(x+1));
	printf("even(x)=%s\n", (even(x))?"true":"false");
	printf("odd(x)=%s\n", (odd(x))?"true":"false");
	printf("ack(1,x)=%d\n", ack(1,x));
	printf("ack(2,x)=%d\n", ack(2,x));
	return 0;
}
