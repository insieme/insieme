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


struct c {
	int b;
};

int counter = 10;

typedef struct {
	int a;
} X;

typedef struct {
	int j;
} Y;

namespace {
	struct A {};
}

namespace x {
	struct B {};
}

class List {
	int value;
	List* next;
};

List fullList;


class Even;
class Odd;

class Even { Odd* next; };
class Odd { Even* next; };


int sum(int* l, int s);

int sum(int* l, int s) {
	int res = 0;
	for (int i=0; i<s; i++) {
		res += l[i];
	}
	return res;
}

int fib(int x) {
	if (x <= 0) return 1;
	return x * fib(x-1);
}

int even(int);
int odd(int);

int even(int x) {
	if (x == 0) return true;
	return odd(x-1);
}

int odd(int x) {
	if (x == 0) return false;
	return even(x-1);
}

int a(int);
int b(int);
int c(int);

int a(int x) { return b(x); }
int b(int x) { return c(x); }
int c(int x) { return a(x); }

int main() {

	static double PI = 3;

	counter++;

	struct C {

		X x;
		A a;
		x::B b;

	};

	int a = 1 + 2;

	List l;
	Even e;
	Odd o;

	even(2);
	odd(2);

	b(12);
	c(12);
}

