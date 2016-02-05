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

int x;
int x1() {
	return x;
}
const int& x2() {
	return x;
};
int&& x3() {
	return static_cast<int&&>(x);
};
const int&& x4() {
	return static_cast<int&&>(x);
};
/*
struct Trivial {};
Trivial y;
Trivial y1() {
	return y;
}
const Trivial& y2() {
	return y;
};
Trivial&& y3() {
	return static_cast<Trivial&&>(y);
};
const Trivial&& y4() {
	return static_cast<Trivial&&>(y);
};

struct NonTrivial {
	~NonTrivial();
};
NonTrivial z;
NonTrivial z1() {
	return z;
}
const NonTrivial& z2() {
	return z;
};
NonTrivial&& z3() {
	return static_cast<NonTrivial&&>(z);
};
const NonTrivial&& z4() {
	return static_cast<NonTrivial&&>(z);
};
*/
void intTest() {
/*
	{
		int a = x1();
		int b = x2();
		int c = x3();
		int d = x4();
	}
	{*/
		const int& a = x1();
		const int& b = x2();
		const int& c = x3();
		const int& d = x4();/*
	}
	{
		int&& a = x1();
		// int&& b = x2(); // NOT ALLOWED
		int&& c = x3();
		// int&& d = x4(); // NOT ALLOWED
	}
	{
		const int&& a = x1();
		// const int&& b = x2(); // NOT ALLOWED
		const int&& c = x3();
		const int&& d = x4();
	}*/
}
/*
void trivialTest() {
    {
        Trivial a = y1();
        Trivial b = y2();
        Trivial c = y3();
        Trivial d = y4();
    }
    {
        const Trivial& a = y1();
        const Trivial& b = y2();
        const Trivial& c = y3();
        const Trivial& d = y4();
    }
    {
        Trivial&& a = y1();
        //Trivial&& b = y2(); // NOT ALLOWED
        Trivial&& c = y3();
        //Trivial&& d = y4(); // NOT ALLOWED
    }
    {
        const Trivial&& a = y1();
        //const Trivial&& b = y2(); // NOT ALLOWED
        const Trivial&& c = y3();
        const Trivial&& d = y4();
    }
}*/
/*
void nonTrivialTest() {
    {
        NonTrivial a = z1();
        NonTrivial b = z2();
        NonTrivial c = z3();
        NonTrivial d = z4();
    }
    {
        const NonTrivial& a = z1();
        const NonTrivial& b = z2();
        const NonTrivial& c = z3();
        const NonTrivial& d = z4();
    }
    {
        NonTrivial&& a = z1();
        //NonTrivial&& b = z2(); // NOT ALLOWED
        NonTrivial&& c = z3();
        //NonTrivial&& d = z4(); // NOT ALLOWED
    }
    {
        const NonTrivial&& a = z1();
        //const NonTrivial&& b = z2(); // NOT ALLOWED
        const NonTrivial&& c = z3();
        const NonTrivial&& d = z4();
    }
}*/

int main() {
	intTest();
	//trivialTest();
	//nonTrivialTest();
}
