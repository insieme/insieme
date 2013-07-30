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

#include <iostream>

using namespace std;


// spetial thanks to Milot's brain dump  who wrotte the example
// http://milotshala.wordpress.com/2012/03/01/fibonacci-sequence-and-c-template-meta-programming/

// Calculate the value passed as T
template <int T>
struct Fibonacci {
	enum { value = (Fibonacci<T - 1>::value + Fibonacci<T - 2>::value) };
};

// In the template meta-programming, we do not have conditionals, so instead
// we use struct-overloading like mechanism to set constraints, we do this for
// numbers 0, 1 and 2, just like our algorithm in the function above.
template <>
struct Fibonacci<0> {
	enum { value = 1 };
};

template <>
struct Fibonacci<1> {
	enum { value = 1 };
};

template <>
struct Fibonacci<2> {
	enum { value = 1 };
};

// in the end, we get the value
int main() {
	int x = Fibonacci<45>::value;
	cout << x << endl;
}
