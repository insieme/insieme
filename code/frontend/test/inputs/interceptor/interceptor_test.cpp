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

// ignore warnings
#pragma GCC diagnostic ignored "-Wall"

#include "interceptor_header.h"

void intercept_simpleFunc() {

#pragma test "{ decl ref<int<4>> v1 = ( var(0)); ns::simpleFunc(1); ns::simpleFunc(( *v1));}"
	{
		int a = 0;
		ns::simpleFunc(1);
		ns::simpleFunc(a);
	}
}

void intercept_memFunc() {
#pragma test "{ decl ref<int<4>> v1 = ( var(0)); decl ref<ns::S> v2 = ns::S::S(( var(undefined(type<ns::S>)))); ns::S::memberFunc(v2, ( *v1));}"
	{
		int a = 0;
		ns::S s;
		s.memberFunc(a);
	}
#pragma test "{ decl ref<int<4>> v1 = ( var(0)); decl ref<ns::S> v2 = ns::S::S(( var(undefined(type<ns::S>)))); ns::S::memberFunc(v2, ( *v1));}"
	{
		using namespace ns;
		int a = 0;
		S s;
		s.memberFunc(a);
	}
}

int main() {
	intercept_simpleFunc();
	intercept_memFunc();
};
