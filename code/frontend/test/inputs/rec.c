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

int g(int x) ;
int f(int x) ;

#pragma test \
	"recFun v11 { v11 = fun(int<4> v13) -> int<4> { return v12((v13-1)); }; v12 = fun(int<4> v15) -> int<4> { return v11((v15+1)); };}"
int f(int x) {
	return g(x-1);
}

#pragma test \
	"recFun v11 { v11 = fun(int<4> v13) -> int<4> { return v12((v13+1)); }; v12 = fun(int<4> v15) -> int<4> { return v11((v15-1)); };}"
int g(int x) {
	return f(x+1);
}

//recFun v1 {
//    v2 = fun(int<4> v4) {
//        return v1((v4+1));
//    };
//    v1 = fun(int<4> v3) {
//        return v2((v3-1));
//    };
//}

