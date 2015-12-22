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

int bar() { return 0; }

void bla(int a, int b, ...);
void foo(int a, ...);

int main() {

	#define PREAMBLE "using \"ext.varargs\"; def IMP_bar = () -> int<4> { return 0;};"
	#define BLA "lit(\"IMP_bla\":(int<4>,int<4>,var_list)->unit)"
	#define FOO "lit(\"IMP_foo\":(int<4>,var_list)->unit)"

	#pragma test expect_ir(PREAMBLE "{",BLA,"(1, 2, varlist_pack(())); }")
	{ bla(1,2); }
	
	#pragma test expect_ir(PREAMBLE "{",BLA,"(2, 3, varlist_pack((17))); }")
	{ bla(2,3, 17); }
	
	#pragma test expect_ir(PREAMBLE "{",BLA,"(5, 6, varlist_pack((5u, 17))); }")
	{ bla(5,6, 5u, 17); }
	
	#pragma test expect_ir(PREAMBLE "{",FOO,"(1, varlist_pack(())); }")
	{ foo(1); }
	
	#pragma test expect_ir(PREAMBLE "{",FOO,"(1, varlist_pack((IMP_bar()))); }")
	{ foo(1, bar()); }

	return 0;
}


