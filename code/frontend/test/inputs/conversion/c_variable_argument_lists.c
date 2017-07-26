/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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


