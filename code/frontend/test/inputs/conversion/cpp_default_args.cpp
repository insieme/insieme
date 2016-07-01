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

int f(int x = 7) {
	return x;
}
#define F_IR "def IMP_f = function (v0 : ref<int<4>,f,f,plain>) -> int<4> { return *v0; }; "


void g(int x, unsigned y = 4u, char z = 'd') {}
#define G_IR "def IMP_g = function (v0 : ref<int<4>,f,f,plain>, v1 : ref<uint<4>,f,f,plain>, v2 : ref<char,f,f,plain>) -> unit { }; "

struct S {};
void s(S s = S()) {};

int main() {
#pragma test expect_ir(F_IR, "IMP_f(4)")
	f(4);

	#pragma test expect_ir(F_IR, "IMP_f(7)")
	f();

	#pragma test expect_ir(G_IR, R"(IMP_g(4, 19u, lit("'s'":char)))")
	g(4, 19u, 's');

	#pragma test expect_ir(G_IR, R"(IMP_g(5, 12u, lit("'d'":char)))")
	g(5, 12u);

	#pragma test expect_ir(G_IR, R"(IMP_g(7, 4u, lit("'d'":char)))")
	g(7);

	#pragma test expect_ir(R"(
		def struct IMP_S {};
		def IMP_s = function (v0 : ref<IMP_S,f,f,plain>) -> unit { };
		IMP_s(ref_cast(IMP_S::(ref_temp(type_lit(IMP_S))), type_lit(f), type_lit(f), type_lit(cpp_rref)))
	)")
	s();

	return 0;
}
