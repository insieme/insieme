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

struct A {
	A() : j(i) {}
	int i;
	int& j;
};

int main() {
	; // this is required because of the clang compound source location bug

	// check accesses with various ref kind combinations
	#pragma test expect_ir(R"(
		def struct IMP_A {
			i : int<4>;
			j : ref<int<4>,f,f,cpp_ref>;
			ctor function () {
				<ref<int<4>,f,f,cpp_ref>>(*(this).j) {(this).i};
			}
		};
		{
			var ref<IMP_A,f,f,plain> v0 = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
			var ref<IMP_A,f,f,cpp_ref> v1 = v0;
			v0.i;
			*v0.j;
			v1.i;
			*v1.j;
		}
	)")
	{
		A a;
		A& b = a;
		a.i;
		a.j;
		b.i;
		b.j;
	}

	return 0;
}
