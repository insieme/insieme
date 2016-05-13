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

// a trivial struct
struct ThisTest {
	ThisTest* getThis() { return this; }
};

#define STRUCT_THISTEST R"(def struct IMP_ThisTest {
	lambda IMP_getThis = () -> ptr<IMP_ThisTest> { return ptr_from_ref(this); }
};)"

struct ThisTest2 {
	float v;
	float fA() { return this->v; }
	float fB() { return v; }
};
#define STRUCT_THISTEST2 R"(def struct IMP_ThisTest2 {
	v : real<4>;
	lambda IMP_fA = () -> real<4> { return *v; }
	lambda IMP_fB = () -> real<4> { return *v; }
};)"

int main() {

	#pragma test expect_ir(STRUCT_THISTEST,R"({
		var ref<IMP_ThisTest,f,f,plain> v0 = IMP_ThisTest::(ref_decl(type_lit(ref<IMP_ThisTest,f,f,plain>)));
		v0.IMP_getThis();
	})")
	{
		ThisTest tt;
		tt.getThis();
	}

	#pragma test expect_ir(STRUCT_THISTEST2,R"({
		var ref<IMP_ThisTest2,f,f,plain> v0 = IMP_ThisTest2::(ref_decl(type_lit(ref<IMP_ThisTest2,f,f,plain>)));
	})")
	{
		ThisTest2 tt2;
	}

	return 0;
}
