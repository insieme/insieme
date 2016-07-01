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
};

struct NonPod {
	virtual ~NonPod() { 5; }
};

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"(
		def struct IMP_A {};
		{
			var ref<array<IMP_A,3>,f,f,plain> v0 = <ref<array<IMP_A,3>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_A,3>,f,f,plain>))) {};
		}
	)")
	{
		A a[3];
	}

	#pragma test expect_ir(R"(
		def struct IMP_A {};
		{
			var ref<IMP_A,f,f,plain> v0 = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
			var ref<IMP_A,f,f,plain> v1 = IMP_A::(ref_decl(type_lit(ref<IMP_A,f,f,plain>)));
			var ref<array<IMP_A,3>,f,f,plain> v2 = <ref<array<IMP_A,3>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_A,3>,f,f,plain>))) {ref_cast(v0, type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_cast(v1, type_lit(t), type_lit(f), type_lit(cpp_ref))};
		}
	)")
	{
		A a1, a2;
		A a[3] { a1, a2 };
	}

	#pragma test expect_ir(R"(
		def struct IMP_NonPod {
			dtor virtual function () { 5; }
		};
		{
			var ref<array<IMP_NonPod,77>,f,f,plain> v0 = <ref<array<IMP_NonPod,77>,f,f,plain>>(ref_decl(type_lit(ref<array<IMP_NonPod,77>,f,f,plain>))) {};
		}
	)")
	{
		NonPod a[77];
	}

	return 0;
}
