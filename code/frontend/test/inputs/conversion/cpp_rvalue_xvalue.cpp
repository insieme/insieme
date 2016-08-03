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

struct Base { };

struct Derived;
Derived&& move(Derived& d) {
	return static_cast<Derived&&>(d);
}

struct Derived : public Base {
	Derived() {}
	Derived(Derived&& other) : Base(move(other)) {}
};

int main() {
	;

	#pragma test expect_ir(R"(
		decl IMP_move : (ref<IMP_Derived,f,f,cpp_ref>) -> ref<IMP_Derived,f,f,cpp_rref>;
		def struct IMP_Base {
		};
		def struct IMP_Derived : [ public IMP_Base ] {
			ctor function () {
				IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)));
			}
			ctor function (v1 : ref<IMP_Derived,f,f,cpp_rref>) {
				IMP_Base::(ref_parent_cast(this, type_lit(IMP_Base)), ref_parent_cast(IMP_move(ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(IMP_Base)));
			}
		};
		def IMP_move = function (v0 : ref<IMP_Derived,f,f,cpp_ref>) -> ref<IMP_Derived,f,f,cpp_rref> {
			return ref_cast(v0, type_lit(f), type_lit(f), type_lit(cpp_rref));
		};
		{
			var ref<IMP_Derived,f,f,plain> v0 = IMP_Derived::(ref_decl(type_lit(ref<IMP_Derived,f,f,plain>)));
			var ref<IMP_Derived,f,f,plain> v1 = IMP_move(ref_kind_cast(v0, type_lit(cpp_ref)));
		}
	)")
	{
		Derived d1;
		Derived d2(move(d1));
	}

	return 0;
}
