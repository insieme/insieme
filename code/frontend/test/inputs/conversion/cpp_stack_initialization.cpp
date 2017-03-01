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
 *
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
