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

#include <vector>

int main() {
	;

	#pragma test expect_ir(R"(
		{
			var ref<array<int<4>,3>,f,f,plain> v0 = <ref<array<int<4>,3>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3>,f,f,plain>))) {1, 2, 3};
			var ref<array<int<4>,3>,f,f,cpp_ref> v1 = v0;
			var ref<ptr<int<4>>,f,f,plain> v2 = ptr_from_array(v1);
			var ref<ptr<int<4>>,f,f,plain> v3 = ptr_add(ptr_from_array(v1), 3l);
			while(ptr_ne(*v2, *v3)) {
				var ref<int<4>,f,f,plain> v4 = *ptr_to_ref(*v2);
				{
					v4;
					ptr_pre_inc(v2);
				}
			}
		}
	)")
	{
		int list[] = {1, 2, 3};
		for(auto element : list) {
			element;
		}
	}

	#pragma test expect_ir(R"(
		{
			var ref<array<int<4>,3>,f,f,plain> v0 = <ref<array<int<4>,3>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3>,f,f,plain>))) {1, 2, 3};
			var ref<array<int<4>,3>,f,f,cpp_ref> v1 = v0;
			var ref<ptr<int<4>>,f,f,plain> v2 = ptr_from_array(v1);
			var ref<ptr<int<4>>,f,f,plain> v3 = ptr_add(ptr_from_array(v1), 3l);
			while(ptr_ne(*v2, *v3)) {
				var ref<int<4>,f,f,plain> v4 = *ptr_to_ref(*v2);
				{
					{
						if(*v4==2) {
							ptr_pre_inc(v2);
							continue;
						}
						v4;
					}
					ptr_pre_inc(v2);
				}
			}
		}
	)")
	{
		int list[] = {1, 2, 3};
		for(auto element : list) {
			if (element == 2) continue;
			element;
		}
	}

	#pragma test expect_ir(R"(
		{
			var ref<array<int<4>,3>,f,f,plain> v0 = <ref<array<int<4>,3>,f,f,plain>>(ref_decl(type_lit(ref<array<int<4>,3>,f,f,plain>))) {1, 2, 3};
			var ref<array<int<4>,3>,f,f,cpp_ref> v1 = v0;
			var ref<ptr<int<4>>,f,f,plain> v2 = ptr_from_array(v1);
			var ref<ptr<int<4>>,f,f,plain> v3 = ptr_add(ptr_from_array(v1), 3l);
			while(ptr_ne(*v2, *v3)) {
				var ref<int<4>,f,f,plain> v4 = *ptr_to_ref(*v2);
				{
					{ }
					ptr_pre_inc(v2);
				}
			}
		}
	)")
	{
		int list[] = {1, 2, 3};
		for(auto element : list);
	}

//	#pragma test expect_ir(R"(
//		{
//		}
//	)")
//	{
//		std::vector<int> list;
//		for(auto element : list) {
//			element;
//		}
//	}

	return 0;
}