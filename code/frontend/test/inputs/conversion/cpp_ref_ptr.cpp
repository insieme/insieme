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

void take_ref(int& a) {}
void take_const_ref(const int& a) {}
void take_ptr(int* a) {}

int* g;
int& gen_ref() { return *g; }
int* gen_ptr() { return g; }

int main() {

	#pragma test expect_ir(R"({
    var ref<int<4>,f,f,plain> v0 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
    var ref<ptr<int<4>>,f,f,plain> v1 = ptr_from_ref(v0);
    var ref<int<4>,f,f,cpp_ref> v2 = v0;
    var ref<int<4>,f,f,plain> v3 = *ptr_to_ref(*v1);
    var ref<ptr<int<4>>,f,f,plain> v4 = ptr_from_ref(ref_cast(v2, type_lit(f), type_lit(f), type_lit(plain)));
    var ref<int<4>,f,f,cpp_ref> v5 = ptr_to_ref(*v1);
})")
	{
		int i;
		int* i_ptr = &i;
		int& i_ref = i;

		int j = *i_ptr;
		int* j_ptr = &i_ref;
		int& j_ref = *i_ptr;
	}

	#pragma test expect_ir(R"(
		def IMP_take_ref = (a: ref<int<4>,f,f,cpp_ref>) -> unit {};
		def IMP_take_ptr = (a: ptr<int<4>>) -> unit {};
		{
			var ref<int<4>,f,f,plain> v0;
			var ref<ptr<int<4>>,f,f,plain> v1 = ptr_from_ref(v0);
			var ref<int<4>,f,f,cpp_ref> v2 = v0;

			IMP_take_ref(ref_kind_cast(ptr_to_ref(*v1), type_lit(cpp_ref)));
			IMP_take_ptr(ptr_from_ref(ref_cast(v2, type_lit(f), type_lit(f), type_lit(plain))));
		}
	)")
	{
		int i;
		int* i_ptr = &i;
		int& i_ref = i;

		take_ref(*i_ptr);
		take_ptr(&i_ref);
	}

	#pragma test expect_ir(R"(
		def IMP_gen_ref = function () -> ref<int<4>,f,f,cpp_ref> {
			return ptr_to_ref(*lit("g" : ref<ptr<int<4>>,f,f,plain>));
		};
		def IMP_gen_ptr = function () -> ptr<int<4>> {
			return *lit("g" : ref<ptr<int<4>>,f,f,plain>);
		};
		{
			IMP_gen_ref() materialize ;
			IMP_gen_ptr();
		}
	)")
	{
		gen_ref();
		gen_ptr();
	}

	#pragma test expect_ir(R"(
		def IMP_take_const_ref = function (v0 : ref<int<4>,t,f,cpp_ref>) -> unit { };
		{
			IMP_take_const_ref(5);
		}
	)")
	{
		take_const_ref(5);
	}

	return 0;
}
