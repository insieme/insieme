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

int producer() {
	return 5;
}
void consumer(int&& i) {}

int foo() { return true; }

using philipp = int;
using driver = philipp;

int main() {
	#pragma test expect_ir("{ var ref<bool> v0; v0 = true; v0 = false; }")
	{
		bool a;
		a = true;
		a = false;
	}

	#pragma test expect_ir("{ var ref<int<4>> v0; var ref<int<4>,f,f,cpp_ref> v1 = v0; }")
	{
		int i;
		int& ref_i = i;
	}

	#pragma test expect_ir("{ var ref<int<4>> v0; var ref<int<4>,t,f,cpp_ref> v1 = v0; }")
	{
		int i;
		const int& ref_i = i;
	}

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = 0;")
	auto var0 = 0;

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = 1;")
	decltype(var0) var1 = 1;

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = 2;")
	philipp var2 = 2;

	#pragma test expect_ir("var ref<int<4>,f,f> v0 = 3;")
	driver var3 = 3;

	#pragma test expect_ir(R"(
		def IMP_consumer = function (v1 : ref<int<4>,f,f,cpp_rref>) -> unit { };
		def IMP_producer = () -> int<4> { return 5; };
		IMP_consumer(ref_kind_cast(IMP_producer() materialize, type_lit(cpp_rref)))
	)")
	consumer(producer());

	#pragma test expect_ir("ptr_null(type_lit(unit), type_lit(f), type_lit(f))")
	nullptr;

	#pragma test expect_ir("ptr_null(type_lit(int<4>), type_lit(f), type_lit(f))")
	(int*)(nullptr);

	#pragma test expect_ir(R"(
		def IMP_foo = function () -> int<4> {
			return bool_to_int(true);
		};
		IMP_foo()
	)")
	foo();

	#pragma test expect_ir(R"(
		var ref<int<4>,f,f,plain> v0 = *v0+1;
	)")
	int x = x+1;

	return 0;
}
