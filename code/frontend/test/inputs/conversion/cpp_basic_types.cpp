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

int producer() {
	return 5;
}
void consumer(int&& i) {}

int foo() { return true; }

using philipp = int;
using driver = philipp;

struct D { int i; };
void dfun(D d) { }

int main() {
	#pragma test expect_ir("{ var ref<bool> v0; v0 = true; v0 = false; }")
	{
		bool a;
		a = true;
		a = false;
	}

	#pragma test expect_ir("{ var ref<int<4>> v0; var ref<int<4>,f,f,cpp_ref> v1 = ref_kind_cast(v0, type_lit(cpp_ref)); }")
	{
		int i;
		int& ref_i = i;
	}

	#pragma test expect_ir("{ var ref<int<4>> v0; var ref<int<4>,t,f,cpp_ref> v1 = ref_kind_cast(v0, type_lit(cpp_ref)); }")
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

	#pragma test expect_ir(R"(
		def struct IMP_D {
			i : int<4>;
		};
		def IMP_dfun = function (v0 : ref<IMP_D,f,f,plain>) -> unit { };
		{
			var ref<IMP_D,f,f,plain> v0 = <ref<IMP_D,f,f,plain>>(ref_decl(type_lit(ref<IMP_D,f,f,plain>))) {1};
			var ref<IMP_D,f,f,plain> v1 = <ref<IMP_D,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(IMP_D)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {1};
			IMP_dfun(<ref<IMP_D,f,f,plain>>(ref_decl(type_lit(ref<IMP_D,f,f,plain>))) {1});
			IMP_dfun(<ref<IMP_D,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(IMP_D)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {1});
		}
	)")
	{
		D d1 {1};
		D d2 = D{1};

		dfun({1});
		dfun(D{1});
	}

	return 0;
}
