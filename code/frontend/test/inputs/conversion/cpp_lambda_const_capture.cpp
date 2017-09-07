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

int main() {
	;

	// Test capturing of const variables, which seems buggy in clang

	// the generated AST is different if we capture a const variable, we initialize it with it's value in a ref_temp
	#pragma test expect_ir(R"(
		def struct __any_string__lambda {
			const function IMP__operator_call_ = () -> unit {
				var ref<int<4>,f,f,plain> v1 = *ref_temp_init(7000);
			}
		};
		{
			var ref<int<4>,t,f,plain> v0 = 7000;
			var ref<__any_string__lambda,f,f,plain> v1 = <ref<__any_string__lambda,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__lambda)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {};
		}
	)")
	{
		const int N = 7000;
		auto a = [&] {
			int i = N;
		};
	}

	// everything is normal though if the const variable isn't initialized with a constant integer literal
	#pragma test expect_ir(R"(
		def struct __any_string__lambda {
			capture_0 : ref<int<4>,t,f,cpp_ref>;
			const function IMP__operator_call_ = () -> unit {
				var ref<int<4>,f,f,plain> v1 = **(this).capture_0;
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = 7000;
			var ref<int<4>,t,f,plain> v1 = *v0;
			var ref<__any_string__lambda,f,f,plain> v2 = <ref<__any_string__lambda,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__lambda)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {ref_kind_cast(v1, type_lit(cpp_ref))};
		}
	)")
	{
		int x = 7000;
		const int N = x;
		auto a = [&] {
			int i = N;
		};
	}

	// weirder still - this seems to be a real bug in clang. We can use a const variable, even though it hasn't been captured
	#pragma test expect_ir(R"(
		decl struct __any_string__lambda;
		decl IMP__conversion_operator_auto__lparen__star__rparen__lparen_void_rparen___minus__gt__void:const __any_string__lambda::() -> ptr<() -> unit,t,f>;
		def struct __any_string__lambda {
			const function IMP__operator_call_ = () -> unit {
				var ref<int<4>,f,f,plain> v1 = *ref_temp_init(7000);
			}
		};
		{
			var ref<int<4>,t,f,plain> v0 = 7000;
			var ref<__any_string__lambda,f,f,plain> v1 = <ref<__any_string__lambda,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(__any_string__lambda)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {};
		}
	)")
	{
		const int N = 7000;
		auto a = [] {
			int i = N;
		};
	}

	return 0;
}
