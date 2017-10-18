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

	int magic;

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = true?1:2;
	})")
	{
		auto ite = true ? 1 : 2;
	}

	#pragma test expect_ir(R"({
		var ref<real<8>,f,f,plain> v0 = true?num_cast(1, type_lit(real<8>)):lit("2.0E+0":real<8>);
	})")
	{
		auto ite = true ? 1 : 2.0;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<int<4>,f,f,plain> v1 = 2;
		var ref<int<4>,f,f,plain> v2 = *(true?v0:v1);
	})")
	{
		int a = 1;
		int b = 2;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,t,f,plain> v0 = 1;
		var ref<int<4>,f,f,plain> v1 = 2;
		var ref<int<4>,f,f,plain> v2 = *(true?v0:ref_cast(v1, type_lit(t), type_lit(f), type_lit(plain)));
	})")
	{
		const int a = 1;
		int b = 2;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,t,f,plain> v0 = 1;
		var ref<int<4>,t,f,plain> v1 = 2;
		var ref<int<4>,f,f,plain> v2 = *(true?v0:v1);
	})")
	{
		const int a = 1;
		const int b = 2;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<int<4>,f,f,plain> v1 = 2;
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		var ref<int<4>,f,f,plain> v3 = *v1;
		var ref<int<4>,f,f,plain> v4 = *(true?ref_cast(v2, type_lit(f), type_lit(f), type_lit(plain)):v3);
	})")
	{
		int i = 1;
		int j = 2;
		int& a = i;
		int b = j;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<int<4>,f,f,plain> v1 = 2;
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		var ref<int<4>,f,f,cpp_ref> v3 = v1;
		var ref<int<4>,f,f,plain> v4 = *(true?v2:v3);
	})")
	{
		int i = 1;
		int j = 2;
		int& a = i;
		int& b = j;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<int<4>,f,f,plain> v1 = 2;
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		var ref<int<4>,t,f,plain> v3 = *v1;
		var ref<int<4>,f,f,plain> v4 = *(true?ref_cast(v2, type_lit(t), type_lit(f), type_lit(plain)):v3);
	})")
	{
		int i = 1;
		int j = 2;
		int& a = i;
		const int b = j;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<int<4>,f,f,plain> v1 = 2;
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		var ref<int<4>,t,f,cpp_ref> v3 = v1;
    var ref<int<4>,f,f,plain> v4 = *(true?ref_cast(v2, type_lit(t), type_lit(f), type_lit(cpp_ref)):v3);
	})")
	{
		int i = 1;
		int j = 2;
		int& a = i;
		const int& b = j;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<real<8>,f,f,plain> v1 = num_cast(2, type_lit(real<8>));
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		var ref<real<8>,f,f,plain> v3 = *v1;
		var ref<real<8>,f,f,plain> v4 = true?num_cast(*v2, type_lit(real<8>)):*v3;
	})")
	{
		int i = 1;
		double j = 2;
		int& a = i;
		double b = j;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<real<8>,f,f,plain> v1 = num_cast(2, type_lit(real<8>));
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		var ref<real<8>,f,f,cpp_ref> v3 = v1;
		var ref<real<8>,f,f,plain> v4 = true?num_cast(*v2, type_lit(real<8>)):*v3;
	})")
	{
		int i = 1;
		double j = 2;
		int& a = i;
		double& b = j;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<real<8>,f,f,plain> v1 = num_cast(2, type_lit(real<8>));
		var ref<int<4>,f,f,cpp_ref> v2 = v0;
		var ref<real<8>,t,f,cpp_ref> v3 = v1;
    var ref<real<8>,f,f,plain> v4 = true?num_cast(*v2, type_lit(real<8>)):*v3;
	})")
	{
		int i = 1;
		double j = 2;
		int& a = i;
		const double& b = j;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<int<4>,f,f,plain> v1 = 2;
		var ref<ptr<int<4>>,f,f,plain> v2 = ptr_from_ref(v0);
		var ref<ptr<int<4>>,f,f,plain> v3 = ptr_from_ref(v1);
		var ref<ptr<int<4>>,f,f,plain> v4 = *(true?v2:v3);
	})")
	{
		int i = 1;
		int j = 2;
		int* a = &i;
		int* b = &j;
		auto ite = true ? a : b;
	}

	#pragma test expect_ir(R"({
		var ref<int<4>,f,f,plain> v0 = 1;
		var ref<int<4>,f,f,plain> v1 = 2;
		var ref<ptr<int<4>>,f,f,plain> v2 = ptr_from_ref(v0);
		var ref<ptr<int<4>,t,f>,f,f,plain> v3 = ptr_from_ref(v1);
    var ref<ptr<int<4>,t,f>,f,f,plain> v4 = true?ptr_cast(*v2, type_lit(t), type_lit(f)):*v3;
	})")
	{
		int i = 1;
		int j = 2;
		int* a = &i;
		const int* b = &j;
		auto ite = true ? a : b;
	}

	return 0;
}
