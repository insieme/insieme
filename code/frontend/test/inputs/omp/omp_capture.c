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
int main() {
	int magic;

	#pragma test expect_ir(R"(
		def __any_string__parfun = function (v0 : ref<ref<int<4>,f,f,plain>,f,f,plain>) -> unit {
			{
				var ref<int<4>,f,f,plain> v1 = **v0;
			}
			merge_all();
		};
		{
			var ref<int<4>,f,f,plain> v0 = 42;
			{
				merge(parallel(job[1ul...] => __any_string__parfun(v0)));
			}
		}
	)")
	{
		int a = 42;
		#pragma omp parallel
		{
			int i = a;
		}
	}

	#pragma test expect_ir(R"(
		def __any_string__parfun = function (v0 : ref<ref<int<4>,f,f,plain>,f,f,plain>) -> unit {
			var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			{
				var ref<int<4>,f,f,plain> v2 = **v0;
				v1 = 5;
			}
			merge_all();
		};
		{
			var ref<int<4>,f,f,plain> v0 = 42;
			var ref<int<4>,f,f,plain> v1 = 1337;
			{
				merge(parallel(job[1ul...] => __any_string__parfun(v0)));
			}
		}
	)")
	{
		int a = 42, b = 1337;
		#pragma omp parallel shared(a) private(b)
		{
			int i = a;
			b = 5;
		}
	}

	#pragma test expect_ir(R"(
		def __any_string__parfun = function (v0 : ref<ptr<int<4>>,f,f,plain>) -> unit {
			{
				ptr_to_ref(*v0) = 5;
			}
			merge_all();
		};
		{
			var ref<int<4>,f,f,plain> v0 = 42;
			var ref<ptr<int<4>>,f,f,plain> v1 = ptr_from_ref(v0);
			{
				merge(parallel(job[1ul...] => __any_string__parfun(*v1)));
			}
		}
	)")
	{
		int a = 42, *b = &a;
		#pragma omp parallel shared(b)
		{
			*b = 5;
		}
	}

	//{
	//	int num = 8;
	//	int arr[num];
	//	#pragma omp parallel
	//	{
	//		arr[3] = 0;
	//	}
	//}
}