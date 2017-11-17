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

struct StructWithOMP {

	int x;

	int foo() {
		int ret;

		#pragma omp parallel
		{
			ret = x;
		}

		#pragma omp parallel
		{
			this;
		}

		return ret;
	}
};


int main() {
	int i;

	#pragma test expect_ir(R"(
		decl struct IMP_StructWithOMP;
		decl _ins_omp_parallel_0 : (ref<int<4>,f,f,plain>, ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>) -> unit;
		decl _ins_omp_parallel_1 : (ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>) -> unit;
		decl IMP_StructWithOMP::x : int<4>;
		decl IMP_foo:IMP_StructWithOMP::() -> int<4>;
		def struct IMP_StructWithOMP {
			x : int<4>;
			function IMP_foo = () -> int<4> {
				var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
				{
					var ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain> v2 = this;
					merge(parallel(job[1ul...] => _ins_omp_parallel_0(v1, v2)));
				}
				{
					var ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain> v3 = this;
					merge(parallel(job[1ul...] => _ins_omp_parallel_1(v3)));
				}
				return *v1;
			}
		};
		def _ins_omp_parallel_0 = function (v0 : ref<ref<int<4>,f,f,plain>,f,f,plain>, v1 : ref<ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>,f,f,plain>) -> unit {
			{
				*v0 = *(**v1).x;
			}
			merge_all();
		};
		def _ins_omp_parallel_1 = function (v0 : ref<ref<ref<IMP_StructWithOMP,f,f,plain>,f,f,plain>,f,f,plain>) -> unit {
			{
				ptr_from_ref(**v0);
			}
			merge_all();
		};
		{
			var ref<IMP_StructWithOMP,f,f,plain> v0 = IMP_StructWithOMP::(ref_decl(type_lit(ref<IMP_StructWithOMP,f,f,plain>)));
		}
	)")
	{
		StructWithOMP s;
	}
}
