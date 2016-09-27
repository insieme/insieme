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

void testParam(int* a) {
	int magic;

	#pragma omp parallel
	#pragma omp single
	#pragma omp task untied
	{
		*a = 1;
	}
}

int testCapture(int* b) {
	float c = 0.5f, d = 2.0f;

	#pragma omp parallel
	#pragma omp single
	#pragma omp task untied
	{
		*b = (int)(c*d);
	}

	return *b;
}

int main() {
	int magic;

	#pragma test expect_ir(R"(
		def __any_string__task_fun = function () -> unit {
			{
				42;
			}
		};
		def __any_string__single_fun = function () -> unit {
			parallel(job[1ul..1ul] => __any_string__task_fun());
		};
		def __any_string__parallel_fun = function () -> unit {
			{
				pfor(get_thread_group(0u), 0, 1, 1, (v0 : int<4>, v1 : int<4>, v2 : int<4>) => __any_string__single_fun());
				barrier(get_thread_group(0u));
			}
			merge_all();
		};
		{
			{
				merge(parallel(job[1ul...] => __any_string__parallel_fun()));
			}
		}
	)")
	{
		#pragma omp parallel
		#pragma omp single
		#pragma omp task untied
		{
			42;
		}
	}

	#pragma test expect_ir(R"(
		def __any_string__task_fun = function (v0 : ref<ptr<int<4>>,f,f,plain>) -> unit {
			{
				ptr_to_ref(*v0) = 1;
			}
		};
		def __any_string__single_fun = function (v0 : ref<ptr<int<4>>,f,f,plain>) -> unit {
			parallel(job[1ul..1ul] => __any_string__task_fun(*v0));
		};
		def __any_string__parallel_fun = function (v0 : ref<ptr<int<4>>,f,f,plain>) -> unit {
			{
				pfor(get_thread_group(0u), 0, 1, 1, (v1 : int<4>, v2 : int<4>, v3 : int<4>) => __any_string__single_fun(*v0));
				barrier(get_thread_group(0u));
			}
			merge_all();
		};
		def IMP_testParam = function (v0 : ref<ptr<int<4>>,f,f,plain>) -> unit {
			var ref<int<4>,f,f,plain> v1 = ref_decl(type_lit(ref<int<4>,f,f,plain>));
			{
				merge(parallel(job[1ul...] => __any_string__parallel_fun(*v0)));
			}
		};
		{
			var ref<int<4>,f,f,plain> v0 = 0;
			IMP_testParam(ptr_from_ref(v0));
		}
	)")
	{
		int p = 0;
		testParam(&p);
	}

	#pragma test expect_ir(R"(
		def __any_string__task_fun = function (v0 : ref<ptr<int<4>>,f,f,plain>, v1 : ref<ref<real<4>,f,f,plain>,f,f,plain>, v2 : ref<ref<real<4>,f,f,plain>,f,f,plain>) -> unit {
			{
				ptr_to_ref(*v0) = num_cast(**v1***v2, type_lit(int<4>));
			}
		};
		def __any_string__single_fun = function (v0 : ref<ptr<int<4>>,f,f,plain>, v1 : ref<ref<real<4>,f,f,plain>,f,f,plain>, v2 : ref<ref<real<4>,f,f,plain>,f,f,plain>) -> unit {
			parallel(job[1ul..1ul] => __any_string__task_fun(*v0, *v1, *v2));
		};
		def __any_string__parallel_fun = function (v0 : ref<ptr<int<4>>,f,f,plain>, v1 : ref<ref<real<4>,f,f,plain>,f,f,plain>, v2 : ref<ref<real<4>,f,f,plain>,f,f,plain>) -> unit {
			{
				pfor(get_thread_group(0u), 0, 1, 1, (v3 : int<4>, v4 : int<4>, v5 : int<4>) => __any_string__single_fun(*v0, *v1, *v2));
				barrier(get_thread_group(0u));
			}
			merge_all();
		};
		def IMP_testCapture = function (v0 : ref<ptr<int<4>>,f,f,plain>) -> int<4> {
			var ref<real<4>,f,f,plain> v1 = 5.0E-1f;
			var ref<real<4>,f,f,plain> v2 = 2.0E+0f;
			{
				merge(parallel(job[1ul...] => __any_string__parallel_fun(*v0, v1, v2)));
			}
			return *ptr_to_ref(*v0);
		};
		{
			var ref<int<4>,f,f,plain> v0 = 0;
			var ref<int<4>,f,f,plain> v1 = IMP_testCapture(ptr_from_ref(v0));
		}
	)")
	{
		int p = 0;
		int i = testCapture(&p);
	}
}
