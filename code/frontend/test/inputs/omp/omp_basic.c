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

int main() {
	int i;

	//===--------------------------------------------------------------------------------------------------------------------------------------- PARALLEL ---===

	#pragma test expect_ir(R"(
		def __any_string__parfun = function () -> unit {
			5;
		};
		{
			{
				merge(parallel( job[1ul...] => __any_string__parfun() ));
			}
		}
	)")
	{
		#pragma omp parallel
		{
			5;
		}
	}

	//===----------------------------------------------------------------------------------------------------------------------------------------- SINGLE ---===

	#pragma test expect_ir(R"(
		def __any_string__loopfun = function () -> unit {
			5;
		};
		def __any_string__parfun = function () -> unit {
			{
				pfor(get_thread_group(0u), 0, 1, 1, (v0 : int<4>, v1 : int<4>, v2 : int<4>) => __any_string__loopfun());
				barrier(get_thread_group(0u));
			}
		};
		{
			{
				merge(parallel( job[1ul...] => __any_string__parfun() ));
			}
		}
	)")
	{
		#pragma omp parallel
		{
			#pragma omp single
			{
				5;
			}
		}
	}

}