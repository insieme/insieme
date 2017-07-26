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

#include <gtest/gtest.h>
#include <pthread.h>
#ifdef _OPENMP
#include <omp.h>
#endif // _OPENMP

#include "irt_all_impls.h"
#include "standalone.h"

#define TEST_COUNT 10000

IRT_DECLARE_ID_TYPE(id_gen_test);
IRT_MAKE_ID_TYPE(id_gen_test);

irt_id_gen_test_id gen_id;
#pragma omp threadprivate(gen_id)

TEST(id_generation, parallel_ops) {
	irt_id_gen_test_id ids[TEST_COUNT];

	#pragma omp parallel
	{
		gen_id.node = 4;
		#ifdef _OPENMP
		gen_id.thread = omp_get_thread_num();
		#else
		gen_id.thread = 0;
		#endif
		#pragma omp parallel
		for(int i = 0; i < TEST_COUNT; ++i) {
			ids[i] = irt_generate_id_gen_test_id(&gen_id);
		}
	}

	for(int i = 0; i < TEST_COUNT; ++i) {
		for(int j = 0; j < TEST_COUNT; ++j) {
			if(i == j) { continue; }
			EXPECT_NE(ids[i].full, ids[j].full);
		}
		EXPECT_EQ(ids[i].node, gen_id.node);
	}
}
