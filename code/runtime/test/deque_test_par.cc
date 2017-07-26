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

#include "utils/deques.h"

#include "irt_all_impls.h"
#include "standalone.h"

#define TEST_ELEMS 777
#define PARALLEL_ITERATIONS 100

typedef struct _irt_deque_test {
	float data;
	struct _irt_deque_test* next_q;
	struct _irt_deque_test* prev_q;
} irt_deque_test;

IRT_DECLARE_DEQUE(deque_test);
IRT_DEFINE_DEQUE(deque_test, next_q, prev_q);

irt_deque_test* make_item(float val) {
	irt_deque_test* item = (irt_deque_test*)calloc(1, sizeof(irt_deque_test));
	item->data = val;
	return item;
}


TEST(deques, mass_parallel_ops) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_deque_test_deque q;
		irt_deque_test_deque_init(&q);

		irt_deque_test* elems[TEST_ELEMS];

		#pragma omp parallel
		{
		#pragma omp for schedule(dynamic, 1)
			for(int i = 0; i < TEST_ELEMS; ++i) {
				elems[i] = make_item(i / 10.0f);
				irt_deque_test_deque_insert_back(&q, elems[i]);
			}
			#pragma omp for schedule(dynamic, 1)
			for(int i = 0; i < TEST_ELEMS; ++i) {
				irt_deque_test_deque_pop_back(&q);
			}

			EXPECT_EQ(0 /* NULL */, q.start);
			EXPECT_EQ(0 /* NULL */, q.end);
		}

		// cleanup
		irt_deque_test_deque_cleanup(&q);
		for(int i = 0; i < TEST_ELEMS; ++i) {
			free(elems[i]);
		}
	}
}
