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
#include <gtest/gtest.h>
#include <pthread.h>
#ifdef _OPENMP
#include <omp.h>
#endif // _OPENMP

#include "irt_all_impls.h"
#include "standalone.h"

#define TEST_ELEMS 77
#define TEST_BUCKETS 111
#define PARALLEL_ITERATIONS 100

IRT_DECLARE_ID_TYPE(lookup_test);
IRT_MAKE_ID_TYPE(lookup_test);

typedef struct _irt_lookup_test {
	irt_lookup_test_id id;
	float data;
	struct _irt_lookup_test* next_lt;
} irt_lookup_test;

IRT_DEFINE_LOCKED_LOOKUP_TABLE(lookup_test, next_lt, IRT_ID_HASH, TEST_BUCKETS)
IRT_CREATE_LOCKED_LOOKUP_TABLE(lookup_test, next_lt, IRT_ID_HASH, TEST_BUCKETS)

// void lock_check() {
//	printf("\n=================\n");
//	for(int i=0; i<TEST_BUCKETS; ++i) {
//		int res = pthread_spin_trylock(&irt_g_lookup_test_table_locks[i]);
//		printf("Bucket %d locked? %s\n", i, res?"unlocked":"locked");
//		if(res == 0) pthread_spin_unlock(&irt_g_lookup_test_table_locks[i]);
//	}
//}

uint32 num = 0;
#pragma omp threadprivate(num)

irt_lookup_test_id dummy_id_generator() {
	irt_lookup_test_id id;
	id.node = 1;
	#ifdef _OPENMP
	id.thread = omp_get_thread_num();
	#else
	id.thread = 0;
	#endif // _OPENMP
	id.index = num++;
	id.cached = NULL;
	return id;
}

irt_lookup_test* make_item(float val) {
	irt_lookup_test* item = (irt_lookup_test*)calloc(1, sizeof(irt_lookup_test));
	item->id = dummy_id_generator();
	item->data = val;
	return item;
}

TEST(lookup_tables, parallel_ops) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_lookup_test_table_init();

		irt_lookup_test* elems[TEST_ELEMS * 100];

		#pragma omp parallel
		{
		#pragma omp for schedule(dynamic, 1)
			for(int i = 0; i < TEST_ELEMS * 50; ++i) {
				elems[i] = make_item(i / 10.0f);
				irt_lookup_test_table_insert(elems[i]);
			}
			#pragma omp for schedule(dynamic, 1)
			for(int i = 0; i < TEST_ELEMS * 50; ++i) {
				EXPECT_EQ(elems[i], irt_lookup_test_table_lookup(elems[i]->id));
			}

			// remove every second element and check all afterwards
			#pragma omp for schedule(dynamic, 1)
			for(int i = 0; i < TEST_ELEMS * 50; i += 2) {
				irt_lookup_test_table_remove(elems[i]->id);
			}
			#pragma omp for schedule(dynamic, 1)
			for(int i = 0; i < TEST_ELEMS * 50; ++i) {
				if(i % 2 == 0) {
					EXPECT_EQ(0 /* NULL */, irt_lookup_test_table_lookup(elems[i]->id));
				} else {
					EXPECT_EQ(elems[i], irt_lookup_test_table_lookup(elems[i]->id));
				}
			}

			// add more and check all again
			#pragma omp for schedule(dynamic, 1)
			for(int i = TEST_ELEMS * 50; i < TEST_ELEMS * 100; ++i) {
				elems[i] = make_item(i / 10.0f);
				irt_lookup_test_table_insert(elems[i]);
			}
			#pragma omp for schedule(dynamic, 1)
			for(int i = 0; i < TEST_ELEMS * 100; ++i) {
				if(i < TEST_ELEMS * 50) {
					if(i % 2 == 0) {
						EXPECT_EQ(0 /* NULL */, irt_lookup_test_table_lookup(elems[i]->id));
					} else {
						EXPECT_EQ(elems[i], irt_lookup_test_table_lookup(elems[i]->id));
					}
				} else {
					EXPECT_EQ(elems[i], irt_lookup_test_table_lookup(elems[i]->id));
				}
			}
		}

		// cleanup
		irt_lookup_test_table_cleanup();
		for(int i = 0; i < TEST_ELEMS * 100; ++i) {
			free(elems[i]);
		}
	}
}
