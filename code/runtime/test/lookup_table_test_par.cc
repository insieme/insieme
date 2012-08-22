/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include <gtest/gtest.h>
#include <pthread.h>
#include <omp.h>

#include <utils/lookup_tables.h>

#include <impl/error_handling.impl.h>

// horrible hack incoming
uint32 irt_g_error_key = 0;

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

IRT_DEFINE_LOOKUP_TABLE(lookup_test, next_lt, IRT_ID_HASH, TEST_BUCKETS);
IRT_CREATE_LOOKUP_TABLE(lookup_test, next_lt, IRT_ID_HASH, TEST_BUCKETS);

//void lock_check() {
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
	id.thread = omp_get_thread_num();
	id.index = num++;
	id.cached = NULL;
	return id;
}

irt_lookup_test* make_item(float val) {
	irt_lookup_test* item = (irt_lookup_test*)calloc(1,sizeof(irt_lookup_test));
	item->id = dummy_id_generator();
	item->data = val;
	return item;
}

TEST(lookup_tables, parallel_ops) {
	for(int j=0; j<PARALLEL_ITERATIONS; ++j) {
		irt_lookup_test_table_init();

		irt_lookup_test* elems[TEST_ELEMS*100];

		#pragma omp parallel
		{
			#pragma omp for schedule(dynamic,1)
			for(int i=0; i<TEST_ELEMS*50; ++i) {
				elems[i] = make_item(i/10.0f);
				irt_lookup_test_table_insert(elems[i]);
			}
			#pragma omp for schedule(dynamic,1)
			for(int i=0; i<TEST_ELEMS*50; ++i) {
				EXPECT_EQ(elems[i], irt_lookup_test_table_lookup(elems[i]->id));
			}

			// remove every second element and check all afterwards
			#pragma omp for schedule(dynamic,1)
			for(int i=0; i<TEST_ELEMS*50; i+=2) {
				irt_lookup_test_table_remove(elems[i]->id);
			}
			#pragma omp for schedule(dynamic,1)
			for(int i=0; i<TEST_ELEMS*50; ++i) {
				if(i%2 == 0) EXPECT_EQ(0 /* NULL */, irt_lookup_test_table_lookup(elems[i]->id));
				else         EXPECT_EQ(elems[i]    , irt_lookup_test_table_lookup(elems[i]->id));
			}

			// add more and check all again
			#pragma omp for schedule(dynamic,1)
			for(int i=TEST_ELEMS*50; i<TEST_ELEMS*100; ++i) {
				elems[i] = make_item(i/10.0f);
				irt_lookup_test_table_insert(elems[i]);
			}
			#pragma omp for schedule(dynamic,1)
			for(int i=0; i<TEST_ELEMS*100; ++i) {
				if(i<TEST_ELEMS*50) {
					if(i%2 == 0) EXPECT_EQ(0 /* NULL */, irt_lookup_test_table_lookup(elems[i]->id));
					else         EXPECT_EQ(elems[i]    , irt_lookup_test_table_lookup(elems[i]->id));
				} else {
					EXPECT_EQ(elems[i], irt_lookup_test_table_lookup(elems[i]->id));
				}
			}
		}

		// cleanup
		irt_lookup_test_table_cleanup();
		for(int i=0; i<TEST_ELEMS*100; ++i) {
			free(elems[i]);
		}
	}
}
