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

#include <gtest/gtest.h>
#include <pthread.h>

#include "irt_all_impls.h"
#include "standalone.h"

#define TEST_ELEMS 77
#define TEST_BUCKETS 111

IRT_DECLARE_ID_TYPE(lookup_test);
IRT_MAKE_ID_TYPE(lookup_test);

typedef struct _irt_lookup_test {
	irt_lookup_test_id id;
	float data;
	struct _irt_lookup_test* next_lt;
} irt_lookup_test;

IRT_DEFINE_LOCKED_LOOKUP_TABLE(lookup_test, next_lt, IRT_ID_HASH, TEST_BUCKETS)
IRT_CREATE_LOCKED_LOOKUP_TABLE(lookup_test, next_lt, IRT_ID_HASH, TEST_BUCKETS)

uint32 num = 0;

irt_lookup_test_id dummy_id_generator() {
	irt_lookup_test_id id;
	id.node = 1;
	id.thread = 0;
	id.index = num++;
	id.cached = NULL;
	id.id_type = IRT_ID_lookup_test;
	return id;
}

irt_lookup_test* make_item(float val) {
	irt_lookup_test* item = (irt_lookup_test*)calloc(1, sizeof(irt_lookup_test));
	item->id = dummy_id_generator();
	item->data = val;
	return item;
}

TEST(lookup_tables, sequential_ops) {
	irt_lookup_test_table_init();

	// using 0 instead of NULL to prevent GCC warning
	EXPECT_EQ(0 /* NULL */, irt_lookup_test_table_lookup(dummy_id_generator()));
	EXPECT_EQ(0 /* NULL */, irt_lookup_test_table_lookup(dummy_id_generator()));

	irt_lookup_test* elems[TEST_ELEMS];

	for(int i = 0; i < TEST_ELEMS; ++i) {
		elems[i] = make_item(i / 10.0f);
		irt_lookup_test_table_insert(elems[i]);
	}
	for(int i = TEST_ELEMS - 1; i >= 0; --i) {
		EXPECT_EQ(elems[i], irt_lookup_test_table_lookup(elems[i]->id));
	}

	// remove single element, check if successful
	irt_lookup_test_table_remove(elems[0]->id);
	EXPECT_EQ(0 /* NULL */, irt_lookup_test_table_lookup(elems[0]->id));

	// remove every second element and check all afterwards
	for(int i = 1; i < TEST_ELEMS; i += 2) {
		irt_lookup_test_table_remove(elems[i]->id);
	}
	for(int i = 1; i < TEST_ELEMS; ++i) {
		if(i % 2 == 1) {
			EXPECT_EQ(0 /* NULL */, irt_lookup_test_table_lookup(elems[i]->id));
		} else {
			EXPECT_EQ(elems[i], irt_lookup_test_table_lookup(elems[i]->id));
		}
	}

	irt_lookup_test* elems2[TEST_ELEMS * 10];

	// check open hashing
	for(int i = 0; i < TEST_ELEMS * 10; ++i) {
		elems2[i] = make_item(i * 10.0f);
		irt_lookup_test_table_insert(elems2[i]);
	}
	for(int i = 0; i < TEST_ELEMS * 10; ++i) {
		EXPECT_EQ(elems2[i], irt_lookup_test_table_lookup(elems2[i]->id));
	}

	// cleanup
	irt_lookup_test_table_cleanup();
	for(int i = 0; i < TEST_ELEMS; ++i) {
		free(elems[i]);
	}
	for(int i = 0; i < TEST_ELEMS * 10; ++i) {
		free(elems2[i]);
	}
}
