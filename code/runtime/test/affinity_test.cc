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

#include "utils/impl/affinity.impl.h"
#include "standalone.h"

TEST(affinity, fill) {
	_irt_hw_set_num_cpus(8);
	irt_g_worker_count = 8;

	irt_affinity_policy pol;
	pol.type = IRT_AFFINITY_FILL;

	for(int i = 0; i < 8; ++i) {
		EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(i, pol), i));
	}
}

TEST(affinity, skip) {
	_irt_hw_set_num_cpus(8);
	irt_g_worker_count = 8;

	irt_affinity_policy pol;
	pol.type = IRT_AFFINITY_SKIP;

	pol.skip_count = 1;
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(0, pol), 0));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(1, pol), 2));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(2, pol), 4));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(3, pol), 6));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(4, pol), 1));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(5, pol), 3));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(6, pol), 5));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(7, pol), 7));

	pol.skip_count = 2;
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(0, pol), 0));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(1, pol), 3));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(2, pol), 6));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(3, pol), 2));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(4, pol), 5));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(5, pol), 1));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(6, pol), 4));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(7, pol), 7));

	pol.skip_count = 3;
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(0, pol), 0));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(1, pol), 4));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(2, pol), 1));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(3, pol), 5));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(4, pol), 2));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(5, pol), 6));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(6, pol), 3));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(7, pol), 7));
}


TEST(affinity, maxdist) {
	_irt_hw_set_num_cpus(8);

	irt_affinity_policy pol;
	pol.type = IRT_AFFINITY_MAX_DISTANCE;

	irt_g_worker_count = 2;
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(0, pol), 4));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(1, pol), 0));

	irt_g_worker_count = 4;
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(0, pol), 2));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(1, pol), 6));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(2, pol), 0));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(3, pol), 4));

	irt_g_worker_count = 8;
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(0, pol), 1));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(1, pol), 3));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(2, pol), 5));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(3, pol), 7));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(4, pol), 0));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(5, pol), 2));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(6, pol), 4));
	EXPECT_TRUE(irt_affinity_mask_is_single_cpu(irt_get_affinity(7, pol), 6));
}

void* dummy_func(void* nada) {
	printf("hello world");
	return NULL;
}

// a test which you will have to step through when debugging
TEST(affinity, manual) {
	// get the initial affinity mask and create the mapping
	irt_affinity_init_physical_mapping(&irt_g_affinity_physical_mapping);
	_irt_print_native_affinity_mask(irt_g_affinity_base_mask);

	// create a thread and set affinity
	irt_thread t;
	irt_thread_create(dummy_func, NULL, &t);
	irt_affinity_mask m = {{1}};
	irt_set_affinity(m, t);
	// this is to check if mask has been set correctly
	irt_set_affinity(m, t);

	// set affinity for main thread
	irt_thread myself;
	irt_thread_get_current(&myself);
	irt_set_affinity(m, myself);
	// clear affinity for main thread
	irt_clear_affinity();
	irt_thread_join(&t);
}
