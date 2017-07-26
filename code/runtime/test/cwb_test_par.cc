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

#include <utils/circular_work_buffers.h>

#include <irt_all_impls.h>
#include <standalone.h>

#define PARALLEL_ITERATIONS 100
#define TEST_ITERATIONS 10000

#define NUM_THREADS 8

#define NUM_MULTI_WIS 10

TEST(circular_work_buffers, token_passing_single) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb;
		irt_cwb_init(&cwb);
		volatile uint32 num = 0;

		irt_work_item wi;
		wi.id.index = 5;
		wi.id.thread = 1;
		wi.id.node = 0;
		irt_cwb_push_front(&cwb, &wi);

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb)) {
					irt_atomic_inc(&num, uint32);
					irt_cwb_push_front(&cwb, swi);
				}
			}
		}
	}
}

TEST(circular_work_buffers, token_passing_single_dual) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb;
		irt_cwb_init(&cwb);
		uint32 num = 0;

		irt_work_item wi;
		wi.id.index = 5;
		wi.id.thread = 1;
		wi.id.node = 0;
		irt_cwb_push_front(&cwb, &wi);

		irt_work_item wi2;
		wi2.id.index = 9;
		wi2.id.thread = 1;
		wi2.id.node = 0;
		irt_cwb_push_front(&cwb, &wi2);

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb)) {
					irt_cwb_push_front(&cwb, swi);
					irt_atomic_inc(&num, uint32);
				}
			}
		}

		EXPECT_EQ(2, irt_cwb_size(&cwb));
	}
}

#ifdef _OPENMP
TEST(circular_work_buffers, token_passing_multi_self) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb[NUM_THREADS];
		for(int i = 0; i < NUM_THREADS; ++i) {
			irt_cwb_init(&cwb[i]);
		}
		volatile uint32 num = 0;

		irt_work_item wi;
		wi.id.index = 5;
		wi.id.thread = 1;
		wi.id.node = 0;
		irt_cwb_push_front(&cwb[0], &wi);

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			uint32 rand_seed = 123;
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb[rand_r(&rand_seed) % NUM_THREADS])) {
					irt_cwb_push_front(&cwb[omp_get_thread_num()], swi);
					irt_atomic_inc(&num, uint32);
				}
			}
		}
	}
}
#endif // _OPENMP

TEST(circular_work_buffers, token_passing_multi_rand) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb[NUM_THREADS];
		for(int i = 0; i < NUM_THREADS; ++i) {
			irt_cwb_init(&cwb[i]);
		}
		volatile uint32 num = 0;

		irt_work_item wi;
		wi.id.index = 5;
		wi.id.thread = 1;
		wi.id.node = 0;
		irt_cwb_push_front(&cwb[0], &wi);

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			uint32 rand_seed = 123;
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb[rand_r(&rand_seed) % NUM_THREADS])) {
					irt_cwb_push_front(&cwb[rand_r(&rand_seed) % NUM_THREADS], swi);
					irt_atomic_inc(&num, uint32);
				}
			}
		}
	}
}

TEST(circular_work_buffers, token_passing_multi_dual_rand) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb[NUM_THREADS];
		for(int i = 0; i < NUM_THREADS; ++i) {
			irt_cwb_init(&cwb[i]);
		}
		volatile uint32 num = 0;

		irt_work_item wi1;
		wi1.id.index = 5;
		wi1.id.thread = 1;
		wi1.id.node = 0;
		irt_cwb_push_front(&cwb[0], &wi1);

		irt_work_item wi2;
		wi2.id.index = 9;
		wi2.id.thread = 1;
		wi2.id.node = 0;
		irt_cwb_push_front(&cwb[0], &wi2);

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			uint32 rand_seed = 123;
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb[rand_r(&rand_seed) % NUM_THREADS])) {
					irt_cwb_push_front(&cwb[rand_r(&rand_seed) % NUM_THREADS], swi);
					irt_atomic_inc(&num, uint32);
				}
			}
		}

		uint32 nwi = 0;
		for(int i = 0; i < NUM_THREADS; ++i) {
			nwi += irt_cwb_size(&cwb[i]);
		}
		EXPECT_EQ(2, nwi);
	}
}

TEST(circular_work_buffers, token_passing_multi_multi_rand) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb[NUM_THREADS];
		for(int i = 0; i < NUM_THREADS; ++i) {
			irt_cwb_init(&cwb[i]);
		}
		volatile uint32 num = 0;

		irt_work_item wis[NUM_MULTI_WIS];
		for(int i = 0; i < NUM_MULTI_WIS; ++i) {
			wis[i].id.index = 1 + i;
			wis[i].id.thread = 1;
			wis[i].id.node = 0;
			irt_cwb_push_front(&cwb[rand() % NUM_THREADS], &wis[i]);
		}

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			uint32 rand_seed = 123;
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb[rand_r(&rand_seed) % NUM_THREADS])) {
					irt_cwb_push_front(&cwb[rand_r(&rand_seed) % NUM_THREADS], swi);
					irt_atomic_inc(&num, uint32);
				}
			}
		}

		uint32 nwi = 0;
		for(int i = 0; i < NUM_THREADS; ++i) {
			nwi += irt_cwb_size(&cwb[i]);
		}
		EXPECT_EQ(NUM_MULTI_WIS, nwi);
	}
}

#ifdef _OPENMP
TEST(circular_work_buffers, token_passing_bench) {
	for(int j = 0; j < PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb;
		irt_cwb_init(&cwb);

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			irt_work_item wi;
			wi.id.index = omp_get_thread_num();
			wi.id.thread = 1;
			wi.id.node = 0;
			irt_work_item* wp = &wi;
			for(int i = 0; i < TEST_ITERATIONS / omp_get_num_threads(); ++i) {
				irt_cwb_push_back(&cwb, wp);
				wp = irt_cwb_pop_back(&cwb);
			}
		}
	}
}
#endif // _OPENMP
