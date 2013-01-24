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

#include <utils/circular_work_buffers.h>

#include <irt_all_impls.h>
#include <standalone.h>

#define PARALLEL_ITERATIONS 100
#define TEST_ITERATIONS 10000

#define NUM_THREADS 12

#define NUM_MULTI_WIS 12

TEST(circular_work_buffers, token_passing_single) {
	for(int j=0; j<PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb;
		irt_cwb_init(&cwb);
		uint32 num = 0;

		irt_work_item wi;
		wi.id.index = 5;
		wi.id.thread = 1;
		wi.id.node = 0;
		irt_cwb_push_front(&cwb, &wi);

		#pragma omp parallel
		{
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb)) {
					num++;
					irt_cwb_push_front(&cwb, swi);
				} 
			}
		}
	}
}

TEST(circular_work_buffers, token_passing_multi_self) {
	for(int j=0; j<PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb[NUM_THREADS];
		for(int i=0; i<NUM_THREADS; ++i) irt_cwb_init(&cwb[i]);
		uint32 num = 0;

		irt_work_item wi;
		wi.id.index = 5;
		wi.id.thread = 1;
		wi.id.node = 0;
		irt_cwb_push_front(&cwb[0], &wi);

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb[rand()%NUM_THREADS])) {
					num++;
					irt_cwb_push_front(&cwb[omp_get_thread_num()], swi);
				} 
			}
		}
	}
}

TEST(circular_work_buffers, token_passing_multi_rand) {
	for(int j=0; j<PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb[NUM_THREADS];
		for(int i=0; i<NUM_THREADS; ++i) irt_cwb_init(&cwb[i]);
		uint32 num = 0;

		irt_work_item wi;
		wi.id.index = 5;
		wi.id.thread = 1;
		wi.id.node = 0;
		irt_cwb_push_front(&cwb[0], &wi);

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb[rand()%NUM_THREADS])) {
					num++;
					irt_cwb_push_front(&cwb[rand()%NUM_THREADS], swi);
				} 
			}
		}
	}
}

TEST(circular_work_buffers, token_passing_multi_dual_rand) {
	for(int j=0; j<PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb[NUM_THREADS];
		for(int i=0; i<NUM_THREADS; ++i) irt_cwb_init(&cwb[i]);
		uint32 num = 0;

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
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb[rand()%NUM_THREADS])) {
					irt_atomic_inc(&num);
					irt_cwb_push_front(&cwb[rand()%NUM_THREADS], swi);
				} 
			}
		}

		uint32 nwi = 0;
		for(int i=0; i<NUM_THREADS; ++i) {
			nwi += irt_cwb_size(&cwb[i]);
		}
		EXPECT_EQ(2, nwi);
	}
}

TEST(circular_work_buffers, token_passing_multi_multi_rand) {
	for(int j=0; j<PARALLEL_ITERATIONS; ++j) {
		irt_circular_work_buffer cwb[NUM_THREADS];
		for(int i=0; i<NUM_THREADS; ++i) irt_cwb_init(&cwb[i]);
		uint32 num = 0;

		irt_work_item wis[NUM_MULTI_WIS];
		for(int i=0; i<NUM_MULTI_WIS; ++i) {
			wis[i].id.index = 1+i;
			wis[i].id.thread = 1;
			wis[i].id.node = 0;
			irt_cwb_push_front(&cwb[rand()%NUM_THREADS], &wis[i]);
		}

		#pragma omp parallel num_threads(NUM_THREADS)
		{
			while(num < TEST_ITERATIONS) {
				if(irt_work_item* swi = irt_cwb_pop_back(&cwb[rand()%NUM_THREADS])) {
					irt_atomic_inc(&num);
					irt_cwb_push_front(&cwb[rand()%NUM_THREADS], swi);
				} 
			}
		}

		uint32 nwi = 0;
		for(int i=0; i<NUM_THREADS; ++i) {
			nwi += irt_cwb_size(&cwb[i]);
		}
		EXPECT_EQ(NUM_MULTI_WIS, nwi);
	}
}
