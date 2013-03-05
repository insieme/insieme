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

#include <irt_all_impls.h>
#include <standalone.h>

#include <irt_maintenance.h>

#define TEST_TIME_MS (5ULL * 1000)
#define THREADS 12
#define ITERATIONS 1

typedef struct {
	uint64 last_ms;
	uint64 sliding_avg_ms;
	uint64 interval;
	uint64 call_count;
	bool cancel;
} mt_timing;

#define EXPECT_INTERVAL(min, max, val) \
	EXPECT_GE(val, min); \
	EXPECT_LE(val, max); 

uint64 test_mt(void* data) {
	mt_timing *timing = (mt_timing*)data;
	if(timing->last_ms != 0) {
		int64 time_diff = irt_time_ms() - timing->last_ms;
		timing->sliding_avg_ms = (timing->sliding_avg_ms*4 + time_diff)/5;
		//printf("interval: %8lu, time: %8lu, avg: %8lu\n", timing->interval, time_diff, timing->sliding_avg_ms);
	}
	timing->last_ms = irt_time_ms();
	timing->call_count++;
	return timing->cancel ? 0 : timing->interval;
}

TEST(maintenance, timing) {
	irt_maintenance_init();
	
	mt_timing mt_150_t = { 0, 150, 150, 0, false };
	irt_maintenance_lambda mt_150 = { test_mt, &mt_150_t, 150, NULL };
	irt_maintenance_register(&mt_150);
	
	mt_timing mt_20_t = { 0, 20, 20, 0, false };
	irt_maintenance_lambda mt_20 = { test_mt, &mt_20_t, 20, NULL };
	irt_maintenance_register(&mt_20);

	mt_timing mt_1000_t = { 0, 1000, 1000, 0, false };
	irt_maintenance_lambda mt_1000 = { test_mt, &mt_1000_t, 1000, NULL };
	irt_maintenance_register(&mt_1000);

	irt_busy_nanosleep(TEST_TIME_MS * 1000 * 1000);
	
	EXPECT_INTERVAL(TEST_TIME_MS/150-2, (TEST_TIME_MS*2)/150, mt_150_t.call_count);
	EXPECT_INTERVAL(TEST_TIME_MS/20-2, (TEST_TIME_MS*2)/20, mt_20_t.call_count);
	EXPECT_INTERVAL(TEST_TIME_MS/1000-2, (TEST_TIME_MS*2)/1000, mt_1000_t.call_count);
	EXPECT_INTERVAL(150/2, 150*12/10, mt_150_t.sliding_avg_ms);
	EXPECT_INTERVAL(20/2, 20*12/10, mt_20_t.sliding_avg_ms);
	EXPECT_INTERVAL(1000/2, 1000*12/10, mt_1000_t.sliding_avg_ms);

	irt_maintenance_cleanup();
}


TEST(maintenance, efficiency) {
	irt_maintenance_init();

	EXPECT_EQ(IRT_MAINTENANCE_SLOTS-1, irt_g_maintenance_min_interval_slot);
	
	mt_timing mt_2000_t = { 0, 2000, 2000, 0, false };
	irt_maintenance_lambda mt_2000 = { test_mt, &mt_2000_t, 2000, NULL };
	irt_maintenance_register(&mt_2000);
	
	EXPECT_EQ(10, irt_g_maintenance_min_interval_slot);

	mt_timing mt_400_t = { 0, 400, 400, 0, false };
	irt_maintenance_lambda mt_400 = { test_mt, &mt_400_t, 400, NULL };
	irt_maintenance_register(&mt_400);
	
	irt_busy_nanosleep((TEST_TIME_MS/2) * 1000 * 1000);
	
	EXPECT_EQ(8, irt_g_maintenance_min_interval_slot);

	mt_400_t.cancel = true;

	irt_busy_nanosleep((TEST_TIME_MS/2) * 1000 * 1000);
	
	EXPECT_EQ(10, irt_g_maintenance_min_interval_slot);
	
	EXPECT_INTERVAL(TEST_TIME_MS/400/2-2, (TEST_TIME_MS*2)/400/2, mt_400_t.call_count);
	EXPECT_INTERVAL(TEST_TIME_MS/2000-2, (TEST_TIME_MS*2)/2000, mt_2000_t.call_count);
	EXPECT_INTERVAL(400/2, 400*12/10, mt_400_t.sliding_avg_ms);
	EXPECT_INTERVAL(2000/2, 2000*12/10, mt_2000_t.sliding_avg_ms);

	irt_maintenance_cleanup();
}

TEST(maintenance, parallel) {

	#pragma omp parallel num_threads(THREADS)
	{
		#pragma omp master
		irt_maintenance_init();
		#pragma omp barrier

		// generate ITERATIONS random intervals per thread, and register them
		mt_timing timings[ITERATIONS];
		irt_maintenance_lambda lambdas[ITERATIONS];
		uint32 r_seed = irt_time_ticks() + omp_get_thread_num();
		for(uint32 i=0; i<ITERATIONS; ++i) {
			uint64 interval = rand_r(&r_seed) % (TEST_TIME_MS/3) + 5;
			timings[i] = (mt_timing){ 0, interval, interval, 0, false };
			lambdas[i] = (irt_maintenance_lambda){ test_mt, &timings[i], interval, NULL };
			irt_maintenance_register(&lambdas[i]);
		}

		// wait for execution to finish and check results
		irt_busy_nanosleep(TEST_TIME_MS * 1000 * 1000);
		for(uint32 i=0; i<ITERATIONS; ++i) {
			EXPECT_INTERVAL((((TEST_TIME_MS/timings[i].interval)*9)/10)-1, 
							(TEST_TIME_MS*2)/timings[i].interval+1, timings[i].call_count);
			EXPECT_INTERVAL(((timings[i].interval/2)*9)/10, timings[i].interval*12/10, timings[i].sliding_avg_ms);
		}
		
		#pragma omp barrier
		#pragma omp master
		irt_maintenance_cleanup();
	}
}

TEST(maintenance, load) {
	irt_maintenance_init();
	
	mt_timing mt_150_t = { 0, 150, 150, 0, false };
	irt_maintenance_lambda mt_150 = { test_mt, &mt_150_t, 150, NULL };
	irt_maintenance_register(&mt_150);
	
	mt_timing mt_20_t = { 0, 20, 20, 0, false };
	irt_maintenance_lambda mt_20 = { test_mt, &mt_20_t, 20, NULL };
	irt_maintenance_register(&mt_20);

	mt_timing mt_1000_t = { 0, 1000, 1000, 0, false };
	irt_maintenance_lambda mt_1000 = { test_mt, &mt_1000_t, 1000, NULL };
	irt_maintenance_register(&mt_1000);

	#pragma omp parallel
	{
		irt_busy_nanosleep(TEST_TIME_MS * 1000 * 1000);
		
		#pragma omp barrier
	}
	
	EXPECT_INTERVAL(TEST_TIME_MS/150-2, (TEST_TIME_MS*2)/150, mt_150_t.call_count);
	EXPECT_INTERVAL(TEST_TIME_MS/20-2, (TEST_TIME_MS*2)/20, mt_20_t.call_count);
	EXPECT_INTERVAL(TEST_TIME_MS/1000-2, (TEST_TIME_MS*2)/1000, mt_1000_t.call_count);
	EXPECT_INTERVAL(150/2, 150*12/10, mt_150_t.sliding_avg_ms);
	EXPECT_INTERVAL(20/2, 20*12/10, mt_20_t.sliding_avg_ms);
	EXPECT_INTERVAL(1000/2, 1000*12/10, mt_1000_t.sliding_avg_ms);

	irt_maintenance_cleanup();
}
