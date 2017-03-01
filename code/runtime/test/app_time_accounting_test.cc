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
#ifndef _GLIBCXX_USE_NANOSLEEP
#define _GLIBCXX_USE_NANOSLEEP
#endif

#include <gtest/gtest.h>
#include <future>
#include <chrono>

#define IRT_ENABLE_APP_TIME_ACCOUNTING

#define IRT_LIBRARY_MAIN
#define IRT_LIBRARY_NO_MAIN_FUN
#include "irt_library.hxx"

#define N 9000

TEST(AppTimeAccounting, Simple) {
	irt::init(4);
	double init_t = irt_time_rts_get_total();
	irt::run([]() {
		irt::merge(irt::parallel([] {
			double x = 0.0, last_app_t = 0.0;
			for(int i = 0; i < N; i++) {
				for(int j = 0; j < N; j++) {
					x += 0.1;
				}
				irt::master([&] {
					// check that time always increases when wis run
					double app_t = irt_time_wis_get_total();
					EXPECT_GT(app_t, last_app_t);
				});
			}
			irt::master([x] { std::cout << "x: " << x << "\n"; });
		}));
	});
	double wi_t = irt_time_wis_get_total();
	double rs_t = irt_time_rts_get_total() - init_t;
	std::cout << "init_t: " << std::fixed << std::setw(11) << std::setprecision(2) << init_t << "\n";
	std::cout << "wi_t:   " << std::fixed << std::setw(11) << std::setprecision(2) << wi_t << "\n";
	std::cout << "rs_t:   " << std::fixed << std::setw(11) << std::setprecision(2) << rs_t << "\n";
	// check that time doesn't increase when non-wis run in process
	EXPECT_EQ(irt_time_wis_get_total(), wi_t);
	// check that wi time is not larger than total process time
	EXPECT_LE(wi_t, rs_t);
	irt::shutdown();
}

TEST(AppTimeAccounting, AppProgress) {
	bool run = true;
	uint64 lastProgress = 0;
	auto extThread = std::async(std::launch::async, [&]() {
		while(run) {
			uint64 curProgress = irt_app_progress_get();
			EXPECT_LE(lastProgress, curProgress);
			std::cout << "app progress: " << curProgress << "\n";
			std::this_thread::sleep_for(std::chrono::milliseconds(100));
		}
	});

	irt::init(4);
	irt::run([]() {
		irt::merge(irt::parallel([] {
			double x = 0.0, last_app_t = 0.0;
			for(int i = 0; i < N; i++) {
				for(int j = 0; j < N; j++) {
					x += 0.1;
				}
				irt::barrier();
			}
			irt::master([x] { std::cout << "x: " << x << "\n"; });
		}));
	});
	irt::shutdown();

	run = false;
	extThread.wait();
}
