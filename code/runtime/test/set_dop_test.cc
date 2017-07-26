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
#include <string>
#include <vector>
#include <sstream>
#include <cmath>
#include <future>
#include <iostream>
using std::string;
using std::ostream;
using std::vector;

#define MAX_PARA 4

#define N 500

double A[N][N];
double B[N][N];
double C[N][N];

#ifdef IRT_USE_HWLOC
// HWLOC is really flowers slow on hudson
// (580 microseconds per runtime startup/shutdown without HWLOC, 30 *milli*seconds with)
#undef IRT_USE_HWLOC
#endif

#define IRT_LIBRARY_MAIN
#define IRT_LIBRARY_NO_MAIN_FUN
#include "irt_library.hxx"

extern "C" {
#include "irt_scheduling.h"
};

TEST(SetDopTest, MMul) {
	irt::init(MAX_PARA);
	irt::run([]() {

		for(int i = 0; i < N; i++) {
			for(int j = 0; j < N; j++) {
				A[i][j] = i * j;
				B[i][j] = (i == j) ? 1 : 0;
			}
		}

		uint32 expected = 0;
		auto workload = [&]() {
			EXPECT_EQ(expected, irt::group_size());
			irt::pfor_impl(0, N, 1, [&](uint64 i) {
				for(int j = 0; j < N; j++) {
					for(int k = 0; k < N; k++) {
						C[i][j] += A[i][k] * B[k][j];
					}
				}
			});
			EXPECT_EQ(expected, irt::group_size());
		};

		irt_scheduling_set_dop(MAX_PARA);
		expected = MAX_PARA;
		irt::merge(irt::parallel(workload));

		irt_scheduling_set_dop(MAX_PARA / 2);
		expected = MAX_PARA / 2;
		irt::merge(irt::parallel(workload));

		irt_scheduling_set_dop(MAX_PARA / 4);
		expected = MAX_PARA / 4;
		irt::merge(irt::parallel(workload));

		irt_scheduling_set_dop(MAX_PARA);
		expected = MAX_PARA;
		irt::merge(irt::parallel(workload));
	});
	irt::shutdown();
}

TEST(SetDopTest, ReducedEnd) {
	irt::init(MAX_PARA);
	irt::run([]() { irt_scheduling_set_dop(1); });
	irt::shutdown();
}

TEST(SetDopTest, Stability) {
	for(int i = 0; i < N; ++i) {
		irt::init(MAX_PARA);
		irt::run([]() { irt_scheduling_set_dop(rand() % MAX_PARA + 1); });
		irt::shutdown();
	}
}

TEST(SetDopTest, External) {
	for(int i = 0; i < N; ++i) {
		irt::init(MAX_PARA);
		volatile bool run = true;
		auto f = std::async(std::launch::async, [&run] {
			while(run) {
				irt_scheduling_set_dop(rand() % MAX_PARA + 1);
				irt_busy_nanosleep(50 * 1000);
			};
			return true;
		});
		irt::run([]() {
			double sum = 0;
			for(int i = 0; i < N; ++i) {
				sum += sin(rand() % 10 / 10.0);
			}
			printf("%f", sum);
		});
		run = false;
		EXPECT_TRUE(f.get());
		irt::shutdown();
	}
}
