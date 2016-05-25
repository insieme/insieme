/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
