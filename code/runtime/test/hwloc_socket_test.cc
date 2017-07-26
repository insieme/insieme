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

#define IRT_LIBRARY_MAIN
#define IRT_LIBRARY_NO_MAIN_FUN
#include "irt_library.hxx"

extern "C" {
#include "abstraction/sockets.h"
};

#define N 500

uint32 count_mask_bits(cpu_set_t cset) {
	uint32 count = 0;
	for(uint32 i = 0; i < CPU_SETSIZE; ++i) {
		if(CPU_ISSET(i, &cset)) { ++count; }
	}
	return count;
}

TEST(HwlocSocketTest, Simple) {
#ifdef IRT_USE_HWLOC
	irt::init(36);
	irt::run([]() {

		EXPECT_GT(irt_hwloc_get_num_sockets(), 0);

		for(uint32 socket = 0; socket < irt_hwloc_get_num_sockets(); socket++) {
			// std::cout << "Socket " << socket
			//	<< " - cores: " << irt_hwloc_get_num_cores_in_socket(socket)
			//	<< " - logical: " << irt_hwloc_get_num_logical_processors_in_socket(socket) << "\n";

			EXPECT_GT(irt_hwloc_get_num_cores_in_socket(socket), 0);
			EXPECT_GT(irt_hwloc_get_num_logical_processors_in_socket(socket), 0);

			// set all workers to socket
			for(uint32 woid = 0; woid < irt_g_worker_count; woid++) {
				irt_worker_move_to_socket(irt_g_workers[woid], socket);

				// check number of logical cores in set
				// cpu_set_t cset;
				// EXPECT_EQ(pthread_self(), irt_g_workers[irt_worker_get_current()->id.thread]->thread);
				// EXPECT_EQ(pthread_getaffinity_np(irt_g_workers[woid]->thread, CPU_SETSIZE, &cset), 0);
				// EXPECT_EQ(count_mask_bits(cset), irt_hwloc_get_num_logical_processors_in_socket(socket));
			}

			irt::merge(irt::parallel([] {
				double x = 0.0;
				for(int i = 0; i < N; i++) {
					for(int j = 0; j < N; j++) {
						x += 0.1;
					}
				}
			}));
		}
	});
	irt::shutdown();
	#endif // IRT_USE_HWLOC
}

TEST(HwlocSocketTest, SetDopPerSocket) {
#ifdef IRT_USE_HWLOC
	irt::init(36);
	if(irt_hwloc_get_num_sockets() >= 2) {
		irt::run([]() {
			auto work = []() {
				irt::merge(irt::parallel([] {
					irt::master([] { std::cout << "Bla: " << irt::group_size() << std::endl; });
					double x = 0.0;
					for(int i = 0; i < N; i++) {
						for(int j = 0; j < N; j++) {
							x += 0.1;
						}
					}
				}));
			};

			{
				uint32 dops[] = {5, 0};
				irt_scheduling_set_dop_per_socket(2, dops);
				work();
			}
			{
				uint32 dops[] = {0, 3};
				irt_scheduling_set_dop_per_socket(2, dops);
				work();
			}
			{
				uint32 dops[] = {3, 7};
				irt_scheduling_set_dop_per_socket(2, dops);
				work();
			}
		});
	}
	irt::shutdown();
	#endif // IRT_USE_HWLOC
}
