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
#define IRT_LIBRARY_MAIN
#include "irt_library.hxx"

#include <iostream>
#include <vector>
#include <chrono>
#include <atomic>

int main(int argc, char** argv) {
	auto x = irt::parallel(2, [argc]() { std::cout << "Hello World " << argc << "\n"; });
	irt::merge(x);

	irt::pfor(1, 10, 1, [](int64 it) { std::cout << "Parallel Iteration " << it << " Thread: " << irt::thread_num() << "\n"; });

	std::vector<int> v{1, 2, 3, 4, 5};
	irt::pmap(v, [](const int& elem) { return elem * 2; });
	for(int i : v) {
		std::cout << i << ", ";
	}
	std::cout << "\n";

	#ifdef BENCHMARK
	std::chrono::high_resolution_clock highc;
	const int64 repeats = 10000000000ll;

	irt::merge(irt::parallel(8, [&] {
		decltype(highc.now()) start, end;
		{
			irt::master([&] { start = highc.now(); });
			uint64 sum;
			irt::pfor_impl(1, repeats, 1, [&](int64 it) { sum += 1; });
			irt::master([&] {
				end = highc.now();
				std::cout << "Individual Time: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count() << "ms\n";
			});
		}

		{
			irt::master([&] { start = highc.now(); });
			uint64 sum;
			irt::pfor_s_impl(1, repeats, 1, [&](int64 start, int64 end, int64 step) {
				for(int64 i = start; i < end; i += step) {
					sum += 1;
				}
			});
			irt::master([&] {
				end = highc.now();
				std::cout << "Sliced Time: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count() << "ms\n";
			});
		}

		{
			irt::master([&] { start = highc.now(); });
			uint64 sum;
			irt::times(repeats, [&](int64 it) { sum += 1; });
			irt::master([&] {
				end = highc.now();
				std::cout << "Generated Time: " << std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count() << "ms\n";
			});
		}
	}));
	#endif
	return 0;
}
