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

#include "insieme/utils/functional_chain.h"

#include <vector>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"


namespace insieme {
namespace utils {

	using namespace std;

	TEST(FunctionChain, Basic) {
		vector<int> data;

		auto f1 = [&]() { data.push_back(1); };
		auto f2 = [&]() { data.push_back(2); };

		auto f = chain(f1, f2);
		f();
		EXPECT_EQ("[1,2]", toString(data));

		data.clear();
		chain(f2, f1, f1, f2)();
		EXPECT_EQ("[2,1,1,2]", toString(data));
	}

	TEST(FunctionChain, Advanced) {
		vector<int> data;

		auto f1 = [&](int x) { data.push_back(x + 1); };
		auto f2 = [&](int x) { data.push_back(x + 2); };

		chain(f1, f2)(3);
		EXPECT_EQ("[4,5]", toString(data));

		data.clear();
		chain(f2, f1, f1, f2)(10);
		EXPECT_EQ("[12,11,11,12]", toString(data));
	}

} // end namespace utils
} // end namespace insieme
