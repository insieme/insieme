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

#include "insieme/utils/cartesian_product_iterator.h"

#include <vector>

#include "insieme/utils/container_utils.h"


namespace insieme {
namespace utils {

	using namespace std;

	TEST(CartesianProductIterator, Simple) {
		// create some input data
		vector<vector<int>> data = {{1, 2}, {3, 4}};

		auto start = cartesian_product_begin(data);
		auto end = cartesian_product_end(data);


		auto cur = start;
		EXPECT_EQ("[1,3]", toString(*cur));
		cur++;
		EXPECT_NE(cur, end);

		EXPECT_EQ("[2,3]", toString(*cur));
		cur++;
		EXPECT_NE(cur, end);

		EXPECT_EQ("[1,4]", toString(*cur));
		cur++;
		EXPECT_NE(cur, end);

		EXPECT_EQ("[2,4]", toString(*cur));
		cur++;
		EXPECT_EQ(cur, end);

		EXPECT_EQ(start, start);
		EXPECT_EQ(end, end);
		EXPECT_NE(start, end);
	}

	TEST(CartesianProductIterator, Range) {
		// create some input data
		vector<vector<int>> data = {{1, 2}, {3, 4}, {5, 6, 7}};

		set<vector<int>> product;
		for(auto cur : cartesian_product(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(12u, product.size());
	}

	TEST(CartesianProductIterator, Range2) {
		// create some input data
		set<set<int>> data = {{1, 2}, {3, 4}, {5, 6, 7}};

		set<vector<int>> product;
		for(auto cur : cartesian_product(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(12u, product.size());
	}

	TEST(CartesianProductIterator, Range3) {
		// create some input data
		vector<set<int>> data = {{1, 2}, {3, 4}, {5, 6, 7}};

		set<vector<int>> product;
		for(auto cur : cartesian_product(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(12u, product.size());
	}

	TEST(CartesianProductIterator, EmptyList) {
		// create some input data
		vector<vector<int>> data = {{1, 2}, {}, {5, 6, 7}};

		set<vector<int>> product;
		for(auto cur : cartesian_product(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(0u, product.size());
	}

	TEST(CartesianProductIterator, EmptyList2) {
		// create some input data
		vector<vector<int>> data = {{1, 2}, {5, 6, 7}, {}};

		set<vector<int>> product;
		for(auto cur : cartesian_product(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(0u, product.size());
	}

} // end namespace utils
} // end namespace insieme
