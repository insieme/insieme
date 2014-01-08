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

#include "insieme/utils/cartesian_product_iterator.h"

#include <vector>

#include "insieme/utils/container_utils.h"


namespace insieme {
namespace utils {

	using namespace std;

	TEST(CartesianProductIterator, Simple) {

		// create some input data
		vector<vector<int>> data = {
				{1,2},
				{3,4}
		};

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
		vector<vector<int>> data = {
				{1,2},
				{3,4},
				{5,6,7}
		};

		set<vector<int>> product;
		for(auto cur : cartesian_product_range(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(12u, product.size());

	}

	TEST(CartesianProductIterator, Range2) {

		// create some input data
		set<set<int>> data = {
				{1,2},
				{3,4},
				{5,6,7}
		};

		set<vector<int>> product;
		for(auto cur : cartesian_product_range(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(12u, product.size());

	}

	TEST(CartesianProductIterator, Range3) {

		// create some input data
		vector<set<int>> data = {
				{1,2},
				{3,4},
				{5,6,7}
		};

		set<vector<int>> product;
		for(auto cur : cartesian_product_range(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(12u, product.size());

	}

	TEST(CartesianProductIterator, EmptyList) {

		// create some input data
		vector<vector<int>> data = {
				{1,2},
				{},
				{5,6,7}
		};

		set<vector<int>> product;
		for(auto cur : cartesian_product_range(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(0u, product.size());

	}

	TEST(CartesianProductIterator, EmptyList2) {

		// create some input data
		vector<vector<int>> data = {
				{1,2},
				{5,6,7},
				{}
		};

		set<vector<int>> product;
		for(auto cur : cartesian_product_range(data)) {
			product.insert(cur);
		}

		EXPECT_EQ(0u, product.size());

	}

} // end namespace utils
} // end namespace insieme
