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

#include <map>
#include <boost/functional/hash.hpp>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/hash_utils.h"

using namespace insieme::utils;

TEST(HashUtils, combineHashes) {
	// create some "objects"

	int x = 15;
	long y = 60;
	bool b = true;
	std::string str = "Hello World!";

	std::size_t hash;
	// compare manually and automatically combined hashes
	hash = 0;
	EXPECT_EQ(hash, combineHashes());

	boost::hash_combine(hash, x);
	EXPECT_EQ(hash, combineHashes(x));

	boost::hash_combine(hash, y);
	EXPECT_EQ(hash, combineHashes(x, y));

	boost::hash_combine(hash, b);
	EXPECT_EQ(hash, combineHashes(x, y, b));

	boost::hash_combine(hash, str);
	EXPECT_EQ(hash, combineHashes(x, y, b, str));
}

TEST(HashUtils, hashLists) {
	// create some list of elements
	auto list = toVector(15, 16, 22);

	// hash list manually
	std::size_t hash = 0;
	boost::hash_combine(hash, list[0]);
	boost::hash_combine(hash, list[1]);
	boost::hash_combine(hash, list[2]);

	EXPECT_EQ(hash, hashList(list));
}
