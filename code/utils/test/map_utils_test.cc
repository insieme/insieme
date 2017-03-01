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

#include <map>
#include <boost/functional/hash.hpp>

#include "insieme/utils/map_utils.h"

using namespace insieme::utils;
using namespace insieme::utils::map;

template <typename Map>
void testMap();

TEST(MapUtilsTest, Printing) {
	typedef std::unordered_map<int, int> Map;

	Map map;
	EXPECT_EQ("{}", toString(map));

	map.insert(std::make_pair(1, 2));
	EXPECT_EQ("{1=2}", toString(map));

	map.insert(std::make_pair(3, 4));
	EXPECT_TRUE(toString(map) == "{1=2, 3=4}" || toString(map) == "{3=4, 1=2}");
}

TEST(MapUtilsTest, HashEquals) {
	testMap<std::unordered_map<int, int>>();
	testMap<std::unordered_map<int, int, boost::hash<int>>>();
	testMap<boost::unordered_map<int, int>>();
	testMap<boost::unordered_map<int, int, boost::hash<int>>>();
}

template <typename Map>
void testMap() {
	Map mapA;
	Map mapB;
	EXPECT_EQ(static_cast<size_t>(0), computeHash(mapA));
	EXPECT_EQ(static_cast<size_t>(0), computeHash(mapB));
	EXPECT_TRUE(insieme::utils::map::equal(mapA, mapB));

	mapA.insert(std::make_pair(1, 2));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapA));
	EXPECT_EQ(static_cast<size_t>(0), computeHash(mapB));
	EXPECT_FALSE(insieme::utils::map::equal(mapA, mapB));

	mapB.insert(std::make_pair(2, 4));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapA));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapB));
	EXPECT_NE(computeHash(mapA), computeHash(mapB));
	EXPECT_FALSE(insieme::utils::map::equal(mapA, mapB));

	mapB.erase(2);
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapA));
	EXPECT_EQ(static_cast<size_t>(0), computeHash(mapB));
	EXPECT_FALSE(insieme::utils::map::equal(mapA, mapB));

	mapB.insert(std::make_pair(1, 2));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapA));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapB));
	EXPECT_EQ(computeHash(mapA), computeHash(mapB));
	EXPECT_TRUE(insieme::utils::map::equal(mapA, mapB));

	mapA.insert(std::make_pair(2, 3));
	mapB.insert(std::make_pair(2, 3));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapA));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapB));
	EXPECT_EQ(computeHash(mapA), computeHash(mapB));
	EXPECT_TRUE(insieme::utils::map::equal(mapA, mapB));

	mapA.erase(2);
	mapA.insert(std::make_pair(2, 2));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapA));
	EXPECT_NE(static_cast<size_t>(0), computeHash(mapB));
	EXPECT_NE(computeHash(mapA), computeHash(mapB));
	EXPECT_FALSE(insieme::utils::map::equal(mapA, mapB));
}


TEST(MapUtilsTest, toPointerMap) {
	int a = 2;
	int b = 3;
	int c = 2;

	auto map = toPointerMap<int*, string>(&a, "Hello");

	EXPECT_EQ("Hello", map[&a]);
	EXPECT_EQ("Hello", map[&c]);

	map = toPointerMap<int*, string>(&a, "Hello", &b, "World", &c, "Under");

	EXPECT_EQ("Under", map[&a]);
	EXPECT_EQ("World", map[&b]);

	a = 3;
	EXPECT_EQ("World", map[&a]);
}

TEST(MapUtilsTest, toMap) {
	typedef std::pair<int, string> entry;

	EXPECT_EQ("{1=Hello, 2=World}", toString(toMap(entry(1, "Hello"), entry(2, "World"))));
}
