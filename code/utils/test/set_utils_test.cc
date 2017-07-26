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

#include <ctime>
#include <memory>

#include <boost/functional/hash.hpp>

#include "insieme/utils/set_utils.h"

// using boost::unordered_set;
using namespace insieme::utils::set;

typedef std::unordered_set<int, boost::hash<int>> Set;
// typedef std::unordered_set<int> Set;
// typedef boost::unordered_set<int> Set;

TEST(SetUtilsTest, toSet) {
	Set set = toSet<Set>(1, 3, 4, 2, 1);

	Set ref;
	ref.insert(1);
	ref.insert(3);
	ref.insert(4);
	ref.insert(2);
	ref.insert(1);

	EXPECT_EQ(static_cast<std::size_t>(4), set.size());
	EXPECT_EQ(static_cast<std::size_t>(4), ref.size());
	EXPECT_EQ(set, ref);
}

TEST(SetUtilsTest, Merge) {
	Set setA;
	setA.insert(1);
	setA.insert(2);

	Set setB;
	setB.insert(3);

	Set merged = merge(setA, setB);

	Set setRef;
	setRef.insert(1);
	setRef.insert(2);
	setRef.insert(3);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ(setRef, merged);
}

TEST(SetUtilsTest, Intersect) {
	Set setA;
	setA.insert(1);
	setA.insert(2);

	Set setB;
	setB.insert(1);

	Set res = intersect(setA, setB);

	Set setRef;
	setRef.insert(1);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ(setRef, res);
}

TEST(SetUtilsTest, Difference) {
	Set setA;
	setA.insert(1);
	setA.insert(2);

	Set setB;
	setB.insert(1);

	Set res = difference(setA, setB);

	Set setRef;
	setRef.insert(2);

	// NOTE: assumes that == is implemented (optional in std)
	EXPECT_EQ(setRef, res);
}
