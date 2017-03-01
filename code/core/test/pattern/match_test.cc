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

#include "insieme/core/pattern/match.h"

namespace insieme {
namespace core {
namespace pattern {

	TEST(Match, MatchPath) {
		MatchPath path;

		EXPECT_EQ("[]", toString(path));

		path.push(10);
		EXPECT_EQ("[10]", toString(path));

		path.push(14);
		EXPECT_EQ("[10,14]", toString(path));

		path.inc();
		EXPECT_EQ("[10,15]", toString(path));

		path.pop();
		EXPECT_EQ("[10]", toString(path));

		path.inc();
		EXPECT_EQ("[11]", toString(path));
	}

	TEST(Match, MatchValue) {
		TreePtr a = makeTree('a');

		MatchValue<tree_target> value(a);
		MatchValue<tree_target> v2 = makeMatchValue<tree_target>(value, value);
		MatchValue<tree_target> v3 = makeMatchValue<tree_target>(v2, v2, v2);

		EXPECT_EQ("a", toString(a));
		EXPECT_EQ("a", toString(value));
		EXPECT_EQ("[a,a]", toString(v2));
		EXPECT_EQ("[[a,a],[a,a],[a,a]]", toString(v3));
	}

	TEST(Match, Trees) {
		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');

		MatchPath path;
		MatchValue<tree_target> value(2);

		path.push(0);
		path.push(0);

		EXPECT_FALSE(value.hasValue(path));
		value.addValue(path, a);
		EXPECT_EQ(a, value.getValue(path));
		EXPECT_EQ("[[a]]", toString(value));
		EXPECT_TRUE(value.hasValue(path));

		path.pop();
		path.push(1);
		value.addValue(path, b);
		EXPECT_EQ(b, value.getValue(path));
		EXPECT_EQ("[[a,b]]", toString(value));

		path.pop();
		path.pop();
		path.push(1);
		path.push(0);
		EXPECT_FALSE(value.hasValue(path));
		value.addValue(path, a);
		EXPECT_EQ(a, value.getValue(path));
		EXPECT_EQ("[[a,b],[a]]", toString(value));
		EXPECT_TRUE(value.hasValue(path));

		path.push(1);
		EXPECT_TRUE(value.hasValue(path));
	}


	TEST(Match, TreeLists) {
		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');

		MatchPath path;
		MatchValue<tree_target> value(3);

		path.push(0);
		path.push(0);

		TreeList list = toVector(a, b);

		EXPECT_FALSE(value.hasListValue(path));
		value.addListValue(path, list);
		EXPECT_EQ(list, value.getListValue(path));
		EXPECT_EQ("[[[a,b]]]", toString(value));
		EXPECT_TRUE(value.hasListValue(path));

		path.inc();
		list = toVector(b, a, a, b);

		EXPECT_FALSE(value.hasListValue(path));
		value.addListValue(path, list);
		EXPECT_EQ(list, value.getListValue(path));
		EXPECT_EQ("[[[a,b],[b,a,a,b]]]", toString(value));
		EXPECT_TRUE(value.hasListValue(path));

		path.pop();
		path.inc();
		path.push();

		list = toVector<TreePtr>();

		EXPECT_FALSE(value.hasListValue(path));
		value.addListValue(path, list);
		EXPECT_EQ(list, value.getListValue(path));
		EXPECT_EQ("[[[a,b],[b,a,a,b]],[[]]]", toString(value));
		EXPECT_TRUE(value.hasListValue(path));
	}

	TEST(Match, Restore) {
		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');

		MatchPath path;
		Match<tree_target> match;

		auto original = match.backup();

		EXPECT_EQ("Match({})", toString(match));

		// add an element
		match.bindTreeVar(path, "x", a);
		EXPECT_EQ("Match({x=a})", toString(match));

		// restore old state
		match.restore(original);
		EXPECT_EQ("Match({})", toString(match));

		// once more
		match.bindTreeVar(path, "y", b);
		EXPECT_EQ("Match({y=b})", toString(match));
		match.restore(original);
		EXPECT_EQ("Match({})", toString(match));


		// no with lists
		path.push(0);
		match.bindTreeVar(path, "x", a);
		EXPECT_EQ("Match({x=[a]})", toString(match));

		auto backup = match.backup();

		path.set(1);
		match.bindTreeVar(path, "x", b);
		EXPECT_EQ("Match({x=[a,b]})", toString(match));

		match.restore(backup);
		EXPECT_EQ("Match({x=[a,null]})", toString(match));

		match.restore(original);
		EXPECT_EQ("Match({})", toString(match));
	}


} // end namespace pattern
} // end namespace core
} // end namespace insieme
