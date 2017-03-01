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

#include "insieme/core/pattern/structure.h"

namespace insieme {
namespace core {
namespace pattern {

	TEST(Tree, Basic) {
		TreePtr tree = makeTree();
		EXPECT_EQ("()", toString(tree));

		tree = makeTree(tree, tree, tree);
		EXPECT_EQ("((),(),())", toString(tree));

		tree = makeTree(tree, makeTree());
		EXPECT_EQ("(((),(),()),())", toString(tree));

		tree = makeTree('a');
		EXPECT_EQ("a", toString(tree));

		tree = makeTree(tree, makeTree('b'), makeTree());
		EXPECT_EQ("(a,b,())", toString(tree));

		// test values:
		tree = makeValue(true);
		EXPECT_EQ("true", toString(tree));

		tree = makeValue(false);
		EXPECT_EQ("false", toString(tree));

		tree = makeValue(15);
		EXPECT_EQ("15", toString(tree));

		tree = makeValue<string>("Hello");
		EXPECT_EQ("\"Hello\"", toString(tree));
	}

	TEST(Tree, Equality) {
		TreePtr a;
		TreePtr b;

		a = makeValue(true);
		b = makeValue(true);

		EXPECT_EQ(*a, *b);

		b = makeValue(false);
		EXPECT_NE(*a, *b);

		a = makeValue(14);
		b = makeValue(14);
		EXPECT_EQ(*a, *b);

		b = makeValue(15);
		EXPECT_NE(*a, *b);

		// simple tree test
		a = makeTree('a');
		b = makeTree('a');
		EXPECT_EQ(*a, *b);

		b = makeTree('b');
		EXPECT_NE(*a, *b);

		// larger tree test
		a = makeTree('a', makeTree('b'), makeValue(14));
		b = makeTree('a', makeTree('b'), makeValue(14));
		EXPECT_EQ(*a, *b);

		b = makeTree('a', makeTree('b'), makeValue(16));
		EXPECT_NE(*a, *b);
	}

	TEST(Tree, Parser) {
		EXPECT_EQ("a", toString(parseTree("a")));
		EXPECT_EQ("a", toString(parseTree("a()")));
		EXPECT_EQ("a(b,c)", toString(parseTree("a(b,c)")));
		EXPECT_EQ("a(b,c)", toString(parseTree("a( b , c )")));
		EXPECT_EQ("a(b,c(b),a)", toString(parseTree("a(b,c(b),a)")));
	}

} // end namespace pattern
} // end namespace core
} // end namespace insieme
