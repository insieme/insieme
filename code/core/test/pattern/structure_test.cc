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

		EXPECT_EQ(*a,*b);

		b = makeValue(false);
		EXPECT_NE(*a,*b);

		a = makeValue(14);
		b = makeValue(14);
		EXPECT_EQ(*a,*b);

		b = makeValue(15);
		EXPECT_NE(*a,*b);

		// simple tree test
		a = makeTree('a');
		b = makeTree('a');
		EXPECT_EQ(*a,*b);

		b = makeTree('b');
		EXPECT_NE(*a,*b);

		// larger tree test
		a = makeTree('a', makeTree('b'), makeValue(14));
		b = makeTree('a', makeTree('b'), makeValue(14));
		EXPECT_EQ(*a,*b);

		b = makeTree('a', makeTree('b'), makeValue(16));
		EXPECT_NE(*a,*b);

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

