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

#include "insieme/transform/pattern/match.h"

namespace insieme {
namespace transform {
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
		MatchValue<tree_target> v3 = makeMatchValue<tree_target>(v2,v2,v2);

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

		TreeList list = toVector(a,b);

		EXPECT_FALSE(value.hasListValue(path));
		value.addListValue(path, list);
		EXPECT_EQ(list, value.getListValue(path));
		EXPECT_EQ("[[[a,b]]]", toString(value));
		EXPECT_TRUE(value.hasListValue(path));

		path.inc();
		list = toVector(b,a,a,b);

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


} // end namespace pattern
} // end namespace transform
} // end namespace insieme

