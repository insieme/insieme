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

#include "insieme/transform/pattern/pattern.h"
#include "insieme/transform/pattern/structure.h"

namespace insieme {
namespace transform {
namespace pattern {

	bool match(const TreePatternPtr& pattern, const TreePtr& tree) {
		return pattern->match(tree);
	}

	bool notMatch(const TreePatternPtr& pattern, const TreePtr& tree) {
		return !match(pattern, tree);
	}

	TEST(TreePattern, Basic) {

		TreePtr treeA = makeTree('a');
		TreePtr treeB = makeTree('b');
		EXPECT_EQ("a", toString(treeA));
		EXPECT_EQ("b", toString(treeB));

		TreePatternPtr pattern = atom(treeA);
		EXPECT_EQ("a", toString(pattern));

		EXPECT_PRED2(match, pattern, treeA);
		EXPECT_PRED2(notMatch, pattern, treeB);

		pattern = atom(treeB);
		EXPECT_EQ("b", toString(pattern));
		EXPECT_PRED2(notMatch, pattern, treeA);
		EXPECT_PRED2(match, pattern, treeB);

		pattern = atom(treeA) | atom(treeB);
		EXPECT_EQ("a | b", toString(pattern));
		EXPECT_PRED2(match, pattern, treeA);
		EXPECT_PRED2(match, pattern, treeB);

		pattern = !pattern;
		EXPECT_EQ("!(a | b)", toString(pattern));
		EXPECT_PRED2(notMatch, pattern, treeA);
		EXPECT_PRED2(notMatch, pattern, treeB);

	}

	TEST(NodePattern, Basic) {

		TreePtr treeA = makeTree('a');
		TreePtr treeB = makeTree(treeA, treeA);
		TreePtr treeC = makeTree(treeA, treeA, treeB);

		EXPECT_EQ("a", toString(treeA));
		EXPECT_EQ("(a,a)", toString(treeB));
		EXPECT_EQ("(a,a,(a,a))", toString(treeC));

		NodePatternPtr pattern = single(atom(treeA));
		TreePatternPtr treePattern = node(pattern);
		EXPECT_EQ("a", toString(pattern));
		EXPECT_EQ("(a)", toString(treePattern));

		EXPECT_PRED2(notMatch, node(pattern), treeA);
		EXPECT_PRED2(notMatch, node(pattern), treeB);
		EXPECT_PRED2(notMatch, node(pattern), treeC);

		// test sequence
		pattern = (pattern, pattern);
		treePattern = node(pattern);
		EXPECT_EQ("a,a", toString(pattern));
		EXPECT_EQ("(a,a)", toString(treePattern));

		EXPECT_PRED2(notMatch, node(pattern), treeA);
		EXPECT_PRED2(match, node(pattern), treeB);
		EXPECT_PRED2(notMatch, node(pattern), treeC);

		// test repetition
		pattern = *(single(atom(treeA)));
		treePattern = node(pattern);
		EXPECT_EQ("[a]*", toString(pattern));
		EXPECT_EQ("([a]*)", toString(treePattern));

		EXPECT_PRED2(match, node(pattern), treeA);
		EXPECT_PRED2(match, node(pattern), treeB);
		EXPECT_PRED2(notMatch, node(pattern), treeC);

		// combination
		pattern = (pattern, single(atom(treeB)));
		treePattern = node(pattern);
		EXPECT_EQ("[a]*,(a,a)", toString(pattern));
		EXPECT_EQ("([a]*,(a,a))", toString(treePattern));

		EXPECT_PRED2(notMatch, node(pattern), treeA);
		EXPECT_PRED2(notMatch, node(pattern), treeB);
		EXPECT_PRED2(match, node(pattern), treeC);

	}

	TEST(Wildcard, Basic) {

		TreePtr treeA = makeTree('a');
		TreePtr treeB = makeTree(treeA, treeA);
		TreePtr treeC = makeTree(treeA, treeA, treeB);

		EXPECT_EQ("(a,a,(a,a))", toString(treeC));

		auto pattern = (single(treeA), single(treeA), single(any));
		EXPECT_EQ("a,a,_", toString(pattern));

		EXPECT_PRED2(notMatch, node(pattern), treeA);
		EXPECT_PRED2(notMatch, node(pattern), treeB);
		EXPECT_PRED2(match, node(pattern), treeC);


		pattern = (single(treeA), single(any), single(treeA));
		EXPECT_EQ("a,_,a", toString(pattern));

		EXPECT_PRED2(notMatch, node(pattern), treeA);
		EXPECT_PRED2(notMatch, node(pattern), treeB);
		EXPECT_PRED2(notMatch, node(pattern), treeC);
	}


	TEST(NodePattern, Extended) {

		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');
		TreePtr c = makeTree('c');

		auto pattern = *(single(a) | (single(a), single(b)) | single(c));

		EXPECT_EQ("[a|a,b|c]*", toString(pattern));
		EXPECT_EQ("([a|a,b|c]*)", toString(node(pattern)));

		// these should all match, since the pattern allows an empty node
		EXPECT_PRED2(match, node(pattern), a);
		EXPECT_PRED2(match, node(pattern), b);
		EXPECT_PRED2(match, node(pattern), c);


		// these should all match, since the pattern allows an empty node
		EXPECT_PRED2(match, node(pattern), makeTree(a));
		EXPECT_PRED2(notMatch, node(pattern), makeTree(b));
		EXPECT_PRED2(match, node(pattern), makeTree(c));
	}

	TEST(Descendant, Basic) {

		// build up test-tree
		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');
		TreePtr c = makeTree('c');

		TreePtr treeA = makeTree(a,a,a);
		TreePtr treeB = makeTree(a,b,a);
		TreePtr treeC = makeTree(b,a,b);
		TreePtr treeD = makeTree(treeA,treeB,treeC);
		TreePtr treeE = makeTree(b,makeTree(b,makeTree(b,makeTree(a))));

		EXPECT_EQ("a", toString(a));
		EXPECT_EQ("b", toString(b));
		EXPECT_EQ("c", toString(c));

		EXPECT_EQ("(a,a,a)", toString(treeA));
		EXPECT_EQ("(a,b,a)", toString(treeB));
		EXPECT_EQ("(b,a,b)", toString(treeC));
		EXPECT_EQ("((a,a,a),(a,b,a),(b,a,b))", toString(treeD));
		EXPECT_EQ("(b,(b,(b,(a))))", toString(treeE));


		// create a descendant pattern
		auto patternA = aT(atom(a));
		auto patternB = aT(atom(b));
		auto patternC = aT(atom(c));
		auto patternD = aT(atom(a), atom(b));

		EXPECT_PRED2(match, patternA, a);
		EXPECT_PRED2(notMatch, patternA, b);
		EXPECT_PRED2(notMatch, patternA, c);

		EXPECT_PRED2(notMatch, patternB, a);
		EXPECT_PRED2(match, patternB, b);
		EXPECT_PRED2(notMatch, patternB, c);

		EXPECT_PRED2(notMatch, patternC, a);
		EXPECT_PRED2(notMatch, patternC, b);
		EXPECT_PRED2(match, patternC, c);

		EXPECT_PRED2(notMatch, patternD, a);
		EXPECT_PRED2(notMatch, patternD, b);
		EXPECT_PRED2(notMatch, patternD, c);

		EXPECT_PRED2(match, patternA, treeA);
		EXPECT_PRED2(match, patternA, treeB);
		EXPECT_PRED2(match, patternA, treeC);
		EXPECT_PRED2(match, patternA, treeD);
		EXPECT_PRED2(match, patternA, treeE);

		EXPECT_PRED2(notMatch, patternB, treeA);
		EXPECT_PRED2(match, patternB, treeB);
		EXPECT_PRED2(match, patternB, treeC);
		EXPECT_PRED2(match, patternB, treeD);
		EXPECT_PRED2(match, patternB, treeE);

		EXPECT_PRED2(notMatch, patternC, treeA);
		EXPECT_PRED2(notMatch, patternC, treeB);
		EXPECT_PRED2(notMatch, patternC, treeC);
		EXPECT_PRED2(notMatch, patternC, treeD);
		EXPECT_PRED2(notMatch, patternC, treeE);

		EXPECT_PRED2(notMatch, patternD, treeA);
		EXPECT_PRED2(match, patternD, treeB);
		EXPECT_PRED2(match, patternD, treeC);
		EXPECT_PRED2(match, patternD, treeD);
		EXPECT_PRED2(match, patternD, treeE);
	}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme

