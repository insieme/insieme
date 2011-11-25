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

#include "insieme/transform/pattern/rule.h"

namespace insieme {
namespace transform {
namespace pattern {

	namespace p = pattern;
	namespace g = pattern::generator;

	TEST(Rule, Identity) {

		Rule rule;

		EXPECT_EQ("_ -> root", toString(rule));

		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');
		TreePtr c = makeTree(a,b,a,b);

		EXPECT_EQ(a, rule.applyTo(a));
		EXPECT_EQ(b, rule.applyTo(b));
		EXPECT_EQ(c, rule.applyTo(c));
	}


	TEST(Rule, Replace) {

		TreePatternPtr pattern;
		TreeGeneratorPtr generator;
		Rule rule;

		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');
		TreePtr c = makeTree('c');


		rule = Rule(p::any, g::atom(a));
		EXPECT_EQ("_ -> a", toString(rule));

		EXPECT_EQ(a, rule.applyTo(a));
		EXPECT_EQ(a, rule.applyTo(b));
		EXPECT_EQ(a, rule.applyTo(c));

	}

	TEST(Rule, Reorder) {

		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');

		TreePatternPtr pattern;
		TreeGeneratorPtr generator;

		pattern   = p::node(0, p::single(p::var("x")) << p::single(p::var("y")));
		generator = g::node(0, g::single(g::var<tree_target>("y")) << g::single(g::var<tree_target>("x")));

		EXPECT_EQ("(0|$x,$y)", toString(pattern));
		EXPECT_EQ("(0|$y,$x)", toString(generator));

		Rule rule(pattern, generator);

		TreePtr in = makeTree(a,b);
		TreePtr out = rule.applyTo(in);

		EXPECT_EQ("(a,b)", toString(in));
		EXPECT_EQ("(b,a)", toString(out));

		// something that shouldn't work ...
		in = makeTree(a,b,a);
		EXPECT_FALSE((bool)rule.applyTo(in));

	}


	TEST(Rule, Reverse) {

		TreePtr a = makeTree('a');
		TreePtr b = makeTree('b');
		TreePtr c = makeTree('c');

		TreePatternPtr pattern;
		TreeGeneratorPtr generator;

		pattern   = p::node(0, *(p::var("x")));
		generator = g::node(0, g::forEach("y", g::reverse(g::varExpr<tree_target>("x")), g::var<tree_target>("y")));

		EXPECT_EQ("(0|[$x]*)", toString(pattern));
		EXPECT_EQ("(0|for y in reverse($x) get $y)", toString(generator));

		Rule rule(pattern, generator);

		TreePtr in = makeTree(a,b);
		TreePtr out = rule.applyTo(in);

		EXPECT_EQ("(a,b)", toString(in));
		EXPECT_EQ("(b,a)", toString(out));

		EXPECT_EQ("(c,b,a)", toString(rule.applyTo(makeTree(a,b,c))));

		EXPECT_EQ("(a,c,b,a)", toString(rule.applyTo(makeTree(a,b,c,a))));

	}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme


