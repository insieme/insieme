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

#include "insieme/utils/automata/regular_language_utils.h"
#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace utils {
namespace automata {

using namespace set;

typedef Automata<>::state_type State;
typedef RegularLanguage<> Lang;

bool match(Lang l, const char* str) {
	return accepts(l.getAutomata(), string(str));
}

TEST(RegLang, Composition) {


	// a larger example
	Lang a('a');
	Lang b('b');
	Lang c('c');
	Lang d('d');

	// pattern a(b|c)*d

	Lang x= sequence(a, repetition(alternativ(b,c)), d);

	// EXPECT_EQ("", toDotGraph(x.getAutomata()));

	EXPECT_TRUE(match(x, "ad"));
	EXPECT_TRUE(match(x, "abd"));
	EXPECT_TRUE(match(x, "abcd"));
	EXPECT_TRUE(match(x, "acbbcccbbcbcd"));

	EXPECT_FALSE(match(x, "abc"));

	// compress it to a NFA
	auto n = toNFA(x.getAutomata());
	// EXPECT_EQ("", toDotGraph(n));

	EXPECT_TRUE(accepts(n, string("ad")));
	EXPECT_TRUE(accepts(n, string("abd")));
	EXPECT_TRUE(accepts(n, string("abcd")));
	EXPECT_TRUE(accepts(n, string("acbbcccbbcbcd")));

	EXPECT_FALSE(accepts(n, string("abc")));

}



} // end namespace automata
} // end namespace core
} // end namespace insieme

