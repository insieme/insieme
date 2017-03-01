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

#include "insieme/utils/automata/regular_language_utils.h"

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

		Lang x = sequence(a, repetition(alternativ(b, c)), d);

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
