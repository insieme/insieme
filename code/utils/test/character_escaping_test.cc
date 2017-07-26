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

#include <string>

#include "insieme/utils/assert.h"
#include "insieme/utils/character_escaping.h"

namespace insieme {
namespace utils {

	using std::string;

	TEST(CharacterEscaping, EscapeString) {

		auto abc = escapeString(R"(abc)");
		EXPECT_EQ(3, abc.length());
		EXPECT_EQ(R"(abc)", abc);

		auto bs = escapeString(R"(\n)");
		EXPECT_EQ(3, bs.length());
		EXPECT_EQ(R"(\\n)", bs);

		auto large = escapeString(R"(bla\r\n\l\v\b)");
		EXPECT_EQ(18, large.length());
		EXPECT_EQ(R"(bla\\r\\n\\l\\v\\b)", large);

	}

	TEST(CharacterEscaping, UnescapeString) {

		auto abc = unescapeString(R"(abc)");
		EXPECT_EQ(R"(abc)", abc);
		EXPECT_EQ(3, abc.length());

		auto squ = unescapeString(R"(\')");
		EXPECT_EQ(R"(')", squ);
		EXPECT_EQ(1, squ.length());

		auto dqu = unescapeString(R"(\")");
		EXPECT_EQ(R"(")", dqu);
		EXPECT_EQ(1, dqu.length());

		auto ze = unescapeString(R"(\\0)");
		EXPECT_EQ(R"(\0)", ze);
		EXPECT_EQ(2, ze.length());
	}

	TEST(CharacterEscaping, EscapeChar) {

		EXPECT_EQ(R"(a)", escapeChar('a'));
		EXPECT_EQ(R"(\n)", escapeChar('\n'));
		EXPECT_EQ(R"(\0)", escapeChar('\0'));
		EXPECT_EQ(R"(\\)", escapeChar('\\'));

	}

	TEST(CharacterEscaping, CharToString) {

		EXPECT_EQ(R"(\n)", escapedCharToString('\n'));
		assert_decl(ASSERT_DEATH(escapedCharToString('b'), "Unsupported escaped character of value 98"););

	}

	TEST(CharacterEscaping, StringToChar) {

		EXPECT_EQ('\n', escapedStringToChar(R"(\n)"));
		EXPECT_EQ('b', escapedStringToChar("b"));

	}

} // end namespace core
} // end namespace insieme
