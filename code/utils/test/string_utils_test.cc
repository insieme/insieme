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

#include <sstream>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/printable.h"

TEST(StringUtilsTest, Format) {
	EXPECT_EQ(format("Hello World"), "Hello World");
	EXPECT_EQ(format("Print %2d ...", 12), "Print 12 ...");
	EXPECT_EQ(format("Print %2d, %2d, %s ...", 12, 14, "hello"), "Print 12, 14, hello ...");
}

TEST(StringUtilsTest, FormatPrintables) {

	struct X : public insieme::utils::Printable {
		std::ostream& printTo(std::ostream& out) const { return out << "I am X"; }
	};

	X x;
	EXPECT_EQ("Say something: I am X", format("Say something: %s",x));
}

TEST(StringUtilsTest, FormatStrings) {
	string hello = "Hello";
	string world = "World";

	EXPECT_EQ("Hello World - 42", format("%s %s - %d", hello, world, 42));
}

TEST(StringUtilsTest, toString) {
	EXPECT_EQ(toString("Hello World"), "Hello World");
	EXPECT_EQ(toString(10), "10");
	EXPECT_EQ(toString('c'), "c");
}

TEST(StringUtilsTest, times) {
	EXPECT_EQ("", toString(times("x", 0)));
	EXPECT_EQ("x", toString(times("x", 1)));
	EXPECT_EQ("xx", toString(times("x", 2)));
	EXPECT_EQ("xxx", toString(times("x", 3)));
	EXPECT_EQ("x,x,x", toString(times("x", 3, ",")));
}

TEST(StringUtilsTest, split) {
	EXPECT_EQ(toVector<string>("Hello", "World!"), split("Hello World!"));
	EXPECT_EQ(toVector<string>("Some", "more", "space"), split("Some    more    space"));
	EXPECT_EQ(toVector<string>(), split(""));
}

TEST(StringUtilsTest, camelcaseToUnderscore) {
	EXPECT_EQ("hello", camelcaseToUnderscore("Hello"));
	EXPECT_EQ("camelcase_to_underscore", camelcaseToUnderscore("camelcaseToUnderscore"));
	EXPECT_EQ("camelcase_to_underscore", camelcaseToUnderscore("CamelcaseToUnderscore"));
	EXPECT_EQ("camelcase_to_underscore", camelcaseToUnderscore("camelcase_to_underscore"));
}

TEST(EscapeTest, escape) {
	std::stringstream out;

	// use the escape utility to stream stuff in an escaping encoding
	escape(out) << "Hello \n \""
	            << " some \\ test ...  \" \' ";

	// check the result
	EXPECT_EQ("Hello \\n \\\" some \\\\ test ...  \\\" \\\' ", out.str());
}

TEST(Join, overloads) {
	vector<int> data({1, 3, 4, 2});
	EXPECT_EQ("1,3,4,2", toString(join(",", data)));

	std::set<int> data2({1, 3, 4, 2});
	EXPECT_EQ("1,2,3,4", toString(join(",", data2)));
}
