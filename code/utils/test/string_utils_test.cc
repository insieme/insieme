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

#include <sstream>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"

TEST(StringUtilsTest, Format) {
	EXPECT_EQ (format("Hello World"), "Hello World");
	EXPECT_EQ (format("Print %2d ...", 12), "Print 12 ...");
	EXPECT_EQ (format("Print %2d, %2d, %s ...", 12, 14, "hello"), "Print 12, 14, hello ...");
}

TEST(StringUtilsTest, FormatStrings) {

	string hello = "Hello";
	string world = "World";

	EXPECT_EQ ("Hello World - 42", format("%s %s - %d", hello, world, 42));
}

TEST(StringUtilsTest, toString) {
	EXPECT_EQ (toString("Hello World"), "Hello World");
	EXPECT_EQ (toString(10), "10");
	EXPECT_EQ (toString('c'), "c");
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


TEST(EscapeTest, escape) {

	std::stringstream out;

	// use the escape utility to stream stuff in an escaping encoding
	escape(out) << "Hello \n \"" << " some \\ test ...  \" \' ";

	// check the result
	EXPECT_EQ("Hello \\n \\\" some \\\\ test ...  \\\" \\\' ", out.str());
}



