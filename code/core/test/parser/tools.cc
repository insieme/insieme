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

#include <algorithm>
#include <string>

#include <fstream>
#include <sstream>

#include "insieme/core/parser/detail/driver.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

// bison generated header
#include "inspire_parser.hpp"

namespace insieme {
namespace core {
namespace parser {
	namespace detail {

		namespace {

			NodeFactory toFactory(const NodePtr& node) {
				return [=]() { return node; };
			}

		}


		TEST(Parser_Tools, context) {
			NodeManager mgr;
			IRBuilder builder(mgr);
			InspireDriver dc("", mgr);

			auto one = builder.intLit(1);
			auto two = builder.intLit(2);

			EXPECT_NE(one, two);

			dc.declareSymbol(location(), "one", toFactory(one));
			dc.declareSymbol(location(), "two", toFactory(two));

			EXPECT_NE(dc.findSymbol(location(), "one"), dc.findSymbol(location(), "two"));
			EXPECT_NE(dc.findSymbol(location(), "one"), two);
			EXPECT_NE(dc.findSymbol(location(), "twp"), one);
			EXPECT_EQ(dc.findSymbol(location(), "one"), one);
			EXPECT_EQ(dc.findSymbol(location(), "two"), two);

			// enter an scope
			dc.openScope();

			auto three = builder.intLit(3);
			dc.declareSymbol(location(), "three", toFactory(three));

			EXPECT_NE(dc.findSymbol(location(), "one"), dc.findSymbol(location(), "two"));
			EXPECT_NE(dc.findSymbol(location(), "one"), two);
			EXPECT_NE(dc.findSymbol(location(), "twp"), one);
			EXPECT_EQ(dc.findSymbol(location(), "one"), one);
			EXPECT_EQ(dc.findSymbol(location(), "two"), two);
			EXPECT_EQ(dc.findSymbol(location(), "three"), three);

			// declare a shadow name
			auto notOne = builder.intLit(4);
			dc.declareSymbol(location(), "one", toFactory(notOne));

			EXPECT_NE(dc.findSymbol(location(), "one"), one);
			EXPECT_EQ(dc.findSymbol(location(), "one"), notOne);

			// once again
			dc.openScope();

			// declare a shadow name
			auto notTwo = builder.intLit(5);
			dc.declareSymbol(location(), "two", toFactory(notTwo));

			EXPECT_NE(dc.findSymbol(location(), "two"), two);
			EXPECT_EQ(dc.findSymbol(location(), "two"), notTwo);
			EXPECT_NE(dc.findSymbol(location(), "one"), one);
			EXPECT_EQ(dc.findSymbol(location(), "one"), notOne);

			dc.closeScope();

			EXPECT_NE(dc.findSymbol(location(), "one"), one);
			EXPECT_EQ(dc.findSymbol(location(), "one"), notOne);

			// close the scope
			dc.closeScope();

			EXPECT_EQ(dc.findSymbol(location(), "one"), one);
			EXPECT_NE(dc.findSymbol(location(), "one"), notOne);
		}

		TEST(Parser_Tools, error_locations) {
			std::string filename("unknown");
			NodeManager mgr;

			{
				std::stringstream ss;
				std::string text("#hello, this is just a text");
				InspireDriver driver(text, mgr);

				position beg(&filename, 1, 2);
				position end(&filename, 1, 10);
				location errorloc(beg, end);

				driver.error(errorloc, "here is the error");
				driver.printErrors(ss, false);

				EXPECT_EQ(ss.str(), "ERROR: unknown:1.2-9 here is the error\n#hello, this is just a text\n ^~~~~~~~\n");
			}

			{
				std::stringstream ss;
				std::string text("int<4.");
				InspireDriver driver(text, mgr);
				driver.parseExpression();
				driver.printErrors(ss, false);
				EXPECT_EQ(ss.str(),
				          "ERROR: 1.1-3 the symbol int was not declared in this context\nint<4.\n^~~\nERROR: 1.1-3 unrecoverable error\nint<4.\n^~~\n");
			}

			{
				std::stringstream ss;
				std::string text("int<4.");
				InspireDriver driver(text, mgr);
				driver.parseType();
				driver.printErrors(ss, false);
				EXPECT_EQ(ss.str(), "ERROR: 1.6 syntax error, unexpected ., expecting >\nint<4.\n     ^\n");
			}

			// acurate tabs
			// there is the thing, a tab is a single char, and the location will be updated by one single possition
			// at the time to print, the easyest way is to replace tabs in input by spaces, and then the green arrow will
			// point the right char
			{
				std::stringstream ss;
				std::string text("\t\t\tx");
				InspireDriver driver(text, mgr);
				driver.parseExpression();
				driver.printErrors(ss, false);
				EXPECT_EQ(ss.str(), "ERROR: 1.4 the symbol x was not declared in this context\n   x\n   ^\nERROR: 1.4 unrecoverable error\n   x\n   ^\n");
			}

			// tabs and newlines
			{
				std::stringstream ss;
				std::string text("\t\t\n\tx");
				InspireDriver driver(text, mgr);
				driver.parseExpression();
				driver.printErrors(ss, false);
				EXPECT_EQ(ss.str(), "ERROR: 2.2 the symbol x was not declared in this context\n x\n ^\nERROR: 2.2 unrecoverable error\n x\n ^\n");
			}
		}

	} // detail
} // parser
} // core
} // insieme
