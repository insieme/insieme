/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <algorithm>
#include <string>

#include <fstream>
#include <sstream>

#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

// bison generated header
#include "inspire_parser.hpp"

namespace insieme {
namespace core {
namespace parser3 {
namespace detail {


	TEST(Parser_Tools, ctx_manager) {

	    NodeManager mgr;
	    IRBuilder builder(mgr);
	    DeclarationContext dc;

	    auto one = builder.concreteIntTypeParam(12);
	    auto two = builder.variableIntTypeParam('c');

	    EXPECT_NE( one, two);

	    dc.add_symb ("one", one);
	    dc.add_symb ("two", two);

	    EXPECT_NE( dc.find("one"), dc.find("two"));
	    EXPECT_NE( dc.find("one"), two);
	    EXPECT_NE( dc.find("twp"), one);
	    EXPECT_EQ( dc.find("one"), one);
	    EXPECT_EQ( dc.find("two"), two);

	    // enter an scope
	    dc.open_scope();

		auto three = builder.variableIntTypeParam('t');
		dc.add_symb ("three", three);
		
		EXPECT_NE( dc.find("one"), dc.find("two"));
		EXPECT_NE( dc.find("one"), two);
		EXPECT_NE( dc.find("twp"), one);
		EXPECT_EQ( dc.find("one"), one);
		EXPECT_EQ( dc.find("two"), two);
		EXPECT_EQ( dc.find("three"), three);
	    
		// declare a shadow name
		auto notOne = builder.variableIntTypeParam('x');
		dc.add_symb ("one", notOne);

		EXPECT_NE( dc.find("one"), one);
		EXPECT_EQ( dc.find("one"), notOne);

		// once again
		dc.open_scope();
	
		    // declare a shadow name
		    auto notTwo = builder.variableIntTypeParam('y');
		    dc.add_symb ("two", notTwo);

		    EXPECT_NE( dc.find("two"), two);
		    EXPECT_EQ( dc.find("two"), notTwo);
		    EXPECT_NE( dc.find("one"), one);
		    EXPECT_EQ( dc.find("one"), notOne);

		dc.close_scope();

		EXPECT_NE( dc.find("one"), one);
		EXPECT_EQ( dc.find("one"), notOne);
	    
	    // close the scope  
	    dc.close_scope();
	    
	    EXPECT_EQ( dc.find("one"), one);
	    EXPECT_NE( dc.find("one"), notOne);
	}

	TEST(Parser_Tools, error_locations) {

	    std::string filename ("unknown");
	    NodeManager mgr;

	    {
			std::stringstream ss;
			std::string text("#hello, this is just a text");
			inspire_driver driver(text, mgr);

			position beg (&filename, 1,2);
			position end (&filename, 1,10);
			location errorloc (beg, end);

			driver.error(errorloc, "here is the error");
			driver.print_errors(ss, false);

			EXPECT_EQ(ss.str(), "ERROR: unknown:1.2-9 here is the error\n#hello, this is just a text\n ^~~~~~~~\n");
	    }

	    {
			std::stringstream ss;
			std::string text("int<4.");
			inspire_driver driver(text, mgr);
			driver.parseExpression();
			driver.print_errors(ss, false);
			EXPECT_EQ(ss.str(), "ERROR: 1.1-3 the symbol int was not declared in this context\nint<4.\n^~~\nERROR: 1.1-3 unrecoverable error\nint<4.\n^~~\n");
	    }
		
	    {
			std::stringstream ss;
			std::string text("int<4.");
			inspire_driver driver(text, mgr);
			driver.parseType();
			driver.print_errors(ss, false);
			EXPECT_EQ(ss.str(), "ERROR: 1.6 syntax error, unexpected ., expecting >\nint<4.\n     ^\n");
	    }

		//acurate tabs
		// there is the thing, a tab is a single char, and the location will be updated by one single possition
		// at the time to print, the easyest way is to replace tabs in input by spaces, and then the green arrow will
		// point the right char
	    {
			std::stringstream ss;
			std::string text("\t\t\tx");
			inspire_driver driver(text, mgr);
			driver.parseExpression();
			driver.print_errors(ss, false);
			EXPECT_EQ(ss.str(), "ERROR: 1.4 the symbol x was not declared in this context\n   x\n   ^\nERROR: 1.4 unrecoverable error\n   x\n   ^\n");
	    }

		//tabs and newlines
	    {
			std::stringstream ss;
			std::string text("\t\t\n\tx");
			inspire_driver driver(text, mgr);
			driver.parseExpression();
			driver.print_errors(ss, false);
			EXPECT_EQ(ss.str(), "ERROR: 2.2 the symbol x was not declared in this context\n x\n ^\nERROR: 2.2 unrecoverable error\n x\n ^\n");
	    }
	}

} // detail
} // parser3
} // core
} // insieme
