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

//#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/logging.h"

#include "insieme/iwir/iwir_condition_builder.h"

using namespace std;

namespace insieme {
namespace iwir {
namespace condition_ast {

TEST(ConditionTest, Parsing) {
	Logger::get(std::cerr, INFO, 1);

	auto inputs = std::list<std::string>( {
				"true",
				"True",
				"false",
				"False",
				"0.0",
				"1.0",
				"+1.0",
				"-1.0",
				"0",
				"1",
				"-1",
				"true and false",
				"portIn = 5",
				"portIn = +1.50",
				"portIn = -1.50",
				"portIn = true",
				"portIn = false",
				"portIn &gt; 6",
				"portIn or portIn1",
				"portIn or portIn1 and portIn2 = false",
				"3&gt;4",
				"3=3",
				"3 &gt; (4 &gt; 3)"
				} );
		for(auto& input : inputs) {

//			ConditionExpr result = parseConditionString(input);
			auto f(std::begin(input)), l(std::end(input));
			parser<decltype(f)> p("ConditionTest");

			try
			{
				ConditionExpr result;
				bool ok = qi::phrase_parse(f,l,p,qi::space,result);

				if (!ok) {
					std::cerr << "invalid input\n";
					EXPECT_TRUE(false);
				} else {
					VLOG(2) << "Successfully parsed - input: " << input << " - result: " << result << "\n";
					EXPECT_TRUE(true);
				}

			} catch (const qi::expectation_failure<decltype(f)>& e)
			{
				std::cerr << "expectation_failure at '" << std::string(e.first, e.last) << "'\n";
				EXPECT_TRUE(false);
			}

			if (f!=l) std::cerr << "unparsed: '" << std::string(f,l) << "'\n";
			EXPECT_TRUE(f==l);
		}
}

TEST(ConditionTest, InvalidParsing) {
	Logger::get(std::cerr, INFO, 1);

	//TODO needs better invalid input detection
	
	auto inputs = std::list<std::string>( {
				"AND false",
				"true AND false",
				"true And false",
				"true OR false",
				"true Or false",
				"true &gt;asdf false"
				} );

	for(auto& input : inputs) 
	{
//		ConditionExpr result = parseConditionString(input);
		auto f(std::begin(input)), l(std::end(input));
		parser<decltype(f)> p("ConditionTest");

		try {
			ConditionExpr result;
			bool ok = qi::phrase_parse(f,l,p,qi::space,result);

			/*
			if (!ok)
				std::cerr << "invalid input\n";
			else
				std::cout << "Successfully parsed - input: " << input << " - result: " << result << "\n";
			*/

		} catch (const qi::expectation_failure<decltype(f)>& e) {
			std::cerr << "expectation_failure at '" << std::string(e.first, e.last) << "'\n";
			EXPECT_TRUE(false);
		}

		EXPECT_TRUE(f!=l);
		if (f!=l) { 
			VLOG(2) << "unparsed: '" << std::string(f,l) << "'\n";
		}
	}
}

} // end namespace condition_ast 
} // end namespace iwir 
} // end namespace insieme
