/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <iostream>

#include "insieme/core/analysis/parentheses.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/parser/ir_parser.h"

namespace insieme {
namespace core {
namespace analysis {

	using namespace insieme::core::analysis;

	bool checkParentheses(NodeManager &nm, const std::string &input, const std::string &expected) {
		IRBuilder builder(nm);

		std::cout << " =============== TEST =============== " << std::endl;

		auto type = builder.parseExpr(input);
		// dumpText(type);

		if(type) {
			insieme::core::printer::PrettyPrinter printer(type,
														  insieme::core::printer::PrettyPrinter::OPTIONS_DEFAULT |
														  insieme::core::printer::PrettyPrinter::PRINT_CASTS |
														  insieme::core::printer::PrettyPrinter::PRINT_DEREFS |
														  insieme::core::printer::PrettyPrinter::NO_LIST_SUGAR |
														  insieme::core::printer::PrettyPrinter::PRINT_ATTRIBUTES |
														  insieme::core::printer::PrettyPrinter::NO_EVAL_LAZY |
														  insieme::core::printer::PrettyPrinter::PRINT_DERIVED_IMPL);

			std::ostringstream output;
			output << printer;
			if(!expected.compare(output.str())) {
				dumpColor(type);
				return true;
			} else {
				std::cout << "Error: Equality check gone wrong..." << std::endl;
				std::cout << "Input:    " << input << "[END]" << std::endl;
				std::cout << "Printer:  " << printer << "[END]" << std::endl;
				std::cout << "Expected: " << expected << "[END]" << std::endl;
				//dumpColor(type);
				return false;
			}
		}

		return false;
	}


TEST(Parentheses, Basic) {
	NodeManager mgr;
	IRBuilder builder(mgr);


	auto needsNoParentheses =  [](const CallExprAddress& expr) {
		return !needsParentheses(expr);
	};

	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$1+2$")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$1+2$+3")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("1+$2+3$")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$1-2$+3")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("1-$2+3$")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$1&2$|3")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("$1|2$&3")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("$1+2$*3")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$1*2$+3")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$1-2+3$")[0].as<CallExprAddress>()->getArgument(0).as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("$1-(2+3)$")[0].as<CallExprAddress>()->getArgument(1).as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("$1-(2-3)$")[0].as<CallExprAddress>()->getArgument(1).as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$(1-2)+3$")[0].as<CallExprAddress>()->getArgument(0).as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$(1+2)+3$")[0].as<CallExprAddress>()->getArgument(0).as<CallExprAddress>());

	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$3*4/5$")[0].as<CallExprAddress>()->getArgument(0).as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$3/4*5$")[0].as<CallExprAddress>()->getArgument(0).as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("3/$4*5$")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("3*$4/5$")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("3/$4/5$")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$3/4$/5")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$1/2*3$")[0].as<CallExprAddress>()->getArgument(0).as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("$1/(2*3)$")[0].as<CallExprAddress>()->getArgument(1).as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("$1/(2/3)$")[0].as<CallExprAddress>()->getArgument(1).as<CallExprAddress>());


	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$true&&false$||true")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$true&&false$")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$true&&false$&&true&&false")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsParentheses, builder.parseAddressesExpression("$true||false$&&true")[0].as<CallExprAddress>());
	EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$true&&false||true$")[0].as<CallExprAddress>()->getArgument(0).as<CallExprAddress>());
	// failure from parser!!
	//EXPECT_PRED1(needsNoParentheses, builder.parseAddressesExpression("$true||false&&true$")[0].as<CallExprAddress>()[0].as<CallExprAddress>());
}


TEST(Parentheses, BasicArithmeticOp) {
	NodeManager manager;

	EXPECT_TRUE(checkParentheses(manager, "((((1))))", "((((1))))"));
	EXPECT_TRUE(checkParentheses(manager, "100-200+300", "100-200+300"));
	EXPECT_TRUE(checkParentheses(manager, "(1+1)", "(1+1)"));
	EXPECT_TRUE(checkParentheses(manager, "(1+1)", "(1+1)"));
	EXPECT_TRUE(checkParentheses(manager, "(1+1)*3", "(1+1)*3"));
	EXPECT_TRUE(checkParentheses(manager, "(1)+(1)", "1+1"));
	EXPECT_TRUE(checkParentheses(manager, "3*(1+2)", "3*(1+2)"));
	EXPECT_TRUE(checkParentheses(manager, "(3*(1+2))", "(3*(1+2))"));
	EXPECT_TRUE(checkParentheses(manager, "1+1+1", "1+1+1"));
	EXPECT_TRUE(checkParentheses(manager, "(1+1)+1", "1+1+1"));
	EXPECT_TRUE(checkParentheses(manager, "1+(1+1)", "1+(1+1)"));
	EXPECT_TRUE(checkParentheses(manager, "1-(1+1)", "1-(1+1)"));
	EXPECT_TRUE(checkParentheses(manager, "1+(1-1)", "1+(1-1)"));
	EXPECT_TRUE(checkParentheses(manager, "1-1-1", "1-1-1"));
	EXPECT_TRUE(checkParentheses(manager, "(1-1)-1", "1-1-1"));
	EXPECT_TRUE(checkParentheses(manager, "1-(1-1)", "1-(1-1)"));
	EXPECT_TRUE(checkParentheses(manager, "1*1*1", "1*1*1"));
	EXPECT_TRUE(checkParentheses(manager, "(1+2)/3", "(1+2)/3"));
	EXPECT_TRUE(checkParentheses(manager, "1+(2/3)", "1+2/3"));
	EXPECT_TRUE(checkParentheses(manager, "(1*2)/3", "1*2/3"));
	EXPECT_TRUE(checkParentheses(manager, "1*(2/3)", "1*(2/3)"));

	EXPECT_FALSE(checkParentheses(manager, "(1)", "1"));
	EXPECT_FALSE(checkParentheses(manager, "(1+1)", "1+1"));
	EXPECT_FALSE(checkParentheses(manager, "(1+1+1)", "1+1+1"));
	EXPECT_FALSE(checkParentheses(manager, "3*(1+2)", "3*1+2"));
	EXPECT_FALSE(checkParentheses(manager, "(1*2)/3", "(1*2)/3"));
	EXPECT_FALSE(checkParentheses(manager, "1*2/3", "(1*2)/3"));

}

	// Not "testable" ... lazy eval is always adding the functions and thus
	// its not comparable (except if we copy the whole thing).
	// But checking by hand has shown, that the results are correct
/*
TEST(Parentheses, BasicLogicOp) {
	NodeManager manager;

	EXPECT_TRUE(checkParentheses(manager, "(true&&true)", "true&&true"));
	EXPECT_TRUE(checkParentheses(manager, "(true||false)", "true||false"));
	EXPECT_TRUE(checkParentheses(manager, "true&&false||true", "true&&false||true"));
}
*/

TEST(Parentheses, BasicBitwiseOp) {
	NodeManager manager;

	EXPECT_TRUE(checkParentheses(manager, "(true&false)", "(true&false)"));
	EXPECT_TRUE(checkParentheses(manager, "(true|false)", "(true|false)"));
	EXPECT_TRUE(checkParentheses(manager, "(true ^ false)", "(true^false)"));
	EXPECT_TRUE(checkParentheses(manager, "true&true|false", "true&true|false"));
	EXPECT_TRUE(checkParentheses(manager, "false ^ false|false", "false^false|false"));
	EXPECT_TRUE(checkParentheses(manager, "true ^ true&false", "true^true&false"));
	EXPECT_TRUE(checkParentheses(manager, "true&true ^ false", "true&true^false"));


}
TEST(Parentheses, BasicGeometricOp) {
	NodeManager manager;

	EXPECT_TRUE(checkParentheses(manager, "(1<2)", "(1<2)"));
	EXPECT_TRUE(checkParentheses(manager, "(1>2)", "(1>2)"));
	EXPECT_TRUE(checkParentheses(manager, "(1<=2)", "(1<=2)"));
	EXPECT_TRUE(checkParentheses(manager, "(1>=2)", "(1>=2)"));
	EXPECT_TRUE(checkParentheses(manager, "(1==2)", "(1==2)"));
	EXPECT_TRUE(checkParentheses(manager, "(1!=2)", "(1!=2)"));
	EXPECT_TRUE(checkParentheses(manager, "(1<2)==true", "1<2==true"));
	EXPECT_TRUE(checkParentheses(manager, "(1==2)!=true", "1==2!=true"));
	EXPECT_TRUE(checkParentheses(manager, "false==(2!=3)", "false==(2!=3)"));

	EXPECT_FALSE(checkParentheses(manager, "(1==2)!=true", "(1==2)!=true"));
	EXPECT_FALSE(checkParentheses(manager, "1==2!=true", "(1==2)!=true"));
	EXPECT_FALSE(checkParentheses(manager, "(1<2)!=true", "(1<2)!=true"));
	EXPECT_FALSE(checkParentheses(manager, "(1>=2)!=true", "(1>=2)!=true"));
	EXPECT_FALSE(checkParentheses(manager, "(1<2)!=true", "(1<2)!=true"));
	EXPECT_FALSE(checkParentheses(manager, "(1==2)!=true", "(1==2)!=true"));
}

}
}
}

















