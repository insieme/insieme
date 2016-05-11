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

#include "insieme/analysis/datalog/boolean_value.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace datalog {

	using namespace core;

	using SymbolTable = std::map<std::string,core::NodePtr>;

	IRBuilder& getBuilder() {
		static NodeManager mgr;
		static IRBuilder builder(mgr);
		return builder;
	}

	bool isTrue(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		return isTrue(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
	}

	bool isFalse(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		return isFalse(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
	}

	bool mayBeTrue(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		return mayBeTrue(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
	}

	bool mayBeFalse(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		return mayBeFalse(ExpressionAddress(getBuilder().parseExpr(code, symbols)));
	}


	TEST(BooleanValue, Constants) {

		EXPECT_TRUE(isTrue("true"));
		EXPECT_TRUE(isFalse("false"));
		EXPECT_TRUE(isTrue("true"));
		EXPECT_TRUE(isFalse("false"));

		EXPECT_TRUE(mayBeTrue("true"));
		EXPECT_TRUE(mayBeFalse("false"));

		EXPECT_FALSE(isTrue("false"));
		EXPECT_FALSE(isFalse("true"));

		EXPECT_FALSE(mayBeTrue("false"));
		EXPECT_FALSE(mayBeFalse("true"));
	}


	TEST(BooleanValue, StringConstants) {

		// check string constants (should be neither true nor false)
		EXPECT_FALSE(isTrue("\"x\""));
		EXPECT_FALSE(isFalse("\"x\""));
		EXPECT_TRUE(mayBeTrue("\"x\""));
		EXPECT_TRUE(mayBeFalse("\"x\""));

	}

	TEST(BooleanValue, FreeVariables) {

		IRBuilder& builder = getBuilder();
		SymbolTable symbols;
		symbols["x"] = builder.variable(builder.parseType("bool"));

		// check that the free variable "x" is neither definitely true nor false
		EXPECT_FALSE(isTrue("x",symbols));
		EXPECT_FALSE(isFalse("x",symbols));
		EXPECT_TRUE(mayBeTrue("x",symbols));
		EXPECT_TRUE(mayBeFalse("x",symbols));

	}

	TEST(BooleanValue, ReturnValue) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( isTrue("()->bool { return true; }()"));
		EXPECT_TRUE(isFalse("()->bool { return false; }()"));

	}

	TEST(BooleanValue, LocalVariable) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( isTrue("()->bool { var bool x = true; return x; }()"));
		EXPECT_TRUE(isFalse("()->bool { var bool x = false; return x; }()"));

	}

	TEST(BooleanValue, ParameterPassing) {

		// test whether the return value of a function is deduced properly
		EXPECT_TRUE( isTrue("(x : 'a)->'a { return x; }(true)"));
		EXPECT_TRUE(isFalse("(x : 'a)->'a { return x; }(false)"));

		// check order of arguments
		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { return a; }(true,false)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { return b; }(true,false)"));

	}

	TEST(BooleanValue, ControlFlow) {

		// test whether control flow restrictions are considered
		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(true,true)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(true,false)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(false,true)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { if (a) { return b; } return false; }(false,false)"));

		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(true,true)"));
		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(true,false)"));
		EXPECT_TRUE( isTrue("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(false,true)"));
		EXPECT_TRUE(isFalse("( a:'a, b:'a )->'a { if (a) { return true; } return b; }(false,false)"));


//		EXPECT_TRUE( isTrue("true && true"));
//		EXPECT_TRUE(isFalse("true && false"));

	}


} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

