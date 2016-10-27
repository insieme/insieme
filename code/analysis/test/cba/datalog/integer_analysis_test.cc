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

#include "insieme/analysis/cba/datalog/integer_analysis.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace datalog {

	using namespace core;

	using SymbolTable = std::map<std::string,core::NodePtr>;

	IRBuilder& getBuilder() {
		static NodeManager mgr;
		static IRBuilder builder(mgr);
		return builder;
	}

	bool isInt32(const std::string &str) {
		auto &builder = getBuilder();
		auto type = builder.parseType("int<4>");
		auto lit = builder.literal(type, str);
		auto &opt_num = lit->template getValueAs<int32_t>();
		return !!opt_num;
	}

	int32_t getAsInt32(const std::string &str) {
		auto &builder = getBuilder();
		auto type = builder.parseType("int<4>");
		auto lit = builder.literal(type, str);
		auto &opt_num = lit->template getValueAs<int32_t>();
		assert(opt_num);
		return *opt_num;
	}

	TEST(IntegerAnalysis, Int32Check) {
		EXPECT_TRUE(isInt32("-10"));
		EXPECT_TRUE(isInt32("-1"));
		EXPECT_TRUE(isInt32("0"));
		EXPECT_TRUE(isInt32("1"));
		EXPECT_TRUE(isInt32("1234"));
		EXPECT_TRUE(isInt32("-2147483648")); /* int32 min */
		EXPECT_TRUE(isInt32("2147483647"));  /* int32 max */

		const std::string str = "267100722";
		printf("Resulting number from string '%s': %d\n", str.c_str(), getAsInt32(str));

		EXPECT_FALSE(isInt32(""));            /* empty */
		EXPECT_FALSE(isInt32(" "));
		EXPECT_FALSE(isInt32("hello world")); /* string */
		EXPECT_FALSE(isInt32("a"));
		EXPECT_FALSE(isInt32("1a"));          /* num is part of string */
		EXPECT_FALSE(isInt32("a1"));
		EXPECT_FALSE(isInt32(" 1"));
		EXPECT_FALSE(isInt32("1 "));
		EXPECT_FALSE(isInt32(" 1 "));
		EXPECT_FALSE(isInt32("1."));         /* floating points */
		EXPECT_FALSE(isInt32("1.0"));
		EXPECT_FALSE(isInt32("1.337"));
	}

	TEST(IntegerAnalysis, DISABLED_Int32OverflowDetection) {
		EXPECT_FALSE(isInt32("-2147483649")); /* invalid value range - int32 min - 1*/
		EXPECT_FALSE(isInt32("2147483648"));  /* int32 max + 1 */
		EXPECT_FALSE(isInt32("-4294967296")); /* int32 min << 2 */
		EXPECT_FALSE(isInt32("4294967294"));  /* int32 max << 2 */
	}

	IntegerSet getValues(const std::string& code, const SymbolTable& symbols = SymbolTable()) {
		auto expr = getBuilder().parseExpr(code, symbols);
//		std::cout << *expr << "\n";
//		std::cout << dumpText(expr) << "\n";
		datalog::Context ctxt;
		return getIntegerValues(ctxt,ExpressionAddress(expr));
	}


	TEST(IntegerAnalysis, Constants) {

		// actual integer constants
		EXPECT_EQ("{1}", toString(getValues("1")));
		EXPECT_EQ("{2}", toString(getValues("2")));
		EXPECT_EQ("{3}", toString(getValues("3")));

		// other constants
		EXPECT_EQ("{-all-}", toString(getValues("\"x\"")));
		EXPECT_TRUE(getValues("\"x\"").isUniversal());

	}

	TEST(IntegerAnalysis, Arithmetic) {

		// support simple operations
		EXPECT_EQ("{3}", toString(getValues("1+2")));

	}


	TEST(IntegerAnalysis, FreeVariables) {

		IRBuilder& builder = getBuilder();
		SymbolTable symbols;
		symbols["x"] = builder.variable(builder.parseType("int<4>"));

		// check that the free variable "x" is neither definitely true nor false
		EXPECT_TRUE(getValues("x",symbols).isUniversal());

		// compute with unknown values
		EXPECT_TRUE(getValues("x+5",symbols).isUniversal());

	}

	TEST(IntegerAnalysis, ReturnValue) {

		// test whether the return value of a function is deduced properly
		EXPECT_EQ("{15}",toString(getValues("()->int<4> { return 15; }()")));
		EXPECT_EQ("{15}",toString(getValues("()->int<8> { return 15; }()")));
		EXPECT_EQ("{15}",toString(getValues("()->uint<8> { return 15; }()")));

	}

	TEST(IntegerAnalysis, OneOutOfTwo) {

		IRBuilder& builder = getBuilder();
		SymbolTable symbols;
		symbols["x"] = builder.variable(builder.parseType("bool"));

		// test whether the return value of a function is deduced properly
		EXPECT_EQ("{15,16}",toString(getValues("(y : bool)->int<4> { if (y) { return 15; } return 16; }(x)", symbols)));

		// compute with multiple values
		EXPECT_EQ("{20,21}",toString(getValues("(y : bool)->int<4> { if (y) { return 15; } return 16; }(x) + 5", symbols)));

	}

	TEST(IntegerAnalysis, LocalVariable) {

		// test whether the return value of a function is deduced properly
		EXPECT_EQ("{12}",toString(getValues("()->int<4> { var int<4> x = 12; return x; }()")));

	}

	TEST(IntegerAnalysis, ParameterPassing) {

		// test whether the return value of a function is deduced properly
		EXPECT_EQ("{12}",toString(getValues("(x : 'a)->'a { return x; }(12)")));

		// check order of arguments
		EXPECT_EQ("{12}",toString(getValues("(a : 'a , b : 'a )->'a { return a; }(12,14)")));
		EXPECT_EQ("{14}",toString(getValues("(a : 'a , b : 'a )->'a { return b; }(12,14)")));

	}


	TEST(IntegerAnalysis, IntegerBasedControlFlow) {

		IRBuilder& builder = getBuilder();
		SymbolTable symbols;
		symbols["x"] = builder.variable(builder.parseType("int<4>"));

		// test whether the return value of a function is deduced properly
		EXPECT_EQ("{1}",toString(getValues("(y : int<4>)->int<4> { if (y < 5) { return 1; } return 2; }(4)")));

		EXPECT_EQ("{2}",toString(getValues("(y : int<4>)->int<4> { if (y < 5) { return 1; } return 2; }(8)")));

		EXPECT_EQ("{1,2}",toString(getValues("(y : int<4>)->int<4> { if (y < 5) { return 1; } return 2; }(x)",symbols)));

	}


	TEST(IntegerAnalysis, Equality) {
		IRBuilder& builder = getBuilder();
		SymbolTable symbols;
		Context ctxt;

		symbols["x"] = builder.variable(builder.parseType("bool"));

		auto genExprAddr = [&](const string &in) {
			return ExpressionAddress(builder.parseExpr(in, symbols));
		};

		auto expr0 = genExprAddr("(y : bool)->int<4> { return 1234; }(x)");
		auto expr1 = genExprAddr("(y : bool)->int<4> { if (y) { return 15; } return 20; }(x)");
		auto expr2 = genExprAddr("(y : bool)->int<4> { if (y) { return 42; } return 69; }(x)");
		auto expr3 = genExprAddr("(y : bool)->int<4> { if (y) { return 69; } return 42; }(x)");
		auto expr4 = genExprAddr("(y : bool)->int<4> { if (y) { return 12; } return 42; }(x)");

		EXPECT_TRUE (integer::areEqual(ctxt, expr0, expr0));
		EXPECT_FALSE(integer::areEqual(ctxt, expr0, expr1));
		EXPECT_FALSE(integer::areEqual(ctxt, expr0, expr2));
		EXPECT_FALSE(integer::areEqual(ctxt, expr0, expr3));
		EXPECT_FALSE(integer::areEqual(ctxt, expr1, expr0));
		EXPECT_FALSE(integer::areEqual(ctxt, expr1, expr1));
		EXPECT_FALSE(integer::areEqual(ctxt, expr1, expr2));
		EXPECT_FALSE(integer::areEqual(ctxt, expr1, expr3));

		EXPECT_TRUE (integer::mayEqual(ctxt, expr0, expr0));
		EXPECT_TRUE (integer::mayEqual(ctxt, expr1, expr1));
		EXPECT_TRUE (integer::mayEqual(ctxt, expr2, expr2));
		EXPECT_TRUE (integer::mayEqual(ctxt, expr2, expr3));
		EXPECT_TRUE (integer::mayEqual(ctxt, expr3, expr4));
		EXPECT_FALSE(integer::mayEqual(ctxt, expr0, expr1));
		EXPECT_FALSE(integer::mayEqual(ctxt, expr1, expr2));
		EXPECT_FALSE(integer::mayEqual(ctxt, expr1, expr3));
		EXPECT_FALSE(integer::mayEqual(ctxt, expr1, expr4));

		EXPECT_TRUE (integer::areNotEqual(ctxt, expr0, expr1));
		EXPECT_TRUE (integer::areNotEqual(ctxt, expr0, expr2));
		EXPECT_TRUE (integer::areNotEqual(ctxt, expr0, expr3));
		EXPECT_TRUE (integer::areNotEqual(ctxt, expr0, expr4));
		EXPECT_TRUE (integer::areNotEqual(ctxt, expr1, expr0));
		EXPECT_TRUE (integer::areNotEqual(ctxt, expr1, expr2));
		EXPECT_TRUE (integer::areNotEqual(ctxt, expr1, expr3));
		EXPECT_TRUE (integer::areNotEqual(ctxt, expr1, expr4));
		EXPECT_FALSE(integer::areNotEqual(ctxt, expr0, expr0));
		EXPECT_FALSE(integer::areNotEqual(ctxt, expr1, expr1));
		EXPECT_FALSE(integer::areNotEqual(ctxt, expr2, expr3));
		EXPECT_FALSE(integer::areNotEqual(ctxt, expr2, expr4));
		EXPECT_FALSE(integer::areNotEqual(ctxt, expr3, expr4));

		EXPECT_TRUE (integer::mayNotEqual(ctxt, expr1, expr1));
		EXPECT_TRUE (integer::mayNotEqual(ctxt, expr1, expr2));
		EXPECT_TRUE (integer::mayNotEqual(ctxt, expr1, expr3));
		EXPECT_TRUE (integer::mayNotEqual(ctxt, expr1, expr4));
		EXPECT_TRUE (integer::mayNotEqual(ctxt, expr2, expr2));
		EXPECT_TRUE (integer::mayNotEqual(ctxt, expr2, expr3));
		EXPECT_TRUE (integer::mayNotEqual(ctxt, expr2, expr4));
		EXPECT_FALSE(integer::mayNotEqual(ctxt, expr0, expr0));
	}


} // end namespace datalog
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

