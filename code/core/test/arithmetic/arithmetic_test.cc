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

#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace arithmetic {

	TEST(ArithmeticTest, Rational) {
		Rational a = 3;
		Rational b = 4;
		Rational c(1, 2);
		Rational d(4, 5);

		EXPECT_EQ("3", toString(a));
		EXPECT_EQ("4", toString(b));
		EXPECT_EQ("1/2", toString(c));
		EXPECT_EQ("4/5", toString(d));

		EXPECT_EQ("7", toString(a + b));
		EXPECT_EQ("13/10", toString(c + d));

		EXPECT_EQ("13/20", toString((c + d) * c));
		EXPECT_EQ("13/5", toString((c + d) / c));

		EXPECT_EQ("3/4", toString(a / b));
		EXPECT_EQ("12", toString(a * b));

		EXPECT_TRUE(c != c.invert());
		EXPECT_TRUE(c == c.invert().invert());
		// EXPECT_NE(c, c.invert());
		// EXPECT_EQ(c, c.invert().invert());

		EXPECT_TRUE(c < d);
		EXPECT_TRUE(c <= d);
		EXPECT_TRUE(c <= c);

		EXPECT_FALSE(c < c);

		EXPECT_TRUE(c >= c);
		EXPECT_FALSE(c > c);
	}

	TEST(ArithmeticTest, Rational_BugReport_NotProperlyReduced) {
		// this was reported to throw an exception
		Rational a = -18;
		Rational b = 1;

		Rational c = a / b;

		EXPECT_EQ("-18", toString(c));

		// works just fine ... so, test for a lot of values
	}

	TEST(ArithmeticTest, Values) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr varA = builder.variable(type, 1);
		VariablePtr varB = builder.variable(type, 2);
		VariablePtr varC = builder.variable(type, 3);

		TypePtr str_type = builder.getLangBasic().getString();
		auto str_A = builder.parseExpr("\"A\"");
		auto str_B = builder.parseExpr("\"B\"");

		EXPECT_PRED1(Value::isValue, varA);
		EXPECT_PRED1(Value::isValue, varB);
		EXPECT_PRED1(Value::isValue, varC);

		EXPECT_PRED1(Value::isValue, str_A);
		EXPECT_PRED1(Value::isValue, str_B);

		// check < operator
		EXPECT_LT(Value(varA), Value(varB));
		EXPECT_LT(Value(varA), Value(varC));

		// check literal support
		LiteralPtr litA = builder.literal(type, "123");
		LiteralPtr litB = builder.literal(builder.refType(type), "X");

		EXPECT_FALSE(Value::isValue(litA));
		EXPECT_PRED1(Value::isValue, builder.deref(litB));
	}

	TEST(ArithmeticTest, ValueConstructor) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr var = builder.variable(type, 1);

		LiteralPtr funA = builder.literal(builder.functionType(type, type), "op");
		EXPECT_FALSE(isValueConstructor(funA));

		ExpressionPtr callA = builder.callExpr(funA, var);
		EXPECT_FALSE(Value::isValue(callA));

		markAsValueConstructor(funA);
		EXPECT_TRUE(isValueConstructor(funA));
		EXPECT_PRED1(Value::isValue, callA);
	}


	TEST(ArithmeticTest, Products) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr varA = builder.variable(type, 1);
		VariablePtr varB = builder.variable(type, 2);
		VariablePtr varC = builder.variable(type, 3);

		// test the product of variables
		Product one;
		EXPECT_EQ("1", toString(one));
		EXPECT_TRUE(one.isOne());

		Product A = varA;
		EXPECT_EQ("v1", toString(A));
		EXPECT_FALSE(A.isOne());

		Product B = varB;
		EXPECT_EQ("v2", toString(B));

		Product C = varC;
		EXPECT_EQ("v3", toString(C));

		EXPECT_EQ("v1*v2", toString(A * B));

		EXPECT_EQ("v1^2", toString(A * A));

		EXPECT_EQ("v1^2*v2", toString(A * A * B));
		EXPECT_EQ("v1^2*v2", toString(A * B * A));

		EXPECT_EQ("1", toString(A / A));
		EXPECT_EQ("v1^-1", toString(one / A));

		EXPECT_EQ("v1^2*v2*v3", toString((C * A * B * A * C) / C));

		// some equality tests
		EXPECT_EQ(A, A);
		EXPECT_EQ(A, A * one);
		EXPECT_EQ(one, A / A);
		EXPECT_EQ(A, A * (B / B));
		EXPECT_EQ(A * A, A * A);
		EXPECT_EQ(A * A * B * C, A * A * B * C);

		// some inequality tests
		EXPECT_NE(A, one);
		EXPECT_NE(A, B);
		EXPECT_NE(A * A, A);
		EXPECT_NE(A * A, A * A * B);


		// fix ordering of products
		EXPECT_LT(C, one);
		EXPECT_LT(B, C);
		EXPECT_LT(A, B);

		EXPECT_LT(A * A, A);
		EXPECT_LT(A * A, A * B);
		EXPECT_LT(A * B, B * B);
		EXPECT_LT(A * A, B * B);


		EXPECT_LT(A, one);
		EXPECT_LT(A * A, one);
		EXPECT_LT(A * A, A);
		EXPECT_LT(A, B);
		EXPECT_LT(B, C);
		EXPECT_LT(A, B * B);
	}

	TEST(ArithmeticTest, Formula) {
		NodeManager manager;
		IRBuilder builder(manager);

		Formula f;
		EXPECT_EQ("0", toString(f));

		f = 5;
		EXPECT_EQ("5", toString(f));

		f = -5;
		EXPECT_EQ("-5", toString(f));

		// test sum
		f = 5;
		f = f + 2;
		EXPECT_EQ("7", toString(f));

		f = 5 - 2;
		EXPECT_EQ("3", toString(f));

		// test variables
		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr varA = builder.variable(type, 1);
		VariablePtr varB = builder.variable(type, 2);
		VariablePtr varC = builder.variable(type, 3);

		// test the product of variables
		Formula zero;
		EXPECT_EQ("0", toString(zero));

		Product one;
		EXPECT_EQ("1", toString(one));

		Product A = varA;
		EXPECT_EQ("v1", toString(A));

		Product B = varB;
		EXPECT_EQ("v2", toString(B));

		Product C = varC;
		EXPECT_EQ("v3", toString(C));

		Formula tmp = zero + one;
		EXPECT_EQ("1", toString(tmp));

		tmp = one + one;
		EXPECT_EQ("2", toString(tmp));

		// introduce variables
		EXPECT_EQ("v1", toString(Formula(A)));

		EXPECT_EQ("v1+2", toString(2 + A));
		EXPECT_EQ("2*v1+v2+5", toString(1 + 1 + A + 3 + B + A));

		EXPECT_EQ("2*v1", toString(2 * A));
		EXPECT_EQ("2*v1+3*v2+5", toString(2 * A + 3 * B + 5));

		EXPECT_EQ("v1^2+2*v1+1", toString((A + 1) * (A + 1)));
		EXPECT_EQ("v1^2-1", toString((A + 1) * (A - 1)));

		EXPECT_EQ("v1^2-1", toString((varA + 1) * (varA - 1)));

		EXPECT_EQ("-v1+1", toString(1 - varA));

		// causing a coefficient == 0
		EXPECT_EQ("0", toString((varA - 1) + (1 - varA)));
	}


	TEST(ArithmeticTest, ProductProperties) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr varA = builder.variable(type, 1);
		VariablePtr varB = builder.variable(type, 2);

		Product one;

		EXPECT_TRUE(one.isOne());
		EXPECT_FALSE(one.isValue());
		EXPECT_TRUE(one.isLinear());
		EXPECT_FALSE(one.isUnivariate());
		EXPECT_TRUE(one.isPolynomial());

		Product tmp;

		tmp = varA;
		EXPECT_FALSE(tmp.isOne());
		EXPECT_TRUE(tmp.isValue());
		EXPECT_TRUE(tmp.isLinear());
		EXPECT_TRUE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = varA * varA;
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_TRUE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = one / varA;
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_TRUE(tmp.isUnivariate());
		EXPECT_FALSE(tmp.isPolynomial());

		tmp = varA * varB;
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_FALSE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = tmp * varA;
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_FALSE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		// check for 1 = 1
		EXPECT_EQ(Product(), varA * (one / varA));
	}


	TEST(ArithmeticTest, FormulaProperties) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr varA = builder.variable(type, 1);
		VariablePtr varB = builder.variable(type, 2);

		Formula zero;
		EXPECT_TRUE(zero.isZero());
		EXPECT_FALSE(zero.isOne());
		EXPECT_TRUE(zero.isConstant());
		EXPECT_FALSE(zero.isValue());
		EXPECT_TRUE(zero.isLinear());
		EXPECT_FALSE(zero.isUnivariate());
		EXPECT_TRUE(zero.isPolynomial());

		Formula one = Product();
		EXPECT_FALSE(one.isZero());
		EXPECT_TRUE(one.isOne());
		EXPECT_TRUE(one.isConstant());
		EXPECT_FALSE(one.isValue());
		EXPECT_TRUE(one.isLinear());
		EXPECT_FALSE(one.isUnivariate());
		EXPECT_TRUE(one.isPolynomial());

		Formula tmp;

		tmp = varA;
		EXPECT_FALSE(tmp.isZero());
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isConstant());
		EXPECT_TRUE(tmp.isValue());
		EXPECT_TRUE(tmp.isLinear());
		EXPECT_TRUE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = 2 * varA;
		EXPECT_FALSE(tmp.isZero());
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isConstant());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_TRUE(tmp.isLinear());
		EXPECT_TRUE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = varA * varA;
		EXPECT_FALSE(tmp.isZero());
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isConstant());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_TRUE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = one / varA;
		EXPECT_FALSE(tmp.isZero());
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isConstant());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_TRUE(tmp.isUnivariate());
		EXPECT_FALSE(tmp.isPolynomial());

		tmp = varA * varB;
		EXPECT_FALSE(tmp.isZero());
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isConstant());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_FALSE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = tmp * varA;
		EXPECT_FALSE(tmp.isZero());
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isConstant());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_FALSE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = 2 * varA + 2 * varB + 3;
		EXPECT_FALSE(tmp.isZero());
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isConstant());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_TRUE(tmp.isLinear());
		EXPECT_FALSE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());

		tmp = 2 * varA + 2 * varA * varB + 3;
		EXPECT_FALSE(tmp.isZero());
		EXPECT_FALSE(tmp.isOne());
		EXPECT_FALSE(tmp.isConstant());
		EXPECT_FALSE(tmp.isValue());
		EXPECT_FALSE(tmp.isLinear());
		EXPECT_FALSE(tmp.isUnivariate());
		EXPECT_TRUE(tmp.isPolynomial());


		// check isConstant
		tmp = 1;
		EXPECT_TRUE(tmp.isConstant());
		tmp = tmp + 2;
		EXPECT_TRUE(tmp.isConstant());
		tmp = tmp - 10;
		EXPECT_TRUE(tmp.isConstant());
		tmp = tmp + varA;
		EXPECT_FALSE(tmp.isConstant());
		tmp = tmp - varA;
		EXPECT_TRUE(tmp.isConstant());


		// check for 0 = 0
		EXPECT_EQ(Formula(), varA + (zero - varA));
	}

	TEST(ArithmeticTest, TicketRequirement) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);


		// input: 2+4+3*i+(2+4)*j
		// output: 3*i+6*j+6

		EXPECT_EQ("3*v1+6*v2+6", toString(2 + 4 + 3 * i + (2 + 4) * j));
	}


	TEST(ArithmeticTest, Division) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);

		Formula tmp;

		tmp = tmp + 2;
		EXPECT_EQ("2", toString(tmp));

		EXPECT_EQ("3/2*v1", toString(9 * i / 6));

		EXPECT_EQ("-3/2*v1", toString(9 * i / -6));

		EXPECT_EQ("6*v2", toString(9 * j / 3 + 6 * j / 2));

		EXPECT_EQ("0", toString(9 * j / 3 + 6 * j / -2));

		tmp = tmp / 2;
		EXPECT_EQ("1", toString(tmp));

		tmp = 2 * i + 2 * j + 2;
		EXPECT_EQ("2*v1+2*v2+2", toString(tmp));

		tmp = tmp / 2;
		EXPECT_EQ("v1+v2+1", toString(tmp));

		tmp = 2 * i + 2 * j + 3;
		EXPECT_EQ("2*v1+2*v2+3", toString(tmp));

		tmp = tmp / 2;
		EXPECT_EQ("v1+v2+3/2", toString(tmp));


		// with variables
		tmp = i * i - j * i - 2;
		EXPECT_EQ("v1^2-v1*v2-2", toString(tmp));

		tmp = tmp / i;
		EXPECT_EQ("v1-v2-2*v1^-1", toString(tmp));
	}

	TEST(ArithmeticTest, Degree) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);

		Formula tmp;

		tmp = tmp + 2;
		EXPECT_EQ("2", toString(tmp));
		EXPECT_EQ(0u, tmp.getDegree());

		tmp = 2 * i + (j ^ 2) * 2 + 2;
		EXPECT_EQ("2*v1+2*v2^2+2", toString(tmp));
		EXPECT_EQ(2u, tmp.getDegree());

		tmp = 2 * (i ^ 3) * j + 2 * (j ^ 2) + 3;
		EXPECT_EQ("2*v1^3*v2+2*v2^2+3", toString(tmp));
		EXPECT_EQ(4u, tmp.getDegree());

		// with variables
		tmp = (i ^ 3) * (j ^ 2) - (j ^ 5) * i - 2;
		EXPECT_EQ("v1^3*v2^2-v1*v2^5-2", toString(tmp));
		EXPECT_EQ(6u, tmp.getDegree());
	}

	TEST(ArithmeticTest, ProductSubscriptOperator) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);


		Product one;

		Product p;
		EXPECT_EQ(0, p[i]);
		EXPECT_EQ(0, p[j]);

		p = i;
		EXPECT_EQ(1, p[i]);
		EXPECT_EQ(0, p[j]);

		p = i * j;
		EXPECT_EQ(1, p[i]);
		EXPECT_EQ(1, p[j]);

		p = i * j * i;
		EXPECT_EQ(2, p[i]);
		EXPECT_EQ(1, p[j]);

		p = one / (i * j * i);
		EXPECT_EQ(-2, p[i]);
		EXPECT_EQ(-1, p[j]);

		p = one / (i * i);
		EXPECT_EQ(-2, p[i]);
		EXPECT_EQ(0, p[j]);
	}


	TEST(ArithmeticTest, FormulaSubscriptOperator) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);


		// input: 2+4+3*i+(2+4)*j
		// output: 3*i+6*j+6

		auto f = 2 + 4 + 3 * i + (2 + 4) * j;
		EXPECT_EQ("3*v1+6*v2+6", toString(f));

		Product one;
		EXPECT_EQ(3, static_cast<int64_t>(f[i]));
		EXPECT_EQ(6, static_cast<int64_t>(f[j]));
		EXPECT_EQ(6, static_cast<int64_t>(f[one]));

		f = 2 * i * j + 3 * i - 2;
		EXPECT_EQ(2, static_cast<int64_t>(f[i * j]));
		EXPECT_EQ(3, static_cast<int64_t>(f[i]));
		EXPECT_EQ(-2, static_cast<int64_t>(f[one]));

		// some none-existing terms
		EXPECT_EQ(0, static_cast<int64_t>(f[i * i]));
		EXPECT_EQ(0, static_cast<int64_t>(f[j]));
	}

	TEST(ArithmeticTest, NastyExample) {
		NodeManager manager;
		IRBuilder builder(manager);

		// from expr: int.add(int.add(int.mul(int.mul(0, 4), 4), int.mul(0, 4)), v112)
		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr var = builder.variable(type, 1);

		auto f = (((0 * 4) * 4) + (0 * 4)) + var;
		EXPECT_EQ("v1", toString(f));
	}


	TEST(ArithmeticTest, Constraint) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr var = builder.variable(type, 1);

		auto f = 4 * var;
		auto g = 4 + var;

		EXPECT_EQ("4*v1", toString(f));
		EXPECT_EQ("v1+4", toString(g));

		EXPECT_EQ("(3*v1-4 <= 0)", toString(f <= g));
		EXPECT_EQ("(!(3*v1-4 <= 0))", toString(f > g));

		EXPECT_EQ("(!(3*v1-4 <= 0)) or (3*v1-4 <= 0 and !(-3*v1+4 <= 0))", toString(ne(f, g)));

		EXPECT_EQ("false", toString(ne(f, f)));
		EXPECT_EQ("true", toString(eq(f, f)));

		EXPECT_EQ("false", toString(Constraint()));
		EXPECT_EQ("true", toString(!Constraint()));
	}


	TEST(ArithmeticTest, PiecewiseCreation) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);

		Piecewise pw1(2 * i <= j, 2 + 4 + 3 * i + (2 + 4) * j, 2);
		EXPECT_EQ("3*v1+6*v2+6 -> if (2*v1-v2 <= 0); 2 -> if (!(2*v1-v2 <= 0))", toString(pw1));

		Piecewise pw2(!(2 * i <= j), 2 + 4 + 3 * i + (2 + 4) * j, 2);
		EXPECT_EQ("3*v1+6*v2+6 -> if (!(2*v1-v2 <= 0)); 2 -> if (2*v1-v2 <= 0)", toString(pw2));
	}

	TEST(ArithmeticTest, PiecewiseCalculation) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);

		Piecewise a(i);
		Piecewise b(4);

		EXPECT_EQ("v1 -> if true", toString(a));
		EXPECT_EQ("4 -> if true", toString(b));


		EXPECT_EQ("v1+4 -> if true", toString(a + b));
		EXPECT_EQ("v1-4 -> if true", toString(a - b));
		EXPECT_EQ("4*v1 -> if true", toString(a * b));

		// with constraint
		Constraint c1 = Formula(i) <= 4;

		a = Piecewise(c1, i, 4);
		EXPECT_EQ("v1 -> if (v1-4 <= 0); 4 -> if (!(v1-4 <= 0))", toString(a));
		EXPECT_EQ("v1+4 -> if (v1-4 <= 0); 8 -> if (!(v1-4 <= 0))", toString(a + b));
		EXPECT_EQ("4*v1 -> if (v1-4 <= 0); 16 -> if (!(v1-4 <= 0))", toString(a * b));


		// with constraints on both sides
		Constraint c2 = Formula(i) <= 2;
		b = Piecewise(c2, 4, i);

		EXPECT_EQ("v1 -> if (v1-4 <= 0); 4 -> if (!(v1-4 <= 0))", toString(a));
		EXPECT_EQ("4 -> if (v1-2 <= 0); v1 -> if (!(v1-2 <= 0))", toString(b));
		EXPECT_EQ("v1+4 -> if (!(v1-4 <= 0) and !(v1-2 <= 0)) or (v1-4 <= 0 and v1-2 <= 0); "
		          "2*v1 -> if (v1-4 <= 0 and !(v1-2 <= 0)); "
		          "8 -> if (!(v1-4 <= 0) and v1-2 <= 0)",
		          toString(a + b));

		EXPECT_EQ("4*v1 -> if (!(v1-4 <= 0) and !(v1-2 <= 0)) or (v1-4 <= 0 and v1-2 <= 0); "
		          "v1^2 -> if (v1-4 <= 0 and !(v1-2 <= 0)); "
		          "16 -> if (!(v1-4 <= 0) and v1-2 <= 0)",
		          toString(a * b));

		// with the same constraints on both sides
		b = Piecewise(c1, 4, i);

		EXPECT_EQ("v1 -> if (v1-4 <= 0); 4 -> if (!(v1-4 <= 0))", toString(a));
		EXPECT_EQ("4 -> if (v1-4 <= 0); v1 -> if (!(v1-4 <= 0))", toString(b));
		EXPECT_EQ("v1+4 -> if true", toString(a + b));
		EXPECT_EQ("4*v1 -> if true", toString(a * b));
	}

	TEST(ArithmeticTest, PiecewiseCreation2) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);

		Piecewise pw1(2 * i >= j, 2 + 4 + 3 * i + (2 + 4) * j, 2);
		EXPECT_EQ("3*v1+6*v2+6 -> if (-2*v1+v2 <= 0); 2 -> if (!(-2*v1+v2 <= 0))", toString(pw1));

		Piecewise npw = -pw1;
		EXPECT_EQ("-3*v1-6*v2-6 -> if (-2*v1+v2 <= 0); -2 -> if (!(-2*v1+v2 <= 0))", toString(npw));

		Piecewise pw2(!(2 * i >= j), 2 + 4 + 3 * i + (2 + 4) * j, 2);
		EXPECT_EQ("3*v1+6*v2+6 -> if (!(-2*v1+v2 <= 0)); 2 -> if (-2*v1+v2 <= 0)", toString(pw2));
	}


	// TEST(ArithmeticTest, Error1) {
	//	NodeManager manager;
	//	IRBuilder builder(manager);
	//
	//	TypePtr type = builder.getLangBasic().getInt4();
	//	VariablePtr i = builder.variable(type, 1);
	//	VariablePtr j = builder.variable(type, 2);
	//
	//
	//	 Formula f1(Rational(-1,64)*i-Rational(63,64));
	//	 Formula f2(Rational(1,4194304)*i+Rational(63,4194304));
	//
	//	 Formula prod = f1*f2;
	//	 EXPECT_EQ("aa", toString(prod));
	//}


	TEST(ArithmeticTest, MinMaxAbs) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = builder.getLangBasic().getInt4();
		VariablePtr i = builder.variable(type, 1);
		VariablePtr j = builder.variable(type, 2);

		Piecewise pw1(i);
		Piecewise pw2(j);
		Piecewise pw3 = Formula(3);
		Piecewise pw4 = Formula(-4);

		EXPECT_EQ("v1 -> if (!(v1-v2 <= 0)); v2 -> if (v1-v2 <= 0)", toString(max(pw1, pw2)));
		EXPECT_EQ("v1 -> if (!(-v1+v2 <= 0)); v2 -> if (-v1+v2 <= 0)", toString(min(pw1, pw2)));
		EXPECT_EQ("v1 -> if (!(v1 <= 0)); -v1 -> if (v1 <= 0)", toString(abs(pw1)));

		EXPECT_EQ("v1 -> if (!(v1-3 <= 0)); 3 -> if (v1-3 <= 0)", toString(max(pw1, pw3)));
		EXPECT_EQ("v1 -> if (!(-v1+3 <= 0)); 3 -> if (-v1+3 <= 0)", toString(min(pw1, pw3)));
		EXPECT_EQ("3 -> if true", toString(abs(pw3)));
		EXPECT_EQ("4 -> if true", toString(abs(pw4)));
	}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme
