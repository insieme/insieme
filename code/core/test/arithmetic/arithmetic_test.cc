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

#include "insieme/core/ast_builder.h"

namespace insieme {
namespace core {
namespace arithmetic {


TEST(ArithmeticTest, Products) {

	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr type = builder.getBasicGenerator().getInt4();
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

	EXPECT_EQ("v1*v2", toString(A*B));

	EXPECT_EQ("v1^2", toString(A*A));

	EXPECT_EQ("v1^2*v2", toString(A*A*B));
	EXPECT_EQ("v1^2*v2", toString(A*B*A));

	EXPECT_EQ("1", toString(A/A));
	EXPECT_EQ("v1^-1", toString(one/A));

	EXPECT_EQ("v1^2*v2*v3", toString((C*A*B*A*C)/C));

	// some equality tests
	EXPECT_EQ(A, A);
	EXPECT_EQ(A, A*one);
	EXPECT_EQ(one, A/A);
	EXPECT_EQ(A, A*(B/B));
	EXPECT_EQ(A*A, A*A);
	EXPECT_EQ(A*A*B*C, A*A*B*C);

	// some inequality tests
	EXPECT_NE(A,one);
	EXPECT_NE(A,B);
	EXPECT_NE(A*A, A);
	EXPECT_NE(A*A, A*A*B);


	// fix ordering of products
	EXPECT_LT(C,one);
	EXPECT_LT(B,C);
	EXPECT_LT(A,B);

	EXPECT_LT(A*A, A);
	EXPECT_LT(A*A, A*B);
	EXPECT_LT(A*B, B*B);
	EXPECT_LT(A*A, B*B);


	EXPECT_LT(A, one);
	EXPECT_LT(A*A, one);
	EXPECT_LT(A*A, A);
	EXPECT_LT(A, B);
	EXPECT_LT(B, C);
	EXPECT_LT(A, B*B);
}

TEST(ArithmeticTest, Formula) {
	NodeManager manager;
	ASTBuilder builder(manager);

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
	TypePtr type = builder.getBasicGenerator().getInt4();
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

	EXPECT_EQ("v1+2", toString(2+A));
	EXPECT_EQ("2*v1+v2+5", toString(1 + 1 + A + 3 + B + A));

	EXPECT_EQ("2*v1", toString(2*A));
	EXPECT_EQ("2*v1+3*v2+5", toString(2*A + 3*B + 5));

	EXPECT_EQ("v1^2+2*v1+1", toString((A+1)*(A+1)));
	EXPECT_EQ("v1^2-1", toString((A+1)*(A-1)));

	EXPECT_EQ("v1^2-1", toString((varA+1)*(varA-1)));

	EXPECT_EQ("-v1+1", toString(1-varA));

	// causing a coefficient == 0
	EXPECT_EQ("0", toString((varA-1)+(1-varA)));


}


TEST(ArithmeticTest, ProductProperties) {

	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr type = builder.getBasicGenerator().getInt4();
	VariablePtr varA = builder.variable(type, 1);
	VariablePtr varB = builder.variable(type, 2);

	Product one;

	EXPECT_TRUE(one.isOne());
	EXPECT_TRUE(one.isLinear());
	EXPECT_TRUE(one.isPolynomial());

	Product tmp;

	tmp = varA;
	EXPECT_FALSE(tmp.isOne());
	EXPECT_TRUE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	tmp = varA * varA;
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	tmp = one / varA;
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isLinear());
	EXPECT_FALSE(tmp.isPolynomial());

	tmp = varA * varB;
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	tmp = tmp * varA;
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	// check for 1 = 1
	EXPECT_EQ(Product(), varA * (one/varA));
}


TEST(ArithmeticTest, FormulaProperties) {

	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr type = builder.getBasicGenerator().getInt4();
	VariablePtr varA = builder.variable(type, 1);
	VariablePtr varB = builder.variable(type, 2);

	Formula zero;
	EXPECT_TRUE(zero.isZero());
	EXPECT_FALSE(zero.isOne());
	EXPECT_TRUE(zero.isConstant());
	EXPECT_TRUE(zero.isLinear());
	EXPECT_TRUE(zero.isPolynomial());

	Formula one = Product();
	EXPECT_FALSE(one.isZero());
	EXPECT_TRUE(one.isOne());
	EXPECT_TRUE(one.isConstant());
	EXPECT_TRUE(one.isLinear());
	EXPECT_TRUE(one.isPolynomial());

	Formula tmp;

	tmp = varA;
	EXPECT_FALSE(tmp.isZero());
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isConstant());
	EXPECT_TRUE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	tmp = varA * varA;
	EXPECT_FALSE(tmp.isZero());
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isConstant());
	EXPECT_FALSE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	tmp = one / varA;
	EXPECT_FALSE(tmp.isZero());
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isConstant());
	EXPECT_FALSE(tmp.isLinear());
	EXPECT_FALSE(tmp.isPolynomial());

	tmp = varA * varB;
	EXPECT_FALSE(tmp.isZero());
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isConstant());
	EXPECT_FALSE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	tmp = tmp * varA;
	EXPECT_FALSE(tmp.isZero());
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isConstant());
	EXPECT_FALSE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	tmp = 2*varA + 2*varB + 3;
	EXPECT_FALSE(tmp.isZero());
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isConstant());
	EXPECT_TRUE(tmp.isLinear());
	EXPECT_TRUE(tmp.isPolynomial());

	tmp = 2*varA + 2*varA*varB + 3;
	EXPECT_FALSE(tmp.isZero());
	EXPECT_FALSE(tmp.isOne());
	EXPECT_FALSE(tmp.isConstant());
	EXPECT_FALSE(tmp.isLinear());
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
	ASTBuilder builder(manager);

	TypePtr type = builder.getBasicGenerator().getInt4();
	VariablePtr i = builder.variable(type, 1);
	VariablePtr j = builder.variable(type, 2);


	// input: 2+4+3*i+(2+4)*j
	// output: 3*i+6*j+6

	EXPECT_EQ("3*v1+6*v2+6", toString(2+4+3*i+(2+4)*j));

}


TEST(ArithmeticTest, Division) {

	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr type = builder.getBasicGenerator().getInt4();
	VariablePtr i = builder.variable(type, 1);
	VariablePtr j = builder.variable(type, 2);


	Formula tmp;

	tmp = tmp + 2;
	EXPECT_EQ("2", toString(tmp));

	tmp = tmp / 2;
	EXPECT_EQ("1", toString(tmp));

	tmp = 2*i + 2*j + 2;
	EXPECT_EQ("2*v1+2*v2+2", toString(tmp));

	tmp = tmp / 2;
	EXPECT_EQ("v1+v2+1", toString(tmp));

	tmp = 2*i + 2*j + 3;
	EXPECT_EQ("2*v1+2*v2+3", toString(tmp));

	tmp = tmp / 2;
	EXPECT_EQ("v1+v2+1", toString(tmp));


	// with variables
	tmp = i * i - j * i - 2;
	EXPECT_EQ("v1^2-v1*v2-2", toString(tmp));

	tmp = tmp / i;
	EXPECT_EQ("v1-v2-2*v1^-1", toString(tmp));

}

TEST(ArithmeticTest, ProductSubscriptOperator) {


	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr type = builder.getBasicGenerator().getInt4();
	VariablePtr i = builder.variable(type, 1);
	VariablePtr j = builder.variable(type, 2);


	Product one;

	Product p;
	EXPECT_EQ(0, p[i]);
	EXPECT_EQ(0, p[j]);

	p = i;
	EXPECT_EQ(1, p[i]);
	EXPECT_EQ(0, p[j]);

	p = i*j;
	EXPECT_EQ(1, p[i]);
	EXPECT_EQ(1, p[j]);

	p = i*j*i;
	EXPECT_EQ(2, p[i]);
	EXPECT_EQ(1, p[j]);

	p = one/(i*j*i);
	EXPECT_EQ(-2, p[i]);
	EXPECT_EQ(-1, p[j]);

	p = one/(i*i);
	EXPECT_EQ(-2, p[i]);
	EXPECT_EQ(0, p[j]);

}


TEST(ArithmeticTest, FormulaSubscriptOperator) {

	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr type = builder.getBasicGenerator().getInt4();
	VariablePtr i = builder.variable(type, 1);
	VariablePtr j = builder.variable(type, 2);


	// input: 2+4+3*i+(2+4)*j
	// output: 3*i+6*j+6

	auto f = 2+4+3*i+(2+4)*j;
	EXPECT_EQ("3*v1+6*v2+6", toString(f));

	Product one;
	EXPECT_EQ(3, f[i]);
	EXPECT_EQ(6, f[j]);
	EXPECT_EQ(6, f[one]);

	f = 2 * i * j + 3 * i - 2;
	EXPECT_EQ(2, f[i*j]);
	EXPECT_EQ(3, f[i]);
	EXPECT_EQ(-2, f[one]);

	// some none-existing terms
	EXPECT_EQ(0, f[i*i]);
	EXPECT_EQ(0, f[j]);

}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme

