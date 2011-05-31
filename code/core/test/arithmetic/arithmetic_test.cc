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

	EXPECT_EQ("v1+v2+5", toString(1 + 1 + A + 3 + B));
}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme

