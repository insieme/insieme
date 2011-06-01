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

#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/ast_builder.h"

namespace insieme {
namespace core {
namespace arithmetic {


TEST(ArithmeticUtilsTest, fromIR) {

	NodeManager manager;
	ASTBuilder builder(manager);
	const lang::BasicGenerator& basic = builder.getBasicGenerator();

	TypePtr type = builder.getBasicGenerator().getInt4();

	LiteralPtr one = builder.intLit(1);
	LiteralPtr two = builder.intLit(2);
	VariablePtr varA = builder.variable(type, 1);
	VariablePtr varB = builder.variable(type, 2);
	VariablePtr varC = builder.variable(type, 3);

	// test constants
	EXPECT_EQ("1", toString(toFormula(one)));
	EXPECT_EQ("2", toString(toFormula(two)));

	// test variables
	EXPECT_EQ("v1", toString(toFormula(varA)));

	// test incorrect expression
	EXPECT_THROW(toFormula(builder.stringLit("hello")), NotAFormulaException);

	// test add operations
	ExpressionPtr tmp;
	tmp = builder.callExpr(basic.getOperator(type, lang::BasicGenerator::Add), varA, one);
	EXPECT_EQ("v1+1", toString(toFormula(tmp)));

	// also test multiplication
	tmp = builder.callExpr(basic.getOperator(type, lang::BasicGenerator::Mul), varB, tmp);
	EXPECT_EQ("v1*v2+v2", toString(toFormula(tmp)));

	// and subtraction
	tmp = builder.callExpr(basic.getOperator(type, lang::BasicGenerator::Sub), tmp, varC);
	EXPECT_EQ("v1*v2+v2-v3", toString(toFormula(tmp)));

}

TEST(ArithmeticTest, toIR) {
	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr type = builder.getBasicGenerator().getInt4();

	VariablePtr varA = builder.variable(type, 1);
	VariablePtr varB = builder.variable(type, 2);
	VariablePtr varC = builder.variable(type, 3);


	Formula f = varA + varB;
	EXPECT_EQ("v1+v2", toString(f));
	EXPECT_EQ("int.add(v1, v2)", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));

	f = 2*varA + varB;
	EXPECT_EQ("2*v1+v2", toString(f));
	EXPECT_EQ("int.add(int.mul(2, v1), v2)", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));


	// test varA^2
	f = 2*varA*varA + varB*varA;
	EXPECT_EQ("2*v1^2+v1*v2", toString(f));
	EXPECT_EQ("int.add(int.mul(2, int.mul(v1, v1)), int.mul(v1, v2))", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));


	Product one;
	f = one / varA;
	EXPECT_EQ("v1^-1", toString(f));
	EXPECT_EQ("int.div(1, v1)", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));


	f = one / (varA*varA*varA);
	EXPECT_EQ("v1^-3", toString(f));
	EXPECT_EQ("int.div(1, int.mul(int.mul(v1, v1), v1))", toString(*toIR(manager, f)));
	EXPECT_EQ(f, toFormula(toIR(manager, f)));
	EXPECT_EQ(toIR(manager,f), toIR(manager, toFormula(toIR(manager, f))));

}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme

