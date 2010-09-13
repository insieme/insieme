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

#include <sstream>

#include <gtest/gtest.h>
#include "statements.h"
#include "expressions.h"
#include "ast_builder.h"
#include "lang_basic.h"

#include "ast_node_test.cc"

using namespace insieme::core;
using namespace insieme::core::lang;

TEST(ExpressionsTest, IntLiterals) {
	ASTBuilder builder;

	LiteralPtr i5 = builder.literal("5", TYPE_INT_GEN_PTR);
	LiteralPtr i7 = builder.literal("7", TYPE_INT_GEN_PTR);
	LiteralPtr i5long = builder.literal("5", TYPE_INT_8_PTR);
	
	EXPECT_EQ( *i5, *builder.literal("5", TYPE_INT_GEN_PTR) );
	EXPECT_NE( *i5, *i5long );
	EXPECT_NE( *i5, *i7 );
	EXPECT_EQ( i5->getValueAs<int>(), 5 );
}

TEST(ExpressionsTest, FloatLiterals) {
	ASTBuilder builder;

	LiteralPtr f5_s = builder.literal("5.0", TYPE_REAL_4_PTR);
	
	// EXPECT_EQ( *f5, *f5_s ); //-- this is not necessarily true
	std::stringstream ss;
	ss << *f5_s;
	EXPECT_EQ( ss.str(), "5.0" );
//	EXPECT_EQ( f5->getValue(), f5_s->getValue() );
}


TEST(ExpressionsTest, RecursiveLambda) {
	ASTBuilder builder;

	// create a recursive even/odd example
	FunctionTypePtr functionType = builder.functionType(lang::TYPE_UINT_4_PTR, lang::TYPE_BOOL_PTR);
	VarExprPtr evenVar = builder.varExpr(functionType, "even");
	VarExprPtr oddVar = builder.varExpr(functionType, "odd");


	LambdaExpr::ParamList param;
	param.push_back(builder.paramExpr(lang::TYPE_UINT_4_PTR, "x"));

	VarExprPtr lessOrEqual = builder.varExpr(lang::TYPE_COMPARISON_UINT_OP_PTR, "==");

	VarExprPtr x = builder.varExpr(lang::TYPE_UINT_4_PTR, "x");
	ExpressionPtr condition = builder.callExpr(lessOrEqual,toVector<ExpressionPtr>(x,lang::CONST_UINT_ZERO_PTR));

	// build even body ...
	StatementPtr evenBody = builder.ifStmt(condition,
			builder.returnStmt(builder.literal("true", lang::TYPE_BOOL_PTR)),
			builder.returnStmt(builder.callExpr(oddVar, toVector<ExpressionPtr>(x))));
	LambdaExprPtr evenLambda = builder.lambdaExpr(functionType, param, evenBody);

	// build odd body ...
	StatementPtr oddBody = builder.ifStmt(condition,
				builder.returnStmt(builder.literal("false", lang::TYPE_BOOL_PTR)),
				builder.returnStmt(builder.callExpr(evenVar, toVector<ExpressionPtr>(x))));
	LambdaExprPtr oddLambda = builder.lambdaExpr(functionType, param, oddBody);

	// finish definition
	RecLambdaDefinition::RecFunDefs definitions;
	definitions.insert(std::make_pair(evenVar, evenLambda));
	definitions.insert(std::make_pair(oddVar, oddLambda));
	RecLambdaDefinitionPtr definition = builder.recLambdaDefinition(definitions);

	// test definition node
	basicNodeTests(definition, toList(toVector<NodePtr>(evenVar, evenLambda, oddVar, oddLambda)));

	// create recursive lambda nodes
	RecLambdaExprPtr even = builder.recLambdaExpr(evenVar, definition);
	RecLambdaExprPtr odd  = builder.recLambdaExpr(oddVar,  definition);

	basicNodeTests(even, toList(toVector<NodePtr>(evenVar, definition)));
	basicNodeTests(odd, toList(toVector<NodePtr>(oddVar, definition)));

	//EXPECT_EQ ("", toString(*even));
}
