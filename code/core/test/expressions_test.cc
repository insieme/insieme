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

#include "statements.h"
#include "expressions.h"
#include "ast_builder.h"
#include "lang_basic.h"
#include "set_utils.h"

#include "ast_node_test.cc"

namespace insieme {
namespace core {

using namespace insieme::core::lang;
using namespace insieme::utils::set;

template<typename PT>
void basicExprTests(PT expression, const TypePtr& type, const Node::ChildList& children = Node::ChildList());


TEST(ExpressionsTest, IntLiterals) {
	ASTBuilder builder;

	LiteralPtr i5 = builder.literal("5", TYPE_INT_GEN_PTR);
	LiteralPtr i7 = builder.literal("7", TYPE_INT_GEN_PTR);
	LiteralPtr i5long = builder.literal("5", TYPE_INT_8_PTR);
	
	EXPECT_EQ( *i5, *builder.literal("5", TYPE_INT_GEN_PTR) );
	EXPECT_NE( *i5, *i5long );
	EXPECT_NE( *i5, *i7 );
	EXPECT_EQ( i5->getValueAs<int>(), 5 );

	basicExprTests(i5, TYPE_INT_GEN_PTR, toList(toVector<NodePtr>(TYPE_INT_GEN_PTR)));
	basicExprTests(i7, TYPE_INT_GEN_PTR, toList(toVector<NodePtr>(TYPE_INT_GEN_PTR)));
	basicExprTests(i5long, TYPE_INT_8_PTR, toList(toVector<NodePtr>(TYPE_INT_8_PTR)));
}

TEST(ExpressionsTest, FloatLiterals) {
	ASTBuilder builder;

	LiteralPtr f5_s = builder.literal("5.0", TYPE_REAL_4_PTR);
	
	basicExprTests(f5_s, TYPE_REAL_4_PTR, toList(toVector<NodePtr>(TYPE_REAL_4_PTR)));

	// EXPECT_EQ( *f5, *f5_s ); //-- this is not necessarily true
	std::stringstream ss;
	ss << *f5_s;
	EXPECT_EQ( ss.str(), "5.0" );
//	EXPECT_EQ( f5->getValue(), f5_s->getValue() );
}

TEST(ExpressionsTest, VarExpr) {
	NodeManager manager;

	VarExprPtr var = VarExpr::get(manager, TYPE_BOOL_PTR, "valid");
	EXPECT_EQ ("valid", toString(*var));

	// check hash codes, children and cloning
	basicExprTests(var, TYPE_BOOL_PTR, toVector<NodePtr>(TYPE_BOOL_PTR));
}

TEST(ExpressionsTest, ParamExpr) {
	NodeManager manager;

	ParamExprPtr var = ParamExpr::get(manager, TYPE_BOOL_PTR, "valid");
	EXPECT_EQ ("bool valid", toString(*var));

	// check hash codes, children and cloning
	basicExprTests(var, TYPE_BOOL_PTR, toVector<NodePtr>(TYPE_BOOL_PTR));
}

TEST(ExpressionsTest, LambdaExpr) {
	NodeManager manager;

	LambdaExpr::ParamList list;
	list.push_back(ParamExpr::get(manager, TYPE_BOOL_PTR, "a"));
	list.push_back(ParamExpr::get(manager, TYPE_BOOL_PTR, "b"));

	StatementPtr body = ReturnStmt::get(manager, CONST_BOOL_TRUE_PTR);
	LambdaExprPtr expr = LambdaExpr::get(manager, TYPE_BINARY_BOOL_OP_PTR, list, body);

	EXPECT_EQ ("fun(bool a, bool b){ return true }", toString(*expr));

	// check hash codes, children and cloning
	basicExprTests(expr, TYPE_BINARY_BOOL_OP_PTR, toVector<NodePtr>(TYPE_BINARY_BOOL_OP_PTR, list[0], list[1], body));
}

TEST(ExpressionsTest, TupleExpr) {
	NodeManager manager;

	TupleExprPtr empty = TupleExpr::get(manager, toVector<ExpressionPtr>());
	TupleExprPtr more = TupleExpr::get(manager, toVector<ExpressionPtr>(CONST_BOOL_TRUE_PTR, CONST_UINT_ONE_PTR));

	TypePtr first = TupleType::get(manager, TupleType::ElementTypeList());
	TypePtr second = TupleType::get(manager, toVector<TypePtr>(TYPE_BOOL_PTR, TYPE_UINT_1_PTR));
	EXPECT_EQ ( *first , *empty->getType() );
	EXPECT_EQ ( *second, *more->getType() );

	EXPECT_EQ ("tuple()", toString(*empty));
	EXPECT_EQ ("tuple(true,1)", toString(*more));


	// check hash codes, children and cloning
	basicExprTests(empty, first, toVector<NodePtr>(first));
	basicExprTests(more, second, toVector<NodePtr>(second, CONST_BOOL_TRUE_PTR, CONST_UINT_ONE_PTR));
}

TEST(ExpressionsTest, VectorExpr) {
	NodeManager manager;

	VectorExprPtr empty = VectorExpr::get(manager, toVector<ExpressionPtr>());
	VectorExprPtr more = VectorExpr::get(manager, toVector<ExpressionPtr>(CONST_BOOL_TRUE_PTR, CONST_BOOL_FALSE_PTR));

	TypePtr first = VectorType::get(manager, TypeVariable::get(manager, "a"), IntTypeParam::getConcreteIntParam(0));
	TypePtr second = VectorType::get(manager, TYPE_BOOL, IntTypeParam::getConcreteIntParam(2));
	EXPECT_EQ ( *first , *empty->getType() );
	EXPECT_EQ ( *second, *more->getType() );

	EXPECT_EQ ("{}", toString(*empty));
	EXPECT_EQ ("{true,false}", toString(*more));


	// check hash codes, children and cloning
	basicExprTests(empty, first, toVector<NodePtr>(first));
	basicExprTests(more, second, toVector<NodePtr>(second, CONST_BOOL_TRUE_PTR, CONST_BOOL_FALSE_PTR));
}

TEST(ExpressionsTest, RecursiveLambda) {
	ASTBuilder builder;

	// create a recursive even/odd example
	FunctionTypePtr functionType = builder.functionType(lang::TYPE_UINT_4_PTR, lang::TYPE_BOOL_PTR);
	VarExprPtr evenVar = builder.varExpr(functionType, "even");
	VarExprPtr oddVar = builder.varExpr(functionType, "odd");


	LambdaExpr::ParamList param;
	param.push_back(builder.paramExpr(lang::TYPE_UINT_4_PTR, "x"));

	VarExprPtr x = builder.varExpr(lang::TYPE_UINT_4_PTR, "x");
	ExpressionPtr condition = builder.callExpr(lang::OP_UINT_EQ_PTR,toVector<ExpressionPtr>(x,lang::CONST_UINT_ZERO_PTR));

	// build even body ...
	StatementPtr evenBody = builder.ifStmt(condition,
			builder.returnStmt(lang::CONST_BOOL_TRUE_PTR),
			builder.returnStmt(
					builder.callExpr(lang::OP_BOOL_NOT_PTR,
							toVector<ExpressionPtr>(builder.callExpr(oddVar, toVector<ExpressionPtr>(x))))
			)
	);
	LambdaExprPtr evenLambda = builder.lambdaExpr(functionType, param, evenBody);

	// build odd body ...
	StatementPtr oddBody = builder.ifStmt(condition,
				builder.returnStmt(lang::CONST_BOOL_FALSE_PTR),
				builder.returnStmt(
						builder.callExpr(lang::OP_BOOL_NOT_PTR,
								toVector<ExpressionPtr>(builder.callExpr(evenVar, toVector<ExpressionPtr>(x))))
				)
	);
	LambdaExprPtr oddLambda = builder.lambdaExpr(functionType, param, oddBody);

	// finish definition
	RecLambdaDefinition::RecFunDefs definitions;
	definitions.insert(std::make_pair(evenVar, evenLambda));
	definitions.insert(std::make_pair(oddVar, oddLambda));
	RecLambdaDefinitionPtr definition = builder.recLambdaDefinition(definitions);

	// test definition node
	typedef typename std::unordered_set<NodePtr, hash_target<NodePtr>, equal_target<NodePtr>> Set;
	EXPECT_TRUE( equal(toSet<Set, NodePtr>(evenVar, evenLambda, oddVar, oddLambda), asSet<Set>(definition->getChildList())) );

	// create recursive lambda nodes
	RecLambdaExprPtr even = builder.recLambdaExpr(evenVar, definition);
	RecLambdaExprPtr odd  = builder.recLambdaExpr(oddVar,  definition);

	basicExprTests(even, functionType, toList(toVector<NodePtr>(evenVar, definition)));
	basicExprTests(odd, functionType, toList(toVector<NodePtr>(oddVar, definition)));

	EXPECT_EQ ( even , even);
	EXPECT_NE ( even , odd);
	EXPECT_NE ( odd , even);
	EXPECT_EQ ( odd , odd);

	EXPECT_NE ( even->hash(), odd->hash());

	EXPECT_TRUE (toString(*even) == "rec even.{even=fun(uint<4> x){ if(uint.eq(x, 0)) return true else return bool.not(odd(x)) }, odd=fun(uint<4> x){ if(uint.eq(x, 0)) return false else return bool.not(even(x)) }}" ||
			     toString(*even) == "rec even.{odd=fun(uint<4> x){ if(uint.eq(x, 0)) return false else return bool.not(even(x)) }, even=fun(uint<4> x){ if(uint.eq(x, 0)) return true else return bool.not(odd(x)) }}");
}


template<typename PT>
void basicExprTests(PT expression, const TypePtr& type, const Node::ChildList& children) {

	typedef typename PT::element_type T;

	// ------------- Basic Node Tests ----------------

	basicNodeTests(expression, children);

	// ------------ Expression Ptr based tests -------------

	// check type
	EXPECT_EQ ( *type, *expression->getType() );

	// ------------ Type Token based tests -------------

	// copy and clone the type
	NodeManager manager;
	T copy = T(*expression);
	T* clone = &*manager.get(expression);

	// check whether all are equal
	T* all[] = { &*expression, &copy, clone };
	for (int i=0; i<3; i++) {
		for (int j=0; j<3; j++) {

			T* a = all[i];
			T* b = all[j];

			EXPECT_EQ ( *a , *b );
			EXPECT_EQ ( *a->getType(), *b->getType() );
		}
	}

	// check type properties
	for (int i=0; i<3; i++) {

		T* cur = all[i];

		// check type
		EXPECT_EQ ( *type, *expression->getType() );

		// check children
		EXPECT_TRUE( equals(children, cur->getChildList(), equal_target<NodePtr>()) );
	}
}

} // end namespace: core
} // end namespace: insieme
