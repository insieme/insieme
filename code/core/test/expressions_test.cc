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

//template<typename PT>
//void basicExprTests(PT expression, const TypePtr& type, const Node::ChildList& children);


TEST(ExpressionsTest, IntLiterals) {
	ASTBuilder builder;

	LiteralPtr i5 = builder.literal(TYPE_INT_GEN_PTR, "5");
	LiteralPtr i7 = builder.literal(TYPE_INT_GEN_PTR, "7");
	LiteralPtr i5long = builder.literal(TYPE_INT_8_PTR, "5");
	
	EXPECT_EQ( *i5, *builder.literal(TYPE_INT_GEN_PTR, "5") );
	EXPECT_NE( *i5, *i5long );
	EXPECT_NE( *i5, *i7 );
	EXPECT_EQ( i5->getValueAs<int>(), 5 );

	basicExprTests(i5, TYPE_INT_GEN_PTR, toList(toVector<NodePtr>(TYPE_INT_GEN_PTR)));
	basicExprTests(i7, TYPE_INT_GEN_PTR, toList(toVector<NodePtr>(TYPE_INT_GEN_PTR)));
	basicExprTests(i5long, TYPE_INT_8_PTR, toList(toVector<NodePtr>(TYPE_INT_8_PTR)));
}

TEST(ExpressionsTest, FloatLiterals) {
	ASTBuilder builder;

	LiteralPtr f5_s = builder.literal(TYPE_REAL_4_PTR, "5.0");
	
	basicExprTests(f5_s, TYPE_REAL_4_PTR, toList(toVector<NodePtr>(TYPE_REAL_4_PTR)));

	// EXPECT_EQ( *f5, *f5_s ); //-- this is not necessarily true
	std::stringstream ss;
	ss << *f5_s;
	EXPECT_EQ( ss.str(), "5.0" );
//	EXPECT_EQ( f5->getValue(), f5_s->getValue() );
}

TEST(ExpressionsTest, Variable) {
	NodeManager manager;

	VariablePtr var = Variable::get(manager, TYPE_BOOL_PTR);
	EXPECT_EQ (format("v%d", var->getId()), toString(*var));

	VariablePtr var2 = Variable::get(manager, TYPE_BOOL_PTR);
	EXPECT_NE(*var, *var2);
	EXPECT_LT(var->getId(), var2->getId());

	VariablePtr var3 = Variable::get(manager, TYPE_BOOL_PTR, var->getId());
	EXPECT_EQ(var, var3);

	// check hash codes, children and cloning
	basicExprTests(var, TYPE_BOOL_PTR, toVector<NodePtr>(TYPE_BOOL_PTR));
	basicExprTests(var2, TYPE_BOOL_PTR, toVector<NodePtr>(TYPE_BOOL_PTR));
}

TEST(ExpressionsTest, LambdaExpr) {
	NodeManager manager;

	LambdaExpr::ParamList list;
	list.push_back(Variable::get(manager, TYPE_BOOL_PTR, 1));
	list.push_back(Variable::get(manager, TYPE_BOOL_PTR, 2));

	StatementPtr body = ReturnStmt::get(manager, CONST_BOOL_TRUE_PTR);
	LambdaExprPtr expr = LambdaExpr::get(manager, TYPE_BINARY_BOOL_OP_PTR, list, body);

	EXPECT_EQ ("fun(bool v1, bool v2){ return true }", toString(*expr));

	// check hash codes, children and cloning
	basicExprTests(expr, TYPE_BINARY_BOOL_OP_PTR, toVector<NodePtr>(TYPE_BINARY_BOOL_OP_PTR, list[0], list[1], body));


	// check capture list
	LambdaExpr::CaptureList capture;
	capture.push_back(DeclarationStmt::get(manager, Variable::get(manager, TYPE_BOOL_PTR, 3), lang::CONST_BOOL_FALSE_PTR));
	LambdaExprPtr lambdaA = LambdaExpr::get(manager, TYPE_BINARY_BOOL_OP_PTR, capture, list, body);

	EXPECT_EQ ("fun[bool v3 = false](bool v1, bool v2){ return true }", toString(*lambdaA));


	capture.push_back(DeclarationStmt::get(manager, Variable::get(manager, TYPE_BOOL_PTR, 4), lang::CONST_BOOL_TRUE_PTR));
	LambdaExprPtr lambdaB = LambdaExpr::get(manager, TYPE_BINARY_BOOL_OP_PTR, capture, list, body);

	EXPECT_EQ ("fun[bool v3 = false, bool v4 = true](bool v1, bool v2){ return true }", toString(*lambdaB));

	EXPECT_EQ(*expr, *expr);
	EXPECT_EQ(*lambdaA, *lambdaA);
	EXPECT_EQ(*lambdaB, *lambdaB);

	EXPECT_NE(*expr, *lambdaA);
	EXPECT_NE(*expr, *lambdaB);
	EXPECT_NE(*lambdaA, *lambdaB);

	basicExprTests(lambdaA, TYPE_BINARY_BOOL_OP_PTR, toVector<NodePtr>(TYPE_BINARY_BOOL_OP_PTR, capture[0], list[0], list[1], body));
	basicExprTests(lambdaB, TYPE_BINARY_BOOL_OP_PTR, toVector<NodePtr>(TYPE_BINARY_BOOL_OP_PTR, capture[0], capture[1], list[0], list[1], body));

}

TEST(ExpressionsTest, TupleExpr) {
	NodeManager manager;

	LiteralPtr one = Literal::get(manager, TYPE_UINT_1_PTR, "1");
	TupleExprPtr empty = TupleExpr::get(manager, toVector<ExpressionPtr>());
	TupleExprPtr more = TupleExpr::get(manager, toVector<ExpressionPtr>(CONST_BOOL_TRUE_PTR, one));

	TypePtr first = TupleType::get(manager, TupleType::ElementTypeList());
	TypePtr second = TupleType::get(manager, toVector<TypePtr>(TYPE_BOOL_PTR, TYPE_UINT_1_PTR));
	EXPECT_EQ ( *first , *empty->getType() );
	EXPECT_EQ ( *second, *more->getType() );

	EXPECT_EQ ("tuple()", toString(*empty));
	EXPECT_EQ ("tuple(true,1)", toString(*more));


	// check hash codes, children and cloning
	basicExprTests(empty, first, toVector<NodePtr>(first));
	basicExprTests(more, second, toVector<NodePtr>(second, CONST_BOOL_TRUE_PTR, one));
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
	TupleTypePtr argumentType = builder.tupleType(toVector<TypePtr>(lang::TYPE_UINT_4_PTR));
	FunctionTypePtr functionType = builder.functionType(argumentType, lang::TYPE_BOOL_PTR);
	VariablePtr evenVar = builder.variable(functionType, 1);
	VariablePtr oddVar = builder.variable(functionType, 2);


	LambdaExpr::ParamList param;
	param.push_back(builder.variable(lang::TYPE_UINT_4_PTR, 3));

	LiteralPtr zero = builder.literal(TYPE_UINT_1_PTR, "0");
	VariablePtr x = builder.variable(lang::TYPE_UINT_4_PTR, 3);
	ExpressionPtr condition = builder.callExpr(lang::TYPE_BOOL_PTR, lang::OP_UINT_EQ_PTR,toVector<ExpressionPtr>(x,zero));

	// build even body ...
	StatementPtr evenBody = builder.ifStmt(condition,
			builder.returnStmt(lang::CONST_BOOL_TRUE_PTR),
			builder.returnStmt(
					builder.callExpr(lang::TYPE_BOOL_PTR, lang::OP_BOOL_NOT_PTR,
							toVector<ExpressionPtr>(builder.callExpr(lang::TYPE_BOOL_PTR, oddVar, toVector<ExpressionPtr>(x))))
			)
	);
	LambdaExprPtr evenLambda = builder.lambdaExpr(functionType, param, evenBody);

	// build odd body ...
	StatementPtr oddBody = builder.ifStmt(condition,
				builder.returnStmt(lang::CONST_BOOL_FALSE_PTR),
				builder.returnStmt(
						builder.callExpr(lang::TYPE_BOOL_PTR, lang::OP_BOOL_NOT_PTR,
								toVector<ExpressionPtr>(builder.callExpr(lang::TYPE_BOOL_PTR, evenVar, toVector<ExpressionPtr>(x))))
				)
	);
	LambdaExprPtr oddLambda = builder.lambdaExpr(functionType, param, oddBody);

	// finish definition
	RecLambdaDefinition::RecFunDefs definitions;
	definitions.insert(std::make_pair(evenVar, evenLambda));
	definitions.insert(std::make_pair(oddVar, oddLambda));
	RecLambdaDefinitionPtr definition = builder.recLambdaDefinition(definitions);

	// test definition node
	typedef std::unordered_set<NodePtr, hash_target<NodePtr>, equal_target<NodePtr>> Set;
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

	EXPECT_TRUE (toString(*even) == "rec v1.{v1=fun(uint<4> v3){ if(uint.eq(v3, 0)) return true else return bool.not(v2(v3)) }, v2=fun(uint<4> v3){ if(uint.eq(v3, 0)) return false else return bool.not(v1(v3)) }}" ||
			     toString(*even) == "rec v1.{v2=fun(uint<4> v3){ if(uint.eq(v3, 0)) return false else return bool.not(v1(v3)) }, v1=fun(uint<4> v3){ if(uint.eq(v3, 0)) return true else return bool.not(v2(v3)) }}");
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
