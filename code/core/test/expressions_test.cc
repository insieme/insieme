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

#include "insieme/core/statements.h"
#include "insieme/core/expressions.h"
#include "insieme/core/ast_builder.h"

#include "insieme/utils/set_utils.h"

#include "ast_node_test.cc"

namespace insieme {
namespace core {

using namespace insieme::core::lang;
using namespace insieme::utils::set;

//template<typename PT>
//void basicExprTests(PT expression, const TypePtr& type, const Node::ChildList& children);


TEST(ExpressionsTest, IntLiterals) {
	ASTBuilder builder;

	LiteralPtr i5 = builder.literal(builder.getBasicGenerator().getIntGen(), "5");
	LiteralPtr i7 = builder.literal(builder.getBasicGenerator().getIntGen(), "7");
	LiteralPtr i5long = builder.literal(builder.getBasicGenerator().getInt8(), "5");
	
	EXPECT_EQ( *i5, *builder.literal(builder.getBasicGenerator().getIntGen(), "5") );
	EXPECT_NE( *i5, *i5long );
	EXPECT_NE( *i5, *i7 );
	EXPECT_EQ( i5->getValueAs<int>(), 5 );

	basicExprTests(i5, builder.getBasicGenerator().getIntGen(), toList(toVector<NodePtr>(builder.getBasicGenerator().getIntGen())));
	basicExprTests(i7, builder.getBasicGenerator().getIntGen(), toList(toVector<NodePtr>(builder.getBasicGenerator().getIntGen())));
	basicExprTests(i5long, builder.getBasicGenerator().getInt8(), toList(toVector<NodePtr>(builder.getBasicGenerator().getInt8())));
}

TEST(ExpressionsTest, FloatLiterals) {
	ASTBuilder builder;

	LiteralPtr f5_s = builder.literal(builder.getBasicGenerator().getFloat(), "5.0");
	
	basicExprTests(f5_s, builder.getBasicGenerator().getFloat(), toList(toVector<NodePtr>(builder.getBasicGenerator().getFloat())));

	// EXPECT_EQ( *f5, *f5_s ); //-- this is not necessarily true
	std::stringstream ss;
	ss << *f5_s;
	EXPECT_EQ( ss.str(), "5.0" );
//	EXPECT_EQ( f5->getValue(), f5_s->getValue() );
}

TEST(ExpressionsTest, Variable) {
	NodeManager manager;

	VariablePtr var = Variable::get(manager, manager.basic.getBool());
	EXPECT_EQ (format("v%d", var->getId()), toString(*var));

	VariablePtr var2 = Variable::get(manager, manager.basic.getBool());
	EXPECT_NE(*var, *var2);
	EXPECT_LT(var->getId(), var2->getId());

	VariablePtr var3 = Variable::get(manager, manager.basic.getBool(), var->getId());
	EXPECT_EQ(var, var3);

	// check hash codes, children and cloning
	basicExprTests(var, manager.basic.getBool(), toVector<NodePtr>(manager.basic.getBool()));
	basicExprTests(var2, manager.basic.getBool(), toVector<NodePtr>(manager.basic.getBool()));
}

TEST(ExpressionsTest, TupleExpr) {
	NodeManager manager;

	LiteralPtr one = Literal::get(manager, manager.basic.getUInt1(), "1");
	TupleExprPtr empty = TupleExpr::get(manager, toVector<ExpressionPtr>());
	TupleExprPtr more = TupleExpr::get(manager, toVector<ExpressionPtr>(manager.basic.getTrue(), one));

	TypePtr first = TupleType::get(manager, TypeList());
	TypePtr second = TupleType::get(manager, toVector<TypePtr>(manager.basic.getBool(), manager.basic.getUInt1()));
	EXPECT_EQ ( *first , *empty->getType() );
	EXPECT_EQ ( *second, *more->getType() );

	EXPECT_EQ ("tuple()", toString(*empty));
	EXPECT_EQ ("tuple(true,1)", toString(*more));


	// check hash codes, children and cloning
	basicExprTests(empty, first, toVector<NodePtr>(first));
	basicExprTests(more, second, toVector<NodePtr>(second, manager.basic.getTrue(), one));
}

TEST(ExpressionsTest, VectorExpr) {
	NodeManager manager;

	VectorExprPtr empty = VectorExpr::get(manager, toVector<ExpressionPtr>());
	VectorExprPtr more = VectorExpr::get(manager, toVector<ExpressionPtr>(manager.basic.getTrue(), manager.basic.getFalse()));

	TypePtr first = VectorType::get(manager, TypeVariable::get(manager, "a"), IntTypeParam::getConcreteIntParam(0));
	TypePtr second = VectorType::get(manager, manager.basic.getBool(), IntTypeParam::getConcreteIntParam(2));
	EXPECT_EQ ( *first , *empty->getType() );
	EXPECT_EQ ( *second, *more->getType() );

	EXPECT_EQ ("{}", toString(*empty));
	EXPECT_EQ ("{true,false}", toString(*more));


	// check hash codes, children and cloning
	basicExprTests(empty, first, toVector<NodePtr>(first));
	basicExprTests(more, second, toVector<NodePtr>(second, manager.basic.getTrue(), manager.basic.getFalse()));
}

TEST(ExpressionsTest, Lambda) {
	NodeManager manager;

	TypePtr type = GenericType::get(manager, "A");
	StatementPtr body = Literal::get(manager, type, "a");
	VariablePtr varA = Variable::get(manager, type, 1);
	VariablePtr varB = Variable::get(manager, type, 2);
	FunctionTypePtr funType = FunctionType::get(manager, TypeList(), type);
	LambdaPtr empty = Lambda::get(manager, funType, Lambda::CaptureList(), Lambda::ParamList(), body);
	LambdaPtr more = Lambda::get(manager, funType, toVector(varA), toVector(varB), body);

	EXPECT_EQ ("fun[]() a", toString(*empty));
	EXPECT_EQ ("fun[A v1](A v2) a", toString(*more));

	// conduct basic node checks
	basicNodeTests(empty, toVector<NodePtr>(funType, body));
	basicNodeTests(more, toVector<NodePtr>(funType, varA, varB, body));
}

TEST(ExpressionsTest, LambdaExpr) {
	ASTBuilder builder;
	const lang::BasicGenerator& gen = builder.getBasicGenerator();

	// create a recursive even/odd example
	FunctionTypePtr functionType = builder.functionType(toVector<TypePtr>(gen.getUInt4()), gen.getBool());
	VariablePtr evenVar = builder.variable(functionType, 1);
	VariablePtr oddVar = builder.variable(functionType, 2);


	Lambda::ParamList param;
	param.push_back(builder.variable(gen.getUInt4(), 3));

	LiteralPtr zero = builder.literal(gen.getUInt4(), "0");
	VariablePtr x = builder.variable(gen.getUInt4(), 3);
	ExpressionPtr condition = builder.callExpr(gen.getBool(), gen.getUnsignedIntEq(), x, zero);

	// build even body ...
	StatementPtr evenBody = builder.ifStmt(condition,
			builder.returnStmt(gen.getTrue()),
			builder.returnStmt(
					builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), oddVar, x))
			)
	);
	LambdaPtr evenLambda = builder.lambda(functionType, Lambda::CaptureList(), param, evenBody);

	// build odd body ...
	StatementPtr oddBody = builder.ifStmt(condition,
				builder.returnStmt(gen.getFalse()),
				builder.returnStmt(
						builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), evenVar, x))
				)
	);
	LambdaPtr oddLambda = builder.lambda(functionType, Lambda::CaptureList(), param, oddBody);

	// finish definition
	LambdaDefinition::Definitions definitions;
	definitions.insert(std::make_pair(evenVar, evenLambda));
	definitions.insert(std::make_pair(oddVar, oddLambda));
	LambdaDefinitionPtr definition = builder.lambdaDefinition(definitions);

	// test definition node
	EXPECT_TRUE( equals(toVector<NodePtr>(evenVar, evenLambda, oddVar, oddLambda), definition->getChildList()) );

	// create recursive lambda nodes
	LambdaExprPtr even = builder.lambdaExpr(evenVar, definition);
	LambdaExprPtr odd  = builder.lambdaExpr(oddVar,  definition);

	basicExprTests(even, functionType, toList(toVector<NodePtr>(evenVar, definition)));
	basicExprTests(odd, functionType, toList(toVector<NodePtr>(oddVar, definition)));

	EXPECT_EQ ( even , even);
	EXPECT_NE ( even , odd);
	EXPECT_NE ( odd , even);
	EXPECT_EQ ( odd , odd);

	EXPECT_NE ( even->hash(), odd->hash());

	EXPECT_TRUE(even->isRecursive());
	EXPECT_TRUE(odd->isRecursive());

	EXPECT_EQ("rec v1.{v1=fun[](uint<4> v3) if(uint.eq(v3, 0)) return true else return bool.not(v2(v3)), v2=fun[](uint<4> v3) if(uint.eq(v3, 0)) return false else return bool.not(v1(v3))}", toString(*even));
	EXPECT_EQ("rec v2.{v1=fun[](uint<4> v3) if(uint.eq(v3, 0)) return true else return bool.not(v2(v3)), v2=fun[](uint<4> v3) if(uint.eq(v3, 0)) return false else return bool.not(v1(v3))}", toString(*odd));
}


TEST(ExpressionsTest, CaptureInitExpr) {

	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr res = builder.genericType("A");
	FunctionTypePtr funType = builder.functionType(TypeList(), res);
	FunctionTypePtr funType2 = builder.functionType(toVector(res,res), TypeList(), res);

	VariablePtr captureVar = builder.variable(res);
	LiteralPtr initValue = builder.literal(res, "X");
	LiteralPtr initValue2 = builder.literal(res, "Y");

	LambdaExprPtr lambda = builder.lambdaExpr(funType, Lambda::ParamList(), builder.returnStmt(builder.literal(res, "A")));
	LambdaExprPtr lambda2 = builder.lambdaExpr(funType2, toVector<VariablePtr>(captureVar), Lambda::ParamList(), builder.returnStmt(builder.literal(res, "A")));

	EXPECT_FALSE(lambda->isRecursive());
	EXPECT_FALSE(lambda2->isRecursive());

	CaptureInitExprPtr empty = builder.captureInitExpr(lambda, toVector<ExpressionPtr>());
	CaptureInitExprPtr more = builder.captureInitExpr(lambda2, toVector<ExpressionPtr>(initValue, initValue2));

	EXPECT_EQ(*lambda->getType(), *empty->getType());
	EXPECT_EQ(*lambda->getType(), *more->getType());
	EXPECT_NE(*lambda2->getType(), *more->getType());

	EXPECT_EQ(*funType, *empty->getType());
	EXPECT_EQ(*funType, *more->getType());

	EXPECT_EQ ("([]rec v4.{v4=fun[]() return A})", toString(*empty));
	EXPECT_EQ ("([X, Y]rec v5.{v5=fun[A v3]() return A})", toString(*more));


	// check hash codes, children and cloning
	basicExprTests(empty, lambda->getType(), toVector<NodePtr>(funType, lambda));
	basicExprTests(more, lambda->getType(), toVector<NodePtr>(funType, lambda2, initValue, initValue2));

}


TEST(ExpressionsTest, MemberAccessExpr) {
	NodeManager manager;

	StructExpr::Members members;

	TypePtr typeA = GenericType::get(manager, "typeA");
	TypePtr typeB = GenericType::get(manager, "typeB");
	members.push_back(StructExpr::Member("a", Literal::get(manager, typeA, "1")));
	members.push_back(StructExpr::Member("b", Literal::get(manager, typeB, "2")));
	StructExprPtr data = StructExpr::get(manager, members);

	MemberAccessExprPtr access = MemberAccessExpr::get(manager, data, "a");
	EXPECT_EQ(*typeA, *access->getType());

	MemberAccessExprPtr access2 = MemberAccessExpr::get(manager, data, "b");
	EXPECT_EQ(*typeB, *access2->getType());

	MemberAccessExprPtr access3 = MemberAccessExpr::get(manager, data, "a");
	EXPECT_EQ(*typeA, *access3->getType());

	EXPECT_NE(access, access2);
	EXPECT_NE(access2, access3);
	EXPECT_EQ(access, access3);

	EXPECT_EQ ("(struct{a=1, b=2}.a)", toString(*access));
	EXPECT_EQ ("(struct{a=1, b=2}.b)", toString(*access2));

	// check hash codes, children and cloning
	basicExprTests(access, typeA, toVector<NodePtr>(data));
	basicExprTests(access2, typeB, toVector<NodePtr>(data));
	basicExprTests(access3, typeA, toVector<NodePtr>(data));
}

TEST(ExpressionsTest, TupleProjectionExpr) {
	NodeManager manager;

	TypePtr typeA = GenericType::get(manager, "typeA");
	TypePtr typeB = GenericType::get(manager, "typeB");

	std::vector<ExpressionPtr> expressions;
	expressions.push_back(Literal::get(manager, typeA, "1"));
	expressions.push_back(Literal::get(manager, typeB, "2"));

	TupleExprPtr tuple = TupleExpr::get(manager, expressions);

	TupleProjectionExprPtr access = TupleProjectionExpr::get(manager, tuple, 0);
	EXPECT_EQ(*typeA, *access->getType());

	TupleProjectionExprPtr access2 = TupleProjectionExpr::get(manager, tuple, 1);
	EXPECT_EQ(*typeB, *access2->getType());

	TupleProjectionExprPtr access3 = TupleProjectionExpr::get(manager, tuple, 0);
	EXPECT_EQ(*typeA, *access3->getType());

	EXPECT_NE(access, access2);
	EXPECT_NE(access2, access3);
	EXPECT_EQ(access, access3);

	EXPECT_EQ ("(tuple(1,2)[0])", toString(*access));
	EXPECT_EQ ("(tuple(1,2)[1])", toString(*access2));

	// check hash codes, children and cloning
	basicExprTests(access, typeA, toVector<NodePtr>(tuple));
	basicExprTests(access2, typeB, toVector<NodePtr>(tuple));
}


TEST(ExpressionsTest, MarkerExpr) {
	NodeManager manager;

	TypePtr type = GenericType::get(manager, "A");
	LiteralPtr literal = Literal::get(manager, type, "1");

	MarkerExprPtr markerA = MarkerExpr::get(manager, literal);
	MarkerExprPtr markerB = MarkerExpr::get(manager, literal);

	EXPECT_NE(markerA, markerB);
	EXPECT_NE(*markerA, *markerB);

	EXPECT_EQ(type, markerA->getType());
	EXPECT_EQ(markerA->getType(), markerB->getType());

	EXPECT_EQ(literal, markerA->getSubExpression());
	EXPECT_EQ(markerA->getSubExpression(), markerB->getSubExpression());

	EXPECT_NE(markerA->getID(), markerB->getID());

	// check hash codes, children and cloning
	basicExprTests(markerA, type, toVector<NodePtr>(literal));
	basicExprTests(markerB, type, toVector<NodePtr>(literal));
}

TEST(ExpressionsTest, JobExpr) {
	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr intType = manager.basic.getUIntGen();
	FunctionTypePtr funType = FunctionType::get(manager, TypeList(), toVector<TypePtr>(), manager.basic.getUnit());
	FunctionTypePtr guardType = FunctionType::get(manager, TypeList(), toVector<TypePtr>(intType, intType), manager.basic.getBool());

	ExpressionPtr handlerA = Variable::get(manager, funType);
	ExpressionPtr handlerB = Variable::get(manager, funType);
	ExpressionPtr handlerC = Variable::get(manager, funType);

	ExpressionPtr guardA = Variable::get(manager, guardType);
	ExpressionPtr guardB = Variable::get(manager, guardType);
	ExpressionPtr guardC = Variable::get(manager, guardType);
	ExpressionPtr defaultHandler = Variable::get(manager, funType);

	JobExpr::GuardedStmts stmts;
	stmts.push_back(JobExpr::GuardedStmt(guardA, handlerA));
	stmts.push_back(JobExpr::GuardedStmt(guardB, handlerB));
	stmts.push_back(JobExpr::GuardedStmt(guardC, handlerC));

	vector<DeclarationStmtPtr> localDeclarations;
	localDeclarations.push_back(DeclarationStmt::get(manager, Variable::get(manager, intType), Literal::get(manager, intType, "1")));
	localDeclarations.push_back(DeclarationStmt::get(manager, Variable::get(manager, intType), Literal::get(manager, intType, "2")));

	ExpressionPtr range = builder.getThreadNumRange(1,40);
	JobExprPtr job = JobExpr::get(manager, range, defaultHandler, stmts, localDeclarations);

	// check hash codes, children and cloning
	TypePtr type = manager.basic.getJob();
	vector<NodePtr> childList;
	childList.push_back(range);
	childList.push_back(localDeclarations[0]);
	childList.push_back(localDeclarations[1]);
	childList.push_back(guardA);
	childList.push_back(handlerA);
	childList.push_back(guardB);
	childList.push_back(handlerB);
	childList.push_back(guardC);
	childList.push_back(handlerC);
	childList.push_back(defaultHandler);

	basicExprTests(job, type, childList);
}

template<typename PT>
void basicExprTests(PT expression, const TypePtr& type, const Node::ChildList& children) {

	typedef typename PT::element_type T;

	// ------------- Basic Node Tests ----------------

	basicNodeTests(expression, children);

	// ------------ Expression Ptr based tests -------------

	// check type
	EXPECT_EQ ( *type, *expression->getType() );

	// type has to be first child - NOTE: not true for recursive lambdas, member access and tuple projection
//	EXPECT_EQ(*type, *(expression->getChildList()[0]));

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
