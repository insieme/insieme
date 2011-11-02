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

#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/set_utils.h"

#include "ast_node_test.inc"

namespace insieme {
namespace core {

using namespace insieme::core::lang;
using namespace insieme::utils::set;

template<typename PT>
void basicExprTests(PT expression, const TypePtr& type, const NodeList& children);


TEST(ExpressionsTest, IntLiterals) {
	NodeManager manager;
	IRBuilder builder(manager);

	LiteralPtr i5 = builder.literal(builder.getLangBasic().getIntGen(), "5");
	LiteralPtr i7 = builder.literal(builder.getLangBasic().getIntGen(), "7");
	LiteralPtr i5long = builder.literal(builder.getLangBasic().getInt8(), "5");
	
	EXPECT_EQ( *i5, *builder.literal(builder.getLangBasic().getIntGen(), "5") );
	EXPECT_NE( *i5, *i5long );
	EXPECT_NE( *i5, *i7 );
	EXPECT_EQ( i5->getValueAs<int>(), 5 );

	basicExprTests(i5, builder.getLangBasic().getIntGen(), toList(toVector<NodePtr>(builder.getLangBasic().getIntGen())));
	basicExprTests(i7, builder.getLangBasic().getIntGen(), toList(toVector<NodePtr>(builder.getLangBasic().getIntGen())));
	basicExprTests(i5long, builder.getLangBasic().getInt8(), toList(toVector<NodePtr>(builder.getLangBasic().getInt8())));
}

TEST(ExpressionsTest, FloatLiterals) {
	NodeManager manager;
	IRBuilder builder(manager);

	LiteralPtr f5_s = builder.literal(builder.getLangBasic().getFloat(), "5.0");
	
	basicExprTests(f5_s, builder.getLangBasic().getFloat(), toList(toVector<NodePtr>(builder.getLangBasic().getFloat())));

	// EXPECT_EQ( *f5, *f5_s ); //-- this is not necessarily true
	std::stringstream ss;
	ss << *f5_s;
	EXPECT_EQ( ss.str(), "5.0" );
//	EXPECT_EQ( f5->getValue(), f5_s->getValue() );
}

TEST(ExpressionsTest, Variable) {
	NodeManager manager;

	VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool());
	EXPECT_EQ (format("v%d", var->getId()), toString(*var));

	VariablePtr var2 = Variable::get(manager, manager.getLangBasic().getBool());
	EXPECT_NE(*var, *var2);
	EXPECT_LT(var->getId(), var2->getId());

	VariablePtr var3 = Variable::get(manager, manager.getLangBasic().getBool(), var->getId());
	EXPECT_EQ(var, var3);

	// check hash codes, children and cloning
	basicExprTests(var, manager.getLangBasic().getBool(), toVector<NodePtr>(manager.getLangBasic().getBool()));
	basicExprTests(var2, manager.getLangBasic().getBool(), toVector<NodePtr>(manager.getLangBasic().getBool()));
}

TEST(ExpressionsTest, TupleExpr) {
	NodeManager manager;
	IRBuilder builder(manager);

	LiteralPtr one = Literal::get(manager, manager.getLangBasic().getUInt1(), "1");
	TupleExprPtr empty = builder.tupleExpr(toVector<ExpressionPtr>());
	TupleExprPtr more = builder.tupleExpr(toVector<ExpressionPtr>(manager.getLangBasic().getTrue(), one));

	TypePtr first = TupleType::get(manager, TypeList());
	TypePtr second = TupleType::get(manager, toVector<TypePtr>(manager.getLangBasic().getBool(), manager.getLangBasic().getUInt1()));
	EXPECT_EQ ( *first , *empty->getType() );
	EXPECT_EQ ( *second, *more->getType() );

	EXPECT_EQ ("tuple()", toString(*empty));
	EXPECT_EQ ("tuple(true,1)", toString(*more));


	// check hash codes, children and cloning
	basicExprTests(empty, first, toVector<NodePtr>(first));
	basicExprTests(more, second, toVector<NodePtr>(second, manager.getLangBasic().getTrue(), one));
}

TEST(ExpressionsTest, VectorExpr) {
	NodeManager manager;
	IRBuilder builder(manager);

	VectorExprPtr empty = builder.vectorExpr(toVector<ExpressionPtr>());
	VectorExprPtr more = builder.vectorExpr(toVector<ExpressionPtr>(manager.getLangBasic().getTrue(), manager.getLangBasic().getFalse()));

	TypePtr first = VectorType::get(manager, TypeVariable::get(manager, "a"), ConcreteIntTypeParam::get(manager, 0));
	TypePtr second = VectorType::get(manager, manager.getLangBasic().getBool(), ConcreteIntTypeParam::get(manager, 2));
	EXPECT_EQ ( *first , *empty->getType() );
	EXPECT_EQ ( *second, *more->getType() );

	EXPECT_EQ ("{}", toString(*empty));
	EXPECT_EQ ("{true,false}", toString(*more));


	// check hash codes, children and cloning
	basicExprTests(empty, first, toVector<NodePtr>(first));
	basicExprTests(more, second, toVector<NodePtr>(second, manager.getLangBasic().getTrue(), manager.getLangBasic().getFalse()));
}

TEST(ExpressionsTest, Lambda) {
	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr type = GenericType::get(manager, "A");
	CompoundStmtPtr body = CompoundStmt::get(manager, Literal::get(manager, type, "a"));
	VariablePtr varA = Variable::get(manager, type, 1);
	VariablePtr varB = Variable::get(manager, type, 2);
	FunctionTypePtr funType = FunctionType::get(manager, TypeList(), type, true);
	LambdaPtr empty = Lambda::get(manager, funType, VariableList(), body);
	LambdaPtr little = Lambda::get(manager, funType, toVector(varA), body);
	LambdaPtr more = Lambda::get(manager, funType, toVector(varA, varB), body);

	EXPECT_EQ ("fun() {a;}", toString(*empty));
	EXPECT_EQ ("fun(A v1) {a;}", toString(*little));
	EXPECT_EQ ("fun(A v1, A v2) {a;}", toString(*more));

	// conduct basic node checks
	basicNodeTests(empty, toVector<NodePtr>(funType, body));
	basicNodeTests(little, toVector<NodePtr>(funType, varA, body));
	basicNodeTests(more, toVector<NodePtr>(funType, varA, varB, body));
}

TEST(ExpressionsTest, LambdaExpr) {
	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& gen = builder.getLangBasic();

	// create a recursive even/odd example
	FunctionTypePtr functionType = builder.functionType(toVector<TypePtr>(gen.getUInt4()), gen.getBool());
	VariablePtr evenVar = builder.variable(functionType, 1);
	VariablePtr oddVar = builder.variable(functionType, 2);


	VariableList param;
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
	LambdaPtr evenLambda = builder.lambda(functionType, param, evenBody);

	// build odd body ...
	StatementPtr oddBody = builder.ifStmt(condition,
				builder.returnStmt(gen.getFalse()),
				builder.returnStmt(
						builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), evenVar, x))
				)
	);
	LambdaPtr oddLambda = builder.lambda(functionType, param, oddBody);

	// finish definition
	vector<LambdaBindingPtr> bindings;
	bindings.push_back(builder.lambdaBinding(evenVar, evenLambda));
	bindings.push_back(builder.lambdaBinding(oddVar, oddLambda));
	LambdaDefinitionPtr definition = builder.lambdaDefinition(bindings);

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

	EXPECT_NE ( (*even).hash(), (*odd).hash());

	EXPECT_TRUE(even->isRecursive());
	EXPECT_TRUE(odd->isRecursive());

	EXPECT_EQ("rec v1.{v1=fun(uint<4> v3) {if(uint.eq(v3, 0)) {return true;} else {return bool.not(v2(v3));};}, v2=fun(uint<4> v3) {if(uint.eq(v3, 0)) {return false;} else {return bool.not(v1(v3));};}}", toString(*even));
	EXPECT_EQ("rec v2.{v1=fun(uint<4> v3) {if(uint.eq(v3, 0)) {return true;} else {return bool.not(v2(v3));};}, v2=fun(uint<4> v3) {if(uint.eq(v3, 0)) {return false;} else {return bool.not(v1(v3));};}}", toString(*odd));
}

TEST(ExpressionsTest, BindExpr) {

	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr typeA = builder.genericType("A");
	TypePtr typeRes = builder.genericType("R");

	FunctionTypePtr funTypeA = builder.functionType(toVector<TypePtr>(), typeRes);
	FunctionTypePtr funTypeB = builder.functionType(toVector(typeA), typeRes);
	FunctionTypePtr funTypeC = builder.functionType(toVector(typeA, typeA), typeRes);

	LiteralPtr funA = builder.literal(funTypeA, "f");
	LiteralPtr funB = builder.literal(funTypeB, "g");
	LiteralPtr funC = builder.literal(funTypeC, "h");

	VariablePtr captureVar1 = builder.variable(typeA, 1);
	VariablePtr captureVar2 = builder.variable(typeA, 2);

	LiteralPtr constantA = builder.literal(typeA, "12");
	LiteralPtr constantB = builder.literal(typeA, "14");

	CallExprPtr callA = builder.callExpr(funA, toVector<ExpressionPtr>());

	CallExprPtr callB1 = builder.callExpr(funB, toVector<ExpressionPtr>(constantA));
	CallExprPtr callB2 = builder.callExpr(funB, toVector<ExpressionPtr>(captureVar1));

	CallExprPtr callC1 = builder.callExpr(funC, toVector<ExpressionPtr>(constantA, constantB));
	CallExprPtr callC2 = builder.callExpr(funC, toVector<ExpressionPtr>(constantA, captureVar1));
	CallExprPtr callC3 = builder.callExpr(funC, toVector<ExpressionPtr>(captureVar2, constantB));
	CallExprPtr callC4 = builder.callExpr(funC, toVector<ExpressionPtr>(captureVar2, captureVar1));


	BindExprPtr empty = builder.bindExpr(toVector<VariablePtr>(), callA);
	EXPECT_EQ("bind(){f()}", toString(*empty));
	EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(), typeRes, false)), *(empty->getType()));

	BindExprPtr B1 = builder.bindExpr(toVector<VariablePtr>(), callB1);
	EXPECT_EQ("bind(){g(12)}", toString(*B1));
	EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(), typeRes, false)), *(B1->getType()));

	BindExprPtr B2 = builder.bindExpr(toVector<VariablePtr>(captureVar1), callB2);
	EXPECT_EQ("bind(v1){g(v1)}", toString(*B2));
	EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(typeA), typeRes, false)), *(B2->getType()));

	BindExprPtr C1 = builder.bindExpr(toVector<VariablePtr>(), callC1);
	EXPECT_EQ("bind(){h(12, 14)}", toString(*C1));
	EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(), typeRes, false)), *(C1->getType()));

	BindExprPtr C2 = builder.bindExpr(toVector<VariablePtr>(captureVar1), callC2);
	EXPECT_EQ("bind(v1){h(12, v1)}", toString(*C2));
	EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(typeA), typeRes, false)), *(C2->getType()));

	BindExprPtr C3 = builder.bindExpr(toVector<VariablePtr>(captureVar2), callC3);
	EXPECT_EQ("bind(v2){h(v2, 14)}", toString(*C3));
	EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(typeA), typeRes, false)), *(C3->getType()));

	BindExprPtr C4 = builder.bindExpr(toVector<VariablePtr>(captureVar1,captureVar2), callC4);
	EXPECT_EQ("bind(v1,v2){h(v2, v1)}", toString(*C4));
	EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(typeA,typeA), typeRes, false)), *(C4->getType()));


	// check hash codes, children and cloning
	FunctionTypePtr funType = builder.functionType(toVector<TypePtr>(), typeRes, false);
	basicExprTests(empty, funType, toVector<NodePtr>(callA));

	funType = builder.functionType(toVector<TypePtr>(), typeRes, false);
	basicExprTests(B1, funType, toVector<NodePtr>(callB1));

	funType = builder.functionType(toVector<TypePtr>(typeA), typeRes, false);
	basicExprTests(B2, funType, toVector<NodePtr>(captureVar1, callB2));

	funType = builder.functionType(toVector<TypePtr>(), typeRes, false);
	basicExprTests(C1, funType, toVector<NodePtr>(callC1));

	funType = builder.functionType(toVector<TypePtr>(typeA), typeRes, false);
	basicExprTests(C2, funType, toVector<NodePtr>(captureVar1, callC2));

	funType = builder.functionType(toVector<TypePtr>(typeA), typeRes, false);
	basicExprTests(C3, funType, toVector<NodePtr>(captureVar2, callC3));

	funType = builder.functionType(toVector<TypePtr>(typeA, typeA), typeRes, false);
	basicExprTests(C4, funType, toVector<NodePtr>(captureVar1, captureVar2, callC4));
}

TEST(ExpressionsTest, MemberAccessExpr) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<NamedValuePtr> members;

	StringValuePtr idA = StringValue::get(manager,"a");
	StringValuePtr idB = StringValue::get(manager,"b");
	TypePtr typeA = GenericType::get(manager, "typeA");
	TypePtr typeB = GenericType::get(manager, "typeB");
	members.push_back(NamedValue::get(manager, idA, Literal::get(manager, typeA, "1")));
	members.push_back(NamedValue::get(manager, idB, Literal::get(manager, typeB, "2")));
	StructExprPtr data = builder.structExpr(manager, members);

	MemberAccessExprPtr access = MemberAccessExpr::get(manager, data, idA);
	EXPECT_EQ(*typeA, *access->getType());

	MemberAccessExprPtr access2 = MemberAccessExpr::get(manager, data, idB);
	EXPECT_EQ(*typeB, *access2->getType());

	MemberAccessExprPtr access3 = MemberAccessExpr::get(manager, data, idA);
	EXPECT_EQ(*typeA, *access3->getType());

	EXPECT_NE(access, access2);
	EXPECT_NE(access2, access3);
	EXPECT_EQ(access, access3);

	EXPECT_EQ ("(struct{a=1, b=2}.a)", toString(*access));
	EXPECT_EQ ("(struct{a=1, b=2}.b)", toString(*access2));

	// check hash codes, children and cloning
	basicExprTests(access, typeA, toVector<NodePtr>(data, idA));
	basicExprTests(access2, typeB, toVector<NodePtr>(data, idB));
	basicExprTests(access3, typeA, toVector<NodePtr>(data, idA));
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
	IRBuilder builder(manager);

	TypePtr intType = manager.getLangBasic().getUIntGen();
	FunctionTypePtr funType = FunctionType::get(manager, toVector<TypePtr>(), manager.getLangBasic().getUnit());
	FunctionTypePtr guardType = FunctionType::get(manager, toVector<TypePtr>(intType, intType), manager.getLangBasic().getBool());

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
	TypePtr type = manager.getLangBasic().getJob();
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
void basicExprTests(PT expression, const TypePtr& type, const NodeList& children) {

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
		EXPECT_PRED2(equalChildLists, children, cur->getChildList());
	}
}

} // end namespace: core
} // end namespace: insieme
