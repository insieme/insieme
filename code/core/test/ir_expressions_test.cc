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
#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/analysis/normalize.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/test/test_utils.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/manipulation.h"

#include "ir_node_test.inc"

namespace insieme {
namespace core {

	using namespace insieme::core::lang;
	using namespace insieme::utils::set;

	template <typename PT>
	void basicExprTests(PT expression, const TypePtr& type, const NodeList& children);


	TEST(ExpressionsTest, IntLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);

		LiteralPtr i5 = builder.literal(builder.getLangBasic().getIntGen(), "5");
		LiteralPtr i7 = builder.literal(builder.getLangBasic().getIntGen(), "7");
		LiteralPtr i5long = builder.literal(builder.getLangBasic().getInt8(), "5");

		EXPECT_EQ(*i5, *builder.literal(builder.getLangBasic().getIntGen(), "5"));
		EXPECT_NE(*i5, *i5long);
		EXPECT_NE(*i5, *i7);
		EXPECT_EQ(i5->getValueAs<int>(), 5);

		basicExprTests(i5, builder.getLangBasic().getIntGen(), toVector<NodePtr>(builder.getLangBasic().getIntGen(), i5->getValue()));
		basicExprTests(i7, builder.getLangBasic().getIntGen(), toVector<NodePtr>(builder.getLangBasic().getIntGen(), i7->getValue()));
		basicExprTests(i5long, builder.getLangBasic().getInt8(), toVector<NodePtr>(builder.getLangBasic().getInt8(), i5long->getValue()));
	}

	TEST(ExpressionsTest, FloatLiterals) {
		NodeManager manager;
		IRBuilder builder(manager);

		LiteralPtr f5_s = builder.literal(builder.getLangBasic().getFloat(), "5.0");

		basicExprTests(f5_s, builder.getLangBasic().getFloat(), toVector<NodePtr>(builder.getLangBasic().getFloat(), f5_s->getValue()));

		// EXPECT_EQ( *f5, *f5_s ); //-- this is not necessarily true
		std::stringstream ss;
		ss << *f5_s;
		EXPECT_EQ(ss.str(), "5.0");
		//	EXPECT_EQ( f5->getValue(), f5_s->getValue() );
	}

	TEST(ExpressionsTest, Variable) {
		NodeManager manager;

		VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool());
		EXPECT_EQ(format("v%d", var->getId()), toString(*var));

		VariablePtr var2 = Variable::get(manager, manager.getLangBasic().getBool());
		EXPECT_NE(*var, *var2);
		EXPECT_LT(var->getId(), var2->getId());

		VariablePtr var3 = Variable::get(manager, manager.getLangBasic().getBool(), var->getId());
		EXPECT_EQ(var, var3);

		// check hash codes, children and cloning
		basicExprTests(var, manager.getLangBasic().getBool(), toVector<NodePtr>(manager.getLangBasic().getBool(), var->getID()));
		basicExprTests(var2, manager.getLangBasic().getBool(), toVector<NodePtr>(manager.getLangBasic().getBool(), var2->getID()));
	}

	TEST(ExpressionsTest, TupleExpr) {
		NodeManager manager;
		IRBuilder builder(manager);

		LiteralPtr one = Literal::get(manager, manager.getLangBasic().getUInt1(), "1");
		TupleExprPtr empty = builder.tupleExpr(toVector<ExpressionPtr>());
		TupleExprPtr more = builder.tupleExpr(toVector<ExpressionPtr>(manager.getLangBasic().getTrue(), one));

		TypePtr first = TupleType::get(manager, TypeList());
		TypePtr second = TupleType::get(manager, toVector<TypePtr>(manager.getLangBasic().getBool(), manager.getLangBasic().getUInt1()));
		EXPECT_EQ(*first, *empty->getType());
		EXPECT_EQ(*second, *more->getType());

		EXPECT_EQ("tuple()", toString(*empty));
		EXPECT_EQ("tuple(true,1)", toString(*more));


		// check hash codes, children and cloning
		basicExprTests(empty, first, toVector<NodePtr>(first, empty->getExpressions()));
		basicExprTests(more, second, toVector<NodePtr>(second, more->getExpressions()));
	}

TEST(ExpressionTest, Parameters) {
	NodeManager manager;
	IRBuilder builder(manager);

	auto root = builder.parseStmt(""
	                              "alias int = int<4>;"
	                              "def f = (a : int, b : int) -> int { return 1; };"
	                              "f(1,2);");

	EXPECT_EQ("AP([v1,v2])", toString(root.as<CallExprPtr>()->getFunctionExpr().as<LambdaExprPtr>()->getParameterList()));
}

	TEST(ExpressionsTest, Lambda) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr type = GenericType::get(manager, "A");
		CompoundStmtPtr body = CompoundStmt::get(manager, Literal::get(manager, type, "a"));
		VariablePtr varA = Variable::get(manager, builder.refType(type), 1);
		VariablePtr varB = Variable::get(manager, builder.refType(type), 2);
		FunctionTypePtr funType = FunctionType::get(manager, TypeList(), type);
		LambdaPtr empty = Lambda::get(manager, funType, VariableList(), body);
		LambdaPtr little = Lambda::get(manager, funType, toVector(varA), body);
		LambdaPtr more = Lambda::get(manager, funType, toVector(varA, varB), body);

		EXPECT_EQ("fun() {a;}", toString(*empty));
		EXPECT_EQ("fun(ref<A,f,f,plain> v1) {a;}", toString(*little));
		EXPECT_EQ("fun(ref<A,f,f,plain> v1, ref<A,f,f,plain> v2) {a;}", toString(*more));

		// conduct basic node checks
		basicNodeTests(empty, toVector<NodePtr>(funType, empty->getParameters(), body));
		basicNodeTests(little, toVector<NodePtr>(funType, little->getParameters(), body));
		basicNodeTests(more, toVector<NodePtr>(funType, more->getParameters(), body));
	}

	TEST(ExpressionsTest, LambdaExpr) {
		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& gen = builder.getLangBasic();

		// create a recursive even/odd example
		FunctionTypePtr functionType = builder.functionType(toVector<TypePtr>(gen.getUInt4()), gen.getBool());
		LambdaReferencePtr evenVar = builder.lambdaReference(functionType, "even");
		LambdaReferencePtr oddVar = builder.lambdaReference(functionType, "odd");


		VariableList param;
		param.push_back(builder.variable(builder.refType(gen.getUInt4()), 3));

		LiteralPtr zero = builder.literal(gen.getUInt4(), "0");
		VariablePtr x = builder.variable(builder.refType(gen.getUInt4()), 3);
		ExpressionPtr condition = builder.callExpr(gen.getBool(), gen.getUnsignedIntEq(), x, zero);

		// build even body ...
		StatementPtr evenBody =
		    builder.ifStmt(condition, builder.returnStmt(gen.getTrue()),
		                   builder.returnStmt(builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), oddVar, x))));
		LambdaPtr evenLambda = builder.lambda(functionType, param, evenBody);

		// build odd body ...
		StatementPtr oddBody =
		    builder.ifStmt(condition, builder.returnStmt(gen.getFalse()),
		                   builder.returnStmt(builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), evenVar, x))));
		LambdaPtr oddLambda = builder.lambda(functionType, param, oddBody);

		// finish definition
		LambdaBindingMap bindings;
		bindings.insert({ evenVar, evenLambda });
		bindings.insert({ oddVar, oddLambda });
		LambdaDefinitionPtr definition = builder.lambdaDefinition(bindings);

		vector<LambdaBindingPtr> compVal;
		for(auto b : bindings) {
			compVal.push_back(builder.lambdaBinding(b.first, b.second));
		}

		// test definition node
		for(auto c : compVal) {
			EXPECT_TRUE(analysis::contains(definition, c));
		}

		// create recursive lambda nodes
		LambdaExprPtr even = builder.lambdaExpr(evenVar, definition);
		LambdaExprPtr odd = builder.lambdaExpr(oddVar, definition);

		basicExprTests(even, functionType, toList(toVector<NodePtr>(functionType, evenVar, definition)));
		basicExprTests(odd, functionType, toList(toVector<NodePtr>(functionType, oddVar, definition)));

		EXPECT_EQ(even, even);
		EXPECT_NE(even, odd);
		EXPECT_NE(odd, even);
		EXPECT_EQ(odd, odd);

		EXPECT_NE((*even).hash(), (*odd).hash());

		// test is recursive
		EXPECT_TRUE(even->isRecursive());
		EXPECT_TRUE(odd->isRecursive());

		LambdaExprPtr simple = builder.lambdaExpr(functionType, toVector(x), builder.returnStmt(builder.boolLit("true")));
		EXPECT_FALSE(simple->isRecursive());

		EXPECT_EQ("rec even.{even=fun(ref<uint<4>,f,f,plain> v3) {if(uint_eq(v3, 0)) {return true;} else {return bool_not(odd(v3));};}, odd=fun(ref<uint<4>,f,f,plain> v3) {if(uint_eq(v3, 0)) "
		          "{return false;} else {return bool_not(even(v3));};}}",
		          toString(*even));
		EXPECT_EQ("rec odd.{even=fun(ref<uint<4>,f,f,plain> v3) {if(uint_eq(v3, 0)) {return true;} else {return bool_not(odd(v3));};}, odd=fun(ref<uint<4>,f,f,plain> v3) {if(uint_eq(v3, 0)) "
		          "{return false;} else {return bool_not(even(v3));};}}",
		          toString(*odd));
	}

	TEST(ExpressionsTest, CallExpr) {
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

		LiteralPtr constantA = builder.literal(typeA, "12");
		LiteralPtr constantB = builder.literal(typeA, "14");

		auto callBArg1 = builder.declaration(typeA, constantA);
		auto callCArg1 = builder.declaration(typeA, constantA);
		auto callCArg2 = builder.declaration(typeA, constantB);

		CallExprPtr callA = builder.callExpr(typeRes, funA, toVector<ExpressionPtr>());
		CallExprPtr callB = builder.callExpr(typeRes, funB, toVector<DeclarationPtr>(callBArg1));
		CallExprPtr callC = builder.callExpr(typeRes, funC, toVector<DeclarationPtr>(callCArg1, callCArg2));

		EXPECT_EQ(callA->getArgumentList(), toVector<ExpressionPtr>());
		EXPECT_EQ(callB->getArgumentList(), toVector<ExpressionPtr>(constantA));
		EXPECT_EQ(callC->getArgumentList(), toVector<ExpressionPtr>(constantA, constantB));

		CallExprAddress callC2 = CallExprAddress(callC);
		EXPECT_EQ(callC2->getArgumentList(), toVector(callC2->getArgument(0), callC2->getArgument(1)));

		basicExprTests(callA, typeRes, toVector<NodePtr>(typeRes, funA));
		basicExprTests(callB, typeRes, toVector<NodePtr>(typeRes, funB, callBArg1));
		basicExprTests(callC, typeRes, toVector<NodePtr>(typeRes, funC, callCArg1, callCArg2));
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
		EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(), typeRes, FK_CLOSURE)), *(empty->getType()));

		BindExprPtr B1 = builder.bindExpr(toVector<VariablePtr>(), callB1);
		EXPECT_EQ("bind(){g(12)}", toString(*B1));
		EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(), typeRes, FK_CLOSURE)), *(B1->getType()));

		BindExprPtr B2 = builder.bindExpr(toVector<VariablePtr>(captureVar1), callB2);
		EXPECT_EQ("bind(v1){g(v1)}", toString(*B2));
		EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(typeA), typeRes, FK_CLOSURE)), *(B2->getType()));

		BindExprPtr C1 = builder.bindExpr(toVector<VariablePtr>(), callC1);
		EXPECT_EQ("bind(){h(12, 14)}", toString(*C1));
		EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(), typeRes, FK_CLOSURE)), *(C1->getType()));

		BindExprPtr C2 = builder.bindExpr(toVector<VariablePtr>(captureVar1), callC2);
		EXPECT_EQ("bind(v1){h(12, v1)}", toString(*C2));
		EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(typeA), typeRes, FK_CLOSURE)), *(C2->getType()));

		BindExprPtr C3 = builder.bindExpr(toVector<VariablePtr>(captureVar2), callC3);
		EXPECT_EQ("bind(v2){h(v2, 14)}", toString(*C3));
		EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(typeA), typeRes, FK_CLOSURE)), *(C3->getType()));

		BindExprPtr C4 = builder.bindExpr(toVector<VariablePtr>(captureVar1, captureVar2), callC4);
		EXPECT_EQ("bind(v1,v2){h(v2, v1)}", toString(*C4));
		EXPECT_EQ(*(builder.functionType(toVector<TypePtr>(typeA, typeA), typeRes, FK_CLOSURE)), *(C4->getType()));


		// check hash codes, children and cloning
		FunctionTypePtr funType = builder.functionType(toVector<TypePtr>(), typeRes, FK_CLOSURE);
		basicExprTests(empty, funType, toVector<NodePtr>(funType, builder.parameters(), callA));

		funType = builder.functionType(toVector<TypePtr>(), typeRes, FK_CLOSURE);
		basicExprTests(B1, funType, toVector<NodePtr>(funType, builder.parameters(), callB1));

		funType = builder.functionType(toVector<TypePtr>(typeA), typeRes, FK_CLOSURE);
		basicExprTests(B2, funType, toVector<NodePtr>(funType, builder.parameters(captureVar1), callB2));

		funType = builder.functionType(toVector<TypePtr>(), typeRes, FK_CLOSURE);
		basicExprTests(C1, funType, toVector<NodePtr>(funType, builder.parameters(), callC1));

		funType = builder.functionType(toVector<TypePtr>(typeA), typeRes, FK_CLOSURE);
		basicExprTests(C2, funType, toVector<NodePtr>(funType, builder.parameters(captureVar1), callC2));

		funType = builder.functionType(toVector<TypePtr>(typeA), typeRes, FK_CLOSURE);
		basicExprTests(C3, funType, toVector<NodePtr>(funType, builder.parameters(captureVar2), callC3));

		funType = builder.functionType(toVector<TypePtr>(typeA, typeA), typeRes, FK_CLOSURE);
		basicExprTests(C4, funType, toVector<NodePtr>(funType, builder.parameters(captureVar1, captureVar2), callC4));
	}

	TEST(ExpressionsTest, MemberAccessExpr) {
		NodeManager manager;
		IRBuilder builder(manager);

		StringValuePtr idA = StringValue::get(manager, "a");
		StringValuePtr idB = StringValue::get(manager, "b");
		TypePtr typeA = GenericType::get(manager, "typeA");
		TypePtr typeB = GenericType::get(manager, "typeB");
		ExpressionList members { Literal::get(manager, typeA, "1"), Literal::get(manager, typeB, "2") };
		FieldList fields { builder.field(idA, typeA), builder.field(idB, typeB) };
		auto structT = builder.structType(fields);

		ExpressionPtr data = builder.deref(builder.initExprTemp(builder.refType(structT), members));

		ExpressionPtr access = builder.accessMember(data, idA);
		EXPECT_EQ(*typeA, *access->getType());

		ExpressionPtr access2 = builder.accessMember(data, idB);
		EXPECT_EQ(*typeB, *access2->getType());

		ExpressionPtr access3 = builder.accessMember(data, idA);
		EXPECT_EQ(*typeA, *access3->getType());

		EXPECT_NE(access, access2);
		EXPECT_NE(access2, access3);
		EXPECT_EQ(builder.normalize(access), builder.normalize(access3));

		EXPECT_EQ("composite_member_access(ref_deref((AP(rec ref_temp.{ref_temp=fun(ref<type<'a>,f,f,plain> v0) {return ref_alloc(ref_deref(v0), "
		          "mem_loc_stack);}}(type<struct "
		          "{a:typeA,b:typeB,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),IMP__operator_assign_(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_"
		          "ref>,IMP__operator_assign_(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}>)) : AP(ref<struct "
		          "{a:typeA,b:typeB,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),IMP__operator_assign_(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_"
		          "ref>,IMP__operator_assign_(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>},f,f,plain>)){AP(1), AP(2)}), a, type<typeA>)",
		          toString(*access));
		EXPECT_EQ("composite_member_access(ref_deref((AP(rec ref_temp.{ref_temp=fun(ref<type<'a>,f,f,plain> v0) {return ref_alloc(ref_deref(v0), "
		          "mem_loc_stack);}}(type<struct "
		          "{a:typeA,b:typeB,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),IMP__operator_assign_(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_"
		          "ref>,IMP__operator_assign_(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}>)) : AP(ref<struct "
		          "{a:typeA,b:typeB,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),IMP__operator_assign_(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_"
		          "ref>,IMP__operator_assign_(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>},f,f,plain>)){AP(1), AP(2)}), b, type<typeB>)",
		          toString(*access2));
	}

	TEST(ExpressionsTest, TupleProjectionExpr) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = GenericType::get(manager, "typeA");
		TypePtr typeB = GenericType::get(manager, "typeB");

		std::vector<ExpressionPtr> expressions;
		expressions.push_back(Literal::get(manager, typeA, "1"));
		expressions.push_back(Literal::get(manager, typeB, "2"));

		TupleExprPtr tuple = builder.tupleExpr(expressions);

		ExpressionPtr access = builder.accessComponent(tuple, 0);
		EXPECT_EQ(*typeA, *access->getType());

		ExpressionPtr access2 = builder.accessComponent(tuple, 1);
		EXPECT_EQ(*typeB, *access2->getType());

		ExpressionPtr access3 = builder.accessComponent(tuple, 0);
		EXPECT_EQ(*typeA, *access3->getType());

		EXPECT_NE(access, access2);
		EXPECT_NE(access2, access3);
		EXPECT_EQ(builder.normalize(access), builder.normalize(access3));

		EXPECT_EQ("tuple_member_access(tuple(1,2), 0, type<typeA>)", toString(*access));
		EXPECT_EQ("tuple_member_access(tuple(1,2), 1, type<typeB>)", toString(*access2));
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
		basicExprTests(markerA, type, toVector<NodePtr>(type, markerA->getID(), literal));
		basicExprTests(markerB, type, toVector<NodePtr>(type, markerB->getID(), literal));
	}

	TEST(ExpressionsTest, JobExpr) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr intType = manager.getLangBasic().getUIntGen();
		FunctionTypePtr funType = FunctionType::get(manager, toVector<TypePtr>(), manager.getLangBasic().getUnit());

		ExpressionPtr body = Variable::get(manager, funType);

		ExpressionPtr range = builder.getThreadNumRange(1, 40);
		JobExprPtr job = builder.jobExpr(range, body);

		// check hash codes, children and cloning
		TypePtr type = manager.getLangBasic().getJob();
		vector<NodePtr> childList;
		childList.push_back(job->getType());
		childList.push_back(range);
		childList.push_back(job->getBody());

		basicExprTests(job, type, childList);
	}

	TEST(ExpressionsTest, LambdaPeeling) {
		NodeManager manager;
		IRBuilder builder(manager);
		LambdaExprPtr lambda;

		// check ordinary lambda
		lambda = builder.parseExpr("let f = ()->unit { 5; } in f").as<LambdaExprPtr>();
		ASSERT_TRUE(lambda);

		EXPECT_FALSE(lambda->isRecursive());
		EXPECT_EQ(lambda, lambda->peel());


		// check recursive lambda
		manager.setNextFreshID(0);
		lambda = builder.normalize(
			builder.parseExpr("decl f: ()->unit;"
							  "def f = ()->unit { 5; f(); }; f").as<LambdaExprPtr>());
		ASSERT_TRUE(lambda);

		EXPECT_TRUE(lambda->isRecursive());
		EXPECT_EQ("rec f.{f=fun() {5; f();}}", toString(*lambda));
		EXPECT_EQ("rec _.{_=fun() {5; rec f.{f=fun() {5; f();}}();}}", toString(*lambda->peel()));


		// check mutual recursive lambdas
		manager.setNextFreshID(0);
		lambda = builder.parseExpr("decl g: ()->unit;"
								   "def f = ()->unit { 1; g(); }; "
		                           "def g = ()->unit { 2; f(); }; "
		                           " f")
		             .as<LambdaExprPtr>();
		ASSERT_TRUE(lambda);

		EXPECT_TRUE(lambda->isRecursive());
		EXPECT_EQ("rec f.{f=fun() {1; g();}, g=fun() {2; f();}}", toString(*lambda));
		EXPECT_EQ("rec _.{_=fun() {1; rec g.{f=fun() {1; g();}, g=fun() {2; f();}}();}}", toString(*lambda->peel()));


		// check nested recursive lambda
		manager.setNextFreshID(0);
		lambda = builder.parseExpr("decl f: ()->unit;"
								   "def f = ()->unit { "
		                           "	5; "
		                           "	()->unit { f(); } (); "
		                           "}; f")
		             .as<LambdaExprPtr>();
		ASSERT_TRUE(lambda);

		EXPECT_TRUE(lambda->isRecursive());
		EXPECT_EQ("rec f.{f=fun() {5; rec _.{_=fun() {f();}}();}}", toString(*lambda));
		EXPECT_EQ("rec _.{_=fun() {5; rec _.{_=fun() {rec f.{f=fun() {5; rec _.{_=fun() {f();}}();}}();}}();}}", toString(*lambda->peel()));
		EXPECT_PRED2(containsSubString, toString(*lambda->peel()), toString(*lambda));

		// check nested mutual recursive lambdas
		manager.setNextFreshID(0);
		lambda = builder.parseExpr("decl g: ()->unit;"
								   "def f = "
		                           "	()->unit { "
		                           "		1; "
		                           "		()->unit { g(); } (); "
		                           "	}; "
		                           "def g = "
								   "	()->unit { "
		                           "		2; "
		                           "		()->unit { f(); } (); "
		                           "	}; "
		                           "f")
		             .as<LambdaExprPtr>();
		ASSERT_TRUE(lambda);

		EXPECT_TRUE(lambda->isRecursive());
		EXPECT_EQ("rec f.{f=fun() {1; rec _.{_=fun() {g();}}();}, g=fun() {2; rec _.{_=fun() {f();}}();}}", toString(*lambda));
		EXPECT_EQ(
		    "rec _.{_=fun() {1; rec _.{_=fun() {rec g.{f=fun() {1; rec _.{_=fun() {g();}}();}, g=fun() {2; rec _.{_=fun() {f();}}();}}();}}();}}",
		    toString(*lambda->peel()));
	}

	TEST(ExpressionsTest, LambdaPeelingEvenOdd) {
		NodeManager manager;
		IRBuilder builder(manager);

		/**
		 * Bug: when peeling recursive functions using the recursive variables within binds (suspected)
		 * 		an invalid node-composition assertion is triggered.
		 */

		LambdaExprPtr even = builder.normalize(builder.parseExpr("alias int = int<4>; "
		                                       "decl odd: (int)->bool;"
											   "def even = (x: int)->bool { return (x==0)?true:odd(x-1); };"
		                                       "def odd = (x: int)->bool { return (x==0)?false:even(x-1); };"
		                                       "even"))
		                         .as<LambdaExprPtr>();

		ASSERT_TRUE(even);

		auto res = check(even, core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->peel(manager), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->peel(manager)->peel(manager), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;


		// -- the same normalized --

		even = analysis::normalize(even);

		res = check(even, core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->peel(manager), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->peel(manager)->peel(manager), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;


		// -- peeling multiple times --

		res = check(even->peel(0), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->peel(1), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->peel(2), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->peel(3), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;
	}


	TEST(ExpressionsTest, LambdaUnrollingEvenOdd) {
		NodeManager manager;
		IRBuilder builder(manager);

		/**
		 * Bug: when peeling recursive functions using the recursive variables within binds (suspected)
		 * 		an invalid node-composition assertion is triggered.
		 */

		LambdaExprPtr even = builder.normalize(builder.parseExpr("alias int = int<4>; "
		                                       "decl odd: (int)->bool;"
											   "def even = (x: int)->bool { return (x==0)?true:(odd(x-1)); };"
		                                       "def odd = (x: int)->bool { return (x==0)?false:(even(x-1)); };"
		                                       "even"))
		                         .as<LambdaExprPtr>();

		ASSERT_TRUE(even);

		auto res = check(even, core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		EXPECT_NE(*even->unroll(2), *even->peel(2));

		//std::cout << "Unroll 0:\n" << core::printer::PrettyPrinter(even->unroll(0)) << "\n\n";
		// std::cout << "Unroll 1:\n" << core::printer::PrettyPrinter(even->unroll(1)) << "\n\n";
		// std::cout << "Unroll 2:\n" << core::printer::PrettyPrinter(even->unroll(2)) << "\n\n";
		// std::cout << "Unroll 5:\n" << core::printer::PrettyPrinter(even->unroll(5)) << "\n\n";

		EXPECT_PRED2(containsSubString, toString(core::printer::PrettyPrinter(even->unroll(2))),
		             "return *v0==0?true:*v0-1==0?false:even(*v0-1-1);");

		res = check(even->unroll(manager, 2), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << even->unroll(manager, 2) << res;

		res = check(even->unroll(manager, 3), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << even->unroll(manager, 3) << res;


		// -- the same normalized --

		even = analysis::normalize(even);

		res = check(even, core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->unroll(manager, 2), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;


		// -- unrolling multiple times --

		res = check(even->unroll(0), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->unroll(1), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->unroll(2), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;

		res = check(even->unroll(3), core::checks::getFullCheck());
		EXPECT_TRUE(res.empty()) << res;
	}


	TEST(ExpressionsTest, LambdaUnrollingNonRecursive) {
		NodeManager manager;
		IRBuilder builder(manager);

		LambdaExprPtr simple = builder.parseExpr("(x: int<4>)->bool { return (x==0); }").as<LambdaExprPtr>();

		ASSERT_TRUE(simple);

		EXPECT_EQ(simple, simple->peel(0));
		EXPECT_EQ(simple, simple->peel(1));
		EXPECT_EQ(simple, simple->peel(2));
		EXPECT_EQ(simple, simple->peel(3));

		EXPECT_EQ(simple, simple->unroll(0));
		EXPECT_EQ(simple, simple->unroll(1));
		EXPECT_EQ(simple, simple->unroll(2));
		EXPECT_EQ(simple, simple->unroll(3));
	}

	TEST(ExpressionsTest, LambdaUnrollingExponential) {
		NodeManager manager;
		IRBuilder builder(manager);

		LambdaExprPtr fun = builder.parseExpr("alias int = int<4> ; "
		                                      "decl fun: (int)->int; "
											  "def fun = (x: int)->int { fun(x-2) + fun(x-1); };"
		                                      "fun")
		                        .as<LambdaExprPtr>();

		ASSERT_TRUE(fun);

		// count number of recursive calls
		auto ref = fun->getReference();
		EXPECT_EQ(2u, fun->getDefinition()->getRecursiveCallsOf(ref).size());

		// this number should grow when unrolling the function
		EXPECT_EQ(4u, fun->unroll(2)->getDefinition()->getRecursiveCallsOf(ref).size());
		EXPECT_EQ(8u, fun->unroll(3)->getDefinition()->getRecursiveCallsOf(ref).size());
		EXPECT_EQ(16u, fun->unroll(4)->getDefinition()->getRecursiveCallsOf(ref).size());


		// number of free variables should be 0 in case of a peeling
		EXPECT_EQ(0u, analysis::getFreeVariableAddresses(fun->peel()).size());
		EXPECT_EQ(0u, analysis::getFreeVariableAddresses(fun->peel(2)).size());
		EXPECT_EQ(0u, analysis::getFreeVariableAddresses(fun->peel(3)).size());
	}


	TEST(ExpressionsTest, LambdaIsRecursiveTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		LambdaExprPtr fun = builder.parseExpr("decl f: (int<4>)->int<4>;	"
											  "def f = (x: int<4>)->int<4> {"
		                                      "	return (x==0)?0:((x==1)?1:f(x-1)+f(x-2));"
		                                      "};  f")
		                        .as<LambdaExprPtr>();

		ASSERT_TRUE(fun);

//		std::cout << core::printer::PrettyPrinter(fun, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS) << "\n";
		EXPECT_TRUE(fun->isRecursive());

		// fix usage of recursive variables
		fun = transform::correctRecursiveLambdaVariableUsage(manager, fun);

//		std::cout << core::printer::PrettyPrinter(fun, core::printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS) << "\n";
		EXPECT_TRUE(fun->isRecursive());
	}

	TEST(ExpressionsTest, UnrollCompactFib) {
		NodeManager manager;
		IRBuilder builder(manager);
		LambdaExprPtr fun = builder.parseExpr("decl f: (int<4>)->int<4>;"
											  "def f = (x: int<4>)->int<4> {"
		                                      "	return (x==0)?0:((x==1)?1:f(x-1)+f(x-2));"
		                                      "}; f")
		                        .as<LambdaExprPtr>();

		ASSERT_TRUE(fun);
		EXPECT_TRUE(fun->isRecursive());

		fun = transform::correctRecursiveLambdaVariableUsage(manager, fun);
		LambdaExprPtr unrolled = fun->unroll(2);

		EXPECT_TRUE(unrolled->isRecursive());
	}

	template <typename PT>
	void basicExprTests(PT expression, const TypePtr& type, const NodeList& children) {
		typedef typename PT::element_type T;

		// ------------- Basic Node Tests ----------------

		basicNodeTests(expression, children);

		// ------------ Expression Ptr based tests -------------

		// check type
		EXPECT_EQ(*type, *expression->getType());

		// type has to be first child - NOTE: not true for recursive lambdas, member access and tuple projection
		//	EXPECT_EQ(*type, *(expression->getChildList()[0]));

		// ------------ Type Token based tests -------------

		// copy and clone the type
		NodeManager manager;
		T copy = T(*expression);
		T* clone = &*manager.get(expression);

		// check whether all are equal
		T* all[] = {&*expression, &copy, clone};
		for(int i = 0; i < 3; i++) {
			for(int j = 0; j < 3; j++) {
				T* a = all[i];
				T* b = all[j];

				EXPECT_EQ(*a, *b);
				EXPECT_EQ(*a->getType(), *b->getType());
			}
		}

		// check type properties
		for(int i = 0; i < 3; i++) {
			T* cur = all[i];

			// check type
			EXPECT_EQ(*type, *expression->getType());

			// check children
			EXPECT_PRED2(equalChildLists, children, cur->getChildList());
		}
	}

} // end namespace: core
} // end namespace: insieme
