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

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/checks/ir_checks.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace core {

bool containsSubstring(string subStr, string full, int minTimes = 1) {
	size_t pos = 0;
	for (int i=0; i<minTimes && pos != string::npos; i++) {
		pos = full.find(subStr, pos+1);
	}
	return pos!=string::npos;
}

bool isSubString(string subStr, string full) {
	return containsSubstring(subStr, full);
}



TEST(Manipulation, Insert) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	stmts.push_back(builder.literal(builder.genericType("X"), "A"));
	stmts.push_back(builder.literal(builder.genericType("X"), "B"));
	stmts.push_back(builder.literal(builder.genericType("X"), "C"));
	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	EXPECT_EQ("{A; B; C;}", toString(*compound));

	StatementPtr stmt = builder.literal(builder.genericType("X"), "X");

	NodePtr res;
	CompoundStmtAddress target(compound);
	res = transform::insert(manager, target, stmt, 0);
	EXPECT_EQ("{X; A; B; C;}", toString(*res));

	res = transform::insert(manager, target, stmt, 1);
	EXPECT_EQ("{A; X; B; C;}", toString(*res));

	res = transform::insert(manager, target, stmt, 2);
	EXPECT_EQ("{A; B; X; C;}", toString(*res));

	res = transform::insert(manager, target, stmt, 3);
	EXPECT_EQ("{A; B; C; X;}", toString(*res));

	// TEST exceeding indices
	res = transform::insert(manager, target, stmt, 15);
	EXPECT_EQ("{A; B; C; X;}", toString(*res));


	// check for deeper scope
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound,stmts[0]));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1,stmts[0]));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::insert(manager, target, stmt, 0);
	EXPECT_EQ("{{{X; A; B; C;}; A;}; A;}", toString(*res));

	res = transform::insert(manager, target, stmt, 1);
	EXPECT_EQ("{{{A; X; B; C;}; A;}; A;}", toString(*res));

	res = transform::insert(manager, target, stmt, 2);
	EXPECT_EQ("{{{A; B; X; C;}; A;}; A;}", toString(*res));

	res = transform::insert(manager, target, stmt, 3);
	EXPECT_EQ("{{{A; B; C; X;}; A;}; A;}", toString(*res));

	// TEST exceeding indices
	res = transform::insert(manager, target, stmt, 15);
	EXPECT_EQ("{{{A; B; C; X;}; A;}; A;}", toString(*res));
}

TEST(Manipulation, InsertList) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	stmts.push_back(builder.literal(builder.genericType("X"), "A"));
	stmts.push_back(builder.literal(builder.genericType("X"), "B"));
	stmts.push_back(builder.literal(builder.genericType("X"), "C"));
	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	EXPECT_EQ("{A; B; C;}", toString(*compound));

	StatementList insertStmts;
	insertStmts.push_back(builder.literal(builder.genericType("X"), "X"));
	insertStmts.push_back(builder.literal(builder.genericType("Y"), "Y"));


	NodePtr res;
	CompoundStmtAddress target(compound);
	res = transform::insert(manager, target, insertStmts, 0);
	EXPECT_EQ("{X; Y; A; B; C;}", toString(*res));

	res = transform::insert(manager, target, insertStmts, 1);
	EXPECT_EQ("{A; X; Y; B; C;}", toString(*res));

	res = transform::insert(manager, target, insertStmts, 2);
	EXPECT_EQ("{A; B; X; Y; C;}", toString(*res));

	res = transform::insert(manager, target, insertStmts, 3);
	EXPECT_EQ("{A; B; C; X; Y;}", toString(*res));

	// TEST exceeding indices
	res = transform::insert(manager, target, insertStmts, 15);
	EXPECT_EQ("{A; B; C; X; Y;}", toString(*res));
}

TEST(Manipulation, InsertBefore) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	auto aLit = builder.literal(builder.genericType("X"), "A");
	stmts.push_back(aLit);
	auto bLit = builder.literal(builder.genericType("X"), "B");
	stmts.push_back(bLit);
	auto cLit = builder.literal(builder.genericType("X"), "C");
	stmts.push_back(cLit);
	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	EXPECT_EQ("{A; B; C;}", toString(*compound));

	StatementPtr stmt = builder.literal(builder.genericType("X"), "X");

	NodePtr res;
	CompoundStmtAddress target(compound);
	auto childAddr = static_address_cast<const Statement>(target.getAddressOfChild(0));
	res = transform::insertBefore(manager, childAddr, stmt);
	EXPECT_EQ("{X; A; B; C;}", toString(*res));

	childAddr = static_address_cast<const Statement>(target.getAddressOfChild(1));
	res = transform::insertBefore(manager, childAddr, stmt);
	EXPECT_EQ("{A; X; B; C;}", toString(*res));

	childAddr = static_address_cast<const Statement>(target.getAddressOfChild(2));
	res = transform::insertBefore(manager, childAddr, stmt);
	EXPECT_EQ("{A; B; X; C;}", toString(*res));
}


TEST(Manipulation, InsertAfter) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	auto aLit = builder.literal(builder.genericType("X"), "A");
	stmts.push_back(aLit);
	auto bLit = builder.literal(builder.genericType("X"), "B");
	stmts.push_back(bLit);
	auto cLit = builder.literal(builder.genericType("X"), "C");
	stmts.push_back(cLit);
	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	EXPECT_EQ("{A; B; C;}", toString(*compound));

	StatementPtr stmt = builder.literal(builder.genericType("X"), "X");

	NodePtr res;
	CompoundStmtAddress target(compound);
	auto childAddr = static_address_cast<const Statement>(target.getAddressOfChild(0));
	res = transform::insertAfter(manager, childAddr, stmt);
	EXPECT_EQ("{A; X; B; C;}", toString(*res));

	childAddr = static_address_cast<const Statement>(target.getAddressOfChild(1));
	res = transform::insertAfter(manager, childAddr, stmt);
	EXPECT_EQ("{A; B; X; C;}", toString(*res));

	childAddr = static_address_cast<const Statement>(target.getAddressOfChild(2));
	res = transform::insertAfter(manager, childAddr, stmt);
	EXPECT_EQ("{A; B; C; X;}", toString(*res));
}

TEST(Manipulation, ReplaceList) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	stmts.push_back(builder.literal(builder.genericType("X"), "A"));
	stmts.push_back(builder.literal(builder.genericType("X"), "B"));
	stmts.push_back(builder.literal(builder.genericType("X"), "C"));
	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	StatementList insertStmts;
	insertStmts.push_back(builder.literal(builder.genericType("X"), "X"));
	insertStmts.push_back(builder.literal(builder.genericType("Y"), "Y"));

	EXPECT_EQ("{A; B; C;}", toString(*compound));

	NodePtr res;
	CompoundStmtAddress target(compound);
	res = transform::replace(manager, target, 0, insertStmts);
	EXPECT_EQ("{X; Y; B; C;}", toString(*res));

	res = transform::replace(manager, target, 1, insertStmts);
	EXPECT_EQ("{A; X; Y; C;}", toString(*res));

	res = transform::replace(manager, target, 2, insertStmts);
	EXPECT_EQ("{A; B; X; Y;}", toString(*res));


	// check for deeper scope
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound, stmts[0]));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1, stmts[0]));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::replace(manager, target, 0, insertStmts);
	EXPECT_EQ("{{{X; Y; B; C;}; A;}; A;}", toString(*res));

	res = transform::replace(manager, target, 1, insertStmts);
	EXPECT_EQ("{{{A; X; Y; C;}; A;}; A;}", toString(*res));

	res = transform::replace(manager, target, 2, insertStmts);
	EXPECT_EQ("{{{A; B; X; Y;}; A;}; A;}", toString(*res));

}

TEST(Manipulation, Replace) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	stmts.push_back(builder.literal(builder.genericType("X"), "A"));
	stmts.push_back(builder.literal(builder.genericType("X"), "B"));
	stmts.push_back(builder.literal(builder.genericType("X"), "C"));
	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	StatementPtr stmt = builder.literal(builder.genericType("X"), "X");

	EXPECT_EQ("{A; B; C;}", toString(*compound));

	NodePtr res;
	CompoundStmtAddress target(compound);
	res = transform::replace(manager, target, 0, stmt);
	EXPECT_EQ("{X; B; C;}", toString(*res));

	res = transform::replace(manager, target, 1, stmt);
	EXPECT_EQ("{A; X; C;}", toString(*res));

	res = transform::replace(manager, target, 2, stmt);
	EXPECT_EQ("{A; B; X;}", toString(*res));


	// check for deeper scope
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound, stmts[0]));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1, stmts[0]));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::replace(manager, target, 0, stmt);
	EXPECT_EQ("{{{X; B; C;}; A;}; A;}", toString(*res));

	res = transform::replace(manager, target, 1, stmt);
	EXPECT_EQ("{{{A; X; C;}; A;}; A;}", toString(*res));

	res = transform::replace(manager, target, 2, stmt);
	EXPECT_EQ("{{{A; B; X;}; A;}; A;}", toString(*res));

}

TEST(Manipulation, Remove) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	stmts.push_back(builder.literal(builder.genericType("X"), "A"));
	stmts.push_back(builder.literal(builder.genericType("X"), "B"));
	stmts.push_back(builder.literal(builder.genericType("X"), "C"));
	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	EXPECT_EQ("{A; B; C;}", toString(*compound));

	NodePtr res;
	CompoundStmtAddress target(compound);
	res = transform::remove(manager, target, 0);
	EXPECT_EQ("{B; C;}", toString(*res));

	res = transform::remove(manager, target, 1);
	EXPECT_EQ("{A; C;}", toString(*res));

	res = transform::remove(manager, target, 2);
	EXPECT_EQ("{A; B;}", toString(*res));


	// check for deeper scope
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound, stmts[0]));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1, stmts[0]));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::remove(manager, target, 0);
	EXPECT_EQ("{{{B; C;}; A;}; A;}", toString(*res));

	res = transform::remove(manager, target, 1);
	EXPECT_EQ("{{{A; C;}; A;}; A;}", toString(*res));

	res = transform::remove(manager, target, 2);
	EXPECT_EQ("{{{A; B;}; A;}; A;}", toString(*res));

}


TEST(Manipulation, RemoveMultiple) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	stmts.push_back(builder.literal(builder.genericType("X"), "A"));
	stmts.push_back(builder.literal(builder.genericType("X"), "B"));
	stmts.push_back(builder.literal(builder.genericType("X"), "C"));

	stmts.push_back(builder.compoundStmt(
		builder.literal(builder.genericType("X"), "D"),
		builder.literal(builder.genericType("X"), "E")
	));

	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	EXPECT_EQ("{A; B; C; {D; E;};}", toString(*compound));


	// get some addresses
	CompoundStmtAddress root(compound);
	auto a = root->getStatement(0);
	auto b = root->getStatement(1);
	auto c = root->getStatement(2);
	auto de = static_address_cast<CompoundStmtAddress>(root->getStatement(3));
	auto d = de->getStatement(0);
	auto e = de->getStatement(1);

	EXPECT_EQ("A", toString(*a));
	EXPECT_EQ("B", toString(*b));
	EXPECT_EQ("C", toString(*c));
	EXPECT_EQ("D", toString(*d));
	EXPECT_EQ("E", toString(*e));
	EXPECT_EQ("{D; E;}", toString(*de));

	// removing multiple
	EXPECT_EQ("{{D; E;};}", toString(*transform::remove(manager, toVector(a,b,c))));
	EXPECT_EQ("{A; C; {E;};}", toString(*transform::remove(manager, toVector(d,b))));
	EXPECT_EQ("{{};}", toString(*transform::remove(manager, toVector(d,a,c,b,e))));

}

TEST(Manipulation, Move) {
	NodeManager manager;
	IRBuilder builder(manager);

	vector<StatementPtr> stmts;
	stmts.push_back(builder.literal(builder.genericType("X"), "A"));
	stmts.push_back(builder.literal(builder.genericType("X"), "B"));
	stmts.push_back(builder.literal(builder.genericType("X"), "C"));
	CompoundStmtPtr compound = builder.compoundStmt(stmts);

	EXPECT_EQ("{A; B; C;}", toString(*compound));

	NodePtr res;
	CompoundStmtAddress target(compound);

	res = transform::move(manager, target, 0, 0);
	EXPECT_EQ("{A; B; C;}", toString(*res));

	res = transform::move(manager, target, 0, 1);
	EXPECT_EQ("{B; A; C;}", toString(*res));

	res = transform::move(manager, target, 0, 2);
	EXPECT_EQ("{B; C; A;}", toString(*res));

	res = transform::move(manager, target, 1, -1);
	EXPECT_EQ("{B; A; C;}", toString(*res));

	res = transform::move(manager, target, 1, 0);
	EXPECT_EQ("{A; B; C;}", toString(*res));

	res = transform::move(manager, target, 1, 1);
	EXPECT_EQ("{A; C; B;}", toString(*res));

	res = transform::move(manager, target, 2, -2);
	EXPECT_EQ("{C; A; B;}", toString(*res));

	res = transform::move(manager, target, 2, -1);
	EXPECT_EQ("{A; C; B;}", toString(*res));

	res = transform::move(manager, target, 2, 0);
	EXPECT_EQ("{A; B; C;}", toString(*res));

	// TEST exceeding displacement
	res = transform::move(manager, target, 0, -1);
	EXPECT_EQ("{A; B; C;}", toString(*res));

	res = transform::move(manager, target, 0, 3);
	EXPECT_EQ("{B; C; A;}", toString(*res));

	// check for deeper scope
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound, stmts[0]));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1, stmts[0]));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::move(manager, target, 0, 0);
	EXPECT_EQ("{{{A; B; C;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 0, 1);
	EXPECT_EQ("{{{B; A; C;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 0, 2);
	EXPECT_EQ("{{{B; C; A;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 1, -1);
	EXPECT_EQ("{{{B; A; C;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 1, 0);
	EXPECT_EQ("{{{A; B; C;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 1, 1);
	EXPECT_EQ("{{{A; C; B;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 2, -2);
	EXPECT_EQ("{{{C; A; B;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 2, -1);
	EXPECT_EQ("{{{A; C; B;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 2, 0);
	EXPECT_EQ("{{{A; B; C;}; A;}; A;}", toString(*res));

	// TEST exceeding displacement
	res = transform::move(manager, target, 0, -1);
	EXPECT_EQ("{{{A; B; C;}; A;}; A;}", toString(*res));

	res = transform::move(manager, target, 0, 3);
	EXPECT_EQ("{{{B; C; A;}; A;}; A;}", toString(*res));

}


TEST(Manipulation, TryFixingParameter_simple) {

	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr type = builder.genericType("T");
	VariablePtr paramA = builder.variable(type, 1);
	VariablePtr paramB = builder.variable(type, 2);

	FunctionTypePtr funType = builder.functionType(toVector(type,type), type);
	LiteralPtr op = builder.literal(funType, "op");
	ExpressionPtr sum = builder.callExpr(op, paramA, paramB);

	StatementPtr body = builder.returnStmt(sum);
	LambdaExprPtr lambda = builder.lambdaExpr(funType, toVector(paramA, paramB), body);

	EXPECT_EQ("return op(v1, v2)", toString(*body));


	// ... now, fix first parameter to be X
	LiteralPtr value = builder.literal(type, "X");
	LambdaExprPtr res = transform::tryFixParameter(manager, lambda, 0, value);
	EXPECT_NE(*res, *lambda);
	EXPECT_EQ("{return op(X, v2);}", toString(*res->getBody()));
	EXPECT_EQ("[]", toString(core::checks::check(res)));

	res = transform::tryFixParameter(manager, lambda, 1, value);
	EXPECT_NE(*res, *lambda);
	EXPECT_EQ("{return op(v1, X);}", toString(*res->getBody()));
	EXPECT_EQ("[]", toString(core::checks::check(res)));

	// nested lambdas
	VariablePtr paramC = builder.variable(type, 3);
	VariablePtr paramD = builder.variable(type, 4);
	StatementPtr body2 = builder.returnStmt(builder.callExpr(lambda, paramC, paramD));
	LambdaExprPtr outer1 = builder.lambdaExpr(funType, toVector(paramC, paramD), body2);

	res = transform::tryFixParameter(manager, outer1, 0, value);
	EXPECT_PRED2(isSubString, "return op(X, v2)", toString(*res));
	EXPECT_EQ("[]", toString(core::checks::check(res)));


	// multiple nested lambdas
	StatementPtr body3 = builder.returnStmt(builder.callExpr(lambda, paramC, builder.callExpr(lambda, paramC, paramD)));
	LambdaExprPtr outer2 = builder.lambdaExpr(funType, toVector(paramC, paramD), body3);

	res = transform::tryFixParameter(manager, outer2, 0, value);
	EXPECT_PRED3(containsSubstring, "return op(X, v2)", toString(*res), 2);
	EXPECT_EQ("[]", toString(core::checks::check(res)));

}

TEST(Manipulation, TryFixParameter_recursive) {

	NodeManager manager;
	IRBuilder builder(manager);

	// build a recursive function call
	LambdaExprPtr lambda = builder.parseExpr(
			"let int = int<4> in "
			"let f = (ref<int> x, int b)->unit {"
			"	if (b == 0) return;"
			"	x = x + b;"
			"	f(x,b-1);"
			"} in f"
			).as<LambdaExprPtr>();

	ASSERT_TRUE(lambda);

	// make sure it is recursive
	EXPECT_TRUE(lambda->isRecursive()) << *lambda;

	// now, try to fix first parameter
	ExpressionPtr globalRef = builder.parseExpr("lit(\"global\":ref<int<4>>)");
	LambdaExprPtr fixed = transform::tryFixParameter(manager, lambda, 0, globalRef);

	// check whether something has changed
	EXPECT_NE(lambda, fixed);
	EXPECT_EQ("AP(rec v1.{v1=fun(int<4> v3) {if(int.eq(v3, 0)) {return unit;} else {}; ref.assign(global, int.add(ref.deref(global), v3)); v1(int.sub(v3, 1));}})", toString(fixed));
	EXPECT_EQ("[]", toString(core::checks::check(fixed)));

	// now, try fixing non-propagated parameter b
	fixed = transform::tryFixParameter(manager, lambda, 1, builder.intLit(5));
	EXPECT_NE(lambda, fixed);
	EXPECT_EQ("AP(rec v5.{v5=fun(ref<int<4>> v2) {if(int.eq(5, 0)) {return unit;} else {}; ref.assign(v2, int.add(ref.deref(v2), 5)); rec v1.{v1=fun(ref<int<4>> v2, int<4> v3) {if(int.eq(v3, 0)) {return unit;} else {}; ref.assign(v2, int.add(ref.deref(v2), v3)); v1(v2, int.sub(v3, 1));}}(v2, int.sub(5, 1));}})", toString(fixed));
	EXPECT_EQ("[]", toString(core::checks::check(fixed)));

}

// TODO: fix when required
//TEST(Manipulation, TryFixParameter_mutual_recursive) {
//
//	NodeManager manager;
//	IRBuilder builder(manager);
//
//	// build a recursive function call
//	LambdaExprPtr lambda = builder.parseExpr(
//			"let int = int<4> in "
//			"let f,g = "
//			"	(ref<int> x, int b)->unit {"
//			"		if (b == 0) return;"
//			"		f(x,b-2);"
//			"		g(b-1,x);"
//			"	},"
//			"	(int b, ref<int> x)->unit {"
//			"		if (b == 0) return;"
//			"		g(b-2,x);"
//			"		f(x,b-1);"
//			"	}"
//			" in f"
//			).as<LambdaExprPtr>();
//
//	ASSERT_TRUE(lambda);
//
//	// now, try to fix first parameter
//	ExpressionPtr globalRef = builder.parseExpr("lit(\"global\":ref<int<4>>)");
//	LambdaExprPtr fixed = transform::tryFixParameter(manager, lambda, 0, globalRef);
//
//	// check whether something has changed
//	EXPECT_NE(lambda, fixed);
//	EXPECT_EQ("AP(rec v1.{v1=fun(int<4> v3) {if(int.eq(v3, 0)) {return unit;} else {}; ref.assign(global, int.add(ref.deref(global), v3)); v1(int.sub(v3, 1));}})", toString(fixed));
//	EXPECT_EQ("[]", toString(core::checks::check(fixed)));
//
//	// now, try fixing non-propagated parameter b
//	fixed = transform::tryFixParameter(manager, lambda, 1, builder.intLit(5));
//	EXPECT_NE(lambda, fixed);
//	EXPECT_EQ("AP(rec v5.{v5=fun(ref<int<4>> v2) {if(int.eq(5, 0)) {return unit;} else {}; ref.assign(v2, int.add(ref.deref(v2), 5)); rec v1.{v1=fun(ref<int<4>> v2, int<4> v3) {if(int.eq(v3, 0)) {return unit;} else {}; ref.assign(v2, int.add(ref.deref(v2), v3)); v1(v2, int.sub(v3, 1));}}(v2, int.sub(5, 1));}})", toString(fixed));
//	EXPECT_EQ("[]", toString(core::checks::check(fixed)));
//
//}



TEST(Manipulation, ExtractLambda) {
	NodeManager man;
	IRBuilder build(man);
	auto& basic = man.getLangBasic();

	StatementList statements;
	VariablePtr out1 = build.variable(basic.getInt4());
	VariablePtr out2 = build.variable(basic.getInt4());
	VariablePtr in1 = build.variable(basic.getInt4());
	VariablePtr inFor = build.variable(basic.getInt4());
	statements.push_back(build.declarationStmt(in1, build.add(out1, out2)));
	statements.push_back(build.forStmt(inFor, out1, in1, out2, build.compoundStmt()));

	CompoundStmtPtr innerStat = build.compoundStmt(statements);

	statements.clear();
	statements.push_back(build.declarationStmt(out1, build.intLit(1)));
	statements.push_back(build.declarationStmt(out2, build.intLit(2)));
	statements.push_back(innerStat);
	
	CompoundStmtPtr wholeStat = build.compoundStmt(statements);

	//LambdaDeltaVisitor ldv;
	//visitDepthFirstPrunable(stat, ldv);
	//
	//insieme::utils::set::PointerSet<VariablePtr> expectedDeclared, expectedUndeclared;
	//expectedDeclared.insert(in1);
	//expectedDeclared.insert(inFor);
	//expectedUndeclared.insert(out1);
	//expectedUndeclared.insert(out2);
	//
	//EXPECT_EQ(expectedDeclared, ldv.declared);
	//EXPECT_EQ(expectedUndeclared, ldv.undeclared);

	StatementAddress addr = CompoundStmtAddress(wholeStat)->getStatement(2);
	BindExprPtr result = transform::extractLambda(man, addr);
	auto expectedBound = toVector<ExpressionPtr>(out1, out2);
	EXPECT_EQ(expectedBound, result->getBoundExpressions());
	
}

TEST(Manipulation, Outline) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto& basic = mgr.getLangBasic();

	StatementList statements;
	VariablePtr out1 = builder.variable(basic.getInt4(),1);
	VariablePtr out2 = builder.variable(basic.getInt4(),2);
	VariablePtr in1 = builder.variable(basic.getInt4(),3);
	VariablePtr inFor = builder.variable(basic.getInt4(),4);
	statements.push_back(builder.declarationStmt(in1, builder.add(out1, out2)));

	ForStmtPtr forStmt = builder.forStmt(inFor, out1, in1, out2, builder.compoundStmt());
	statements.push_back(forStmt);

	CompoundStmtPtr innerStat = builder.compoundStmt(statements);

	statements.clear();
	statements.push_back(builder.declarationStmt(out1, builder.intLit(1)));
	statements.push_back(builder.declarationStmt(out2, builder.intLit(2)));
	statements.push_back(innerStat);

	CompoundStmtPtr wholeStat = builder.compoundStmt(statements);

	EXPECT_EQ(0u, transform::outline(mgr, wholeStat)->getArguments().size());
	EXPECT_EQ(3u, transform::outline(mgr, forStmt)->getArguments().size());

	EXPECT_PRED2(containsSubString, toString(*transform::outline(mgr, forStmt)), "(v1, v2, v3)");
}

TEST(Manipulation, OutlineExpr) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto& basic = mgr.getLangBasic();

	VariablePtr a = builder.variable(basic.getInt4(),1);
	VariablePtr b = builder.variable(basic.getInt4(),2);

	ExpressionPtr sum = builder.add(a,b);

	EXPECT_EQ("int.add(v1, v2)", toString(*sum));
	EXPECT_EQ("rec v5.{v5=fun(int<4> v3, int<4> v4) {return int.add(v3, v4);}}(v1, v2)", toString(*transform::outline(mgr, sum)));

	EXPECT_EQ(sum, transform::tryInlineToExpr(mgr, transform::outline(mgr, sum)));
}

TEST(Manipulation, Inline) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	auto& basic = mgr.getLangBasic();

	StatementList statements;
	VariablePtr out1 = builder.variable(basic.getInt4(),1);
	VariablePtr out2 = builder.variable(basic.getInt4(),2);
	VariablePtr in1 = builder.variable(basic.getInt4(),3);
	VariablePtr inFor = builder.variable(basic.getInt4(),4);
	statements.push_back(builder.declarationStmt(in1, builder.add(out1, out2)));

	ForStmtPtr forStmt = builder.forStmt(inFor, out1, in1, out2, builder.compoundStmt());
	statements.push_back(forStmt);

	CompoundStmtPtr innerStat = builder.compoundStmt(statements);

	statements.clear();
	statements.push_back(builder.declarationStmt(out1, builder.intLit(1)));
	statements.push_back(builder.declarationStmt(out2, builder.intLit(2)));
	statements.push_back(innerStat);

	CompoundStmtPtr wholeStat = builder.compoundStmt(statements);

	EXPECT_NE(wholeStat, transform::outline(mgr, wholeStat));
	EXPECT_EQ(wholeStat, transform::tryInlineToStmt(mgr, transform::outline(mgr, wholeStat)));

}

TEST(Manipulation, InlineFunction) {
	NodeManager mgr;
	IRBuilder builder(mgr);
	auto& basic = mgr.getLangBasic();

	TypePtr type = basic.getInt4();

	// -- create a function --

	VariablePtr p1 = builder.variable(type, 3);
	VariablePtr p2 = builder.variable(type, 4);

	CompoundStmtPtr body = builder.compoundStmt(
			builder.add(p1,p2),
			builder.sub(p2,p1)
	);

	LambdaExprPtr fun = builder.lambdaExpr(body, toVector(p1,p2));

	EXPECT_EQ("rec v1.{v1=fun(int<4> v3, int<4> v4) {int.add(v3, v4); int.sub(v4, v3);}}", toString(*fun));


	// -- create a call --

	VariablePtr v1 = builder.variable(type, 1);
	VariablePtr v2 = builder.variable(type, 2);

	CallExprPtr call = builder.callExpr(fun, v1, builder.add(v1,v2));
	EXPECT_EQ("rec v1.{v1=fun(int<4> v3, int<4> v4) {int.add(v3, v4); int.sub(v4, v3);}}(v1, int.add(v1, v2))", toString(*call));
	EXPECT_EQ("{int<4> v5 = int.add(v1, v2); int.add(v1, v5); int.sub(v5, v1);}", toString(*transform::tryInlineToStmt(mgr, call)));

}


TEST(Manipulation, CorrectRecursiveLambdaVariableUsage) {
	NodeManager manager;
	IRBuilder builder(manager);

	// an easy case
	LambdaExprPtr in = builder.parseExpr(
		"let int = int<4> in "
		"let f = (int a)->int {"
		"	((int)->int g)->unit {"
		"		g(5);"
		"	}(f);"
		"} in f"
	).as<LambdaExprPtr>();

	ASSERT_TRUE(in);

	EXPECT_EQ("rec v1.{v1=fun(int<4> v2) {rec v4.{v4=fun() {v1(5);}}();}}", 
		toString(*transform::correctRecursiveLambdaVariableUsage(manager, in))
	);

	// an advanced case 
	in = builder.parseExpr(
		"let int = int<4> in "
		"let h = ((int)->int f)->int { return f(5); } in "
		"let f,g = "
		"	(int a)->int {"
		"		h(f);"
		"		f(4);"
		"		g(f(4));"
		"		h(g);"
		"	},"
		"	(int a)->int {"
		"		h(f);"
		"		f(g(4));"
		"		g(4);"
		"		h(g);"
		"	}"
		"in g"
	).as<LambdaExprPtr>();

	ASSERT_TRUE(in);

	EXPECT_EQ("rec v13.{v12=fun(int<4> v14) {rec v11.{v11=fun() {return v12(5);}}(); v12(4); v13(v12(4)); rec v11.{v11=fun() {return v13(5);}}();}, v13=fun(int<4> v16) {rec v11.{v11=fun() {return v12(5);}}(); v12(v13(4)); v13(4); rec v11.{v11=fun() {return v13(5);}}();}}", 
		toString(*transform::correctRecursiveLambdaVariableUsage(manager, in))
	);
}

} // end namespace core
} // end namespace insieme

