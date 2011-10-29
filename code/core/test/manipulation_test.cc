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
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/checks/ir_checks.h"

#include "insieme/core/printer/pretty_printer.h"

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

} // end namespace core
} // end namespace insieme

