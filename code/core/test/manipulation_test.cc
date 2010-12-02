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

#include "insieme/core/ast_builder.h"
#include "insieme/core/transform/manipulation.h"

namespace insieme {
namespace core {

TEST(Manipulation, Insert) {
	NodeManager manager;
	ASTBuilder builder(manager);

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
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::insert(manager, target, stmt, 0);
	EXPECT_EQ("{{{X; A; B; C;};};}", toString(*res));

	res = transform::insert(manager, target, stmt, 1);
	EXPECT_EQ("{{{A; X; B; C;};};}", toString(*res));

	res = transform::insert(manager, target, stmt, 2);
	EXPECT_EQ("{{{A; B; X; C;};};}", toString(*res));

	res = transform::insert(manager, target, stmt, 3);
	EXPECT_EQ("{{{A; B; C; X;};};}", toString(*res));

	// TEST exceeding indices
	res = transform::insert(manager, target, stmt, 15);
	EXPECT_EQ("{{{A; B; C; X;};};}", toString(*res));
}

TEST(Manipulation, InsertBefore) {
	NodeManager manager;
	ASTBuilder builder(manager);

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
	res = transform::insertBefore(manager, target, stmt, aLit);
	EXPECT_EQ("{X; A; B; C;}", toString(*res));

	res = transform::insertBefore(manager, target, stmt, bLit);
	EXPECT_EQ("{A; X; B; C;}", toString(*res));

	res = transform::insertBefore(manager, target, stmt, cLit);
	EXPECT_EQ("{A; B; X; C;}", toString(*res));
}


TEST(Manipulation, InsertAfter) {
	NodeManager manager;
	ASTBuilder builder(manager);

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
	res = transform::insertAfter(manager, target, stmt, aLit);
	EXPECT_EQ("{A; X; B; C;}", toString(*res));

	res = transform::insertAfter(manager, target, stmt, bLit);
	EXPECT_EQ("{A; B; X; C;}", toString(*res));

	res = transform::insertAfter(manager, target, stmt, cLit);
	EXPECT_EQ("{A; B; C; X;}", toString(*res));
}

TEST(Manipulation, Remove) {
	NodeManager manager;
	ASTBuilder builder(manager);

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
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::replace(manager, target, 0, stmt);
	EXPECT_EQ("{{{X; B; C;};};}", toString(*res));

	res = transform::replace(manager, target, 1, stmt);
	EXPECT_EQ("{{{A; X; C;};};}", toString(*res));

	res = transform::replace(manager, target, 2, stmt);
	EXPECT_EQ("{{{A; B; X;};};}", toString(*res));

}

TEST(Manipulation, Replace) {
	NodeManager manager;
	ASTBuilder builder(manager);

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
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::remove(manager, target, 0);
	EXPECT_EQ("{{{B; C;};};}", toString(*res));

	res = transform::remove(manager, target, 1);
	EXPECT_EQ("{{{A; C;};};}", toString(*res));

	res = transform::remove(manager, target, 2);
	EXPECT_EQ("{{{A; B;};};}", toString(*res));

}

TEST(Manipulation, Move) {
	NodeManager manager;
	ASTBuilder builder(manager);

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
	CompoundStmtPtr outer1 = builder.compoundStmt(toVector<StatementPtr>(compound));
	CompoundStmtPtr outer2 = builder.compoundStmt(toVector<StatementPtr>(outer1));
	target = static_address_cast<const CompoundStmt>(NodeAddress(outer2).getAddressOfChild(0).getAddressOfChild(0));

	res = transform::move(manager, target, 0, 0);
	EXPECT_EQ("{{{A; B; C;};};}", toString(*res));

	res = transform::move(manager, target, 0, 1);
	EXPECT_EQ("{{{B; A; C;};};}", toString(*res));

	res = transform::move(manager, target, 0, 2);
	EXPECT_EQ("{{{B; C; A;};};}", toString(*res));

	res = transform::move(manager, target, 1, -1);
	EXPECT_EQ("{{{B; A; C;};};}", toString(*res));

	res = transform::move(manager, target, 1, 0);
	EXPECT_EQ("{{{A; B; C;};};}", toString(*res));

	res = transform::move(manager, target, 1, 1);
	EXPECT_EQ("{{{A; C; B;};};}", toString(*res));

	res = transform::move(manager, target, 2, -2);
	EXPECT_EQ("{{{C; A; B;};};}", toString(*res));

	res = transform::move(manager, target, 2, -1);
	EXPECT_EQ("{{{A; C; B;};};}", toString(*res));

	res = transform::move(manager, target, 2, 0);
	EXPECT_EQ("{{{A; B; C;};};}", toString(*res));

	// TEST exceeding displacement
	res = transform::move(manager, target, 0, -1);
	EXPECT_EQ("{{{A; B; C;};};}", toString(*res));

	res = transform::move(manager, target, 0, 3);
	EXPECT_EQ("{{{B; C; A;};};}", toString(*res));

}


} // end namespace core
} // end namespace insieme

