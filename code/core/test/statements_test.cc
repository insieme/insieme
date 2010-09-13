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
#include "string_utils.h"
#include "ast_builder.h"
#include "lang_basic.h"

using namespace insieme::core;
using namespace insieme::core::lang;

TEST(StatementsTest, Management) {
	NodeManager manager;
	NodeManager manager2;
	
	BreakStmtPtr bS = BreakStmt::get(manager);
	CompoundStmtPtr nS = CompoundStmt::get(manager);
	
	CompoundStmtPtr bSC = CompoundStmt::get(manager, bS);
	CompoundStmtPtr nSC = CompoundStmt::get(manager, nS);

	vector<StatementPtr> stmtVec;
	stmtVec.push_back(bS);
	stmtVec.push_back(nSC);
	stmtVec.push_back(nS);
	stmtVec.push_back(bSC);
	CompoundStmtPtr bSCVec = CompoundStmt::get(manager, stmtVec);

	EXPECT_EQ (5, manager.size());
	EXPECT_EQ (0, manager2.size());

	CompoundStmtPtr bSCVec2 = CompoundStmt::get(manager2, stmtVec);

	EXPECT_EQ (5, manager.size());
	EXPECT_EQ (5, manager2.size());

	// TODO:
//	DepthFirstVisitor<NodePtr> stmt2check([&](const NodePtr& cur) {
//		EXPECT_TRUE(manager2.addressesLocal(cur));
//	});
//	stmt2check.visit(bSCVec2);
	
	EXPECT_FALSE(manager.addressesLocal(bSCVec2));
	EXPECT_TRUE(manager.contains(bSCVec2));
}

TEST(StatementsTest, CreationAndIdentity) {
	NodeManager manager;

	BreakStmtPtr bS = BreakStmt::get(manager);
	EXPECT_EQ(bS, BreakStmt::get(manager));

	CompoundStmtPtr nS = CompoundStmt::get(manager);
	EXPECT_NE(*bS, *nS);
}

TEST(StatementsTest, CompoundStmt) {
	NodeManager stmtMan;
	BreakStmtPtr bS = BreakStmt::get(stmtMan);
	ContinueStmtPtr cS = ContinueStmt::get(stmtMan);
	
	CompoundStmtPtr empty = CompoundStmt::get(stmtMan);
	CompoundStmtPtr bSC = CompoundStmt::get(stmtMan, bS);
	vector<StatementPtr> stmtVec;
	stmtVec.push_back(bS);
	CompoundStmtPtr bSCVec = CompoundStmt::get(stmtMan, stmtVec);
	EXPECT_EQ(bSC , bSCVec);
	EXPECT_EQ(*bSC , *bSCVec);
	stmtVec.push_back(cS);
	CompoundStmtPtr bScSCVec = CompoundStmt::get(stmtMan, stmtVec);
	EXPECT_NE(bSC , bScSCVec);
	EXPECT_NE(bSC->hash() , bScSCVec->hash());
	EXPECT_EQ((*bSC)[0], (*bScSCVec)[0]);
	EXPECT_EQ("{\nbreak;\ncontinue;\n}\n", toString(*bScSCVec));
}

TEST(StatementsTest, Literals) {
	ASTBuilder builder;

	{
		LiteralPtr intLit = builder.literal("-10", lang::TYPE_INT_2_PTR);
		int val = intLit->getValueAs<int>();
		EXPECT_EQ(val, -10);
	}
	{
		LiteralPtr intLit = builder.literal("0x10", lang::TYPE_INT_2_PTR);
		unsigned long val = intLit->getValueAs<unsigned long>();
		EXPECT_EQ(val, 16);
	}
	{
		LiteralPtr intLit = builder.literal("-0x10", lang::TYPE_INT_2_PTR);
		long val = intLit->getValueAs<long>();
		EXPECT_EQ(val, -16);
	}

}

TEST(StatementsTest, DefaultParams) {
	ASTBuilder builder;

	LiteralPtr one = builder.literal("1", TYPE_INT_GEN_PTR);
	DeclarationStmtPtr decl = builder.declarationStmt(TYPE_INT_GEN_PTR, Identifier("bla"), one);
	ForStmtPtr forStmt = builder.forStmt(decl, decl, one, one);
	
	EXPECT_EQ(one, forStmt->getStep());

}
