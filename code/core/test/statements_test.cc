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


TEST(StatementsTest, CreationAndIdentity) {
	TypeManager typeMan;
	StatementManager stmtMan(typeMan);

	BreakStmtPtr bS = BreakStmt::get(stmtMan);
	EXPECT_TRUE (bS == BreakStmt::get(stmtMan));

	NoOpStmtPtr nS = NoOpStmt::get(stmtMan);
	EXPECT_FALSE (*bS == *nS);
}

TEST(StatementsTest, CompoundStmt) {
	TypeManager typeMan;
	StatementManager stmtMan(typeMan);
	BreakStmtPtr bS = BreakStmt::get(stmtMan);
	ContinueStmtPtr cS = ContinueStmt::get(stmtMan);
	
	CompoundStmtPtr empty = CompoundStmt::get(stmtMan);
	CompoundStmtPtr bSC = CompoundStmt::get(stmtMan, bS);
	vector<StmtPtr> stmtVec;
	stmtVec.push_back(bS);
	CompoundStmtPtr bSCVec = CompoundStmt::get(stmtMan, stmtVec);
	EXPECT_TRUE (bSC == bSCVec);
	EXPECT_TRUE (*bSC == *bSCVec);
	stmtVec.push_back(cS);
	CompoundStmtPtr bScSCVec = CompoundStmt::get(stmtMan, stmtVec);
	EXPECT_FALSE (bSC == bScSCVec);
	EXPECT_FALSE (bSC->hash() == bScSCVec->hash());
	EXPECT_TRUE ((*bSC)[0] == (*bScSCVec)[0]);
	EXPECT_TRUE (bScSCVec->toString() == "{\nbreak;\ncontinue;\n}");
}
