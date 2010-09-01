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

#include <vector>

#include <gtest/gtest.h>

#include "ast_builder.h"

using namespace insieme::core;

TEST(ASTBuilder, Basic) {

	NodeManager manager;

	// With Builder
	ASTBuilder build(manager);
	std::vector<StatementPtr> statements;
	statements.push_back(build.breakStmt());
	statements.push_back(build.declarationStmt(build.boolType(), "test", build.boolLiteral(true)));
	auto compound = build.compoundStmt(statements);

	// Without Builder
	std::vector<StatementPtr> statements2;
	statements2.push_back(BreakStmt::get(manager));
	statements2.push_back(DeclarationStmt::get(manager, BoolType::get(manager), "test", BoolLiteral::get(manager, true)));
	auto compound2 = CompoundStmt::get(manager, statements2);

	EXPECT_EQ(compound2, compound);
}

