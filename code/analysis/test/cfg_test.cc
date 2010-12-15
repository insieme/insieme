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

#include "insieme/core/program.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/statements.h"
#include "insieme/analysis/cfg.h"

using namespace insieme::core;
using namespace insieme::analysis;

TEST(SimpleBackend, BasicCFG) {

	NodeManager manager;

	LiteralPtr literal = Literal::get(manager, manager.basic.getInt4(), "12");
	DeclarationStmtPtr stmt1 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 1), literal);
	DeclarationStmtPtr stmt2 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 2), literal);
	DeclarationStmtPtr stmt3 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 3), literal);

	CompoundStmtPtr cs0 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt1, stmt2, stmt3));
	CompoundStmtPtr cs1 = CompoundStmt::get(manager, toVector<StatementPtr>(cs0, stmt1));
	CompoundStmtPtr cs2 = CompoundStmt::get(manager, toVector<StatementPtr>(stmt2, cs1));

//	CFG cfg = CFG::buildCFG(cs2);

//	std::cout << cfg;
}

TEST(SimpleBackend, IfStmt) {

	NodeManager manager;
	LiteralPtr literal = Literal::get(manager, manager.basic.getInt4(), "12");
	VariablePtr var = Variable::get(manager, manager.basic.getBool(), 1);
	DeclarationStmtPtr stmt1 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 1), literal);
	DeclarationStmtPtr stmt2 = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 2), literal);

	IfStmtPtr stmt = IfStmt::get(manager, var, stmt1, stmt2);

//	CFG cfg = CFG::buildCFG(stmt);

//	std::cout << cfg;
}
