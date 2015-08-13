/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/utils/stmt_wrapper.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_builder.h"


namespace insieme {
namespace frontend {
namespace stmtutils {

	using namespace insieme::core;

	// Tried to aggregate statements into a compound statement (if more than 1 statement is present)
	StatementPtr tryAggregateStmts(const IRBuilder& builder, const StatementList& stmtVect) {
		if(stmtVect.empty()) { return builder.compoundStmt(); }
		if(stmtVect.size() == 1) { return tryAggregateStmt(builder, stmtVect[0]); }
		return builder.compoundStmt(stmtVect);
	}

	StatementPtr tryAggregateStmt(const IRBuilder& builder, const StatementPtr& stmt) {
		if(stmt.isa<CompoundStmtPtr>()) { return tryAggregateStmts(builder, stmt.as<CompoundStmtPtr>()->getStatements()); }
		return stmt;
	}

	ExpressionPtr makeOperation(const IRBuilder& builder, const ExpressionPtr& lhs, const ExpressionPtr& rhs, const lang::BasicGenerator::Operator& op) {
		return builder.callExpr(lhs->getType(),                                         // return type
		                        builder.getLangBasic().getOperator(lhs->getType(), op), // get the operator
		                        lhs, rhs);                                              // LHS and RHS of the operation
	}

} // end namespace stmtutils
} // end namespace frontend
} // end namespace insieme
