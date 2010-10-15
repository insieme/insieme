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

#pragma once

#include "ast_visitor.h"

namespace insieme {
namespace core {

class ASTBuilder;

namespace transform {

/**
 * Clones the nodes of the IR.
 * Alone this Visitor is pointless, but combined with other visitors can be used to replace/remove/insert nodes to an existing tree.
 */
class NodeCloner: public core::ASTVisitor<NodePtr> {
	const core::ASTBuilder& builder;

public:
	NodeCloner(const core::ASTBuilder& builder);

protected:
	NodePtr visitBreakStmt(const core::BreakStmtPtr& breakStmt) { return breakStmt; }

	NodePtr visitContinueStmt(const core::ContinueStmtPtr& contStmt) { return contStmt; }

	NodePtr visitReturnStmt(const core::ReturnStmtPtr& retStmt);

	NodePtr visitDeclarationStmt(const core::DeclarationStmtPtr& declStmt);

	NodePtr visitCompoundStmt(const core::CompoundStmtPtr& compStmt);

	NodePtr visitWhileStmt(const core::WhileStmtPtr& whileStmt);

	NodePtr visitForStmt(const core::ForStmtPtr& forStmt);

	NodePtr visitIfStmt(const core::IfStmtPtr& declStmt);

	NodePtr visitSwitchStmt(const core::SwitchStmtPtr& declStmt);

	NodePtr visitLiteral(const core::LiteralPtr& lit) { return lit; }

	NodePtr visitVarExpr(const core::VarExprPtr& varExpr) { return varExpr; }

	NodePtr visitParamExpr(const core::ParamExprPtr& paramExpr) { return paramExpr; }

	NodePtr visitLambdaExpr(const core::LambdaExprPtr& lambdaExpr);

	NodePtr visitCallExpr(const core::CallExprPtr& callExpr);

	NodePtr visitCastExpr(const core::CastExprPtr& castExpr);

// TODO:
//	NodePtr visitUnionExpr(const core::UnionExprPtr& unionExpr);
//
//	NodePtr visitStructExpr(const core::StructExprPtr& structExpr);
//
//	NodePtr visitJobExpr(const core::JobExprPtr& jobExpr);
};

} // End transform namespace
} // End core namespace
} // End insieme namespace
