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

#include "transform/node_cloner.h"

#include "ast_builder.h"
#include "expressions.h"
#include "statements.h"

// Utility macros
#define STMT_REF(wrapper)  \
		core::dynamic_pointer_cast<const core::Statement>((wrapper).ref)

#define EXPR_REF(wrapper)  \
	core::dynamic_pointer_cast<const core::Expression>((wrapper).ref)

#define REF(wrapper)  ((wrapper).ref)

namespace insieme {
namespace core {
namespace transform {

NodeCloner::NodeCloner(const core::ASTBuilder& builder): builder(builder) { }


/////------------------ leaf nodes -----------------------------------------------------------------------------------------------------------------------------
NodeWrapper NodeCloner::visitBreakStmt(const core::BreakStmtPtr& breakStmt) {
	return NodeWrapper( breakStmt );
}

NodeWrapper NodeCloner::visitContinueStmt(const core::ContinueStmtPtr& contStmt) {
	return NodeWrapper( contStmt );
}

NodeWrapper NodeCloner::visitLiteral(const core::LiteralPtr& lit) {
	return NodeWrapper( lit );
}

NodeWrapper NodeCloner::visitVarExpr(const core::VarExprPtr& varExpr) {
	return NodeWrapper( varExpr );
}

NodeWrapper NodeCloner::visitParamExpr(const core::ParamExprPtr& paramExpr) {
	return NodeWrapper( paramExpr );
}

/////---------------- non leaf nodes ---------------------------------------------------------------------------------------------------------------------------

NodeWrapper NodeCloner::visitReturnStmt(const core::ReturnStmtPtr& retStmt) {
	return NodeWrapper( builder.returnStmt(EXPR_REF( visit(retStmt->getReturnExpr()) )) );
}

NodeWrapper NodeCloner::visitDeclarationStmt(const core::DeclarationStmtPtr& declStmt) {
	return NodeWrapper(
		builder.declarationStmt(
				declStmt->getVarExpression()->getType(),
				core::dynamic_pointer_cast<const core::VarExpr>(visit(declStmt->getVarExpression()).ref)->getIdentifier(),
				EXPR_REF( visit(declStmt->getInitialization()) )
		)
	);
}

NodeWrapper NodeCloner::visitCompoundStmt(const core::CompoundStmtPtr& compStmt) {
	vector<core::StatementPtr> stmts;
	std::for_each(compStmt->getChildList().begin(), compStmt->getChildList().end(),
		[ this, &stmts ](core::NodePtr curr){
			stmts.push_back(core::dynamic_pointer_cast<const core::Statement>(this->visit(curr).ref));
		});
	return NodeWrapper( builder.compoundStmt(stmts) );
}

NodeWrapper NodeCloner::visitWhileStmt(const core::WhileStmtPtr& whileStmt) {
	return NodeWrapper( builder.whileStmt(
			EXPR_REF( visit(whileStmt->getCondition()) ),
			STMT_REF( visit(whileStmt->getBody()) )
		)
	);
}

NodeWrapper NodeCloner::visitForStmt(const core::ForStmtPtr& forStmt) {
	return NodeWrapper( builder.forStmt(
			core::dynamic_pointer_cast<const core::DeclarationStmt>(REF( visit(forStmt->getDeclaration()) )),
			STMT_REF( visit(forStmt->getBody()) ),
			EXPR_REF( visit(forStmt->getEnd()) ),
			EXPR_REF( visit(forStmt->getStep()) ) )
	);
}

NodeWrapper NodeCloner::visitIfStmt(const core::IfStmtPtr& ifStmt) {
	return NodeWrapper( builder.ifStmt(
			EXPR_REF( visit(ifStmt->getCondition()) ),
			STMT_REF( visit(ifStmt->getThenBody()) ),
			STMT_REF( visit(ifStmt->getElseBody()) )
		)
	);
}

NodeWrapper NodeCloner::visitSwitchStmt(const core::SwitchStmtPtr& switchStmt) {
	std::vector<core::SwitchStmt::Case> cases;
	std::for_each(switchStmt->getCases().begin(), switchStmt->getCases().end(),
		[ this, &cases ](const core::SwitchStmt::Case& curr){
			cases.push_back(
				core::SwitchStmt::Case( EXPR_REF( this->visit(curr.first) ), STMT_REF( this->visit(curr.second) )) );
		});

	return NodeWrapper( builder.switchStmt(
			EXPR_REF( visit(switchStmt->getSwitchExpr()) ),
			cases,
			STMT_REF( visit(switchStmt->getDefaultCase()) )
		)
	);
}

NodeWrapper NodeCloner::visitCallExpr(const core::CallExprPtr& callExpr) {
	vector<core::ExpressionPtr> args;

	std::for_each(callExpr->getArguments().begin(), callExpr->getArguments().end(),
		[ this, &args ](const core::ExpressionPtr& curr){
			args.push_back(EXPR_REF( this->visit(curr) ));
		});

	return NodeWrapper(builder.callExpr(callExpr->getType(), EXPR_REF( visit(callExpr->getFunctionExpr()) ), args));
}

NodeWrapper NodeCloner::visitLambdaExpr(const core::LambdaExprPtr& lambdaExpr) {

	LambdaExpr::ParamList params;
	std::for_each(lambdaExpr->getParams().begin(), lambdaExpr->getParams().end(),
		[ this, &params ](const core::ParamExprPtr& curr){
			params.push_back(
				dynamic_pointer_cast<const ParamExpr>( this->visit(curr).ref )
			);
		});

	StatementPtr body = STMT_REF( visit(lambdaExpr->getBody()) );

	return NodeWrapper(	builder.lambdaExpr(lambdaExpr->getType(), params, body) );

}

NodeWrapper NodeCloner::visitCastExpr(const core::CastExprPtr& castExpr) {
	return NodeWrapper(
		builder.castExpr(castExpr->getType(),
		EXPR_REF( visit(castExpr->getSubExpression())) )
	);
}

// TODO:
//NodeWrapper ReplaceNodeVisitor::visitUnionExpr(const core::UnionExprPtr& unionExpr) {
//
//	return NodeWrapper();
//}
//
//NodeWrapper ReplaceNodeVisitor::visitStructExpr(const core::StructExprPtr& structExpr) {
//	return NodeWrapper();
//}
//
//NodeWrapper ReplaceNodeVisitor::visitJobExpr(const core::JobExprPtr& jobExpr) {
//	return NodeWrapper();
//}

} // End transform namespace
} // End core namespace
} // End insieme namespace
