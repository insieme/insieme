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

#include "frontend/analysis/loop_analyzer.h"

#include "frontend/conversion.h"
#include "core/lang_basic.h"

// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <clang/AST/Expr.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/StmtVisitor.h>

using namespace clang;

namespace {

/**
 * Returns the list of variables referenced within an expression
 */
struct VarRefFinder: public StmtVisitor<VarRefFinder>, insieme::frontend::analysis::LoopAnalyzer::VarDeclSet {

	VarRefFinder(const Expr* expr) {
		VisitStmt( const_cast<Expr*>(expr) );
	}

	void VisitDeclRefExpr(DeclRefExpr *declRef) {
		if(VarDecl* varDec = dyn_cast<VarDecl>(declRef->getDecl())) {
			insert(varDec);
		}
	}

	void VisitStmt(Stmt* stmt) {
		std::for_each(stmt->child_begin(), stmt->child_end(),
			[ this ](clang::Stmt* curr) { if(curr) this->Visit(curr); }
		);
	}
};

}

namespace insieme {
namespace frontend {
namespace analysis {

	//TODO: Recheck: Visual Studio 2010 fix: loopHelper( { NULL, NULL, NULL } ) do not work
	LoopAnalyzer::LoopAnalyzer(const clang::ForStmt* forStmt, const ConversionFactory& convFact): convFact(convFact), loopHelper(LoopHelper()) {
	// we look for the induction variable
	findInductionVariable(forStmt);
	// we know the induction variable, we analyze the increment expression
	handleIncrExpr(forStmt);
	// we look for the condition expression
	handleCondExpr(forStmt);
}

void LoopAnalyzer::findInductionVariable(const clang::ForStmt* forStmt) {
	// an induction variable of a loop should appear in both the condition and increment expressions
	VarDeclSet&& incExprVars = VarRefFinder(forStmt->getInc());
	VarDeclSet&& condExprVars = VarRefFinder(forStmt->getCond());

	// do an intersection
	VarDeclSet commonVars;
	std::set_intersection(incExprVars.begin(), incExprVars.end(), condExprVars.begin(), condExprVars.end(),
			std::inserter(commonVars, commonVars.begin())
	);

	if( commonVars.size() == 1 ) {
		// only 1 variable is common among the increment expression and the condition expression,
		// we can be 100% sure this is the induction variable of the loop
		loopHelper.inductionVar = *commonVars.begin();
		return;
	}
	// loop not in normal form!
	// TODO: handle border cases here

	// if we cannot still determine the induction variable, throw an exception
	throw InductionVariableNotFoundException();
}

void LoopAnalyzer::handleIncrExpr(const clang::ForStmt* forStmt) {
	assert(loopHelper.inductionVar && "Loop induction variable not found, impossible to handle increment expression.");

	if( const UnaryOperator* unOp = dyn_cast<const UnaryOperator>(forStmt->getInc()) ) {
		switch(unOp->getOpcode()) {
		case UO_PreInc:
		case UO_PostInc:
			loopHelper.incrExpr = convFact.getASTBuilder().literal("1", core::lang::TYPE_INT_GEN);
			return;
		case UO_PreDec:
		case UO_PostDec:
			loopHelper.incrExpr = convFact.getASTBuilder().literal("-1", core::lang::TYPE_INT_GEN);
			return;
		default:
			assert(false && "UnaryOperator differet from post/pre inc/dec (++/--) not supported in loop increment expression");
		}
	}

	if( const BinaryOperator* binOp = dyn_cast<const BinaryOperator>(forStmt->getInc()) ) {
		switch(binOp->getOpcode()) {
		case BO_AddAssign:
		case BO_SubAssign: {
			assert(isa<const DeclRefExpr>(binOp->getLHS()));
			const DeclRefExpr* lhs = dyn_cast<const DeclRefExpr>(binOp->getLHS());
			assert(lhs->getDecl() == loopHelper.inductionVar);
			loopHelper.incrExpr = convFact.convertExpr( binOp->getRHS() );
			break;
		}
		default:
			// the increment operation cannot be translated into a normal form loop
			throw LoopNormalizationError();
			// assert(false && "BinaryOperator not supported in increment expression");
		}
	}
}

void LoopAnalyzer::handleCondExpr(const clang::ForStmt* forStmt) {
	// analyze the condition expression
	const Expr* cond = forStmt->getCond();
	if(!cond)
		throw LoopNormalizationError();

	if( const BinaryOperator* binOp = dyn_cast<const BinaryOperator>(cond) ) {
		assert(isa<const DeclRefExpr>(binOp->getLHS()));
		const DeclRefExpr* lhs = dyn_cast<const DeclRefExpr>(binOp->getLHS());
		assert(lhs->getDecl() == loopHelper.inductionVar);
		loopHelper.condExpr = convFact.convertExpr( binOp->getRHS() );
		return;
	}
	throw LoopNormalizationError();
}

} // End analysis namespace
} // End froentend namespace
} // End insieme namespace
