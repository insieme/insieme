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

#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace frontend {
namespace analysis {

/**
 * Defines the ordering for Variables used within the VarSet set
 */
struct lt_ident {

  bool operator()(const core::VariablePtr& s1, const core::VariablePtr& s2) const {
    return s1->getId() < s2->getId();
  }

};

typedef std::set<core::VariablePtr, lt_ident> VarSet;

/**
 * Returns the list of variables referenced within an expression.
 * This class is used when a code block needs to be transformed into a function
 */
struct VarRefFinder: public core::IRVisitor<void>, public VarSet {

	VarRefFinder(const core::NodePtr& node) : core::IRVisitor<void>(false) {
		visit(node);
		// we have to remove eventual variables which are declared inside this block of code
		VarSet nonDecls;
		lt_ident comp;
		std::set_difference( begin(), end(), declaredVars.begin(), declaredVars.end(), std::inserter(nonDecls, nonDecls.begin()), comp);
		VarSet::operator=(nonDecls);
	}

	void visitVariable(const core::VariablePtr& varExpr) { insert(varExpr); }

	// don't look inside the body of functions
	void visitLambdaExpr(const core::LambdaExprPtr& lambdaExpr) { }

	void visitDeclarationStmt(const core::DeclarationStmtPtr& declStmt) {
		declaredVars.insert( declStmt->getVariable() );
	}

	void visitNode(const core::NodePtr& node) {
		std::for_each(node->getChildList().begin(), node->getChildList().end(),
			[ this ] (core::NodePtr curr){
				this->visit(curr);
			});
	}

private:
	VarSet declaredVars;
};

} // End analysis namespace
} // End frontend namespace
} // End insieme namespace
