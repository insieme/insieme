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

#include "insieme/analysis/cfg.h"
#include "insieme/core/ast_visitor.h"

#include <stack>

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::cfg;

namespace {

struct CFGBuilder: public ASTVisitor<> {

	CFG& cfg;
	CFGElement curr;
	std::stack<StatementPtr> scopeStack;

	CFGBuilder(CFG& cfg) : cfg(cfg) { }

	void visitIfStmt(const IfStmtPtr& ifStmt) {
		return CFGElement(ifStmt);
	}

	void visitCompoundStmt(const CompoundStmtPtr& compStmt) {

		for_each(compStmt->getStatements().begin(), compStmt->getStatements().end(),
			[ this ](const StatementPtr& curr){
				this->curr.setStatement(curr);
				this->visit(curr);
				CFG::VertexTy currVert = this->cfg.addCFGNode(  );
			}
		);
		return CFGElement( compStmt->getStatements().back() );
	}

	cfg::CFGElement visitStatement(const StatementPtr& stmt) {
		return CFGElement(stmt);
	}

};


} // end anonymous namespace

namespace insieme {
namespace analysis {

CFG CFG::buildCFG(const StatementPtr& rootNode) {
	CFG cfg;
	CFGBuilder builder(cfg);
	builder.visit(rootNode);
	return cfg;
}

}
}
