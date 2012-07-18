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

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/iter_dom.h"
#include "insieme/analysis/polyhedral/iter_vec.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace transform {

using namespace core;
using namespace analysis::polyhedral;

// remove dead code 
core::NodePtr deadBranchElimination(const core::NodePtr& node) {
	auto& mgr = node->getNodeManager();

	utils::map::PointerMap<NodePtr, NodePtr> replacements;

	// search for the property
	visitDepthFirstOnce(node, makeLambdaVisitor([&](const StatementPtr& stmt){
	
		IterationVector iv;

		if (stmt->getNodeType() != NT_IfStmt && stmt->getNodeType() != NT_WhileStmt) 
			return;

		ExpressionPtr cond = (stmt->getNodeType() == NT_IfStmt) ?
			stmt.as<IfStmtPtr>()->getCondition() : 
			stmt.as<WhileStmtPtr>()->getCondition();

		try {
			auto iterDom = extractFromCondition(iv, cond);

			if (iterDom.universe() && stmt->getNodeType() == NT_IfStmt) {
				replacements.insert( { stmt, stmt.as<IfStmtPtr>()->getThenBody() } );
				return;
			} 
			if (iterDom.empty()) {

				if ( (stmt->getNodeType() == NT_IfStmt && !stmt.as<IfStmtPtr>()->getElseBody()) || 
					 (stmt->getNodeType() == NT_WhileStmt)
				)  replacements.insert( { stmt, IRBuilder(mgr).getNoOp() } );

				else if (stmt->getNodeType() == NT_IfStmt)
					replacements.insert( { stmt, stmt.as<IfStmtPtr>()->getElseBody() } );
				
			}
		} catch(scop::NotASCoP e) { }

	}, true));

	LOG(INFO) << "**** DeadBrancheElimination: Eliminated '" 
			  << replacements.size() << "' dead branch(es)"; 

	if (replacements.empty()) { return node; }

	return core::transform::replaceAll(mgr, node, replacements);
}

} // end transform namespace 
} // end insieme namespace 
