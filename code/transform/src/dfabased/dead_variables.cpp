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

#include "insieme/transform/dfabased/dead_variables.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/analysis/dfa/analyses/live_vars.h"
#include "insieme/analysis/dfa/solver.h"

#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace transform {

using namespace insieme::analysis;
using namespace insieme::analysis::dfa;
 
core::NodePtr removeDeadVariables(core::NodeManager& mgr, const core::NodePtr& root) {

	std::map<core::NodeAddress, core::NodePtr> replacements;

	const auto& gen = mgr.getLangBasic();

	// Build the CFG 
	CFGPtr cfg = CFG::buildCFG(root);

	// LOG(INFO) << *cfg;

	Solver<dfa::analyses::LiveVariables> s(*cfg);
	auto&& result = s.solve();

	/* print dataflow solver data */
	//Solver<dfa::analyses::LiveVariables>::printDataflowData(std::cout, result);;
	
	AccessManager aMgr = s.getProblemInstance().getAccessManager();
	
	// LOG(INFO) << aMgr;

	// For each block fo the CFG remove declaration to variables which are dead
	auto blockVisitor = [&] (const cfg::BlockPtr& block) {

		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& stmt) {

			auto stmtAddr = stmt.getStatementAddress();
			bool isAssignment = false;

			if ((stmtAddr->getNodeType() == core::NT_DeclarationStmt && (isAssignment=true)) || 
				(stmtAddr->getNodeType() == core::NT_CallExpr && 
				core::analysis::isCallOf(stmtAddr.as<core::CallExprAddress>().getAddressedNode(), gen.getRefAssign()))) 
			{
				// This block of the CFG refers to a stmt that in the original program is an
				// assignment stmt or a declaration stmt. 
				
				core::NodeAddress addr(stmt.getAnalysisStatement());

				if (isAssignment)
					addr = addr.as<core::DeclarationStmtAddress>()->getVariable();
				else 
					addr = addr.as<core::CallExprAddress>()->getArgument(0);
		
				// LOG(INFO) << *addr;

				auto classPtr = aMgr.findClass(getImmediateAccess(mgr, cfg::Address(block,0,addr)));

				const auto& blockResultRef = result[block->getBlockID()];
				auto fit = blockResultRef.find(classPtr);

				if (fit != blockResultRef.end()) { return; }

				// remove stmt
				replacements.insert( {stmtAddr, core::IRBuilder(mgr).getNoOp() } );

			}
		});
	};

	cfg->visitDFS(blockVisitor);

	if (replacements.empty()) { return root; }

	return core::transform::replaceAll(mgr, replacements);
}

} // end transform namespace 
} // end insieme namespace 
