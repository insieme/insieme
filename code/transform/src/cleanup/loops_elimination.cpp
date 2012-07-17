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

namespace {

NodeAddress getOutermostSCoP(const NodeAddress& addr) {
	NodeAddress parent = addr, prev;
	
	do {
		prev = parent;
	} while( (parent = parent.getParentAddress(1)) && 
			 (parent->hasAnnotation( scop::ScopRegion::KEY )) &&
			 (parent->getNodeType() != NT_LambdaExpr)
		   );
	assert(prev && prev->hasAnnotation( scop::ScopRegion::KEY ));
	return prev;
}

} // end anonymous namespace 

// remove dead code 
core::NodePtr loopElimination(const core::NodePtr& node) {
	auto& mgr = node->getNodeManager();

	// run the ScoP analysis to determine SCoPs 
	auto scopsList = scop::mark(node);
	
	if (scopsList.empty()) return node;

	utils::map::PointerMap<NodePtr, NodePtr> replacements;

	// search for the property
	visitDepthFirstOnce(NodeAddress(node), makeLambdaVisitor([&](const ForStmtAddress& forStmt) {
	
		if (!forStmt->hasAnnotation( scop::ScopRegion::KEY )) { return; }

		NodeAddress outermostScop = getOutermostSCoP(forStmt);
		Scop scop = *scop::ScopRegion::toScop( outermostScop );
		
		if (replacements.find(outermostScop) != replacements.end()) { return; }

		LOG(DEBUG) << "replacing " << *outermostScop << " with " << scop.toIR(mgr);
		replacements.insert( { outermostScop, scop.toIR(mgr) } );
		
		/*std::set<int64_t> card;*/
		//// look for the forStmt inside the Scop object
		//for(StmtPtr stmt : scop.getStmts()) {
			//if (isChildOf(forStmt, stmt->getAddr()) ) {
				//auto pw = cardinality(mgr, stmt->getDomain());
				//if (pw.size() == 1) {
					//auto piece = *pw.begin();
					//if (piece.first->isTrue()) {
						//try {
							//card.insert(asConstant( piece.second ));
						//} catch (...) { return; }
					//}
				//}
			//}
		//}

		//if (card.size() == 1) {
			//// all stmts with the same cardinality, we apply transformation 
			//if (*card.begin() == 0) {
				//// remove the for
				//replacements.insert( { forStmt, IRBuilder(mgr).getNoOp() } );
				//return ;
			//}
			//if (*card.begin() == 1) {
				//replacements.insert( { forStmt, forStmt->getBody().getAddressedNode() } );
			//}
		/*}*/


	}, true));

	LOG(INFO) << "**** DeadBrancheElimination: Eliminated '" 
			  << replacements.size() << "' dead branch(es)"; 

	if (replacements.empty()) { return node; }

	return core::transform::replaceAll(mgr, node, replacements);
}

} // end transform namespace 
} // end insieme namespace 
