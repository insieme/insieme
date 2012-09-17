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

#include "insieme/analysis/dfa/analyses/const_prop.h"
#include "insieme/analysis/dfa/solver.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace transform {

using namespace insieme::analysis;
using namespace insieme::analysis::dfa;
 
core::NodePtr doConstantPropagation(const core::NodePtr& root) {
//	
//	//auto order = [&] (const core::NodeAddress& lhs, const core::NodeAddress& rhs) -> bool { return lhs.getPath() > rhs.getPath(); };
//	std::map<core::NodeAddress, core::NodePtr> replacements;
//
//	core::NodeManager& mgr = root->getNodeManager();
//
//	// Build the CFG 
//	CFGPtr cfg = CFG::buildCFG(root);
//
//
//	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
//	auto&& analysis = s.solve();
//	
//	// For each block fo the CFG apply replace constants 
//	
//	auto blockVisitor = [&] (const cfg::BlockPtr& block) {
//
//		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& stmt) {
//
//			LOG(INFO) << *((*block)[0].getStatementAddress());
//			LOG(INFO) << block->getBlockID() << " -> " << analysis[block->getBlockID()];
//
//			for (const auto& cur : analysis[block->getBlockID()]) {
//			
//				// LOG(INFO) << stmt.getStatementAddress() << " " 
//				// 		  << std::get<0>(cur).getAccessExpression();
//
//				// if the addressed access is a constant, then store the replacement 
//				if (std::get<1>(cur).isValue()) {
//					
//					core::ExpressionAddress expr = std::get<0>(cur).getAccessExpression();
//					if (expr->getNodeType() == core::NT_Variable) {
//						// this may be a temp
//						core::ExpressionAddress addr = cfg->getTmpVarMap().getMappedExpr( expr.as<core::VariableAddress>().getAddressedNode() );
//						if (addr) { expr = addr; }
//						LOG(INFO) << *expr;
//					}
//
//					core::NodeAddress rep = core::Address<const core::Expression>::find(expr, stmt.getStatementAddress());
//					if (rep) { 
//						
//						core::StatementPtr stmtPtr = stmt.getStatementAddress().getAddressedNode();
//
//						LOG(INFO) << stmtPtr; 
//						// if this is a decl stmt then we have to exclude any replacement of the
//						// declared variable
//						if ( (stmtPtr->getNodeType() == core::NT_DeclarationStmt) && 
//						 	 (*rep.getAddressedNode() == *stmtPtr.as<core::DeclarationStmtPtr>()->getVariable()) ) 
//							continue;
//	
//						// check whether this is an assignment operation 
//						if (stmtPtr->getNodeType() == core::NT_CallExpr && 
//							core::analysis::isCallOf(stmtPtr.as<core::ExpressionPtr>(), mgr.getLangBasic().getRefAssign()) && 
//							(*rep.getAddressedNode() == *stmtPtr.as<core::CallExprPtr>().getArgument(0))) 
//							continue;
//
//						//if ( core::analysis::isRefType(rep.as<core::ExpressionAddress>()->getType()) ) {
//							// assumes the parent operation is a deref
//						// 	rep = rep.getParentAddress(1);
//						//}
//
//						LOG(INFO) << " TRG : " << rep << " " << *rep;
//						core::NodeAddress repAddr = core::concat<const core::Node>(stmt.getStatementAddress(), rep);
//						LOG(INFO) << repAddr;
//
//						auto fit = replacements.find(repAddr);
//						if(fit == replacements.end()) {
//							LOG(INFO) << "~~~~~Apply replacement: " << cur << " " << repAddr << " " << *repAddr; 
//							replacements.insert( { repAddr, std::get<1>(cur).value() } );
//						}
//					}
//
//				}
//			}
//
//		});
//	};
//
//	cfg->visitDFS(blockVisitor);
//
//	if (replacements.empty()) { return root; }
//
//	// get rid of redundant
//	LOG(DEBUG) << "PROPAGATING: " << replacements.size() << " constnts";
//	LOG(DEBUG) << replacements;
//
//	return core::transform::replaceAll(mgr, replacements);
	return root;
}

} // end transfrom namespace 
} // end insieme namespace 
