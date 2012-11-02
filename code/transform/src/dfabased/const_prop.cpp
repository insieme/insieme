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

#include "insieme/transform/dfabased/const_prop.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/analysis/dfa/analyses/const_prop.h"
#include "insieme/analysis/dfa/solver.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/timer.h"

#include <stack>

namespace insieme {
namespace transform {

using namespace insieme::analysis;
using namespace insieme::analysis::dfa;
 
core::NodePtr doConstProp(core::NodeManager& mgr, const core::NodePtr& root, CFGPtr cfg) {
	
	std::map<core::NodeAddress, core::NodePtr> replacements;

	const auto& gen = mgr.getLangBasic();

	utils::Timer t("Constant.Propagation");
	FinalActions fa( [&](){ t.stop(); LOG(INFO) << t;} );

	utils::Timer tAnalyze("Constant.Propagation.Analysis");
	if (!cfg) {
		// Build the CFG (in the case a valid CFG was not provided by the caller)
		cfg = CFG::buildCFG(root);
	}

	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
	auto&& const_prop_result = s.solve();

	tAnalyze.stop();
	LOG(INFO) << tAnalyze;

	AccessManager aMgr = s.getProblemInstance().getAccessManager();
	
	// For each block fo the CFG apply replace constants 
	auto blockVisitor = [&] (const cfg::BlockPtr& block) {

		// Avoid to handle call blocks // ret blocks 
		//   this is primarly connected with the fact that analysis stmts in the call block may 
		//   be erroneusly translated into IR addresses because of pending issues in the CFG
		//   generation: FIXME
		if ( dynamic_cast<const cfg::CallBlock*>(block.get()) || 
			 dynamic_cast<const cfg::RetBlock*>(block.get()) ) { return; }

		size_t stmt_idx = 0;

		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& stmt) {
	
			// Incremebet the stmt_idx at the end of the loopbody
			FinalActions fa( [&](){ ++stmt_idx;} );

			auto stmtPtr = stmt.getAnalysisStatement();

			// Early exit, avoid to waste cycles for extracting accesses that will not be used 
			if (const_prop_result[block->getBlockID()].empty()) {
				return ;
			}

			// Extract accesses within this statement 
			auto accesses = getAccesses(
							mgr, 
							UnifiedAddress(cfg::Address(block,stmt_idx,core::NodeAddress(stmtPtr))), 
							cfg->getTmpVarMap()
						);

			for (const auto& cur : const_prop_result[block->getBlockID()]) {

				// if the addressed access is a constant, then store the replacement 
				if (std::get<1>(cur).isValue()) {
	
					// get corresponding classes 
					std::vector<AccessClassSet> classes;
					std::transform(accesses.begin(), accesses.end(), std::back_inserter(classes), 
						[&](const AccessPtr& cur) { return aMgr.getClassFor(cur); });
					
					auto accClass = std::get<0>(cur);

					// find all accesses in this statement 
					auto fit = std::find_if(classes.begin(), classes.end(), [&](const AccessClassSet& cur) {
							return cur.find(accClass) != cur.end();
						});

					if (fit != classes.end()) {

						size_t idx = std::distance(classes.begin(), fit);
						if (core::NodeAddress addr = accesses[idx]->getAddress().getAbsoluteAddress(cfg->getTmpVarMap())) 
						{
	//						LOG(INFO) << addr << " " << *addr;

							// Avoid ref-variables to be sobstituted by constants 
							if (core::analysis::isRefType(addr.as<core::ExpressionAddress>()->getType())) 
								continue;

							// if the value we want to replace is the left hand side of an
							// assignment stmt, or the variable in a declaration stmt
							// then do not perform the assignemnt 
							core::NodeAddress parent = addr.getParentAddress();
							if (parent->getNodeType() == core::NT_DeclarationStmt && 
								parent.as<core::DeclarationStmtAddress>()->getVariable() == addr)
							{	
								continue;
							}
					
							if (parent->getNodeType() == core::NT_CallExpr && 
								core::analysis::isCallOf(parent.getAddressedNode(), gen.getRefAssign()) &&
								parent.as<core::CallExprAddress>()->getArgument(0) == addr)
							{	
								continue;
							}

							// FIXME: this doesn't work for context sensitive analysis 
							if (addr.getRootNode() != root) {
								// we are inside a function and we have to find the outermost lambda
								auto callExprAddr = core::Address<const core::Node>::find(addr.getRootNode(), root);
								addr = core::concat( callExprAddr, addr );
							}

							// do replace 
							replacements.insert( {addr, std::get<1>(cur).value()} );

						}
					}
				}
			}

		});
	};

	cfg->visitDFS(blockVisitor);

	if (replacements.empty()) { return root; }

//	LOG(INFO) << "Replacements :" << 
//		join("\n",replacements,[&](std::ostream& jout, const std::map<core::NodeAddress, core::NodePtr>::value_type& val){ jout << *val.first << " -> " << *val.second; });
//
	
	LOG(INFO) << "**** Constant propagation: replacing '" << replacements.size() << "' constant(s)";

	return core::transform::replaceAll(mgr, replacements);
}

} // end transfrom namespace 
} // end insieme namespace 
