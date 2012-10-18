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

namespace insieme {
namespace transform {

using namespace insieme::analysis;
using namespace insieme::analysis::dfa;
 
core::NodePtr doConstProp(core::NodeManager& mgr, const core::NodePtr& root) {
	
	std::map<core::NodeAddress, core::NodePtr> replacements;

	const auto& gen = mgr.getLangBasic();

	// Build the CFG 
	CFGPtr cfg = CFG::buildCFG(root);

	Solver<dfa::analyses::ConstantPropagation> s(*cfg);
	auto&& const_prop_result = s.solve();

	AccessManager aMgr = s.getProblemInstance().getAccessManager();
	
	// For each block fo the CFG apply replace constants 
	
	auto blockVisitor = [&] (const cfg::BlockPtr& block) {

		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& stmt) {

			auto stmtPtr = stmt.getAnalysisStatement();
	//		LOG(INFO) << *stmtPtr;

			for (const auto& cur : const_prop_result[block->getBlockID()]) {

				// Extract accesses within this statement 
				auto accesses = getAccesses(
									mgr, 
									UnifiedAddress(cfg::Address(block,0,core::NodeAddress(stmtPtr))), 
									cfg->getTmpVarMap()
								);

				// get corresponding classes 
				std::vector<AccessClassPtr> classes;
				std::transform(accesses.begin(), accesses.end(), std::back_inserter(classes), 
					[&](const AccessPtr& cur) { return aMgr.getClassFor(cur); });

				// if the addressed access is a constant, then store the replacement 
				if (std::get<1>(cur).isValue()) {
					
					auto accClass = std::get<0>(cur);

					// find all accesses in this statement 
					auto fit = std::find(classes.begin(), classes.end(), accClass);

					if (fit != classes.end()) {

						size_t idx = std::distance(classes.begin(), fit);
						if(core::NodeAddress addr = accesses[idx]->getAddress().getAbsoluteAddress(cfg->getTmpVarMap())) {

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

	return core::transform::replaceAll(mgr, replacements);
}

} // end transfrom namespace 
} // end insieme namespace 
