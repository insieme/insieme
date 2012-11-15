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

#include "insieme/analysis/dfa/analyses/live_vars.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace analysis {
namespace dfa {
namespace analyses {

typedef typename LiveVariables::value_type value_type;

/**
 * LiveVariables Problem
 */
 
value_type LiveVariables::meet(const value_type& lhs, const value_type& rhs) const {
	value_type ret;
	std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(ret,ret.begin()));
	return ret;
}


std::pair<value_type, value_type> LiveVariables::transfer_func(const value_type& in, const cfg::BlockPtr& block) const {
	
	value_type gen, kill;

	if (block->empty()) { return {gen,kill}; }

	const AccessManager& aMgr = getAccessManager();

	size_t stmt_idx = 0;
	for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {

		core::StatementPtr 		stmtPtr  = cur.getAnalysisStatement();
		core::StatementAddress	stmt = core::StatementAddress(stmtPtr);

		core::NodeManager& mgr = stmt->getNodeManager();
		
		// Makes sure that whatever exit path is taken in the body of this lambda, the stmt_idx
		// is going to be updated correctly 
		FinalActions fa([&](){ ++stmt_idx; });

		auto handle_rhs = [&](const core::ExpressionAddress& expr) {

			auto accesses = getAccesses(mgr, 
				UnifiedAddress(cfg::Address(block,stmt_idx,expr)), 
				getCFG().getTmpVarMap()
			);

			// for each usage of a variable add it to the gen set 
			for (const auto& acc : accesses) { 
				auto liveClasses = aMgr.findClass(acc);
				std::copy(liveClasses.begin(), liveClasses.end(), std::inserter(gen, gen.begin()));
			}
		};


		auto handle_def = [&](const core::ExpressionAddress& lhs, const core::ExpressionAddress& rhs) { 
					
			auto defAccess = getImmediateAccess(stmt->getNodeManager(), 
								cfg::Address(block, stmt_idx, lhs), 
								getCFG().getTmpVarMap()
							);

			auto defClasses = aMgr.findClass(defAccess);
			assert(!defClasses.empty() && "Invalid class for access. Something wrong in the extract() method");

			AccessClassSet depClasses = getConflicting(defClasses);
			std::copy(defClasses.begin(), defClasses.end(), std::inserter(depClasses, depClasses.begin()));

			bool found = false;

			// Kill Entities 
			for(auto it = in.begin(), end=in.end(); it != end; ++it) {
				if (std::find_if( depClasses.begin(), depClasses.end(), 
						[&](const AccessClassPtr& cur) { return *cur == **it; }) != depClasses.end() ) { 
							found = true; 
							kill.insert( *it ); 
				}
			}

			// if the LHS is a not live variable then the RHS should not be detected as a live
			// variable 
			if (!found) { 
				return; // then any of the uses in the LHS are relevant 
			}

			handle_rhs(rhs);
		};


		// assume scalar variables 
		if (auto decl = core::dynamic_address_cast<const core::DeclarationStmt>(stmt)) {

			handle_def(decl->getVariable(), decl->getInitialization());

		} else if (auto call = core::dynamic_address_cast<const core::CallExpr>(stmt)) {

			if (core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign()) ) {
				handle_def(call->getArgument(0), call->getArgument(1));
				return;
			}

			// function 
			handle_rhs(call);

		} else if (cur.getType() == cfg::Element::LOOP_INCREMENT) {

			auto cfgAddr = getCFG().find(
					cur.getStatementAddress().as<core::ForStmtAddress>()->getDeclaration()->getVariable()
				);

			auto accessPtr = getImmediateAccess(mgr, cfgAddr, getCFG().getTmpVarMap());
			auto liveClasses = aMgr.findClass(accessPtr);

			std::copy(liveClasses.begin(), liveClasses.end(), std::inserter(gen, gen.begin()));

		} else {

			handle_rhs( core::ExpressionAddress(stmt.as<core::ExpressionPtr>()) );

		}
	});
	
	return {gen,kill};
}

} // end analyses namespace 
} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 
