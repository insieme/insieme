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

/**
 * LiveVariables Problem
 */
typename LiveVariables::value_type 
LiveVariables::meet(const typename LiveVariables::value_type& lhs, const typename LiveVariables::value_type& rhs) const 
{
	typename LiveVariables::value_type ret;
	std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(ret,ret.begin()));
	return ret;
}


typename LiveVariables::value_type 
LiveVariables::transfer_func(const typename LiveVariables::value_type& in, const cfg::BlockPtr& block) const {
	typename LiveVariables::value_type gen, kill;
	
	if (block->empty()) { return in; }

	LOG(DEBUG) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	LOG(DEBUG) << "Block " << block->getBlockID();
	LOG(DEBUG) << "IN: " << in;

	assert(block->size() == 1);

	size_t stmt_idx = 0;

	core::StatementPtr stmtPtr = (*block)[0].getAnalysisStatement();
	core::StatementAddress stmt = core::StatementAddress(stmtPtr);

	core::NodeManager& mgr = stmt->getNodeManager();

	const AccessManager& aMgr = getAccessManager();

	auto handle_body = [&](const core::ExpressionAddress& expr) {

		auto accesses = getAccesses(mgr, UnifiedAddress(cfg::Address(block,stmt_idx,expr)), getCFG().getTmpVarMap());
		for (const auto& acc : accesses) {
			gen.insert( aMgr.findClass(acc) );
		}

	};

	auto handle_def = [&](const core::ExpressionAddress& lhs, const core::ExpressionAddress& rhs) { 
				
		auto defAccess = 
			getImmediateAccess(stmt->getNodeManager(), cfg::Address(block, stmt_idx, lhs), getCFG().getTmpVarMap());

		auto defClass = aMgr.findClass(defAccess);
		assert(defClass && "Invalid class for access. Something wrong in the extract() method");

		AccessClassSet depClasses = defClass->getConflicting();
		depClasses.insert(defClass);

		// Kill Entities 
		for(auto it = in.begin(), end=in.end(); it != end; ++it) {
			if (std::find_if( depClasses.begin(), depClasses.end(), [&](const AccessClassPtr& cur) { 
						return *cur == **it; 
					}) != depClasses.end() ) { kill.insert( *it ); }
		}

		try {
			auto useAccess = 
				getImmediateAccess(stmt->getNodeManager(), cfg::Address(block, stmt_idx, rhs), getCFG().getTmpVarMap());
			
			// if the RHS is a not live variable 
			auto fit = in.find(defClass);
			if (fit == in.end()) {
				return ;
			}

		} catch(NotAnAccessException&& e) { }

		handle_body(rhs);
	};

	// assume scalar variables 
	if (core::DeclarationStmtAddress decl = 
			core::dynamic_address_cast<const core::DeclarationStmt>(stmt)) {

		handle_def(decl->getVariable(), decl->getInitialization());

	} else if (core::CallExprAddress call = core::dynamic_address_cast<const core::CallExpr>(stmt)) {

		if (core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign()) ) {
			handle_def(call->getArgument(0), call->getArgument(1));
		} else {
			handle_body(call);
		}

	} else if ((*block)[0].getType() == cfg::Element::LOOP_INCREMENT) {

		auto cfgAddr = getCFG().find((*block)[0].getStatementAddress().as<core::ForStmtAddress>()->getDeclaration()->getVariable());
		auto accessPtr = getImmediateAccess(mgr, cfgAddr, getCFG().getTmpVarMap());
		gen.insert( aMgr.findClass(accessPtr) );

	} else {
		handle_body( core::ExpressionAddress(stmt.as<core::ExpressionPtr>()) );
	}
	
	LOG(DEBUG) << "KILL: " << kill;
	LOG(DEBUG) << "GEN:  " << gen;

	typename LiveVariables::value_type set_diff, ret;
	std::set_difference(in.begin(), in.end(), kill.begin(), kill.end(), std::inserter(set_diff, set_diff.begin()));
	std::set_union(set_diff.begin(), set_diff.end(), gen.begin(), gen.end(), std::inserter(ret, ret.begin()));

	LOG(DEBUG) << "RET: " << ret;
	return ret;
}

} // end analyses namespace 
} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 
