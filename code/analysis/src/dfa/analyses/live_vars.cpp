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

	LOG(INFO) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	LOG(INFO) << "Block " << block->getBlockID();
	LOG(INFO) << "IN: " << in;

	assert(block->size() == 1);

	size_t stmt_idx = 0;

	core::StatementPtr stmt = (*block)[0].getAnalysisStatement();

	core::NodeManager& mgr = stmt->getNodeManager();

	const AccessManager& aMgr = getAccessManager();


	auto handle_def = [&](const core::ExpressionAddress& rhs, const core::ExpressionAddress& lhs, bool isDecl) { 
				
		cfg::Address cfgAddr(block, stmt_idx, rhs);
		auto defAccess = getImmediateAccess(stmt->getNodeManager(), cfgAddr, getCFG().getTmpVarMap());

		auto defClass = aMgr.findClass(defAccess);
		assert(defClass && "Invalid class for access. Something wrong in the extract() method");

		AccessClassSet depClasses = defClass->getConflicting();
		depClasses.insert(defClass);

		// Kill Entities 
		if (defAccess->isReference()) {
			for(auto it = in.begin(), end=in.end(); it != end; ++it) {
				if (std::find_if( depClasses.begin(), depClasses.end(), [&](const AccessClassPtr& cur) { 
							return *cur == **it; 
						}) != depClasses.end() ) { kill.insert( *it ); }
			}
		}

		auto addr = cfg::Address(block,stmt_idx,lhs);

		auto accesses = getAccesses(mgr, UnifiedAddress(addr), getCFG().getTmpVarMap());
		for (const auto& acc : accesses) {

			gen.insert( aMgr.findClass(acc) );

		}

	};

	// assume scalar variables 
	if (core::DeclarationStmtAddress decl = 
			core::dynamic_address_cast<const core::DeclarationStmt>(core::NodeAddress(stmt))) {

		handle_def(decl->getVariable(), decl->getInitialization(), true);

	} else if (core::CallExprAddress call = core::dynamic_address_cast<const core::CallExpr>(core::NodeAddress(stmt))) {

		if(core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign()) );  

			handle_def(call->getArgument(0), call->getArgument(1), false);

	} else {
		LOG(WARNING) << *block; 
	}
	
	LOG(INFO) << "KILL: " << kill;
	LOG(INFO) << "GEN:  " << gen;

	typename LiveVariables::value_type set_diff, ret;
	std::set_difference(in.begin(), in.end(), kill.begin(), kill.end(), std::inserter(set_diff, set_diff.begin()));
	std::set_union(set_diff.begin(), set_diff.end(), gen.begin(), gen.end(), std::inserter(ret, ret.begin()));

	LOG(INFO) << "RET: " << ret;
	return ret;
}

} // end analyses namespace 
} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 
