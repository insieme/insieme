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

#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/logging.h"

#include "insieme/analysis/access.h"

namespace insieme { namespace analysis { namespace dfa {

template <>
typename container_type_traits< dfa::elem< cfg::Address >  >::type 
extract(const Entity< dfa::elem<cfg::Address> >& e, const CFG& cfg) {

	std::set<cfg::Address> entities;

	auto collector = [&entities, &cfg] (const cfg::BlockPtr& block) {
		size_t stmt_idx=0;
		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {
			++stmt_idx;

			auto stmt = core::NodeAddress(cur.getAnalysisStatement());
	
			if (cur.getType() == cfg::Element::LOOP_INCREMENT) { 
				stmt.as<core::ForStmtPtr>()->getDeclaration()->getVariable();
				// FIXME
			}

			if (auto declStmt = core::dynamic_address_cast<const core::DeclarationStmt>(stmt)) {
				entities.insert( cfg::Address(block, stmt_idx-1, declStmt->getVariable()) );
				return;
			}

			auto expr = core::dynamic_address_cast<const core::Expression>(stmt);
			if (!expr) { return; }

			if (core::analysis::isCallOf(expr.getAddressedNode(), expr->getNodeManager().getLangBasic().getRefAssign())) {
				entities.insert( cfg::Address(block, stmt_idx-1, expr.as<core::CallExprAddress>()->getArgument(0)) );
				return;
			}
		});
	};
	cfg.visitDFS(collector);

	return entities;
}
	
namespace analyses {

typedef typename ReachingDefinitions::value_type AnalysisDataType;

/**
 * ReachingDefinitions Problem
 */

AnalysisDataType ReachingDefinitions::meet(const AnalysisDataType& lhs, const AnalysisDataType& rhs) const {
	LOG(DEBUG) << "MEET ( " << lhs << ", " << rhs << ") -> ";
	AnalysisDataType ret;
	std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), std::inserter(ret,ret.begin()));
	LOG(DEBUG) << ret;
	return ret;
}


void definitionsToAccesses(const AnalysisDataType& data, AccessManager& aMgr) {
	for(const auto& dfAddress : data) {
		aMgr.getClassFor( getImmediateAccess( dfAddress.getAddressedNode()->getNodeManager(),dfAddress ) );
	}
}

typedef std::set<AccessClassPtr, compare_target<AccessClassPtr>> AccessClassSet;

// reach the parent class 
void addSubClasses(AccessClassSet& classes, const AccessClassPtr& cl) {
	for (const auto& cur : cl->getSubClasses()) {
		// Visit the parent until the kind of access changes 
		if (std::get<0>(cur) == AccessClass::DT_LEVEL)
			break;

		auto thisClass = std::get<1>(cur).lock();
		if(classes.insert(thisClass).second)
			addSubClasses(classes, thisClass);
	}
}

AnalysisDataType ReachingDefinitions::transfer_func(const AnalysisDataType& in, const cfg::BlockPtr& block) const {

	AnalysisDataType gen, kill;
	
	if (block->empty()) { return in; }

	LOG(DEBUG) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	LOG(DEBUG) << "~ Block " << block->getBlockID();
	LOG(DEBUG) << "~ IN: " << in;

	AccessManager mgr(&getCFG(), getCFG().getTmpVarMap());
	definitionsToAccesses(in, mgr);

	size_t stmtIdx=0;
	for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {

		core::StatementAddress stmt = core::StatementAddress(cur.getAnalysisStatement());

		auto handle_def = [&](const core::VariableAddress& varAddr) { 

			assert(varAddr && "Variable not found within the statement of the CFG Block");

			cfg::Address cfgAddr(block, stmtIdx, varAddr);

			auto access = getImmediateAccess(stmt->getNodeManager(), cfgAddr, getCFG().getTmpVarMap());

			// Get the class to which the access belongs to 
			AccessClassPtr collisionClass = mgr.getClassFor(access);

			AccessClassSet classes;
			classes.insert(collisionClass);
			// Add subclasses which are affected by this definition
			addSubClasses(classes, collisionClass);

			// Kill Entities 
			if (access->isReference()) 
				for (auto& curClass : classes) {
					for (auto& acc : *curClass) {
						kill.insert( acc->getAddress().as<cfg::Address>() );
					}
				}

			gen.insert( access->getAddress().as<cfg::Address>() );

		};

		if (stmt->getNodeType() == core::NT_Literal) { return; }

		if (core::DeclarationStmtAddress decl = core::dynamic_address_cast<const core::DeclarationStmt>(stmt)) {

			if (!getCFG().getTmpVarMap().isTmpVar(decl->getVariable().getAddressedNode())) {
				handle_def( decl->getVariable() );
			}

		} else if (core::CallExprAddress call = core::dynamic_address_cast<const core::CallExpr>(stmt)) {

			if (core::analysis::isCallOf(call.getAddressedNode(), call->getNodeManager().getLangBasic().getRefAssign()) ) { 
				handle_def( call->getArgument(0).as<core::VariableAddress>() );
			}
		}

		if (cur.getType() == cfg::Element::LOOP_INCREMENT) {
			handle_def( stmt.as<core::ForStmtAddress>()->getDeclaration()->getVariable() );
		}

		++stmtIdx;
	});

	// TODO: Factorize outside the analysis code 
	LOG(DEBUG) << "~ KILL: " << kill;
	LOG(DEBUG) << "~ GEN:  " << gen;

	AnalysisDataType set_diff, ret;
	std::set_difference(in.begin(), in.end(), kill.begin(), kill.end(), std::inserter(set_diff, set_diff.begin()));
	std::set_union(set_diff.begin(), set_diff.end(), gen.begin(), gen.end(), std::inserter(ret, ret.begin()));
	LOG(DEBUG) << "~ RET: " << ret;

	return ret;
}

} } } } // end insieme::analysis::dfa::analyses namespace 
