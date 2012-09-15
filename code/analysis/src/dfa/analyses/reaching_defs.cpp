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
typename container_type_traits< dfa::elem<analyses::LValue>  >::type 
extract(const Entity< dfa::elem<analyses::LValue> >& e, const CFG& cfg) 
{ 
	std::set<analyses::LValue> entities;

	auto collector = [&entities, &cfg] (const cfg::BlockPtr& block) {
		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {
	
			// TODO: 
			if (cur.getType() == cfg::Element::LOOP_INCREMENT) { /* skip */ return; }

			auto stmt = cur.getAnalysisStatement();
			if (auto declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>(stmt)) {
				entities.insert( analyses::LValue(declStmt->getVariable()) );
				return;
			}

			core::ExpressionPtr expr = core::dynamic_pointer_cast<const core::Expression>(stmt);
			if (!expr) { return; }

			if (core::analysis::isCallOf(expr, expr->getNodeManager().getLangBasic().getRefAssign())) {
				entities.insert( analyses::LValue(expr.as<core::CallExprPtr>()->getArgument(0)) );
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


void definitionsToAccesses(const AnalysisDataType& data, AccessManager& mgr) {
	for(const auto& dfValue : data) {

		auto reachExpr	= std::get<0>(dfValue).getLValueExpr();
		auto reachBlock = std::get<1>(dfValue);

		core::ExpressionAddress retAddr;
		size_t stmtIdx=0;

		// find the statement in this block which contains the expr 
		for(auto it=reachBlock->stmt_begin(), end=reachBlock->stmt_end(); !retAddr && it!=end; ++it, ++stmtIdx) {
			retAddr = core::Address<const core::Expression>::find(reachExpr, it->getAnalysisStatement());
		}

		--stmtIdx;

		assert(retAddr && stmtIdx<reachBlock->size() && "Stmt address not found");

		mgr.getClassFor( 
				getImmediateAccess( reachExpr->getNodeManager(), 
									cfg::Address(reachBlock, stmtIdx, retAddr) ));
	}
}

AnalysisDataType ReachingDefinitions::transfer_func(const AnalysisDataType& in, const cfg::BlockPtr& block) const {

	AnalysisDataType gen, kill;
	
	if (block->empty()) { return in; }

	assert(block->size() == 1 && "Blocks with more than 1 stmts are not supported"); // FIXME: relax

	LOG(DEBUG) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	LOG(DEBUG) << "~ Block " << block->getBlockID();
	LOG(DEBUG) << "~ IN: " << in;

	AccessManager mgr(&getCFG(), getCFG().getTmpVarMap());
	definitionsToAccesses(in, mgr);

	size_t stmtIdx=0;
	for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {

		core::StatementPtr stmt = cur.getAnalysisStatement();

		auto handle_def = [&](const core::VariablePtr& varPtr) { 

			auto addr = core::Address<const core::Expression>::find(varPtr,stmt);
			auto access = getImmediateAccess( 
							stmt->getNodeManager(), 
							cfg::Address(block, stmtIdx, addr), 
							getCFG().getTmpVarMap() 
						);
		//	std::cout << access << std::endl;
			AccessClassPtr collisionClass = mgr.getClassFor(access);

			// Kill Entities 
			if (access->isReference()) 
				for (auto& acc : *collisionClass) {
					kill.insert( std::make_tuple(
							LValue(acc->getRoot().getVariable()), 
							acc->getAddress().as<cfg::Address>().getBlockPtr()) 
					 );
				}

			auto var = LValue(varPtr);
			gen.insert( std::make_tuple(var, block) );

		};

		if (stmt->getNodeType() == core::NT_Literal) { return; }

		// assume scalar variables 
		if (core::DeclarationStmtPtr decl = core::dynamic_pointer_cast<const core::DeclarationStmt>(stmt)) {

			if (!getCFG().getTmpVarMap().isTmpVar(decl->getVariable()))
				handle_def( decl->getVariable() );

		} else if (core::CallExprPtr call = core::dynamic_pointer_cast<const core::CallExpr>(stmt)) {

			if (core::analysis::isCallOf(call, call->getNodeManager().getLangBasic().getRefAssign()) ) { 
				handle_def( call->getArgument(0).as<core::VariablePtr>() );
			}

		} else {
			LOG(WARNING) << stmt;
			assert(false && "Stmt not handled");
		}

		stmtIdx++;
	});

	LOG(DEBUG) << "~ KILL: " << kill;
	LOG(DEBUG) << "~ GEN:  " << gen;

	AnalysisDataType set_diff, ret;
	std::set_difference(in.begin(), in.end(), kill.begin(), kill.end(), std::inserter(set_diff, set_diff.begin()));
	std::set_union(set_diff.begin(), set_diff.end(), gen.begin(), gen.end(), std::inserter(ret, ret.begin()));

	LOG(DEBUG) << "~ RET: " << ret;

	return ret;
}

} } } } // end insieme::analysis::dfa::analyses namespace 
