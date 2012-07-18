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

#include "insieme/analysis/dfa/problem.h"

#include "insieme/utils/logging.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace dfa {

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

	core::StatementPtr stmt = (*block)[0].getAnalysisStatement();

	auto visitor = core::makeLambdaVisitor(
			[&gen] (const core::VariablePtr& var) { gen.insert( var ); }, true);
	auto v = makeDepthFirstVisitor( visitor );

	// assume scalar variables 
	if (core::DeclarationStmtPtr decl = core::dynamic_pointer_cast<const core::DeclarationStmt>(stmt)) {

		kill.insert( decl->getVariable() );
		v.visit(decl->getInitialization());

	} else if (core::CallExprPtr call = core::dynamic_pointer_cast<const core::CallExpr>(stmt)) {

		std::vector<core::ExpressionPtr>::const_iterator begin = call->getArguments().begin(), 
														 end = call->getArguments().end();

		if (core::analysis::isCallOf(call, call->getNodeManager().getLangBasic().getRefAssign()) ) { 
			kill.insert( call->getArgument(0).as<core::VariablePtr>() );
			++begin;
		}

		std::for_each(begin, end, [&](const core::ExpressionPtr& cur) { v.visit(cur); });

	} else {
		LOG(WARNING) << *block; 
		// assert(false);
	}
	
	LOG(DEBUG) << "KILL: " << kill;
	LOG(DEBUG) << "GEN:  " << gen;

	typename LiveVariables::value_type set_diff, ret;
	std::set_difference(in.begin(), in.end(), kill.begin(), kill.end(), std::inserter(set_diff, set_diff.begin()));
	std::set_union(set_diff.begin(), set_diff.end(), gen.begin(), gen.end(), std::inserter(ret, ret.begin()));

	LOG(DEBUG) << "RET: " << ret;
	return ret;
}












} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 
