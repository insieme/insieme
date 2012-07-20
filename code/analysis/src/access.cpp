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

#include "insieme/analysis/access.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;

namespace {

struct MultipleAccessException : public std::logic_error {
	MultipleAccessException() : std::logic_error("multiple accesses")  { }
};

struct NotAnAccessException : public std::logic_error {
	NotAnAccessException() : std::logic_error("not an access")  { }
};

}  // end anonymous namespace 

namespace insieme { 
namespace analysis { 

bool Access::isRef() const { 
	return core::analysis::isRefType(base_expr->getType()); 
}

std::ostream& Access::printTo(std::ostream& out) const { 
	out << *variable; 
	if (path) { out << "< " << *path << " >"; }
	return out << ":" << (isRef()?"ref":"val");
}

Access makeAccess(const core::ExpressionAddress& expr) {
	
	const lang::BasicGenerator& gen = expr->getNodeManager().getLangBasic();
	datapath::DataPathBuilder dpBuilder(expr->getNodeManager());

	if (expr->getNodeType() == NT_Literal) 
		throw NotAnAccessException();

	// We skip cast expression as we are not interested in them 
	if (expr->getNodeType() == NT_CastExpr) 
		return makeAccess(expr.as<CastExprAddress>()->getSubExpression());

	if (expr->getNodeType() == NT_Variable) {
		return Access(expr, 
					expr.getAddressedNode().as<VariablePtr>(), 
					dpBuilder.getPath(), 
					VarType::SCALAR);
	}

	assert(expr->getNodeType() == NT_CallExpr);

	CallExprAddress callExpr = expr.as<CallExprAddress>();

	if (gen.isRefDeref(callExpr->getFunctionExpr())) {
		assert( callExpr->getArgument(0)->getNodeType() == NT_Variable);
		return Access(expr, 
					callExpr->getArgument(0).getAddressedNode().as<VariablePtr>(), 
					dpBuilder.getPath(), 
					VarType::SCALAR);
	} 

	assert( callExpr->getArgument(0)->getNodeType() == NT_Variable );
	core::VariablePtr var = callExpr->getArgument(0).getAddressedNode().as<VariablePtr>();

	// Handle member access functions 
	if ( gen.isMemberAccess(callExpr->getFunctionExpr()) ) {

		if ( gen.isUnsignedInt( callExpr->getArgument(1)->getType() ) ) {
			// this is a tuple access
			return Access(
					callExpr, 
					var, 
					dpBuilder.component(callExpr->getArgument(1).as<LiteralAddress>().getValue()).getPath(),
					VarType::TUPLE);
		}

		// This is a member access 
		if ( gen.isIdentifier( callExpr->getArgument(1)->getType() ) ) {
			
			// this is a tuple access
			return Access(
					callExpr,
					var,
					dpBuilder.member(callExpr->getArgument(1).as<LiteralAddress>()->getValue().getValue()).getPath(),
					VarType::MEMBER);
		}

		assert( false && "Type of member access not supported" );
	}

	// Handle Array/Vector subscript operator 
	if ( gen.isSubscriptOperator(callExpr->getFunctionExpr()) ) {

		try {
			arithmetic::Formula f = arithmetic::toFormula(callExpr->getArgument(1));
			if (f.isConstant()) {
				return Access(
						callExpr,
						var,
						dpBuilder.element(static_cast<int64_t>(f.getConstantValue())).getPath(),
						VarType::ARRAY);
			}
		} catch (arithmetic::NotAFormulaException&& e) { }
		
	}
	throw MultipleAccessException();
}

void extractFromStmt(const core::StatementAddress& stmt, std::set<Access>& entities) {
	
	/**
	 * This function extracts entities from CFG blocks, therefore due to the construction properties
	 * of CFG Blocks, only the following cases are possible:
	 *
	 * 1) decl A = callexpr( vars... );
	 * 2) decl A = cast( vars... );
	 * 3) A = callexpr( vars ... );
	 * 4) A = cast( vars ...);
	 * 5) callexpr( vars... );
	 * 6) cast( vars...);
	 */

	auto scanArguments = [&] ( const ExpressionAddress& expr ) {
		// this expression there are multiple references, therefore we skip the call-expr
		// and examine the single variables 
		CallExprAddress call = expr.as<CallExprAddress>();
		for(auto& arg : call->getArguments()) {
			try {
				entities.insert( makeAccess(arg) );
			} catch(NotAnAccessException&& e) { /* This is not an access, do nothing */ }
		}
	};

	if (core::DeclarationStmtAddress declStmt = core::dynamic_address_cast<const DeclarationStmt>(stmt)) {
		entities.insert( makeAccess(declStmt->getVariable()) );

		try {
			entities.insert( makeAccess(declStmt->getInitialization()) );
		} catch (NotAnAccessException&& e) { /* if this is not an access, simply do nothing */ }
		 catch (MultipleAccessException&& e) {
			scanArguments(declStmt->getInitialization());
			return;
		}

		return;
	}

	if (core::ExpressionAddress expr = core::dynamic_address_cast<const Expression>(stmt)) {

		try {
			// try to extract the access (if this is a single supported access)
			entities.insert(makeAccess(stmt.as<ExpressionAddress>()));
			return;
		} catch (MultipleAccessException&& e) {
			scanArguments(expr);
			return;
		}
	}
	assert( false );
}

std::set<Access> extractFromStmt(const core::StatementAddress& stmt) {
	
	std::set<Access> accesses;
	extractFromStmt(stmt, accesses);
	return std::move(accesses);

}



} } // end insieme::analysis namespace 

