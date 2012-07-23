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

#include "insieme/utils/logging.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;

namespace {

struct MultipleAccessException : public std::logic_error {
	MultipleAccessException() : std::logic_error("multiple accesses")  { }
};

struct NotAnAccessException : public std::logic_error {

	bool isLit;

	NotAnAccessException(bool isLit) :
		std::logic_error("not an access"), isLit(isLit) { }
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

	// A literal is not an access 
	if (expr->getNodeType() == NT_Literal) 
		throw NotAnAccessException(true);

	// For cast expressions, we simply recur 
	if (expr->getNodeType() == NT_CastExpr) 
		return makeAccess(expr.as<CastExprAddress>()->getSubExpression());

	// If this is a scalar variable, then return the access to this variable 
	if (expr->getNodeType() == NT_Variable) {
		return Access(expr, 
					expr.getAddressedNode().as<VariablePtr>(), 
					dpBuilder.getPath(), 
					VarType::SCALAR);
	}

	assert(expr->getNodeType() == NT_CallExpr);

	CallExprAddress callExpr = expr.as<CallExprAddress>();
	auto args = callExpr->getArguments();

	// If the callexpr is not a subscript or a member access, then it means this is not 
	// a direct memory access, but it could be we are processing a binary operator or other
	// which may contain multiple accesses. Therefore we throw an exception.
	if (!gen.isMemberAccess(callExpr->getFunctionExpr()) &&
		!gen.isSubscriptOperator(callExpr->getFunctionExpr()) &&
		!gen.isRefDeref(callExpr->getFunctionExpr()) ) 
	{
		throw NotAnAccessException(false);
	}

	// because of the construction of the CFG, the arguments of the deref operation must be a
	// variable
	if ( args[0]->getNodeType() != NT_Variable ) { throw std::logic_error("error"); }

	core::VariablePtr var = args[0].getAddressedNode().as<VariablePtr>();

	if (gen.isRefDeref(callExpr->getFunctionExpr())) {
		return Access(expr, var, dpBuilder.getPath(), VarType::SCALAR);
	} 

	// Handle member access functions 
	if ( gen.isMemberAccess(callExpr->getFunctionExpr()) ) {

		// this is a tuple access
		if ( gen.isUnsignedInt( args[1]->getType() ) ) {
			return Access(
					callExpr, 
					var, 
					dpBuilder.component( args[1].as<LiteralAddress>().getValue() ).getPath(),
					VarType::TUPLE);
		}

		// This is a member access 
		if ( gen.isIdentifier( args[1]->getType() ) ) {
			return Access(
					callExpr,
					var,
					dpBuilder.member( args[1].as<LiteralAddress>()->getValue().getValue()).getPath(),
					VarType::MEMBER);
		}

		assert( false && "Type of member access not supported" );
	}

	// Handle Array/Vector subscript operator 
	if ( gen.isSubscriptOperator(callExpr->getFunctionExpr()) ) {

		try {
			// Extract the formula from the argument 1 
			arithmetic::Formula f = arithmetic::toFormula( args[1].getAddressedNode() );
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
			} catch(NotAnAccessException&& e) { 
				assert(e.isLit);
				/* This is not an access, do nothing */ 
			}
		}
	};

	if (core::DeclarationStmtAddress declStmt = core::dynamic_address_cast<const DeclarationStmt>(stmt)) {
		entities.insert( makeAccess(declStmt->getVariable()) );

		try {
			entities.insert( makeAccess(declStmt->getInitialization()) );
			return ;
		} catch (NotAnAccessException&& e) { 
			if (e.isLit) { return; }
		} catch (MultipleAccessException&& e) { 	}

		scanArguments(declStmt->getInitialization());
		return;
	}

	if (core::ExpressionAddress expr = core::dynamic_address_cast<const Expression>(stmt)) {

		try {
			// try to extract the access (if this is a single supported access)
			entities.insert(makeAccess(stmt.as<ExpressionAddress>()));
			return;
		} catch (NotAnAccessException&& e) {  
			if (e.isLit) { return; } 
		} catch (MultipleAccessException&& e) {  }

		scanArguments(expr);
		return;
	}
	assert( false );
}

std::set<Access> extractFromStmt(const core::StatementAddress& stmt) {
	
	std::set<Access> accesses;
	extractFromStmt(stmt, accesses);
	return std::move(accesses);

}


bool isConflicting(const Access& acc1, const Access& acc2, const AliasMap& aliases) {

	NodeManager& mgr = acc1.getAccessedVariable()->getNodeManager();
	const lang::BasicGenerator& gen = mgr.getLangBasic();

	if (*acc1.getAccessedVariable() == *acc2.getAccessedVariable()) {
		// check the paths 
		if (*acc1.getPath() == *gen.getDataPathRoot()) return true; 
		if (*acc2.getPath() == *gen.getDataPathRoot()) return true; 
		
		// else check if a path includes the other 
		
		NodeAddress path1(acc1.getPath());
		NodeAddress path2(acc2.getPath());

		if ( isChildOf(path1, path2) ) return true;
		if ( isChildOf(path2, path1) ) return true;

		return false;
	}

	if (aliases.empty()) { return false; }

	Access a1 = acc1, a2 = acc2;

	ExpressionAddress expr = aliases.getMappedExpr( acc1.getAccessedVariable() );
	if ( expr )
		a1 = makeAccess(expr);

	expr = aliases.getMappedExpr( acc2.getAccessedVariable() );
	if ( expr )
		a2 = makeAccess(expr);

	auto acc1Aliases = aliases.lookupAliases(a1.getAccessExpression());
	auto acc2Aliases = aliases.lookupAliases(a2.getAccessExpression());

	//LOG(INFO) << acc1Aliases;
	//LOG(INFO) << acc2Aliases;
	std::set<VariablePtr> res;
	std::set_intersection(acc1Aliases.begin(), acc1Aliases.end(), 
						  acc2Aliases.begin(), acc2Aliases.end(), 
						  std::inserter(res, res.begin()));
	
	// LOG(INFO) << res;
	return !res.empty();

}


} } // end insieme::analysis namespace 

