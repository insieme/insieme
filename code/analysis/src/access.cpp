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
// #include "insieme/core/dump/text_dump.h"

#include "insieme/analysis/polyhedral/iter_dom.h"
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;

namespace insieme { 
namespace analysis { 

bool Access::isRef() const { 
	return core::analysis::isRefType(base_expr->getType()); 
}

std::ostream& Access::printTo(std::ostream& out) const { 
	out << *variable << ":["; 
	if (cfgBlock) { 
		out << "{B" << cfgBlock->getBlockID() << ":" << stmtIdx << "}"; 
	}
	out << base_expr << "]";

	// Print path 
	if (path && !variable->getNodeManager().getLangBasic().isDataPathRoot(path)) { 
		out << "<" << *path << ">"; 
	}
	
	out << ":" << (isRef()?"ref":"val");

	if (array_access) {
		out << " if -> " << *array_access;
	}

	return out;
}

bool Access::operator<(const Access& other) const {
	// First check if the accesses are pointing to the same variable 
	if (variable < other.variable) { return true; }
	if (variable > other.variable) { return false; }

	// Next check th type of variable to which the access refers 
	if (type != VarType::ARRAY)    { return path < other.path; }

	// If this is an array access, then we have to check whether we have associated information on
	// the range of elements being accessed which refers to a domain on which we are sure variables
	// have the same meaning.
	if (!ctx && other.ctx) 		   	{ return true; }
	if (ctx && !other.ctx) 		   	{ return false; }

	// we check whether the two constraint refers to the same context 
	if (ctx != other.ctx)		   	{ return ctx < other.ctx; }

	return true;
	// we are in the same context 
	// if (array_access != other.array_access)	{ return array_access < other.array_access; }

	// otherwise if the dom is the same, we have to compare the expression
	// return toIR(base_expr->getNodeManager(), array_access) < toIR(base_expr->getNodeManager(), other.array_access);
}

/** 
 * Get the immediate access 
 */
Access getImmediateAccess(
		const core::ExpressionAddress& 					expr, 
		const std::pair<cfg::BlockPtr, size_t>& 		cfgAddr, 
		const TmpVarMap& 								tmpVarMap) 
{
	
	NodeManager& mgr = expr->getNodeManager();
	const lang::BasicGenerator& gen = expr->getNodeManager().getLangBasic();
	datapath::DataPathBuilder dpBuilder(expr->getNodeManager());

	// A literal is not an access 
	if (expr->getNodeType() == NT_Literal) 
		throw NotAnAccessException(true);

	// For cast expressions, we simply recur 
	if (expr->getNodeType() == NT_CastExpr) 
		return getImmediateAccess(expr.as<CastExprAddress>()->getSubExpression(), cfgAddr, tmpVarMap);

	// If this is a scalar variable, then return the access to this variable 
	if (expr->getNodeType() == NT_Variable) {
		return Access(expr, 
				expr.getAddressedNode().as<VariablePtr>(), 
				dpBuilder.getPath(), 
				VarType::SCALAR,
				cfgAddr.first,
				cfgAddr.second
			);
	}
	assert(expr->getNodeType() == NT_CallExpr);

	CallExprAddress callExpr = expr.as<CallExprAddress>();
	auto args = callExpr->getArguments();

	LOG(DEBUG) << *callExpr;

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
		return Access(expr, var, dpBuilder.getPath(), VarType::SCALAR, cfgAddr.first, cfgAddr.second);
	} 

	// Handle member access functions 
	if ( gen.isMemberAccess(callExpr->getFunctionExpr()) ) {

		// this is a tuple access
		if ( gen.isUnsignedInt( args[1]->getType() ) ) {
			return Access(
					callExpr, 
					var, 
					dpBuilder.component( args[1].as<LiteralAddress>().getValue() ).getPath(),
					VarType::TUPLE,
					cfgAddr.first,
					cfgAddr.second
				);
		}

		// This is a member access 
		if ( gen.isIdentifier( args[1]->getType() ) ) {
			return Access(
					callExpr,
					var,
					dpBuilder.member( args[1].as<LiteralAddress>()->getValue().getValue()).getPath(),
					VarType::MEMBER,
					cfgAddr.first,
					cfgAddr.second
				);
		}

		assert( false && "Type of member access not supported" );
	}

	// Handle Array/Vector subscript operator 
	if ( gen.isSubscriptOperator(callExpr->getFunctionExpr()) ) {

		// Create the variable used to express the range information for this access. 
		// Because we need to be able to compare accesses of different arrays for inclusion we need
		// to make sure the variable used to access the array i (i.e. A[i]) is the same for all the
		// generated accesses. This is obtained using a variable whose ID is very large 
		
		core::VariablePtr idxVar = 
			core::Variable::get(mgr, gen.getUInt8(), std::numeric_limits<unsigned int>::max());
		
		try {
			// Extract the formula from the argument 1 
			arithmetic::Formula f = arithmetic::toFormula( args[1].getAddressedNode() );
			if (f.isConstant()) {

				polyhedral::IterationVector iterVec; 
				iterVec.add( polyhedral::Iterator( idxVar ) );
				polyhedral::AffineFunction af( iterVec, { 1, -static_cast<int>(static_cast<int64_t>(f.getConstantValue())) } );

				return Access(
						callExpr,
						var,
						dpBuilder.element(static_cast<int64_t>(f.getConstantValue())).getPath(),
						VarType::ARRAY,
						cfgAddr.first,
						cfgAddr.second,
						iterVec,
						makeCombiner(utils::Constraint<polyhedral::AffineFunction>(af, utils::ConstraintType::EQ)) 
					);
			}

			// the access function is not a constant but a function 
			ExpressionAddress expr = args[1];
			if ( VariableAddress var = core::dynamic_address_cast<const Variable>( expr ) ) {
				// if the index expression is a single variable we may be in the case where this
				// variable is an alias for an other expression
				if ( ExpressionAddress aliasExpr = tmpVarMap.getMappedExpr( var.getAddressedNode() ) ) {
					// If this was an alias, use the aliased expression as array access 
					expr = aliasExpr;
				}
			}

			auto dom = polyhedral::getVariableDomain( expr );
			
			if (dom.first) { 
				
				const polyhedral::IterationVector& oldIter = 
					dom.first.getAnnotation(polyhedral::scop::ScopRegion::KEY)->getIterationVector();

				polyhedral::IterationVector iterVec;

				std::for_each(oldIter.iter_begin(), oldIter.iter_end(), [&](const polyhedral::Iterator& iter) {
					iterVec.add( polyhedral::Iterator(iter.getExpr().as<VariablePtr>(), true) );
				});
				// std::for_each(oldIter.param_begin(), oldIter.param_end(), [&](const polyhedral::Parameter& param) {
				//	iterVec.add( param );
				//});

				iterVec.add( polyhedral::Iterator(idxVar) );

				polyhedral::AffineFunction af(iterVec, core::arithmetic::Formula(idxVar) - f);

				// this region is a SCoP
				return Access(
					callExpr,
					var,
					dpBuilder.element( args[1].getAddressedNode() ).getPath(),
					VarType::ARRAY,
					cfgAddr.first,
					cfgAddr.second,
					iterVec,
					cloneConstraint(iterVec, dom.second) and 
						makeCombiner(utils::Constraint<polyhedral::AffineFunction>(af, utils::ConstraintType::EQ)),
					dom.first
				);
			}

			return Access(
				callExpr,
				var,
				dpBuilder.element( args[1].getAddressedNode() ).getPath(),
				VarType::ARRAY,
				cfgAddr.first,
				cfgAddr.second
			);

		} catch (arithmetic::NotAFormulaException&& e) { 
			// What if this is a piecewise? we can handle it 
			assert (false && "Array access is not a formula?");
		} 	
	}
	assert(false && "Access not supported");
}


bool Access::isContextDependent() const {
	
	// check whether this is an array access, if not then this access is not context dependent 
	if (type != VarType::ARRAY) { return false; }

	// we have an array access, now check whether we have a bound expression limiting the range of
	// accessed elements 
	if(array_access) {
		// if there are parameters in this access, then this access depends on the context 
		if (iterVec.getParameterNum() > 0)
			return true;
	}
	
	return false;
}

void extractFromStmt(const core::StatementAddress& stmt, std::set<Access>& entities, const TmpVarMap& tmpVarMap) {
	
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
				entities.insert( getImmediateAccess(arg, {nullptr, 0}, tmpVarMap) );
			} catch(NotAnAccessException&& e) { 
				assert(e.isLit);
				/* This is not an access, do nothing */ 
			}
		}
	};

	if (core::DeclarationStmtAddress declStmt = core::dynamic_address_cast<const DeclarationStmt>(stmt)) {
		entities.insert( getImmediateAccess(declStmt->getVariable(), {nullptr, 0}, tmpVarMap) );

		try {
			entities.insert( getImmediateAccess(declStmt->getInitialization(), {nullptr, 0}, tmpVarMap) );
			return ;
		} catch (NotAnAccessException&& e) { 
			if (e.isLit) { return; }
		}

		scanArguments( declStmt->getInitialization() );
		return;
	}

	if (core::ExpressionAddress expr = core::dynamic_address_cast<const Expression>(stmt)) {

		try {
			// try to extract the access (if this is a single supported access)
			entities.insert(getImmediateAccess(stmt.as<ExpressionAddress>(), {nullptr, 0}, tmpVarMap));
			return;
		} catch (NotAnAccessException&& e) {  
			if (e.isLit) { return; } 
		}

		scanArguments(expr);
		return;
	}

	assert( false && "expression not supported" );
}

std::set<Access> extractFromStmt(const core::StatementAddress& stmt, const TmpVarMap& tmpVarMap) {
	std::set<Access> accesses;
	
	extractFromStmt(stmt, accesses, tmpVarMap);
	return accesses;
}



bool isConflicting(const Access& acc1, const Access& acc2, const TmpVarMap& tmpVarMap) {

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

	if (tmpVarMap.empty()) { return false; }

	Access a1 = acc1, a2 = acc2;

//	ExpressionAddress expr = tmpVarMap.getMappedExpr( acc1.getAccessedVariable() );
//	if ( expr ) 
//		try {
//			a1 = getImmediateAccess(expr, tmpVarMap);
//		} catch( ... ) { } 
//
//	expr = tmpVarMap.getMappedExpr( acc2.getAccessedVariable() );
//	if ( expr ) 
//		try {
//			a2 = getImmediateAccess(expr, tmpVarMap);
//		} catch ( ... ) { }
//
// 	auto acc1Aliases = aliases.lookupAliases(a1.getAccessExpression());
// 	auto acc2Aliases = aliases.lookupAliases(a2.getAccessExpression());
// 
// 	//LOG(INFO) << acc1Aliases;
// 	//LOG(INFO) << acc2Aliases;
// 	std::set<VariablePtr> res;
// 	std::set_intersection(acc1Aliases.begin(), acc1Aliases.end(), 
// 						  acc2Aliases.begin(), acc2Aliases.end(), 
// 						  std::inserter(res, res.begin()));
// 	
// 	// LOG(INFO) << res;
// 	return !res.empty();

	return false;
}


Access getCFGBasedAccess(const core::ExpressionAddress& expr, const CFGPtr& cfg) {

	auto alias = cfg->getTmpVarMap().lookupImmediateAlias(expr);

	if (!alias) { alias = expr.getAddressedNode().as<VariablePtr>(); }

	std::cout << alias << std::endl;
	auto cfgBlock = cfg->find(expr);
	assert(cfgBlock.first && "Expr not found in the code");

	return getImmediateAccess(
			core::Address<const Expression>::find(
				alias, 
				(*cfgBlock.first)[cfgBlock.second].getAnalysisStatement()
			), {cfgBlock.first, cfgBlock.second}); 
}

// AccessClass ================================================================

std::ostream& AccessClass::printTo(std::ostream& out) const {
	return out << "AccessClass(" << uid << ")"
		// print list of accesses in this class 
		<< " [" << join(",", accesses, [&](std::ostream& jout, const AccessPtr& cur) { jout << *cur; }) << "]" 
		<< " PARENT(" << (!parentClass.expired() ? utils::numeric_cast<std::string>(parentClass.lock()->getUID()) : "NONE" ) << ")" 
		<< " SUB_CLASSES {" << join(",", subClasses, [&](std::ostream& jout, const Dependence& cur) { jout << *cur.first.lock() << ":" << cur.second; }) << "}";
}

std::set<ExpressionAddress> extractRealAddresses(const AccessClass& cl, const TmpVarMap& tmpVarMap) {

	std::set<ExpressionAddress> addrList;

	for (auto& access : cl) {
	
		auto accessAddr = access->getAccessExpression();
		// LOG(INFO) << accessAddr << " " << *accessAddr;

		if (accessAddr->getNodeType() == NT_Variable && 
			tmpVarMap.isTmpVar(accessAddr.getAddressedNode().as<VariablePtr>())) {
			continue;
		}

		cfg::BlockPtr cfgBlock;
		if (cfgBlock = access->getCFGBlock()) {
			// This is an address relative to the CFG, 
			core::VariablePtr var = access->getAccessedVariable();
			if (tmpVarMap.isTmpVar(var)) {
				auto tmpAddr = tmpVarMap.getMappedExpr(var);
				// LOG(INFO) << tmpAddr; 
				assert( tmpAddr );
				addrList.insert(tmpAddr.getParentAddress().as<ExpressionAddress>());
				continue;
			}

			// LOG(INFO) << "entering";
			auto stmtAddr 	  = (*cfgBlock)[access->getStmtIdx()].getStatementAddress();
			auto analysisStmt = (*cfgBlock)[access->getStmtIdx()].getAnalysisStatement();
			
			if (*(stmtAddr.getAddressedNode()) == *(accessAddr.getAddressedNode())) { 
				addrList.insert(stmtAddr.as<ExpressionAddress>());
				continue;
			}

			if (*stmtAddr.getAddressedNode() == *analysisStmt) {
				addrList.insert( 
					core::concat(stmtAddr.as<NodeAddress>(), accessAddr.as<NodeAddress>()
				).as<ExpressionAddress>() );
				continue;
			}

			// LOG(INFO) << stmtAddr << " " << *stmtAddr; 
			// LOG(INFO) << analysisStmt;
	
			// search common root
			NodeAddress rootAddr=accessAddr;
			std::vector<size_t> path;
			while(!rootAddr.isRoot() && rootAddr.getAddressedNode() != stmtAddr.getAddressedNode()) { 
				path.push_back(rootAddr.getIndex());
				rootAddr = rootAddr.getParentAddress();
			}

			NodeAddress newAddr = stmtAddr;
			for_each(path.rbegin(), path.rend(), [&](size_t idx) {
				newAddr = newAddr.getAddressOfChild(idx);
			});

			// std::cout << newAddr << " " << *newAddr << std::endl;
			addrList.insert(newAddr.as<ExpressionAddress>());
			continue;
		} 

		// addrList.push_back(accessAddr);
	}

	return addrList;
}

// AccessManager ==============================================================

AccessClassPtr AccessManager::getClassFor(const Access& access) {

	auto getAccessForAlias = [&](const core::VariablePtr var) -> boost::optional<Access> {

		core::ExpressionAddress aliasedExpr = tmpVarMap.getMappedExpr(var);
		if (aliasedExpr) {
			// this is an alias indeed 
			std::pair<cfg::BlockPtr,size_t> block;
			if (cfg) {
				block = cfg->find(aliasedExpr);
				assert(block.first && "Failed to lookup expression in the CFG");
			}
			assert((!cfg || (cfg && block.first)) && "Block cannot be empty");

			try {
				core::ExpressionAddress relativeAddr = aliasedExpr;

				if (block.first) {
					auto cfgElement = (*block.first)[block.second];
					relativeAddr = 
						DeclarationStmtAddress(cfgElement.getAnalysisStatement().as<DeclarationStmtPtr>())
							->getInitialization();
				}

				assert(relativeAddr && "Error while forming the relative address");

				return getImmediateAccess(relativeAddr,block);

			} catch (NotAnAccessException&& e) { }
		}

		return boost::optional<Access>();
	};

	/* 
	 * Iterate through the existing classes and determine whether this access belongs to one of
	 * the exising classes, if not create a new class 
	 */
	for (auto& cl : classes) {

		for (auto& ac : cl->accesses) {
			
			// If we find the access already in one of the classes, then we simply return it 
			if (*ac == access) { return cl; }

			// otherwise we are in a situation where 2 expression addresses accessing the same
			// variable, in this case we check for range (if possible), 
			if (*ac->getAccessedVariable() == *access.getAccessedVariable()) {
				assert(ac->getType() == access.getType() && "Accessing the same variable with different types");

				switch(ac->getType()) {
				case VarType::SCALAR: 	
					cl->storeAccess(access);
					return cl;

				case VarType::MEMBER:	
				case VarType::TUPLE:
					/**
					 * If this is exactly the same memeber, than we can add this access to the
					 * same access class, otherwise a new class can be created 
					 */
					if (*ac->getPath() == *access.getPath()) { cl->storeAccess(access); return cl; }
					break;

				case VarType::ARRAY:
					// TODO: detemrine the common subrange and split classes 
					cl->storeAccess(access);
					return cl;
					
				default:
					assert(false && "Not supported");
				}
			}
		}
	}

	// it might be that this access is an alias for an expression for which we already defined a
	// class. 
	if (auto potentialAlias = 
		core::dynamic_pointer_cast<const core::Variable>(access.getAccessExpression().getAddressedNode())) 
	{
		if (auto ret = getAccessForAlias(potentialAlias) ) {
			auto thisClass = getClassFor( *ret );
			thisClass->storeAccess(access);
			return thisClass;
		}
	}

	/** 
	 * This might be an access to a subrange of a class.
	 *
	 * This can happen either when a compound member of a struct is accessed. or when the (N-x)th
	 * dimension of a N dimensional array is accessed 
	 */
	AccessClassPtr parentClass;
	if (auto potentialAlias = access.getAccessedVariable())	{
		if (auto ret = getAccessForAlias(potentialAlias) )
			parentClass = getClassFor( *ret );
	}

	// check if the parent class already has a child to represent this type of access 
	if (parentClass) { 
		for(auto cl : parentClass->getSubClasses()) {
			if (*cl.second == *access.getPath()) {
				cl.first.lock()->storeAccess(access);
				return cl.first.lock();
			}
		}
	}

	// Creates a new alias class 
	AccessClass newClass(std::cref(*this), classes.size(), parentClass);
	newClass.storeAccess(access);
	auto accessClassPtr = std::make_shared<AccessClass>(newClass);
	
	classes.emplace_back( accessClassPtr );

	if (parentClass) {
		parentClass->addSubClass( AccessClass::Dependence(accessClassPtr,access.getPath()) );
	}

	return classes.back();
}

std::ostream& AccessManager::printTo(std::ostream& out) const { 
	return out << "AccessManager [" << size() << "]\n\t" << 
		join("\n\t", classes, [&](std::ostream& jout, const AccessClassPtr& cur) { 
				jout << *cur; 
			}) << "]";
}

} } // end insieme::analysis namespace 


