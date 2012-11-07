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

#include "insieme/core/arithmetic/arithmetic.h"
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


//=== UnifiedAddress ==============================================================================

namespace {

	/** 
	 * Extracts the addressed node starting from an unified address 
	 */
	struct NodeExtractorVisitor : public boost::static_visitor<core::NodePtr> {
		template <class T>
		core::NodePtr operator()(const T& addr) const {
			return addr.getAddressedNode();
		}
	};

	struct AddrChildVisitor : public boost::static_visitor<UnifiedAddress> {

		unsigned idx;

		AddrChildVisitor(unsigned idx) : idx(idx) { }

		template <class T>
		UnifiedAddress operator()(const T& addr) const {
			return addr.getAddressOfChild(idx);
		}
	};

} // end anonymous namespace


bool UnifiedAddress::isCFGAddress() const {
	struct checkCFGAddrVisitor : public boost::static_visitor<bool> {
		bool operator()(const cfg::Address&) const { return true; }
		bool operator()(const core::NodeAddress&) const { return false; }
	};
	return boost::apply_visitor(checkCFGAddrVisitor(), address);
}


core::NodePtr UnifiedAddress::getAddressedNode() const {
	return boost::apply_visitor(NodeExtractorVisitor(), address);
}

core::NodeAddress UnifiedAddress::getAbsoluteAddress(const TmpVarMap& varMap) const {
	if (isCFGAddress()) {
		return boost::get<const cfg::Address&>(address).toAbsoluteAddress(varMap);
	}

	return boost::get<const core::NodeAddress&>(address);
}

UnifiedAddress UnifiedAddress::getAddressOfChild(unsigned idx) const {
	return boost::apply_visitor(AddrChildVisitor(idx), address);
}


UnifiedAddress UnifiedAddress::extendAddressFor(const std::vector<unsigned>& idxs) const {

	UnifiedAddress ret = *this;
	for (auto idx : idxs) {
		ret = ret.getAddressOfChild(idx);
	}
	return ret;

}


bool UnifiedAddress::operator==(const UnifiedAddress& other) const {
	if (this == &other) {
		return true;
	}
	if (isCFGAddress() == other.isCFGAddress()) {
		if (isCFGAddress()) {
			return as<cfg::Address>() == other.as<cfg::Address>();
		}
		return as<core::NodeAddress>() == other.as<core::NodeAddress>();
	}
	return false;
}




/**
 * Get the immediate access
 */
AccessPtr getImmediateAccess(NodeManager& mgr, const UnifiedAddress& expr, const TmpVarMap& tmpVarMap, bool final) {

	NodePtr exprNode = expr.getAddressedNode();

	const lang::BasicGenerator& gen = mgr.getLangBasic();

	// A literal is not an access
	if (exprNode->getNodeType() == NT_Literal) {
		throw NotAnAccessException(toString(*exprNode));
	}

	// For cast expressions, we simply recur
	if (exprNode->getNodeType() == NT_CastExpr)
		return getImmediateAccess(mgr, expr.getAddressOfChild(1), tmpVarMap, final);

	// If this is a scalar variable, then return the access to this variable
	if (exprNode->getNodeType() == NT_Variable) {
		return std::make_shared<BaseAccess>(expr, final);
	}
	
	if (exprNode->getNodeType() == NT_TupleExpr || 
		exprNode->getNodeType() == NT_StructExpr ||
		exprNode->getNodeType() == NT_VectorExpr) 
		throw NotAnAccessException(toString(*exprNode));

	assert(exprNode->getNodeType() == NT_CallExpr && 
			"Expected a call expression");

	CallExprPtr callExpr = exprNode.as<CallExprPtr>();
	auto args = callExpr->getArguments();

	if (core::analysis::isCallOf(exprNode, gen.getRefReinterpret()) ||
		core::analysis::isCallOf(exprNode, gen.getScalarToArray())) 
	{
		return getImmediateAccess(mgr, expr.getAddressOfChild(2), tmpVarMap, final);
	}

	// If the callexpr is not a subscript or a member access, then it means this is not
	// a direct memory access, but it could be we are processing a binary operator or other
	// which may contain multiple accesses. Therefore we throw an exception.
	if (!gen.isMemberAccess(callExpr->getFunctionExpr()) &&
			!gen.isSubscriptOperator(callExpr->getFunctionExpr()) &&
			!gen.isRefDeref(callExpr->getFunctionExpr())  &&
			!gen.isRefVar(callExpr->getFunctionExpr()) )
	{
		throw NotAnAccessException(toString(*callExpr));
	}

	auto subAccess = getImmediateAccess(mgr, expr.getAddressOfChild(2), tmpVarMap, false);

	
	if (gen.isRefVar(callExpr->getFunctionExpr())) { return subAccess; }


	if (gen.isRefDeref(callExpr->getFunctionExpr())) {
		return std::make_shared<Deref>(expr, subAccess, final);
	}


	// Handle member access functions
	if ( gen.isMemberAccess(callExpr->getFunctionExpr()) ) {

		// this is a tuple access
		if ( gen.isUnsignedInt( args[1]->getType() ) || gen.isIdentifier( args[1]->getType() ) ) {
			return std::make_shared<Member>(expr, subAccess, args[1].as<LiteralPtr>(), final);
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
			arithmetic::Formula f = arithmetic::toFormula( args[1] );
			if (f.isConstant()) {

				polyhedral::IterationVector iterVec;
				iterVec.add( polyhedral::Iterator( idxVar ) );
				polyhedral::AffineFunction af(
						iterVec, { 1, -static_cast<int>(static_cast<int64_t>(f.getConstantValue())) }
					);

				return std::make_shared<Subscript>(
						expr,
						subAccess,
						final,
						core::NodeAddress(),
						iterVec,
						makeCombiner(utils::Constraint<polyhedral::AffineFunction>(af, utils::ConstraintType::EQ))
					);
			}

			// the access function is not a constant but a function
			auto idxExpr = expr.getAddressOfChild(3);
			auto idxExprAddr = idxExpr.getAbsoluteAddress(tmpVarMap).as<core::ExpressionAddress>();
			
			if ( VariablePtr var = core::dynamic_pointer_cast<const Variable>( idxExprAddr.getAddressedNode() ) ) {
				// if the index expression is a single variable we may be in the case where this
				// variable is an alias for an other expression
				if ( ExpressionAddress aliasExpr = tmpVarMap.getMappedExpr( var ) ) {
					// If this was an alias, use the aliased expression as array access
					idxExprAddr = aliasExpr;
				}
			}

			auto dom = polyhedral::getVariableDomain(idxExprAddr);
			if (dom.first) {

				const polyhedral::IterationVector& oldIter =
					dom.first.getAnnotation(polyhedral::scop::ScopRegion::KEY)->getIterationVector();

				polyhedral::IterationVector iterVec;

				std::for_each(oldIter.iter_begin(), oldIter.iter_end(), [&](const polyhedral::Iterator& iter) {
						iterVec.add( polyhedral::Iterator(iter.getExpr().as<VariablePtr>(), true) );
						});

				iterVec.add( polyhedral::Iterator(idxVar) );

				polyhedral::AffineFunction af(iterVec, core::arithmetic::Formula(idxVar) - f);

				return std::make_shared<Subscript>(
						expr,
						subAccess,
						final,
						dom.first,
						iterVec,
						cloneConstraint(iterVec, dom.second) and
						utils::Constraint<polyhedral::AffineFunction>(af, utils::ConstraintType::EQ)
					);
			}

			return std::make_shared<Subscript>(expr, subAccess);

		} catch (arithmetic::NotAFormulaException&& e) {
			// What if this is a piecewise? we can handle it
			assert (false && "Array access is not a formula");
		}
	}
	assert(false && "Access not supported");
}


namespace {

	void getAddressIndexes(const NodeAddress& addr, std::vector<unsigned>& idxs) {
		if (addr.getDepth() > 2)
			getAddressIndexes(addr.getParentAddress(), idxs);
		if (addr.getDepth() > 1)
			idxs.push_back(addr.getIndex());
	}

} // end empty namespace 

std::vector<AccessPtr> getAccesses(core::NodeManager& mgr, const UnifiedAddress& expr, const TmpVarMap& tmpVarMap) {
	
	struct ExploreAccesses : public IRVisitor<bool, core::Address> {
		
		core::NodeManager& 		mgr;
		const TmpVarMap& 		tmpVarMap;
		const UnifiedAddress& 	base;
		std::vector<AccessPtr>& accesses;

		ExploreAccesses(core::NodeManager& mgr, 
						const TmpVarMap& tmpVarMap,
						const UnifiedAddress& base, 
						std::vector<AccessPtr>& accesses) 
			:  mgr(mgr), tmpVarMap(tmpVarMap), base(base), accesses(accesses) { }

		bool visitVariable(const core::VariableAddress& addr) {
			// turn the address into a vector of indexes from the root 
			std::vector<unsigned> idxs;
			getAddressIndexes(addr, idxs);
			auto accessAddress = base.extendAddressFor(idxs);

			accesses.push_back( getImmediateAccess(mgr, accessAddress, tmpVarMap) );
			return true;
		}

		bool visitCallExpr(const core::CallExprAddress& callExpr) {
			
			const auto& gen = callExpr->getNodeManager().getLangBasic();
			const auto& func = callExpr->getFunctionExpr();

			if (!gen.isBuiltIn(func) && func->getNodeType() == core::NT_Literal) {
				// this is an external function
				return false;
			}

			LOG(INFO) << *callExpr;

			if (gen.isBitwiseOp(func) || gen.isCompOp(func) || gen.isArithOp(func) || 
				gen.isRefAssign(func) || gen.isVarlistPack(func))
			{
				return false;
			}

			std::vector<unsigned> idxs;
			getAddressIndexes(callExpr, idxs);
			auto accessAddress = base.extendAddressFor(idxs);
			
			try {
				accesses.push_back( getImmediateAccess(mgr, accessAddress, tmpVarMap) );
			} catch (NotAnAccessException&& e) { }
			return true;
		}

	};

	core::NodePtr node = expr.getAddressedNode();
	std::vector<AccessPtr> accesses;
	visitDepthFirstPrunable(NodeAddress(node), ExploreAccesses(mgr,tmpVarMap,expr,accesses));

	return accesses;
}

/**
 * Pretty printer for accesses which prints them using indentation for easier read
 */
class AccessPrinter : public RecAccessVisitor<std::string> {

	unsigned level;

	std::string indent(char sep=' ') const {
		return ""; //return std::string(level*4, sep);
	}

public:
	AccessPrinter() : level(0)  { }

	std::string visitBaseAccess(const BaseAccessPtr& access) {
		std::ostringstream ss;
		ss << indent() << (access->isFinal()?"+":"") 
			<< *access->getAddress().getAddressedNode() << "{@" << access->getAddress() << "}";
		return ss.str();
	}

	std::string visitDeref(const DerefPtr& access) {
		std::ostringstream ss;
		ss << indent() << (access->isFinal()?"+":"")  << "deref:{@" << access->getAddress() << "}(";
		++level;
		ss << visit(access->getSubAccess());
		--level;
		ss << indent() << ")";
		return ss.str();
	}

	std::string visitMember(const MemberPtr& access) {
		std::ostringstream ss;
		if (access->getSubAccess()) {
			ss << indent() << (access->isFinal()?"+":"")  << "member{@" << access->getAddress() << "}(";
			++level;
			ss << visit(access->getSubAccess());
		} else {
			ss << "*";
		}
		ss << indent() << "." << *access->getMember();
		--level;
		ss << indent() << ")";
		return ss.str();
	}

	std::string visitSubscript(const SubscriptPtr& access) {
		std::ostringstream ss;
		if (access->getSubAccess()) {
			ss << indent() << (access->isFinal()?"+":"") << "subscript:{@" << access->getAddress() << "}(";
			++level;
			ss << visit(access->getSubAccess());
		} else {
			ss << "*";
		}
		auto rangeStr = access->getRange() ? toString(*access->getRange()) : "unbounded";

		size_t pos;
		while( (pos = rangeStr.find("v4294967295")) != -1) {
			auto it = rangeStr.begin()+pos;
			rangeStr = rangeStr.replace(it, it+(std::string("v4294967295").length()), "i", 1);
		}

		ss << indent() << "[i:" << rangeStr << "]";
		--level;
		ss << indent() << ")";
		return ss.str();
	}
};


bool equalPath(const AccessPtr& lhs, const AccessPtr& rhs) {

	// If both are null ptrs then these are compatible accesses
	if (!lhs && !rhs) { return true;  }
	if (!lhs || !rhs) { return false; }

	// this must hold at this point
	assert ( lhs && rhs );

	// make sure to skip any deref nodes
	if (lhs->getType() == AccessType::AT_DEREF)
		return equalPath(cast<Deref>(lhs)->getSubAccess(), rhs);

	if (rhs->getType() == AccessType::AT_DEREF)
		return equalPath(lhs,cast<Deref>(rhs)->getSubAccess());

	// despite removing derefs, the current component is not the same, therefore the two paths are
	// not equal
	if (lhs->getType() != rhs->getType()) { return false; }

	switch(lhs->getType()) {

		case AccessType::AT_BASE:
			return cast<BaseAccess>(lhs)->getVariable() ==
				cast<BaseAccess>(rhs)->getVariable();

		case AccessType::AT_MEMBER: 
		{
			auto lhsM = cast<Member>(lhs);
			auto rhsM = cast<Member>(rhs);
			return equalPath(lhsM->getSubAccess(),rhsM->getSubAccess()) &&
							 (lhsM->getMember() == rhsM->getMember());
		}

		case AccessType::AT_SUBSCRIPT: 
		{
			auto lhsS = cast<Subscript>(lhs);
			auto rhsS = cast<Subscript>(rhs);

			if(equalPath(lhsS->getSubAccess(), rhsS->getSubAccess())) {

				auto ctx = polyhedral::makeCtx();
				// Build up a set from the contraint expression of the LHS expr
				auto lhsSet = polyhedral::makeSet(ctx, 
						lhsS->getRange() ?
							polyhedral::IterationDomain(lhsS->getRange()) :
							polyhedral::IterationDomain(lhsS->getIterationVector(), false)
					);
				// Build up a set from the contraint expression of the RHS expr
				auto rhsSet = polyhedral::makeSet(ctx, 
						rhsS->getRange() ?
							polyhedral::IterationDomain(rhsS->getRange()) :
							polyhedral::IterationDomain(rhsS->getIterationVector(), false)
					);
				// compute the difference, if it is empty then the two ranges are equivalent 
				return *lhsSet == *rhsSet;
			}
			return false;
		}

		default:
			assert(false && "not supported");
	}
}


AccessPtr switchRoot(const AccessPtr& access, const AccessPtr& newRoot) {

	if (auto baseAccess = std::dynamic_pointer_cast<const BaseAccess>(access)) {
		return newRoot;
	}

	auto decAccess = cast<AccessDecorator>(access);
	assert(decAccess);
	return decAccess->switchSubAccess( switchRoot(decAccess->getSubAccess(), newRoot) );
}

// AccessClass ================================================================

std::ostream& AccessClass::printTo(std::ostream& out) const {
	return out << "AccessClass(" << uid << ")"
		// print list of accesses in this class
		<< " [" << join(",", accesses, [&](std::ostream& jout, const AccessPtr& cur) {
				jout << cur;
				}) << "]"

	// Print the ID of the parent class if any
	<< " PARENT(" << (!parentClass.expired() ?
				utils::numeric_cast<std::string>(parentClass.lock()->getUID()) :
				"NONE"
				)
		<< ")"

		// Print the direct subclasses for this class
		<< " SUB_CLASSES {" << join(",", subClasses,
				[&](std::ostream& jout, const Dependence& cur) {
				jout << std::get<0>(cur).lock()->getUID() << ":" << cast<Access>(std::get<1>(cur));
				})
	<< "}";
}
/** 
 * Given an access class (which contains accesses to the same memory area, this function returns the
 * actual addresses from the root used during the analysis 
 */
std::set<core::ExpressionAddress> extractRealAddresses(const AccessClass& cl, const TmpVarMap& map) {
	std::set<core::ExpressionAddress> ret;
	std::for_each(cl.begin(), cl.end(), [&](const AccessPtr& cur) { 
				if (cur->isFinal())
				 	ret.insert(cur->getAddress().getAbsoluteAddress(map).as<core::ExpressionAddress>()); 
			});
	return ret;
}

// AccessManager ==============================================================




AccessClassSet AccessManager::findClass(const AccessPtr& access) const {

	AccessClassSet ret;
	for (const auto& accClass : classes) {
		for (const auto& acc : accClass->getAccesses()) {
				if (*acc == *access || equalPath(acc,access)) { 
				ret.insert( accClass );
			}
		}
	}

	return ret;
}

void AccessManager::printDotGraph(std::ostream& out) const { 

	// print dot header
	out << "digraph G {" << std::endl;

	for(unsigned idx=0; idx<size(); ++idx) {

		out << "\t" << idx << " [shape=\"box\",label=\"UID: " << classes[idx]->getUID() << "\\n" << 
		    join("\\l", classes[idx]->getAccesses(), [&](std::ostream& jout, const AccessPtr& cur) {
				jout << "+ " << cur;
			}) << "\\l\"];" << std::endl;

		// check dependencies 
		for (const auto& dep : classes[idx]->getSubClasses()) {
		
			out << "\t" << idx << " -> " << std::get<0>(dep).lock()->getUID() 
				<< " [label=\"" << std::static_pointer_cast<const Access>(std::get<1>(dep)) << "\"];" 
				<< std::endl;

		}
	}
	
	out << "}" << std::endl;
}


AccessClassPtr AccessManager::addClass(AccessClassPtr parent, const AccessPtr& access, const AccessDecoratorPtr& dec, bool append_to_parent) {
	// Creates a new alias class  (can't use make_shared because the constructor is private)
	auto newClass = std::shared_ptr<AccessClass>(new AccessClass(std::cref(*this), classes.size(), parent));
	if (access) {
		newClass->storeAccess(access);
	}
	classes.emplace_back( newClass );

	if (append_to_parent && parent) {
		parent->addSubClass( AccessClass::Dependence( newClass, dec ) );
	}
	return newClass;
}


AccessClassSet AccessManager::getClassFor(const AccessPtr& access, bool subAccess) {

	AccessClassSet retClasses;

    /*
     * Iterate through the existing classes and determine whether this access belongs to one of
     * the exising classes, if not create a new class
     */
	for (auto& cl : classes) {

		const auto& classAccesses = cl->accesses;
		bool found=false, belongs=false;

		for(const auto& cur : classAccesses) {
			if (*cur == *access) {
				found = belongs = true; // the access is already in the class, therefore
										// we mark it as found
				break;
			} else if (!belongs && equalPath(cur, access)) {
				belongs = true;
			}
		}

		if (belongs) {
			// the access is already stored in this class, therefore we simply return it
			if (!found) { cl->storeAccess(access); }
			retClasses.insert(cl);
		}
	}

	if (!retClasses.empty()) { return retClasses; }


	// it might be that this access is an alias for an expression for which we already defined a
	// class.
	if (auto potentialAlias =
			core::dynamic_pointer_cast<const core::Variable>(access->getAddress().getAddressedNode())) {

		UnifiedAddress aliasedExpr(tmpVarMap.getMappedExpr( potentialAlias ));

		if (aliasedExpr.getAbsoluteAddress() && cfg) {
			aliasedExpr = cfg->find(aliasedExpr.getAbsoluteAddress());
		}

		if (aliasedExpr.getAbsoluteAddress()) {
			try {
				auto aliasAccess = getImmediateAccess(potentialAlias->getNodeManager(), aliasedExpr, TmpVarMap(), subAccess);
				auto clSet = getClassFor(aliasAccess, subAccess);

				for (auto& cl : clSet) {
					cl->storeAccess(access);
				}

				return clSet;
			} catch (NotAnAccessException&& e) { }
		}
	}

	/**
	 * This might be an access to a subrange of a class.
	 *
	 * This can happen either when a compound member of a struct is accessed. or when the (N-x)th
	 * dimension of a N dimensional array is accessed
	 */
	AccessClassSet 				parentClasses;
	AccessDecoratorPtr 			subLevel;
	
	AccessPtr skipDeref = access;
	// Skips any derefs present through the access. 
	while (skipDeref->getType() == AccessType::AT_DEREF) {
		skipDeref = cast<Deref>(skipDeref)->getSubAccess();
	}

	// Check whether this access is accessing a sublevel 
	if (skipDeref->getType() == AccessType::AT_MEMBER || skipDeref->getType() == AccessType::AT_SUBSCRIPT) {
		parentClasses = getClassFor( cast<AccessDecorator>(skipDeref)->getSubAccess(), false );
		subLevel = cast<AccessDecorator>(skipDeref)->switchSubAccess(AccessPtr());
	}

	// check if the parent class already has a child to represent this type of access
	if (!parentClasses.empty()) {
		assert(subLevel && "Invalid sublevel");

		typedef std::tuple<AccessClassPtr, AccessClassPtr, AccessDecoratorPtr> AccessInfo;
		std::vector<AccessInfo> toAppend;

		for (auto& parentClass : parentClasses) { 
			
			bool classified = false;

			// This is the access used for comparison in the case we are threating 
			// subscript accesses. 
			auto cmpAccess = skipDeref;

			for(auto& dep : parentClass->getSubClasses()) {
				
				const auto& subClass  = std::get<0>(dep).lock();
				const auto& subAccess = std::get<1>(dep);
				assert(subClass && subAccess);

				//LOG(INFO) << subClass;
				//LOG(INFO) << subAccess;
				//LOG(INFO) << *this;

				// assume that the subclasses are disjoints 
				if (subAccess->getType() == AccessType::AT_MEMBER) {

					assert(skipDeref->getType() == AccessType::AT_MEMBER);

					if (*cast<Member>(skipDeref)->getMember() == *cast<Member>(subAccess)->getMember()) {
						subClass->storeAccess(access);
						retClasses.insert( subClass );
						classified = true;
						break;
					}
					continue;
				}

				// Subscripts 
				if (subAccess->getType() == AccessType::AT_SUBSCRIPT) {
					
					assert(skipDeref->getType() == AccessType::AT_SUBSCRIPT);

					auto classRange = cast<Subscript>(subAccess);
					auto accessRange = cast<Subscript>(cmpAccess);

					// Create a polyhedral context so that we can work with sets 
					auto ctx = polyhedral::makeCtx();
					auto classSet  = polyhedral::makeSet(ctx, 
							classRange->getRange() ?
								polyhedral::IterationDomain(classRange->getRange()) :
								polyhedral::IterationDomain(classRange->getIterationVector(), false)
							);
					auto accessSet = polyhedral::makeSet(ctx, 
							accessRange->getRange() ?
								polyhedral::IterationDomain(accessRange->getRange()) :
								polyhedral::IterationDomain(accessRange->getIterationVector(), false)
							);
					
					// compute the difference, if it is empty then the two ranges are equivalent 
					auto intersection = classSet * accessSet;
					// LOG(INFO) << "intersection " << *intersection << " " << ", "; 

					if ( !intersection->empty() ) { 
						
						auto splitAccess = [&]() {
							auto diff = accessSet - intersection;

							polyhedral::IterationVector iv;
							auto diffCons = diff->toConstraint(access->getRoot().getVariable()->getNodeManager(), iv);
								
							// Try to classify the remaining range 
							cmpAccess = std::make_shared<Subscript>(
											cmpAccess->getAddress(),
											cast<Subscript>(cmpAccess)->getSubAccess(),
											cmpAccess->isFinal(),
											cast<Subscript>(cmpAccess)->getContext(),
											iv,
											diffCons
										);

							subLevel = cast<AccessDecorator>(cmpAccess)->switchSubAccess(AccessPtr());
						};

						auto splitClass = [&]() {

							polyhedral::IterationVector iv;
							auto interCons = intersection->toConstraint(
												access->getRoot().getVariable()->getNodeManager(), iv);
	

							// Try to classify the remaining range 
							auto accDecLevel = std::make_shared<Subscript>(
											NodeAddress(),
											AccessPtr(),
											cmpAccess->isFinal(),
											cast<Subscript>(cmpAccess)->getContext(),
											iv,
											interCons
										);
														
							// now change the orginal class by subclassing by the difference 
							auto diff = classSet - intersection;

							polyhedral::IterationVector iv2;
							auto diffCons = diff->toConstraint(
												access->getRoot().getVariable()->getNodeManager(), iv2);

							// Try to classify the remaining range 
							auto diffLevel = std::make_shared<Subscript>(
											NodeAddress(),
											AccessPtr(),
											cmpAccess->isFinal(),
											cast<Subscript>(cmpAccess)->getContext(),
											iv2,
											diffCons
										);
							
							// modify the current access 
							std::get<1>(dep) = diffLevel;

							auto newClass = addClass(parentClass, access, accDecLevel, false);
							
							toAppend.push_back( AccessInfo(newClass, parentClass, accDecLevel) );

							for (auto& acc : subClass->getAccesses()) {
								newClass->storeAccess( acc );
							}

							retClasses.insert( newClass );
						};

						// We hit the same class, add the access to the class  
						if (*intersection == *accessSet && *intersection == *classSet ) 
						{
							subClass->storeAccess(access);
							retClasses.insert( subClass );
							classified = true;
							break;
						}

						// We are in the case where the intersection is equal to the class set. 
						// This means we have to create a new class and append the old class as a subrange 
						if (*intersection == *classSet)  {
							
							// Add a new class representing the intersection
							subClass->storeAccess( access );
							retClasses.insert( subClass );

							splitAccess();
							continue;
						}

						if (*intersection == *accessSet) {

							splitClass();
							classified = true;
							continue;
						}
							
						// We have an intersection, however this is a subset of both the current 
						// access and the subclass selector of the parent class 
						splitClass();
						splitAccess();
					}

				}

			}

			if (!toAppend.empty()) {
				for (const auto& cur : toAppend) 
					std::get<1>(cur)->addSubClass( AccessClass::Dependence( std::get<0>(cur), std::get<2>(cur) ) );
			}

			if (!classified) {
				// the access was not classified 
				retClasses.insert( addClass(parentClass, access, subLevel) );

				if (subLevel->getType() == AccessType::AT_SUBSCRIPT) {

					// if there is an access for a subrange then we have to define a new class
					// which contains the possible remaining accesses 

					auto subAccess = cast<Subscript>(subLevel);

					auto ctx = polyhedral::makeCtx();
					auto universe = polyhedral::makeSet(ctx, 
							polyhedral::IterationDomain(subAccess->getIterationVector())
						);
					auto accessSet = polyhedral::makeSet(ctx, 
							subAccess->getRange() ?
								polyhedral::IterationDomain(subAccess->getRange()) :
								polyhedral::IterationDomain(subAccess->getIterationVector(), false)
							);

					polyhedral::IterationVector iv;
					
					auto diffCons = (universe-accessSet)->toConstraint(
								access->getRoot().getVariable()->getNodeManager(), iv);

					// Try to classify the remaining range 
					auto accDecLevel = std::make_shared<Subscript>(
											NodeAddress(),
											AccessPtr(),
											cmpAccess->isFinal(),
											subAccess->getContext(),
											iv,
											diffCons
										);
														
					addClass(parentClass, AccessPtr(), accDecLevel);
							
				}
				
			}
		}

		return retClasses;
	}

	return { addClass(nullptr, access, subLevel) };
}

std::ostream& AccessManager::printTo(std::ostream& out) const {
	return out << "AccessManager [" << size() << "]\n{\t" <<
		join("\n\t", classes, [&](std::ostream& jout, const AccessClassPtr& cur) {
				jout << *cur;
				}) << std::endl << "}";
}


void addSubClasses(const AccessClassPtr& thisClass, AccessClassSet& collect) {

	for (const auto& cur : thisClass->getSubClasses()) {

		auto thisSubClass = std::get<0>(cur).lock();

		if(collect.insert(thisSubClass).second) {
			addSubClasses(thisSubClass, collect);	
		}
	}
}

AccessClassSet getConflicting(const AccessClassSet& classes) {
	
	AccessClassSet ret;
	for (const auto& classPtr : classes) {
		addSubClasses(classPtr, ret);

		AccessClassPtr cur = classPtr;

		AccessClassPtr parent;
		while ( parent = cur->getParentClass() )  {
			ret.insert(parent);
			cur = parent;
		}
	}
	return ret;

}

} } // end insieme::analysis namespace

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::analysis::AccessPtr& access) {
		return out << insieme::analysis::AccessPrinter().visit(access);
	}

}// end std namespace


