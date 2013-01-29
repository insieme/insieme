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

#include "insieme/analysis/access/access.h"
#include "insieme/analysis/access/visitor.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/analysis/polyhedral/iter_dom.h"
#include "insieme/analysis/polyhedral/backend.h"
#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/utils/logging.h"

using namespace insieme::core;




namespace insieme {
namespace analysis {
namespace access {

	bool Access::isReference() const {
		return addr.getAddressedNode().as<core::ExpressionPtr>()->getType()->getNodeType() == core::NT_RefType; 
	}

	//========================== PrettyPrinting ===============================================
	std::ostream& BaseAccess::printTo(std::ostream& out) const {
		return out << (isFinal()?"+":"") 
				   << *getAddress().getAddressedNode() << "{@" << getAddress() << "}";

	}

	std::ostream& Ref::printTo(std::ostream& out) const {
		return out << "ref:{@" << getAddress() << "}(" << getSubAccess() << ")";
	}

	std::ostream& Deref::printTo(std::ostream& out) const {
		return out << (isFinal()?"+":"")  << "deref:{@" << getAddress() << "}("
				   << getSubAccess() << ")";
	}

	std::ostream& Member::printTo(std::ostream& out) const { 
		if (getSubAccess()) {
			out << (isFinal()?"+":"")  << "member{@" << getAddress() << "}(";
			out << getSubAccess();
		} else {
			out << "*";
		}
		out << "." << getMember() << ")";
		return out;
	}

	std::ostream& Subscript::printTo(std::ostream& out) const {
		if (getSubAccess()) {
			out << (isFinal()?"+":"") << "subscript:{@" << getAddress() << "}(";
			out << getSubAccess();
		} else {
			out << "*";
		}
		auto rangeStr = getRange() ? toString(*getRange()) : "unbounded";

		size_t pos;
		while( (pos = rangeStr.find("v4294967295")) != -1) {
			auto it = rangeStr.begin()+pos;
			rangeStr = rangeStr.replace(it, it+(std::string("v4294967295").length()), "i", 1);
		}

		out << "[i:" << rangeStr << "]"  << ")";

		return out;
	}
	//==========================================================================================

	AccessPtr switchRoot(const AccessPtr& access, const AccessPtr& newRoot) {

		if (auto baseAccess = std::dynamic_pointer_cast<const BaseAccess>(access)) {
			return newRoot;
		}

		auto decAccess = cast<AccessDecorator>(access);
		assert(decAccess);
		return decAccess->switchSubAccess( switchRoot(decAccess->getSubAccess(), newRoot) );
	}

	namespace {

		// handle subscript expressions 
		SubscriptPtr extractArrayAccess(core::NodeManager& 		mgr, 
										const UnifiedAddress& 	expr,
								 		const AccessPtr& 		subAccess, 
										const TmpVarMap& 		tmpVarMap,
										bool 					final) 
		{

			const lang::BasicGenerator& gen = mgr.getLangBasic();

			// Create the variable used to express the range information for this access.
			// Because we need to be able to compare accesses of different arrays for inclusion we need
			// to make sure the variable used to access the array i (i.e. A[i]) is the same for all the
			// generated accesses. This is obtained using a variable whose ID is very large

			core::VariablePtr idxVar =
				core::Variable::get(mgr, gen.getUInt8(), std::numeric_limits<unsigned int>::max());

			try {

				// the access function is not a constant but a function
				auto idxExpr 	 = expr.getAddressOfChild(3);
				auto idxExprAddr = idxExpr.getAbsoluteAddress(tmpVarMap).as<core::ExpressionAddress>();

				// Extract the formula from the argument 1
				arithmetic::Formula f = arithmetic::toFormula( idxExprAddr.getAddressedNode() );

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

				auto dom = polyhedral::getVariableDomain(idxExprAddr);
				if (dom.first) {

					const polyhedral::IterationVector& oldIter =
						dom.first.getAnnotation(polyhedral::scop::ScopRegion::KEY)->getIterationVector();


					polyhedral::IterationVector iterVec;

					std::for_each(oldIter.iter_begin(), oldIter.iter_end(), 
						[&](const polyhedral::Iterator& iter) {
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

	} // end anonymous namespace 

	/**
	 * Analyze an IR expression and creates an Access descriptor representing the expression. 
	 */
	AccessPtr getImmediateAccess(NodeManager& mgr, 
							     const UnifiedAddress& expr, 
								 const TmpVarMap& tmpVarMap, 
								 bool final) 
	{

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

		assert(exprNode->getNodeType() == NT_CallExpr && "Expected a call expression");

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
				!gen.isRefVar(callExpr->getFunctionExpr()) && 
				!gen.isRefNew(callExpr->getFunctionExpr()) )
		{
			throw NotAnAccessException(toString(*callExpr));
		}

		auto subAccess = getImmediateAccess(mgr, expr.getAddressOfChild(2), tmpVarMap, false);

		auto funcExpr = callExpr->getFunctionExpr();

		/* The variable is weapped into a ref.var or ref.new expression */
		if (gen.isRefVar(funcExpr) || gen.isRefNew(funcExpr)) { return std::make_shared<Ref>(expr, subAccess); }

		/* This is a deref expression */
		if (gen.isRefDeref(funcExpr)) { return std::make_shared<Deref>(expr, subAccess, final); }

		// Handle member access functions
		if ( gen.isMemberAccess(funcExpr) ) {
			// this is a tuple access
			if ( gen.isUnsignedInt( args[1]->getType() ) || gen.isIdentifier( args[1]->getType() ) ) {
				return std::make_shared<Member>(expr, subAccess, args[1].as<LiteralPtr>(), final);
			}
			assert( false && "Type of member access not supported" );
		}

		// Handle Array/Vector subscript operator
		if ( gen.isSubscriptOperator(funcExpr) ) {
			return extractArrayAccess(mgr, expr, subAccess, tmpVarMap, final);
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

		class SubscriptVisitor : public RecAccessVisitor<void> {

			core::NodeManager&	mgr;
			AccessVector& 		ret;	
			const TmpVarMap& 	tmpVarMap;

		public:
			SubscriptVisitor(core::NodeManager& mgr, AccessVector& ret, const TmpVarMap& tmpVarMap) : 
				mgr(mgr), ret(ret), tmpVarMap(tmpVarMap) { }

		private:
			void visitBaseAccess(const BaseAccessPtr& access) { }

			// whenever we have a subscript operation we take the address to the subscript
			// expression and extract variables from there 
			void visitSubscript(const SubscriptPtr& access) {
				// access the second argument of the call expression which contains the index
				// expression
				auto idxExpr = access->getAddress().getAddressOfChild(3);

				auto sub = getAccesses(mgr, idxExpr, tmpVarMap);
				std::copy(sub.begin(), sub.end(), std::back_inserter(ret));
			}
		};


	} // end empty namespace 

	/** 
	 * Given an expression, this method scans and returns a vector of (top-level) accesses within the expression
	 */
	AccessVector getAccesses(core::NodeManager& mgr, const UnifiedAddress& expr, const TmpVarMap& tmpVarMap) {
		
		struct ExploreAccesses : public IRVisitor<bool, core::Address> {
			
			core::NodeManager& 		mgr;
			const TmpVarMap& 		tmpVarMap;
			const UnifiedAddress& 	base;
			std::vector<AccessPtr>& accesses;

			ExploreAccesses(core::NodeManager& 		mgr, 
							const TmpVarMap& 		tmpVarMap,
							const UnifiedAddress& 	base, 
							std::vector<AccessPtr>& accesses) 
				:  mgr(mgr), tmpVarMap(tmpVarMap), base(base), accesses(accesses) { }

			bool visitVariable(const core::VariableAddress& addr) {
				// turn the address into a vector of indexes from the root 
				std::vector<unsigned> idxs;
				getAddressIndexes(addr, idxs);

				// update the unified address representation to point to the current addr 
				auto accessAddress = base.extendAddressFor(idxs);

				accesses.push_back( getImmediateAccess(mgr, accessAddress, tmpVarMap) );
				return true;
			}

			bool visitCallExpr(const core::CallExprAddress& callExpr) {
				
				const auto& gen  = callExpr->getNodeManager().getLangBasic();
				const auto& func = callExpr->getFunctionExpr();

				if (!gen.isBuiltIn(func) && func->getNodeType() == core::NT_Literal) {
					// this is an external function
					return false;
				}

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

					auto& last = accesses.back(); 
					SubscriptVisitor sv(mgr, accesses, tmpVarMap);
					sv.visit(last);

				} catch (NotAnAccessException&& e) { }

				return true;
			}

		};

		core::NodePtr node = expr.getAddressedNode();
		std::vector<AccessPtr> accesses;
		visitDepthFirstPrunable(NodeAddress(node), ExploreAccesses(mgr,tmpVarMap,expr,accesses));

		return accesses;
	}



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


} // end access namespace 
} // end analysis namespace 
} // end insieme namespace 

namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::analysis::access::AccessPtr& access) {
		return out << toString(*access);
	}

}// end std namespace
