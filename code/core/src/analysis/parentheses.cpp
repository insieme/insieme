/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/analysis/parentheses.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/reference.h"

namespace insieme {
namespace core {
namespace analysis {

	//precedence_container.first => precedence;
	//precedence_container.second => associativity;
	using PrecedenceContainer = std::pair<int,int>;
	using PrecedenceMap = std::map<NodePtr, PrecedenceContainer>;

	PrecedenceMap createPrecedenceMap(NodeManager& nm) {

		auto& lang = nm.getLangBasic();
		auto& refs = nm.getLangExtension<lang::ReferenceExtension>();

		//TODO: check why using the attached value is slower than recreating the map everytime?!?!
		//if (nm.hasAttachedValue<precedence_map>()) return nm.getAttachedValue<precedence_map>();

		PrecedenceMap m;

		m[refs.getGenPostInc()] = {2,0};
		m[refs.getGenPostDec()] = {2,0};
		m[lang.getCompositeMemberAccess()] = {2,0};
		m[refs.getRefMemberAccess()] = {2,0};
		m[refs.getRefDeref()] = {3,0};
		m[lang.getBoolLNot()] = {3,1};
		m[lang.getSignedIntNot()] = {3,1};
		m[lang.getUnsignedIntNot()] = {3,1};
		m[refs.getGenPreInc()] = {3,1};
		m[refs.getGenPreDec()] = {3,1};
		m[lang.getSignedIntMul()] = {5,0};
		m[lang.getGenMul()] = {5,0};
		m[lang.getUnsignedIntMul()] = {5,0};
		m[lang.getRealMul()] = {5,0};
		m[lang.getCharMul()] = {5,0};
		m[lang.getSignedIntMod()] = {5,0};
		m[lang.getGenMod()] = {5,0};
		m[lang.getUnsignedIntMod()] = {5,0};
		m[lang.getCharMod()] = {5,0};
		m[lang.getSignedIntDiv()] = {5,0};
		m[lang.getGenDiv()] = {5,0};
		m[lang.getUnsignedIntDiv()] = {5,0};
		m[lang.getRealDiv()] = {5,0};
		m[lang.getCharDiv()] = {5,0};
		m[lang.getSignedIntAdd()] = {6,0};
		m[lang.getGenAdd()] = {6,0};
		m[lang.getUnsignedIntAdd()] = {6,0};
		m[lang.getRealAdd()] = {6,0};
		m[lang.getCharAdd()] = {6,0};
		m[lang.getSignedIntSub()] = {6,0};
		m[lang.getGenSub()] = {6,0};
		m[lang.getUnsignedIntSub()] = {6,0};
		m[lang.getRealSub()] = {6,0};
		m[lang.getCharSub()] = {6,0};
		m[lang.getSignedIntLShift()] = {7,0};
		m[lang.getGenLShift()] = {7,0};
		m[lang.getSignedIntRShift()] = {7,0};
		m[lang.getGenRShift()] = {7,0};
		m[lang.getUnsignedIntLShift()] = {7,0};
		m[lang.getUnsignedIntRShift()] = {7,0};
		m[lang.getSignedIntLt()] = {8,0};
		m[lang.getGenLt()] = {8,0};
		m[lang.getRealLt()] = {8,0};
		m[lang.getUnsignedIntLt()] = {8,0};
		m[lang.getCharLt()] = {8,0};
		m[lang.getSignedIntLe()] = {8,0};
		m[lang.getGenLe()] = {8,0};
		m[lang.getRealLe()] = {8,0};
		m[lang.getUnsignedIntLe()] = {8,0};
		m[lang.getCharLe()] = {8,0};
		m[lang.getSignedIntGt()] = {8,0};
		m[lang.getGenGt()] = {8,0};
		m[lang.getRealGt()] = {8,0};
		m[lang.getUnsignedIntGt()] = {8,0};
		m[lang.getCharGt()] = {8,0};
		m[lang.getSignedIntGe()] = {8,0};
		m[lang.getGenGe()] = {8,0};
		m[lang.getRealGe()] = {8,0};
		m[lang.getUnsignedIntGe()] = {8,0};
		m[lang.getCharGe()] = {8,0};
		m[lang.getSignedIntEq()] = {9,0};
		m[lang.getGenEq()] = {9,0};
		m[lang.getRealEq()] = {9,0};
		m[lang.getUnsignedIntEq()] = {9,0};
		m[lang.getCharEq()] = {9,0};
		m[lang.getBoolEq()] = {9,0};
		m[lang.getTypeEq()] = {9,0};
		m[lang.getSignedIntNe()] = {9,0};
		m[lang.getGenNe()] = {9,0};
		m[lang.getUnsignedIntNe()] = {9,0};
		m[lang.getCharNe()] = {9,0};
		m[lang.getBoolNe()] = {9,0};
		m[lang.getRealNe()] = {9,0};
		m[lang.getSignedIntAnd()] = {10,0};
		m[lang.getGenAnd()] = {10,0};
		m[lang.getUnsignedIntAnd()] = {10,0};
		m[lang.getBoolAnd()] = {10,0};
		m[lang.getSignedIntXor()] = {11,0};
		m[lang.getGenXor()] = {11,0};
		m[lang.getUnsignedIntXor()] = {11,0};
		m[lang.getBoolXor()] = {11,0};
		m[lang.getSignedIntOr()] = {12,0};
		m[lang.getGenOr()] = {12,0};
		m[lang.getUnsignedIntOr()] = {12,0};
		m[lang.getBoolOr()] = {12,0};
		m[lang.getBoolLAnd()] = {13,0};
		m[lang.getBoolLOr()] = {14,0};

		//attach the map to the node manager
		//nm.attachValue<precedence_map>(m);
		//assert_true(nm.hasAttachedValue<precedence_map>()) << "cannot attach precedence map to node manager.";

		return m;
	}

	namespace {

		/**
		 * Helper-function to determine the closest operator
		 */
		CallExprAddress getEnclosingOperatorCall(const NodeAddress& adr) {
			// check for target
			if (auto call = adr.isa<CallExprAddress>()) {
				if (call.isRoot() || !call.getParentNode().isa<BindExprPtr>()) {
					return call;
				}
			}

			// check for root
			if (!adr || adr.isRoot()) {
				return CallExprAddress();
			}

			// otherwise: keep walking
			return getEnclosingOperatorCall(adr.getParentAddress());
		}

	}


	/**
	 * This function check an Address for its parents and children and determines,
	 * if a parentheses is necessary or not.
	 *
	 * @param cur Address to be checked
	 * @return true, if there is the need for parentheses, false otherwise
	 */
	bool needsParentheses(const CallExprAddress& cur) {

		const PrecedenceMap& pm = createPrecedenceMap(cur->getNodeManager());

		// in the case, where the operation is already the root
		// there is no need for parentheses
		if(cur.getDepth()<=2) {
			return false;
		}

		// In every other case we need to check the parent,
		// to determine whether we need parentheses or not.
		if(auto enclosingOp = getEnclosingOperatorCall(cur.getParentAddress(2))) {

			// look up precedences
			auto curOp = pm.find(cur->getFunctionExpr());
			auto parentOp = pm.find(enclosingOp->getFunctionExpr());

			// check whether precedences could be obtained
			if (curOp == pm.end() || parentOp == pm.end()) {
				return false;
			}

			// If the precedence of the parent element is higher,
			// we need braces for the current
			if (parentOp->second.first < curOp->second.first)
				return true;

			// If the precedence of the two elements is the same,
			// we need to determine other things
			if (parentOp->second.first == curOp->second.first) {

				// put parentheses if cur is the second argument of the parent
				auto parentAddress = cur.getParentAddress(2);
				auto parentCall = parentAddress.isa<CallExprPtr>();
				if(parentCall && parentCall->getNumArguments() > 1) {
					if(cur.as<CallExprPtr>() == parentCall->getArgument(1)) {
						return true;
					}
				}
			}
		}

		return false;
	}
}
}
}
