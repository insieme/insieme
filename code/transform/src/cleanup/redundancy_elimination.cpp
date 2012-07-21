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

#include "insieme/analysis/defuse_collect.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_mapper.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/annotation.h"
#include "insieme/utils/logging.h"

#include <map>

using namespace insieme;
using namespace insieme::core;

// TODO: move to general analysis (and improve)
namespace {

struct SideEffectFreeFlag {
	bool sef;
	bool operator==(const SideEffectFreeFlag& other) const { return true; }
	SideEffectFreeFlag(bool sef) : sef(sef) { }
};

class SideEffectCheckVisitor : public core::IRVisitor<bool, Pointer> {
	const static char* SIDE_EFFECT_FREE_C_FUNCTIONS[];

public:
	SideEffectCheckVisitor() : core::IRVisitor<bool, Pointer>(true) {}

	bool visitNode(const NodePtr& node) {
		if(node->getNodeCategory() == NC_Type) return true;
		if(node->hasAttachedValue<SideEffectFreeFlag>()) return node->getAttachedValue<SideEffectFreeFlag>().sef;
		bool sideEffectFree = true;
		if(const CallExprPtr call = dynamic_pointer_cast<const CallExprPtr>(node)) sideEffectFree = checkCall(call);
		else for_each(node.getChildList(), [&](const NodePtr arg) { sideEffectFree = sideEffectFree && this->visit(arg); });
		node->attachValue(SideEffectFreeFlag(sideEffectFree));
		return sideEffectFree;
	}

	bool checkCall(const CallExprPtr& call);
};
const char* SideEffectCheckVisitor::SIDE_EFFECT_FREE_C_FUNCTIONS[] = {
	"cos", "sin", "tan", "acos", "asin", "atan", "atan2", "cosh", "sinh", "tanh", 
	"exp", "frexp", "ldexp", "log", "log10", "modf", "pow", "sqrt", "ceil", "fabs", "floor", "fmod" };
bool SideEffectCheckVisitor::checkCall(const CallExprPtr& call) {
	bool sideEffectFree = false;
	const core::lang::BasicGenerator& basic = call->getNodeManager().getLangBasic();
	ExpressionPtr fun = call->getFunctionExpr();
	if(basic.isPure(fun)) 
		sideEffectFree = true;
	LiteralPtr litFun = dynamic_pointer_cast<const Literal>(fun);
	if(litFun) {
		const string& name = litFun->getStringValue();
		for(unsigned i=0; i<sizeof(SIDE_EFFECT_FREE_C_FUNCTIONS)/sizeof(const char*); ++i) {
			if(name == SIDE_EFFECT_FREE_C_FUNCTIONS[i]) {
				sideEffectFree = true;
				break;
			}
		}
	}
	if(sideEffectFree) {
		// visit arguments
		for_each(call->getArguments(), [&](const ExpressionPtr arg) { sideEffectFree = sideEffectFree && this->visit(arg); });
	}
	return sideEffectFree;
}

class RedundancyMapper : protected insieme::core::transform::CachedNodeMapping {
	core::NodeManager& mgr;
	unsigned removedCount;

	core::NodePtr eliminateRedundantAssigns(const LambdaPtr& lambda) {
		NodePtr ret = lambda;
		vector<StatementAddress> stmtsToRemove;
		analysis::RefList refs;
		bool refsCollected = false;
		visitDepthFirstPrunable(NodeAddress(lambda), [&](const NodeAddress& node) -> bool {
			// prune if reached a type or lambda expression
			if(node->getNodeCategory() == NC_Type) return true;
			if(node->getNodeType() == NT_LambdaExpr) return true;
			// act in case a call is found
			CallExprAddress call = dynamic_address_cast<const CallExpr>(node); 
			if(!call) return false;
			if(mgr.getLangBasic().isRefAssign(call->getFunctionExpr().getAddressedNode())) {
			
				// + checking if assignment target is variable
				ExpressionPtr target = call->getArgument(0);
				VariablePtr targetVar = dynamic_pointer_cast<const Variable>(target);
				if(!targetVar) return false;

				// + check if passed to function
				if(containsPtrToTarget(lambda->getParameterList(), targetVar)) return false;

				// + check if RHS expression is side-effect free
				ExpressionPtr valueExp = call->getArgument(1);
				SideEffectCheckVisitor secv;
				if(!secv.visit(valueExp)) return false;

				// + check if variable is used later
				if(!refsCollected) {
					refs = analysis::collectDefUse(lambda);
					//for(auto it = refs.begin(); it != refs.end(); ++it) {
					//	analysis::RefPtr ref = *it;
					//	LOG(INFO) << "Ref: " << *ref->getBaseExpression() << " //\n" << *ref;
					//}
					refsCollected = true;
				}
				bool startCheck = false;
				for(auto it = refs.begin(); it != refs.end(); ++it) {
					analysis::RefPtr ref = *it;
					//if(targetVar->getId() == 169) LOG(INFO) << startCheck << " // " << ref->getUsage() << " // " << ref->getBaseExpression().getAddressedNode();
					if(*ref->getBaseExpression().getAddressedNode() != *targetVar) continue;
					if(!startCheck) {
						// find def corresponding to assignment
						NodeAddress currentInstanceAddress = ref->getBaseExpression().getFirstParentOfType(NT_CallExpr);
						if(currentInstanceAddress == call) startCheck = true;
						// otherwise check if shared path contains a loop (in lieu of real control flow analysis)
						else if(currentInstanceAddress) {
							bool loopy = false;
							auto loopCheckVisitor = makeLambdaVisitor([&](const NodeAddress& addr) { 
								NodeType nt = addr->getNodeType();
								if(nt == NT_ForStmt || nt == NT_WhileStmt) loopy = true;
							});
							visitSharedPathTopDown(call, currentInstanceAddress, loopCheckVisitor);
							if(loopy) startCheck = true;
						}
					}
					if(startCheck) { // no, this is not the same as "else"
						// check if any use after assignment (to do more exact check we need control flow analysis)
						if((ref->getUsage() == analysis::Ref::USE) || (ref->getUsage() == analysis::Ref::UNKNOWN)) return false;
					}
				}
				stmtsToRemove.push_back(call);
			
				// some informative output if we are removing statements not added by the frontend
				bool hasCeil = false;
				visitDepthFirstOnceInterruptible(valueExp, [&](const CallExprPtr& call) { 
					LiteralPtr litFun = dynamic_pointer_cast<const Literal>(call->getFunctionExpr());
					if(litFun && litFun->getStringValue() == "ceil") hasCeil = true;
					return hasCeil;
				});
				if(!hasCeil) {
					LOG(INFO) << "** Cleanup: removable assignment *not* generated from loop: " << printer::PrettyPrinter(call.getAddressedNode());
				}
			}
			return false;
		});
		if(!stmtsToRemove.empty()) {
			ret = transform::remove(mgr, stmtsToRemove);
			removedCount += stmtsToRemove.size();
		}

		return ret;
	} 

public:
	RedundancyMapper(core::NodeManager& mgr) : mgr(mgr), removedCount(0) {
	};

	virtual const NodePtr resolveElement(const NodePtr& node) {
		if(node->getNodeCategory() == NC_Type) return node;
		if(node->getNodeCategory() == NC_Value) return node;
		NodePtr newNode = node->substitute(mgr, *this);
		if(LambdaPtr lambda = dynamic_pointer_cast<const Lambda>(newNode)) {
			newNode = eliminateRedundantAssigns(lambda);
		}
		if(newNode != node) transform::utils::migrateAnnotations(node, newNode);
		return newNode;
	}

	template<class T>
	Pointer<T> eliminateAssignments(const Pointer<T>& root) {
		removedCount = 0;
		Pointer<T> ret = map(root);
		LOG(INFO) << "**** Cleanup: removed " << removedCount << " unique assignments.";
		return ret;
	}
};

} // anonymous namespace

namespace insieme {
namespace transform {
// TODO: extend to non-scalar assignments
core::NodePtr eliminateRedundantAssignments(core::NodePtr root) {

	NodePtr in = root;
	NodePtr out = root;
	do {
		in = out;
		out = RedundancyMapper(root->getNodeManager()).eliminateAssignments(in);
	} while(*in != *out);

	return out;
}

} // namespace transform
} // namespace insieme
