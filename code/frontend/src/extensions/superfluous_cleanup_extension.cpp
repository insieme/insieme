/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/extensions/superfluous_cleanup_extension.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_class_info.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/annotations/c/decl_only.h"
#include "insieme/annotations/c/include.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/tu/ir_translation_unit.h"
#include "insieme/frontend/utils/memalloc.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/frontend/tu/ir_translation_unit_io.h"

#include "insieme/annotations/data_annotations.h"

#include "insieme/utils/assert.h"

#include "insieme/frontend/omp/omp_annotation.h"

#include <functional>

namespace insieme {
namespace frontend {
namespace extensions {
namespace cleanup {

namespace {
using namespace core;
using namespace analysis;

// check if initialization is "safe" for eventual variable removal
// for now, simply only allow newly allocated vars to be removed
bool isSaveVarInitialization(const DeclarationStmtPtr& decl) {
	const lang::BasicGenerator& basic = decl->getNodeManager().getLangBasic();
	if(decl->getInitialization().isa<CallExprPtr>()) {
		CallExprPtr initCall = decl->getInitialization().as<CallExprPtr>();
		return basic.isAllocOp(initCall->getFunctionExpr());
	}
	return false;
}

struct UnneccesaryAssignmentVisitor : IRVisitor<bool, Address> {
	insieme::utils::map::PointerMap<VariablePtr, NodeAddress> declaredVars;
	
	bool visitVariable(const VariableAddress& n) {
		const lang::BasicGenerator& basic = n->getNodeManager().getLangBasic();
		auto parent = n.getParentNode();
		auto parentAddr = n.getParentAddress();
		auto nVar = n.as<VariablePtr>();
		// first check if this is a new declaration
		if(parent.isa<DeclarationStmtPtr>()) {
			// we don't want declarations inside for loops
			if(parentAddr.getDepth()>1 && parentAddr.getParentNode().isa<ForStmtPtr>()) {
				return false;
			}
			// we also need to make sure they aren't aliases to something
			if(!isSaveVarInitialization(parent.as<DeclarationStmtPtr>())) {
				return false;
			}
			declaredVars.insert(std::make_pair(nVar, parentAddr));
			return false;
		}
		if(declaredVars.find(nVar) == declaredVars.end()) {
			return false;
		}
		// check if used as non-deref (and not in an assignment)
		if(!core::analysis::isCallOf(parent, basic.getRefAssign())
		        && !core::analysis::isCallOf(parent, basic.getRefDeref())) {
			declaredVars.erase(nVar);
			return false;
		}
		declaredVars[nVar] = parentAddr;
		return false;
	}
	
	bool visitCallExpr(const CallExprAddress& c) {
		const lang::BasicGenerator& basic = c->getNodeManager().getLangBasic();
		// For assignments, we visit the right hand side first
		// to ensure proper capture of uses in the RHS
		if(core::analysis::isCallOf(c.getAddressedNode(), basic.getRefAssign())) {
			visitDepthFirstPrunable(c->getArgument(1), *this);
			visitDepthFirstPrunable(c->getArgument(0), *this);
			return true;
		}
		return false;
	}
};

// check if the variable is read in some level of any enclosing loop
bool isUsedPreviouslyInEnclosingLoop(NodeAddress nodeAdd, VariablePtr var) {
	const lang::BasicGenerator& basic = var->getNodeManager().getLangBasic();
	NodeAddress outermostLoop = nodeAdd;
	//std::cout << "Checking isUsedPreviouslyInEnclosingLoop with " << *nodeAdd << " and " << *var << "\n";
	visitPathTopDownInterruptible(nodeAdd, [&](const NodeAddress& addr) {
		if(addr.isa<ForStmtAddress>() || addr.isa<WhileStmtAddress>()) {
			outermostLoop = addr;
			return true;
		}
		return false;
	});
	//std::cout << "outermostLoop " << *outermostLoop << "\n";
	if(outermostLoop == nodeAdd) {
		return false;
	}
	bool used = false;
	visitDepthFirstInterruptible(outermostLoop, [&](const VariableAddress& va) {
		//std::cout << "visiting var use " << *va << " (" << va << ") nodeAdd (" << nodeAdd << ") \n";
		if(va >= nodeAdd) {
			return true;
		}
		//std::cout << "-- not >= nodeAdd\n";
		if(va.getAddressedNode() == var) {
			//std::cout << "-- found!\n";
			if(!core::analysis::isCallOf(va.getParentNode(), basic.getRefAssign())) {
				//std::cout << "-- not assignment!\n";
				used = true;
				return true;
			}
		}
		//std::cout << "-- not what we're looking for!\n";
		return false;
	});
	return used;
}

LambdaExprPtr removeUnneccesaryAssignmentsIteration(LambdaExprPtr lambda) {
	auto& nodeMan = lambda->getNodeManager();
	const lang::BasicGenerator& basic = nodeMan.getLangBasic();
	
	UnneccesaryAssignmentVisitor uv;
	visitDepthFirstPrunable(LambdaExprAddress(lambda), uv);
	//std::cout << "decls: " << uv.declaredVars << "\n";
	
	vector<StatementAddress> toRemove;
	for(auto& pair : uv.declaredVars) {
		// if the last occurrence is an assignment we can kill it
		if(core::analysis::isCallOf(pair.second.getAddressedNode(), basic.getRefAssign())) {
			// but only if it's not used in a loop previously
			if(!isUsedPreviouslyInEnclosingLoop(pair.second, pair.first)) {
				// we also need to check if the RHS of the assignment is side-effect free
				auto ass = pair.second.as<CallExprPtr>();
				if(analysis::isSideEffectFree(ass.getArgument(1))) {
					toRemove.push_back(pair.second.as<StatementAddress>());
					if(!basic.isInt(pair.first->getType()) && !(analysis::isRefType(pair.first->getType()) && basic.isInt(getReferencedType(pair.first->getType()))))
						std::cout << "\nRemoving non-int " << *pair.first << "(" << *pair.first->getType() << ") in:\n"
						          << dumpPretty(pair.second) << "\n======================\n";
				}
			}
		}
		// same if its only declared but never used
		if(pair.second.isa<DeclarationStmtAddress>()) {
			auto declAddr = pair.second.as<DeclarationStmtAddress>();
			if(declAddr.getVariable().getAddressedNode() == pair.first) {
				toRemove.push_back(declAddr);
			}
		}
	}
	if(!toRemove.empty()) {
		lambda = transform::remove(nodeMan, toRemove).as<LambdaExprPtr>();
	}
	
	return lambda;
}

struct SuperfluousControlFlowVisitor : IRVisitor<bool, Address> {
	vector<StatementAddress> toDelete;
	
	bool visitForStmt(const ForStmtAddress& adr) {
		auto n = adr.as<ForStmtPtr>();
		if(n->getBody()->getStatements().empty()) {
			if(isSideEffectFree(n->getStart()) && isSideEffectFree(n->getStep()) && isSideEffectFree(n->getEnd())) {
				toDelete.push_back(adr);
			}
			return true;
		}
		return false;
	}
	
	bool visitWhileStmt(const WhileStmtAddress& adr) {
		auto n = adr.as<WhileStmtPtr>();
		if(n->getBody()->getStatements().empty()) {
			if(isSideEffectFree(n->getCondition())) {
				toDelete.push_back(adr);
			}
			return true;
		}
		return false;
	}
	
	bool visitIfStmt(const IfStmtAddress& adr) {
		auto n = adr.as<IfStmtPtr>();
		if(n->getThenBody()->getStatements().empty() && n->getElseBody()->getStatements().empty()) {
			if(isSideEffectFree(n->getCondition())) {
				toDelete.push_back(adr);
			}
			return true;
		}
		return false;
	}
};

LambdaExprPtr removeSuperfluousControlFlowIteration(LambdaExprPtr lambda) {
	SuperfluousControlFlowVisitor sfcfv;
	visitDepthFirstPrunable(LambdaExprAddress(lambda), sfcfv);
	//std::cout << "toDelete: " << sfcfv.toDelete << "\n";
	
	if(!sfcfv.toDelete.empty()) {
		lambda = transform::remove(lambda->getNodeManager(), sfcfv.toDelete).as<LambdaExprPtr>();
	}
	
	return lambda;
}
}

LambdaExprPtr removeObviouslySuperfluousCode(LambdaExprPtr lambda) {
	//std::cout << "\n\n===== Begin:\n\n";
	//dumpPretty(lambda);
	auto ret = lambda;
	do {
		lambda = ret;
		ret = removeUnneccesaryAssignmentsIteration(ret);
		//{
		//	auto errors = checks::check(ret);
		//	if(!errors.empty()) {
		//		std::cout << "=========== removeObviouslySuperfluousCode ============== errors after removeUnneccesaryAssignmentsIteration ========\n";
		//		std::cout << errors;
		//		std::cout << "\n\n===== Before:\n\n";
		//		dumpPretty(lambda);
		//		std::cout << "\n\n===== After:\n\n";
		//		dumpPretty(ret);
		//		assert_fail() << "Errors during cleanup";
		//	}
		//}
		ret = removeSuperfluousControlFlowIteration(ret);
		//{
		//	auto errors = checks::check(ret);
		//	if(!errors.empty()) {
		//		std::cout << "=========== removeObviouslySuperfluousCode ============== errors after removeSuperfluousControlFlowIteration ========\n";
		//		std::cout << errors;
		//		std::cout << "\n\n===== Before:\n\n";
		//		dumpPretty(lambda);
		//		std::cout << "\n\n===== After:\n\n";
		//		dumpPretty(ret);
		//		assert_fail() << "Errors during cleanup";
		//	}
		//}
	}
	while(ret != lambda);
	//dumpPretty(lambda);
	return ret;
}

void removeObviouslySuperfluousCode(tu::IRTranslationUnit& trans) {
	// This parallel version is actually slower
	//#pragma omp parallel
	//#pragma omp single
	//for(auto& pair : trans.getFunctions()) {
	//	#pragma omp task shared(pair, trans)
	//	{
	//		NodeManager mgr;
	//		LambdaExprPtr migratedNode;
	//		#pragma omp critical
	//		migratedNode = mgr.get(pair.second);
	//		auto cleanedNode = removeObviouslySuperfluousCode(migratedNode);
	//		if(cleanedNode != migratedNode) {
	//			#pragma omp critical
	//			trans.getFunctions()[pair.first] = pair.second->getNodeManager().get(cleanedNode);
	//		}
	//	}
	//}
	for(auto& pair : trans.getFunctions()) {
		trans.getFunctions()[pair.first] = removeObviouslySuperfluousCode(pair.second);
	}
}
}

insieme::frontend::tu::IRTranslationUnit SuperfluousCleanupExtension::IRVisit(insieme::frontend::tu::IRTranslationUnit& tu) {
	cleanup::removeObviouslySuperfluousCode(tu);
	return tu;
}

} // extensions
} // frontend
} // insieme
