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

#include "insieme/transform/ir_cleanup.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace transform {

using namespace insieme::core;

namespace {

// TODO: split elimination of refs and arrays ... one after another

bool isArrayType(const TypePtr& cur) {

	// check for null pointer
	if (!cur) {
		return false;
	}

	// accept pure array type
	if (cur->getNodeType() == NT_ArrayType) {
		return true;
	}

	// also accept ref/array combination
	if ((cur->getNodeType() == NT_RefType && static_pointer_cast<const RefType>(cur)->getElementType()->getNodeType() == NT_ArrayType)) {
		return true;
	}
	return false;
}


core::NodePtr removePseudoArraysInStructs(const core::NodePtr& node) {

	// Step 1) search list of structs containing arrays as elements
	utils::set::PointerSet<StructTypePtr> structs;

	// search for the property
	visitDepthFirstOnce(node, makeLambdaVisitor([&](const NodePtr& cur){
		if (cur->getNodeType() != NT_StructType) {
			return;
		}

		StructTypePtr type = static_pointer_cast<const StructType>(cur);
		if (any(type->getEntries(), [](const core::NamedTypePtr& cur)->bool { return isArrayType(cur->getType()); })) {
			structs.insert(type);
			return;
		}

	}, true));

	// print list of structs
	for_each(structs, [](const StructTypePtr& cur) {
		std::cout << "Current struct: " << *cur << std::endl;
	});

	return node;
}

struct LoopCollector : public core::IRVisitor<void> {
	typedef std::vector<ForStmtPtr> LoopList;

	LoopCollector() : core::IRVisitor<void>(false) { }

	void visitForStmt( const ForStmtPtr& forStmt ) {
		visit(forStmt->getBody());

		loops.push_back(forStmt);
	}

	LoopList operator()(const NodePtr& root) {
		visit(root);
		return loops;
	}

	void visitNode(const core::NodePtr& node) {
		std::for_each(node->getChildList().begin(), node->getChildList().end(),
			[ this ] (core::NodePtr curr){
				this->visit(curr);
			});
	}

private:
	LoopList loops;
};

core::NodePtr normalizeLoops(const core::NodePtr& node) {

	LoopCollector lc;
	LoopCollector::LoopList&& loops = lc(node);

	LOG(DEBUG) << loops.size();
	std::for_each(loops.begin(), loops.end(), [](const ForStmtPtr& cur){
		LOG(DEBUG) << *cur;
	} );

	return node;
}


} // end anonymous namespace


core::NodePtr cleanup(const core::NodePtr& node) {

	// start by doing nothing ..
	core::NodePtr res = node;

	// remove unnecessary array indirections
//	res = removePseudoArraysInStructs(res);
	res = normalizeLoops(res);

	// done
	return res;
}

class PseudoArrayEliminationMapping {
public:
	PseudoArrayEliminationMapping() {

	}

	virtual const NodePtr resolveElement(const NodePtr& ptr) {
		return ptr;
	}
};

core::NodePtr eliminatePseudoArrays(const core::NodePtr& node) {
	auto& mgr = node->getNodeManager();
	auto& basic = mgr.getLangBasic();
	IRBuilder builder(mgr);

	// search for array variable declarations
	visitDepthFirstInterruptible(NodeAddress(node), [&](const DeclarationStmtAddress& curdecl) -> bool {
		auto var = curdecl.getAddressedNode()->getVariable();
		auto type = var->getType();
		auto init = curdecl.getAddressedNode()->getInitialization();

		unsigned numRefs = 0;
		while(auto refType = dynamic_pointer_cast<const RefType>(type)) {
			type = refType->getElementType();
			numRefs++;
		}

		if(type->getNodeType() == NT_ArrayType) {
			bool isPseudoArray = false;
			// array variable, check indexing
			//visitDepthFirstPrunable(curdecl, [&](const CallExprAddress& curcall) -> bool {
			//	if(!isPseudoArray) return true; // stop iteration if determined that it's not a pseudo array
			//	if(basic.isArrayRefElem1D(curcall.getAddressedNode())) {
			//		try {
			//			auto formula = arithmetic::toFormula(curcall->getArgument(1));
			//			if(formula.isZero()) { // fine, array indexed at zero
			//			} else {
			//				isPseudoArray = false;
			//			}
			//		}
			//		catch(arithmetic::NotAFormulaException e) {
			//			isPseudoArray = false;
			//		}
			//	} else { // check if array is passed to a function
			//		if(any(curcall->getArguments(), [&](const ExpressionPtr& arg) { return arg == var; })) {
			//			isPseudoArray = false; // break off for now
			//		}
			//	}
			//	return false;
			//} );
			if(CallExprPtr call = dynamic_pointer_cast<const CallExpr>(init)) {
				if(basic.isArrayCreate1D(call->getFunctionExpr())) {
					try {
						auto formula = arithmetic::toFormula(call->getArgument(1));
						if(formula.isOne()) {
							isPseudoArray = true;
						}
					} catch(arithmetic::NotAFormulaException e) { /* nothing */ }
				}
			}
			LOG(INFO) << "Array " << var << " is pseudo array: " << isPseudoArray;
		}

		return false;
	});

	//visitDepthFirstInterruptible(NodeAddress(node), [&](const CallExprAddress& curcall) -> bool {
	//	for(int argIndex = 0; argIndex < curcall->getArguments().size(); ++argIndex) {
	//		if(CallExprPtr convertcall = dynamic_pointer_cast<const CallExpr>(curcall->getArgument(argIndex))) {
	//			if(basic.isScalarToArray(convertcall->getFunctionExpr())) {
	//				//LOG(INFO) << "**************************************\n====\nparam:\n " << printer::PrettyPrinter(param) << "\n*********************\n";
	//				if(LambdaExprPtr called = dynamic_pointer_cast<const LambdaExpr>(curcall->getFunctionExpr())) {
	//					VariablePtr param = called->getParameterList()[argIndex];
	//					visitDepthFirstInterruptible(NodeAddress(called), [&](const VariableAddress& var) {
	//						if(var.getAddressedNode() == param) {
	//							//LOG(INFO) << "****\n- used:\n " << printer::PrettyPrinter(var.getParentNode()) << "\n";
	//							if(CallExprPtr usecall = dynamic_pointer_cast<const CallExpr>(var.getParentNode())) {
	//								if(basic.isArrayRefElem1D(usecall->getFunctionExpr())) {
	//									try {
	//										auto formula = arithmetic::toFormula(usecall->getArgument(1));
	//										if(formula.isZero()) {
	//											LOG(INFO) << "- used in array.ref.elem.1D: OK";
	//										} else {
	//											LOG(INFO) << "- used in array.ref.elem.1D with: " << formula;
	//										}
	//									} catch(arithmetic::NotAFormulaException e) {
	//										LOG(INFO) << "- used in array.ref.elem.1D with non-formula: " << usecall;
	//									}
	//								} else {
	//									LOG(INFO) << "- used in unexpected call: " << usecall;
	//								}
	//							} else {
	//								LOG(INFO) << "****\n- used in non-call: " << printer::PrettyPrinter(var.getParentNode());
	//							}
	//						}
	//						return false
	//					});
	//				}
	//			}
	//		}
	//	}
	//	return false;
	//});

	return node;
}

} // end of namespace transform
} // end of namespace insieme
