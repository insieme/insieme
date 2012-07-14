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

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/transform/connectors.h"
#include "insieme/transform/pattern/ir_pattern.h"

namespace insieme { namespace transform {

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

core::NodePtr removeUnecessaryDerefs(const core::NodePtr& node) {
	
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	// Search for deref operations followed by a var.ref which should be replaced by a noop
	TreePatternPtr&& pattern = 
		aT( irp::callExpr(any, irp::literal(any,"ref.var"), 
				single(irp::callExpr(any, irp::literal(any,"ref.deref"), *any))) 
		  );

	auto&& match = pattern->matchPointer( node );
	if (!match) { return node; }

	// LOG(INFO) << match; // SOON

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



core::NodePtr pointerSema(const core::NodePtr& node) {
	auto& mgr = node->getNodeManager();
	IRBuilder builder(mgr);
	auto& basic = mgr.getLangBasic();

	NodePtr ret = node;

	visitDepthFirstOnce(node, [&](const LambdaExprPtr& lambdaExpr) {

		LambdaExprAddress lambdaExprAddr(lambdaExpr);

		// Prepare replacement mapper 
		std::map<NodeAddress, NodePtr> replacements;

		std::set<VariableAddress> pseudoArrays;
		// Checks if any of the parameters is an array
		for(const auto& param : lambdaExprAddr->getParameterList()) {
			if ( !isArrayType(param->getType()) ) { continue; }
			pseudoArrays.insert(param);
		}

		LOG(INFO) << pseudoArrays;

		typedef std::map<VariableAddress, std::vector<ExpressionAddress>> PseudoArrayOccurrenceMap;

		PseudoArrayOccurrenceMap occurrences;
		for(const auto& cur : pseudoArrays) { occurrences.insert( { cur, std::vector<ExpressionAddress>( ) } ); }

		// now retrieve all the variables accessed in this block
		visitDepthFirstInterruptible(lambdaExprAddr->getBody(), makeLambdaVisitor([&](const VariableAddress& var) -> bool {

			if (occurrences.empty()) { return true; }

			// check whether the variable is one of the pseudo arrays 
			auto fit = find_if(occurrences.begin(), occurrences.end(), 
				[&] (const PseudoArrayOccurrenceMap::value_type& cur) { 
					return cur.first.getAddressedNode() == var.getAddressedNode(); 
				});

			if (fit == occurrences.end()) return false;

			core::NodePtr parent = var.getParentNode();
			LOG(INFO) << *parent;
			// the parent expression can be either a deref, followed by a subscript
			// or a refelement1d.

			if ( analysis::isCallOf(parent, basic.getArraySubscript1D()) || 
				 analysis::isCallOf(parent, basic.getArrayRefElem1D()) ) 
			{
				bool isPseudo = false;
				try {
					auto&& formula = arithmetic::toFormula(parent.as<CallExprPtr>()->getArgument(1));
					isPseudo = formula.isZero(); // fine, array indexed at zero
				}
				catch(arithmetic::NotAFormulaException e) {
					isPseudo = false;
				}
				if (isPseudo) {
					LOG(DEBUG) << "Found a pseudo array: " << var.getParentNode();		
					fit->second.push_back( static_address_cast<const Expression>(var.getParentAddress(1)) );
					return false;
				}
			}
			LOG(INFO) << "Removing array " << var << " because of access " << parent;
			occurrences.erase(fit);

			return occurrences.empty();
		}));

		LOG(INFO) << occurrences;
		// Variable of type ref<array<'a,1>> will be replaced by ref<ref<'a>>

		for(auto occ : occurrences) {
			TypePtr type = occ.first->getType();

			if (analysis::isRefType(type) ) {
				type = analysis::getReferencedType(type); 
			}
			
			assert(type->getNodeType() == NT_ArrayType);
	
			type = type.as<ArrayTypePtr>()->getElementType();
			VariablePtr newVar = builder.variable( builder.refType(builder.refType(type)) );

			// build a new type for the lambda expr
			NodeAddress argTypeAddr = lambdaExprAddr.getType().getAddressOfChild(0).getAddressOfChild(occ.first.getIndex());
			replacements.insert( { argTypeAddr, newVar->getType() } );

			for(const auto& cur : occ.second) {
				replacements.insert( { cur, builder.deref(newVar) } );
			}

			// replace the variable declared in the signature of the lambda expression
			replacements.insert( { occ.first, newVar } );
		}

		if (replacements.empty()) { return; } 

		NodePtr newLambdaExpr = core::transform::replaceAll(mgr, replacements);
		LOG(DEBUG) << newLambdaExpr;

		ret =  core::transform::replaceAll(mgr, ret, lambdaExpr, newLambdaExpr);
	}, false);

	
}

} // end anonymous namespace


core::NodePtr cleanup(const core::NodePtr& node) {

	// start by doing nothing ..
	core::NodePtr res = node;

	// remove unnecessary array indirections
//	res = removePseudoArraysInStructs(res);
	res = normalizeLoops(res);
//	res = removeUnecessaryDerefs(res);
//
	res = pointerSema(res);

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
