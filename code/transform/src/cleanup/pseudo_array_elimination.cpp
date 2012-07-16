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

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/logging.h"

namespace insieme { 
namespace transform {

using namespace core;

namespace {

bool isArrayType(const TypePtr& cur) {

	// check for null pointer
	if (!cur) { return false; }

	// accept pure array type
	if (cur->getNodeType() == NT_ArrayType) { return true; }

	// also accept ref/array combination
	if ((cur->getNodeType() == NT_RefType && 
		 cur.as<RefTypePtr>()->getElementType()->getNodeType() == NT_ArrayType)) 
	{
		return true;
	}
	return false;
}

// Given a node, find the enclosing lambda. Because this procedure is started passing a callexpr node
// we are sure to find an enclosing lambda.
LambdaDefinitionAddress findEnclosingLambdaDefinition(const NodeAddress& addr) {
	NodeAddress parent = addr;
	while( (parent = parent.getParentAddress(1)) && (parent->getNodeType() != NT_LambdaDefinition) ) ;
	assert(parent);
	return parent.as<LambdaDefinitionAddress>();
}

} // end anonymous namespace 

core::NodePtr eliminatePseudoArrays(const core::NodePtr& node) {
	auto& mgr = node->getNodeManager();
	IRBuilder builder(mgr);
	auto& basic = mgr.getLangBasic();

	std::map< NodePtr, std::vector<unsigned> > lambdaReplacements;
	std::map< LambdaDefinitionPtr, LambdaDefinitionPtr > lambdaDefinitions;

	utils::set::PointerSet<LambdaPtr> visitedLambdas;

	// Prepare replacement mapper to replace all the call sites of functions for which the signature
	// has been changed 
	std::map<NodeAddress, NodePtr> callExprReplacements;
	utils::map::PointerMap<VariablePtr, VariablePtr> lambdaVarReplacements;

	visitDepthFirst(NodeAddress(node), [&](const CallExprAddress& callExpr) {

		core::ExpressionPtr funcExpr = callExpr->getFunctionExpr();
		
		// if this is not a lambda expr or a variable (which is used for recursive functions)
		// then there is nothing to do 
		if (funcExpr->getNodeType() != NT_LambdaExpr && funcExpr->getNodeType() != NT_Variable) {
			return;
		}

		LambdaDefinitionPtr lambdaDef;
		LambdaPtr			lambda;
		VariablePtr			lambdaVar;

		if (funcExpr->getNodeType() == NT_Variable) { 
			// This might me a recursive call, retrieve the lambda associated to it
			lambdaDef = findEnclosingLambdaDefinition(callExpr);
			lambda = lambdaDef->getDefinitionOf(funcExpr.as<VariablePtr>());
			if (!lambda) {
				// this is a callexpr based on a function pointer, therefore we leave
				return;
			}
			lambdaVar = funcExpr.as<VariablePtr>();
		} else {
			lambdaDef = funcExpr.as<LambdaExprPtr>()->getDefinition();
			lambda = funcExpr.as<LambdaExprPtr>().getLambda();
			lambdaVar = funcExpr.as<LambdaExprPtr>()->getVariable();
		}

		assert(lambdaDef && lambda && lambdaVar);

		/**
		 * If we never visited this lambda then we analyze it to determine the following properties:
		 *  1) one of the argument is an array type
		 *  2) within the body fo the lambdas, only the element 0 of the array is accessed 
		 *  3) the array is never passed to another function as alias
		 *  
		 * when all this conditions holds, then the input array is only utilized as a pointer and we
		 * can replace the type of the variable from ref<array<'a,1>> to ref<ref<'a>>.
		 */
		if (!visitedLambdas.contains(lambda)) { 

			// make sure the same lambda is not visited twice 
			visitedLambdas.insert( lambda );

			// We build an address starting from this lamdba 
			LambdaAddress lambdaAddr( lambda );

			// Prepare replacement map for expressions within the lambda
			std::map<NodeAddress, NodePtr> replacements;

			std::set<VariableAddress> possiblyPseudoArrays;
			// Checks if any of the parameters is an array
			for(const auto& param : lambdaAddr->getParameterList()) {

				if ( !isArrayType(param->getType()) ) { continue; }

				possiblyPseudoArrays.insert(param);
			}

			//LOG(DEBUG) << "Possibily pseudo arrays detected from the lambda signature: { "
			//		   << join(", ", possiblyPseudoArrays, [&](std::ostream& out, const VariableAddress& addr) {
			//				   out << *addr;
			//			   }) << " }";

			// Stores occurrences of use of pseudo-arrays
			typedef std::map<VariableAddress, std::vector<ExpressionAddress>> PseudoArrayOccurrenceMap;

			PseudoArrayOccurrenceMap occurrences;
			// Initialize the array with the current set of possibly pseudo arrays
			for(const auto& cur : possiblyPseudoArrays) { 
				occurrences.insert( { cur, std::vector<ExpressionAddress>( ) } ); 
			}

			// now retrieve all the variables accessed in this block
			visitDepthFirstInterruptible(lambdaAddr->getBody(), makeLambdaVisitor([&](const VariableAddress& var) -> bool {

				// If there are no more array to analysis, simply stop the visitor
				if (occurrences.empty()) { return true; }

				// check whether the variable is one of the pseudo arrays 
				auto fit = find_if(occurrences.begin(), occurrences.end(), 
					[&] (const PseudoArrayOccurrenceMap::value_type& cur) { 
						return cur.first.getAddressedNode() == var.getAddressedNode(); 
					});

				// This variable is not a possible pseudo array, therefore continue visiting
				if (fit == occurrences.end()) return false;

				// We found an usage of one of the possibly pseudo arrays 
				core::NodePtr parent = var.getParentNode();

				// the parent expression can be either a deref, followed by a subscript or a refelement1d.
				if ( core::analysis::isCallOf(parent, basic.getArraySubscript1D()) || 
					 core::analysis::isCallOf(parent, basic.getArrayRefElem1D()) ) 
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
						//LOG(DEBUG) << "Found proper use of pseudo array: '" << *var << "': " 
						//		   << var.getParentNode();		

						fit->second.push_back( var.getParentAddress(1).as<ExpressionAddress>() );
						return false;
					}
				}

				// if the variable is used differently, then we cannot safely retype the variable
				// LOG(DEBUG) << "Removing pseudo-array '" << *var << "' because of access: " << parent;
				occurrences.erase(fit);
				
				// continue if there are still valid pseudo-arrays to analyze
				return occurrences.empty();
			}));

			//LOG(DEBUG) << "Analyiss has returned the following pseudo-arrays: { " 
			//		   << join(", ", occurrences, [&](std::ostream& out, const PseudoArrayOccurrenceMap::value_type& addr) {
			//				   out << *addr.first;
			//			   }) << " }";


			// Variable of type ref<array<'a,1>> will be replaced by ref<ref<'a>>
			std::vector<unsigned> indexes;

			utils::map::PointerMap<VariablePtr, VariablePtr> localVarReplacements;

			for(auto occ : occurrences) {
				TypePtr type = occ.first->getType();

				if (core::analysis::isRefType(type) ) {
					type = core::analysis::getReferencedType(type); 
				}
				
				assert(type->getNodeType() == NT_ArrayType);
		
				type = type.as<ArrayTypePtr>()->getElementType();

				// Create the variable which will be utilized to replace the previous pseudo-array 
				VariablePtr newVar = builder.variable( builder.refType(type) );

				// The index of the variable within the parameter list is its last-level address index 
				unsigned argIdx = occ.first.getIndex();
				indexes.push_back(argIdx);

				// lambda type
				NodeAddress argTypeAddr = lambdaAddr.getType().getAddressOfChild(0).getAddressOfChild(argIdx);
 				replacements.insert( { argTypeAddr, newVar->getType() } );

				// build a new type for the lambda expr
				for(const auto& cur : occ.second) {
					replacements.insert( { cur, newVar } );
				}

				// replace the variable declared in the signature of the lambda expression
				localVarReplacements.insert( { occ.first, newVar } );
			}

			if (!replacements.empty()) { 
				
				NodePtr newLambdaDef = lambdaDef;
				// get latest lambda definition for this node 
				auto fit = lambdaDefinitions.find(lambdaDef);
				if (fit != lambdaDefinitions.end()) {
					newLambdaDef = fit->second;
				}

				NodeAddress addr(lambdaDef);
				LambdaDefinitionAddress laddr = addr.as<LambdaDefinitionAddress>();
				std::vector<LambdaBindingAddress> definitions = laddr->getDefinitions();
				for(LambdaBindingAddress def : definitions) {
					if (def->getVariable().getAddressedNode() == lambdaVar) {
						addr = def->getLambda();
						break;
					}
				}

				NodePtr newLambda = core::transform::replaceAll(mgr, replacements);
				newLambda = core::transform::replaceVars(mgr, newLambda, localVarReplacements);

				// now replace the variable associated to this lambda 
				addr = addr.switchRoot(newLambdaDef);

				lambdaVarReplacements.insert( { lambdaVar, builder.variable(newLambda.as<LambdaPtr>()->getType()) } );
				newLambdaDef = core::transform::replaceNode(mgr, addr, newLambda);
				newLambdaDef = core::transform::replaceVars(mgr, newLambdaDef, lambdaVarReplacements);
				
				lambdaDefinitions[lambdaDef] = newLambdaDef.as<LambdaDefinitionPtr>();

				lambdaReplacements.insert( { lambda, indexes } );

			}
		}
		
		auto fit = lambdaReplacements.find(lambda);

		// If the call expression is to a lambda for which we did no replacements, we simply exit
		if (fit == lambdaReplacements.end()) { return ; }
	
		ExpressionPtr lambdaExpr = funcExpr;

		auto vit = lambdaVarReplacements.find(lambdaVar);
		assert(vit != lambdaVarReplacements.end());

		// IF the lambda was called via the recursive variable, then use the new introduced variable
		if (funcExpr->getNodeType() == NT_Variable) {
			// get the new variable
			lambdaExpr = vit->second; 
		} else {
			// build a lambda expr
			lambdaExpr = LambdaExpr::get(mgr, vit->second, lambdaDefinitions[lambdaDef]);
		}

		std::map<NodeAddress, NodePtr> localCallExprReplacements;
		localCallExprReplacements.insert( { callExpr->getFunctionExpr(), lambdaExpr } );

		// replace the CallExpr
		for ( unsigned idx : fit->second ) {
			NodeAddress arg = callExpr->getArgument(idx);
			
			if (core::analysis::isCallOf(arg, basic.getScalarToArray())) 
	//			analysis::isCallOf(arg, basic.getRefVectorToRefArray())	) 
			{
				localCallExprReplacements.insert ( 
						{ arg, 
						  arg.as<CallExprAddress>().getArgument(0).getAddressedNode()
						}
					);
			} else {
				// FIXME: add an handler for other cases
				// in the case we cannot handle the argument, we switch back to the old call
				// probably creating multi-versioning 
				return ;
			}
		}

		// copy the replacements to the glbal map
		std::copy(localCallExprReplacements.begin(), 
				  localCallExprReplacements.end(), 
				  std::inserter(callExprReplacements, callExprReplacements.begin())
				);


	}, false);
	
	if (callExprReplacements.empty()) { return node; }

	LOG(INFO) << "**** PseudoArrayElimination: Modified " << lambdaReplacements.size() 
			  << " lamdba(s) resulting in " << callExprReplacements.size() 
			  << " call expression(s) modified.";

	return core::transform::replaceAll(mgr, callExprReplacements);
	
}

} // end transform namespace 
} // end insieme namespace 

