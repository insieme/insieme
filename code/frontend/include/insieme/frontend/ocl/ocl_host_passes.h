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

#pragma once

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/ir_address.h"

#include "insieme/frontend/program.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace frontend {
namespace ocl {

typedef insieme::utils::map::PointerMap<core::ExpressionPtr, size_t > EquivalenceMap;

/**
 * A visitor that checks if two variables lie on the same path in the ast
 */


/**
 * This specialized hasher hashes array accesses to the same variable to the same bin
 * regardless of the array index
 */
struct hash_target_specialized : public hash_target<core::ExpressionPtr> {

	core::IRBuilder builder;
	EquivalenceMap& eqMap;

	hash_target_specialized(core::IRBuilder build, EquivalenceMap& equivalenceMap) : hash_target(), builder(build), eqMap(equivalenceMap) {}

	/**
	 * Computes the hash value of the given pointer based on the target it is pointing to. For subscript operations only the subscripted variable/call
	 * is considered
	 */
	std::size_t operator()(const core::ExpressionPtr expr) const {
		if(!expr)
			return 0;

		if(const core::VariablePtr var = dynamic_pointer_cast<const core::Variable>(expr)){
/*			if(eqMap.find(var) != eqMap.end()) {
				return hasher(*builder.uintLit(eqMap[var]));
			}
			else*/
				return 0; // all variables are mapped to the same bin
		}

		const core::CallExprPtr& call = dynamic_pointer_cast<const core::CallExpr>(expr);

		if(!call)
			return hasher(*expr);

//		while(const core::CallExprPtr& tmp = dynamic_pointer_cast<const core::CallExpr>(call->getArgument(0)))
//			call = tmp;

		if(builder.getNodeManager().getLangBasic().isSubscriptOperator(call->getFunctionExpr()))
			return this->operator()(call->getArgument(0));

		if(builder.getNodeManager().getLangBasic().isMemberAccess(call->getFunctionExpr())) {
			// the type argument can be ignored since it should always be related to the identifier/index
			return this->operator()(call->getArgument(0)) + this->operator()(call->getArgument(1));
		}

		return hasher(*expr);
	}
};

/**
 * compares two Expressions. Returns true if they are equal or if they are both a SubscriptOperator on the same variable,
 * regardless of the index
 */
struct equal_variables {// : public std::binary_function<const core::ExpressionPtr&, const core::ExpressionPtr&, bool> {
	const core::IRBuilder& builder;
	const core::ProgramPtr& root;

	equal_variables(const core::IRBuilder& build, const core::ProgramPtr& program) : builder(build), root(program) {}

	/**
	 * Performs the actual comparison by using the operator== of the generic
	 * pointer type.
	 *
	 * @param x the pointer to the first element to be compared
	 * @param y the pointer to the second element to be compared
	 */
	bool operator()(const core::ExpressionPtr& x, const core::ExpressionPtr& y) const {
		if(x == y || *x == *y)
			return true;

		core::CallExprPtr xCall = dynamic_pointer_cast<const core::CallExpr>(x);
		core::CallExprPtr yCall = dynamic_pointer_cast<const core::CallExpr>(y);

		// remove deref operation
		// TODO of questionable use, maybe remove?
/*		if(!!xCall && builder.getNodeManager().getLangBasic().isRefDeref(xCall->getFunctionExpr())) {
			return this->operator ()(xCall->getArgument(0), y);
		}
		if(!!yCall && builder.getNodeManager().getLangBasic().isRefDeref(yCall->getFunctionExpr())) {
			return this->operator ()(x, yCall->getArgument(0));
		}
*/

		if(!!xCall && builder.getNodeManager().getLangBasic().isScalarToArray(xCall->getFunctionExpr()))
			return this->operator ()(xCall->getArgument(0), y);

		if(!!yCall && builder.getNodeManager().getLangBasic().isScalarToArray(yCall->getFunctionExpr()))
			return this->operator ()(x, yCall->getArgument(0));

		if(!!xCall && builder.getNodeManager().getLangBasic().isSubscriptOperator(xCall->getFunctionExpr()))
			return this->operator ()(xCall->getArgument(0), y);

		if(!!yCall && builder.getNodeManager().getLangBasic().isSubscriptOperator(yCall->getFunctionExpr()))
			return this->operator ()(x, yCall->getArgument(0));

		if(!!xCall && builder.getNodeManager().getLangBasic().isMemberAccess(xCall->getFunctionExpr()))
			if(!!yCall && builder.getNodeManager().getLangBasic().isMemberAccess(yCall->getFunctionExpr())){
				// the type argument can be ignored since it should always be related to the identifier/index
				return this->operator()(xCall->getArgument(0), yCall->getArgument(0)) && this->operator()(xCall->getArgument(1), yCall->getArgument(1));
			}

		const core::VariablePtr& xVar = dynamic_pointer_cast<const core::Variable>(x);
		const core::VariablePtr& yVar = dynamic_pointer_cast<const core::Variable>(y);

//std::cout << std::endl  << " " << xVar << " vs "  << " " << yVar << std::endl;

		if(!xVar || !yVar) {
			return false;
		}

		core::NodeAddress xAddr = core::Address<const core::Variable>::find(xVar, root);
		core::NodeAddress yAddr = core::Address<const core::Variable>::find(yVar, root);

		bool reverse;
		if(xAddr.getDepth() > yAddr.getDepth()) {
			core::NodeAddress tmp = xAddr;
			xAddr = yAddr;
			yAddr = tmp;
			reverse = true;
		}

		auto visitor = core::makeLambdaVisitor([&](const core::NodeAddress& addr) {
			bool ret = false;
			if(const core::CallExprAddress call = core::dynamic_address_cast<const core::CallExpr>(addr)) {
				if(const core::LambdaExprPtr lambda = core::dynamic_pointer_cast<const core::LambdaExpr>(call.getAddressedNode()->getFunctionExpr())) {
					for_range(make_paired_range(lambda->getParameterList(), call->getArguments()),
							[&](const std::pair<core::VariablePtr, core::ExpressionPtr>& cur) {
						// get rid of f**king deref and vectorToArray operations
						core::ExpressionPtr arg = cur.second;
						core::CallExprPtr unneccecaryFunction = dynamic_pointer_cast<const core::CallExpr>(cur.second);
						if(unneccecaryFunction && (
							builder.getNodeManager().getLangBasic().isRefDeref(unneccecaryFunction->getFunctionExpr()) ||
							builder.getNodeManager().getLangBasic().isRefVectorToRefArray(unneccecaryFunction->getFunctionExpr())  ))
								arg = unneccecaryFunction->getArgument(0);

//std::cout << "\n 1 " << *yAddr << " - " << *cur.first << std::endl;
						if(*yAddr == *cur.first) {
//std::cout << " 2 " << *xAddr << " - " << *arg << std::endl;
							if(*xAddr == *arg)
								ret = true;
							else
								ret = this->operator ()(arg, reverse ? y : x);
						}
					});
				}
			}
			return ret;
		});

		return core::visitPathBottomUpInterruptible(yAddr, visitor);
	}
};
typedef insieme::utils::map::PointerMap<core::VariablePtr, core::VariablePtr> ClmemTable;
typedef boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, hash_target_specialized, equal_variables> KernelArgsOld;
typedef boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, hash_target_specialized, equal_variables> KernelArgs;
//typedef std::map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, equal_variables> KernelArgs;
//typedef insieme::utils::map::PointerMap<core::ExpressionPtr, std::vector<core::ExpressionPtr> > KernelArgs;
typedef boost::unordered_map<string, core::ExpressionPtr, boost::hash<string> > KernelNames;
typedef boost::unordered_map<core::ExpressionPtr, core::LambdaExprPtr, hash_target_specialized, equal_variables> KernelLambdas;
typedef boost::unordered_map<core::ExpressionPtr, std::vector<core::DeclarationStmtPtr>, hash_target_specialized, equal_variables > LocalMemDecls;


} //namespace ocl
} //namespace frontend
} //namespace insieme
