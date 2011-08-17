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

#include "insieme/core/ast_builder.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/frontend/program.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace frontend {
namespace ocl {


/**
 * This specialized hasher hashes array accesses to the same variable to the same bin
 * regardless of the array index
 */
struct hash_target_specialized : public hash_target<core::ExpressionPtr> {

	core::ASTBuilder builder;

	hash_target_specialized(core::ASTBuilder build) : hash_target(), builder(build) {}

	/**
	 * Computes the hash value of the given pointer based on the target it is pointing to. For subscript operations only the subscripted variable/call
	 * is considered
	 */
	std::size_t operator()(const core::ExpressionPtr expr) const {
		if(!expr)
			return 0;

		const core::CallExprPtr& call = dynamic_pointer_cast<const core::CallExpr>(expr);

		if(!call)
			return hasher(*expr);

//		while(const core::CallExprPtr& tmp = dynamic_pointer_cast<const core::CallExpr>(call->getArgument(0)))
//			call = tmp;

		if(builder.getNodeManager().basic.isSubscriptOperator(call->getFunctionExpr()))
			return this->operator ()(call->getArgument(0));

		return hasher(*expr);
	}
};

/**
 * compares two Expressions. Returns true if they are equal or if they are both a SubscriptOperator on the same variable,
 * regardless of the index
 */
struct equal_variables {// : public std::binary_function<const core::ExpressionPtr&, const core::ExpressionPtr&, bool> {
	// needed to perform isSubscriptOperator()
	core::ASTBuilder& builder;

	equal_variables(core::ASTBuilder& build) : builder(build) {}

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

		core::CallExprPtr xCall =  dynamic_pointer_cast<const core::CallExpr>(x);

		core::CallExprPtr yCall = dynamic_pointer_cast<const core::CallExpr>(y);

		if(builder.getNodeManager().basic.isSubscriptOperator(xCall->getFunctionExpr()))
			if(builder.getNodeManager().basic.isSubscriptOperator(yCall->getFunctionExpr()))
				if(this->operator ()(xCall->getArgument(0), yCall->getArgument(0)) )
					return true;


		return false;
	}
};
typedef insieme::utils::map::PointerMap<core::VariablePtr, core::VariablePtr> ClmemTable;
typedef boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, hash_target_specialized, equal_variables> KernelArgs;
//typedef std::map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, equal_variables> KernelArgs;
//typedef insieme::utils::map::PointerMap<core::ExpressionPtr, std::vector<core::ExpressionPtr> > KernelArgs;
typedef boost::unordered_map<string, core::ExpressionPtr, boost::hash<string> > KernelNames;
typedef boost::unordered_map<core::ExpressionPtr, core::LambdaExprPtr, hash_target_specialized, equal_variables> KernelLambdas;
typedef insieme::utils::map::PointerMap<core::ExpressionPtr, vector<core::DeclarationStmtPtr> > LocalMemDecls;


} //namespace ocl
} //namespace frontend
} //namespace insieme
