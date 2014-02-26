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

#include "insieme/frontend/ocl/ocl_type_fixer.h"
#include "insieme/core/printer/pretty_printer.h"

using namespace insieme::core;

namespace insieme {
namespace frontend {
namespace ocl {

const NodePtr TypeFixer::resolveElement(const NodePtr& element){
	// stop recursion at type level
	if (element->getNodeCategory() == NodeCategory::NC_Type) {
		return element;
	}

	NodeManager& mgr = element->getNodeManager();
	IRBuilder builder(mgr);

	if(const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(element)) {
		const vector<TypePtr>& paramTys = static_pointer_cast<const FunctionType>(call->getFunctionExpr()->getType())->getParameterTypes()->getTypes();
		ExpressionList newArgs;
		bool update = false;
		int cnt = 0;

		if(paramTys.size() == call->getArguments().size()) { // undefined functions have an empty parameter list
			for_each(call->getArguments(), [&](const ExpressionPtr& arg){
				const CallExprPtr& fArg = dynamic_pointer_cast<const CallExpr>(arg);

				if( fArg &&	builder.getNodeManager().getLangBasic().isRefDeref(fArg->getFunctionExpr()) &&
						(!dynamic_pointer_cast<const RefType>(arg->getType()) && arg->getType()->getNodeType() != core::NT_GenericType ) &&
						(!!dynamic_pointer_cast<const RefType>(paramTys.at(cnt)))) {
					update = true; // remove unnecessary drefs
					newArgs.push_back(fArg->getArgument(0));
				} else {
					newArgs.push_back(arg);
				}
				++cnt;
			});
			if(update) {
				return builder.callExpr(call->getType(), call->getFunctionExpr(), newArgs)->substitute(builder.getNodeManager(), *this);
			}
		}

		// removes cl_* variables from argument lists of lambdas
		if(const LambdaExprPtr& lambda = dynamic_pointer_cast<const LambdaExpr>(call->getFunctionExpr())) {
			ExpressionList newArgs;
			core::VariableList newParams;
			const core::VariableList& oldParams = lambda->getParameterList()->getElements();
			TypeList paramTypes;
			bool update = false;
			int cnt = 0;

			for_each(call->getArguments(), [&](const ExpressionPtr& arg){
				// do nothing if the argument type is not a cl_* type
				if(arg->getType()->toString().find("array<_cl_") == string::npos) {
					newArgs.push_back(arg);
					newParams.push_back(oldParams.at(cnt));
					paramTypes.push_back(oldParams.at(cnt)->getType());
				} else {
					// do not port cl_* types to the new type
					update = true;
//std::cout << "\ndropping " << *arg->getType() << " - " << *arg << std::endl;
				}
				++cnt;
			});
			if(update) {
				const LambdaExprPtr newLambda = builder.lambdaExpr(builder.functionType(paramTypes, call->getType()), newParams, lambda->getBody());
				return builder.callExpr(call->getType(), newLambda, newArgs)->substitute(builder.getNodeManager(), *this);
			}
		}

	}

	return element->substitute(builder.getNodeManager(), *this);
}


}
}
}
