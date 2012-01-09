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

#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"
#include "insieme/backend/ocl_kernel/kernel_analysis_utils.h"

#include "insieme/transform/pattern/ir_generator.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

using namespace insieme::annotations::ocl;
using namespace insieme::core;
using namespace insieme::transform::pattern;
namespace irg = insieme::transform::pattern::generator::irg;

/*
 * checks if the passed variable is one of the 6 loop induction variables
 */
bool InductionVarMapper::isGetId(ExpressionPtr expr) const {
	if(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(expr))
		return isGetId(cast->getSubExpression());

	if(const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(expr)){
		ExpressionPtr fun = call->getFunctionExpr();
		if(*fun == *extensions.getGlobalID || *fun == *extensions.getLocalID)// || *fun == *extensions.getGroupID
			return true;
	}

	return false;
}

/*
 * checks if the first argument of the passed call is an integer literal. If yes and the value is between 0 and 2,
 * it's value is returned, otherwise an assertion is raised
 */
size_t InductionVarMapper::extractIndexFromArg(CallExprPtr call) const {
	ExpressionList args = call->getArguments();
	assert(args.size() > 0 && "Call to opencl get id function must have one argument");
	size_t retval = 0;

	// try to read literal
	ExpressionPtr arg = args.at(0);
	// remove casts
	CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(arg);
	while(cast) {
		arg = cast->getSubExpression();
		cast = dynamic_pointer_cast<const CastExpr>(arg);
	}
	if(const LiteralPtr dim = dynamic_pointer_cast<const Literal>(arg))
		retval = dim->getValueAs<size_t>();

	assert(retval <= 2 && "Argument of opencl get id function must be a literal between 0 an 2");
	return retval;
}

const NodePtr InductionVarMapper::resolveElement(const NodePtr& ptr) {
	// stop recursion at type level
	if (ptr->getNodeCategory() == core::NodeCategory::NC_Type) {
		return ptr;
	}

	if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(ptr)) {
		const ExpressionPtr fun = call->getFunctionExpr();

		// replace calls to get_*_id with accesses to the appropriate loop variable
		if(isGetGlobalID(fun)){
			size_t dim = extractIndexFromArg(call);
//std::cout << "\nGlobalID " << dim << " - "  << call << std::endl;
			globalDim = std::max(dim, globalDim);
			return builder.callExpr(BASIC.getUInt4(), extensions.getGlobalID, call->getArgument(0));
		}
		if(isGetLocalID(fun)) {
			size_t dim = extractIndexFromArg(call);
//std::cout << "\nLocalID " << dim << " - " << call << std::endl;
			localDim = std::max(dim, localDim);
			return builder.callExpr(BASIC.getUInt4(), extensions.getLocalID, call->getArgument(0));
		}
/*			if(0) {//isGetGroupID(call)))
			size_t dim = extractIndexFromArg(call);
			globalDim = std::max(dim, globalDim);
			localDim = std::max(dim, localDim);
			return builder.callExpr(BASIC.getUInt4(), extensions.getGoupID, call->getArgument(0));
		}
*/
	}

	// replace variable with loop induction variable if semantically correct
	if(const VariablePtr var = dynamic_pointer_cast<const Variable>(ptr)) {
//			std::cout << "Variable: " << *var << " " << replacements.size() << std::endl;
		if(replacements.find(var) != replacements.end()){
			ExpressionPtr replacement =  static_pointer_cast<const Expression>(replacements[var]);
			if(*replacement->getType() == *var->getType())
				return replacement;

			// add a cast expression to the type of the node we are replacing
			return builder.castExpr(var->getType(), replacement);
		}
	}

	// try to replace varariables with loop-induction variables whereever possible
	if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(ptr)) {
		if(BASIC.isRefAssign(call->getFunctionExpr())) {
			ExpressionPtr rhs = call->getArgument(1)->substitute(mgr, *this);
			ExpressionPtr lhs = call->getArgument(0);
			// removing caching of the variable to be replaced
			clearCacheEntry(lhs);

			if(isGetId(rhs)) {// an induction variable is assigned to another variable. Use the induction variable instead
				replacements[lhs] = rhs;
				// remove variable from chache since it's mapping has been changed now
				return builder.getNoOp();
			}
		}
	}
	if(const DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(ptr)) {
		ExpressionPtr init = decl->getInitialization()->substitute(mgr, *this);

		// use of variable as argument of ref.new or ref.var
		if(const CallExprPtr initCall = dynamic_pointer_cast<const CallExpr>(init))
			if(BASIC.isRefNew(initCall->getFunctionExpr()) || BASIC.isRefVar(initCall->getFunctionExpr()))
				init = initCall->getArgument(0);

		// remove cast
		while(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(init))
			init = cast->getSubExpression();

		// plain use of variable as initialization
		if(isGetId(init)) {
			replacements[decl->getVariable()] = init;
			return builder.getNoOp();
		}
	}

	return ptr->substitute(mgr, *this);
}

AccessExprCollector::AccessExprCollector(const IRBuilder& build) : IRVisitor<void>(false), builder(build) {
	globalAccess = irp::callExpr( any, irp::callExpr( irp::literal("_ocl_unwrap_global"), var("global_var") << *any) << var("index_expr"));
}

void AccessExprCollector::visitCallExpr(const CallExprPtr& call){
	// check if call is an access
	if(BASIC.isSubscriptOperator(call->getFunctionExpr())) {
		// check if access is to a global variable
		MatchOpt&& match = globalAccess->matchPointer(call);
		if(match) {
			VariablePtr globalVar = dynamic_pointer_cast<const Variable>(match->getVarBinding("global_var").getValue());
			assert(globalVar && "_ocl_unwrap_global should only be used on a variable");

			ExpressionPtr idxExpr = dynamic_pointer_cast<const Expression>(match->getVarBinding("index_expr").getValue());
			assert(idxExpr && "Cannot extract index expression from access to ocl global variable");

			accesses[globalVar][idxExpr] = 0;
		}

	}
}

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme

