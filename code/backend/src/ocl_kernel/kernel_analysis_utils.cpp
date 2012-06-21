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

#include "insieme/transform/pattern/ir_generator.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"
#include "insieme/backend/ocl_kernel/kernel_analysis_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

using namespace insieme::annotations::ocl;
using namespace insieme::core;
using namespace insieme::transform::pattern;
namespace irg = insieme::transform::pattern::generator::irg;


/*
 * 'follows' the first argument as long it is a call expression until it reaches a variable. If a variable is found it returns it, otherwise NULL is returned
 * Useful to get variable out of nests of array and struct accesses
 */
core::VariablePtr getVariableArg(const core::ExpressionPtr& function, const core::IRBuilder& builder) {
	if(const core::CallExprPtr& call = dynamic_pointer_cast<const core::CallExpr>(function))
		return getVariableArg(call->getArgument(0), builder);
	return dynamic_pointer_cast<const core::Variable>(function);
}

/*
 * checks if the passed variable an alias to get_global_id
 */
bool InductionVarMapper::isGetId(ExpressionPtr expr) {
	if(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(expr))
		return isGetId(cast->getSubExpression());

	if(const CallExprPtr& call = dynamic_pointer_cast<const CallExpr>(expr)){
		ExpressionPtr fun = call->getFunctionExpr();
		if(*fun == *extensions.getGlobalID /*|| *fun == *extensions.getLocalID || *fun == *extensions.getGroupID*/)
			return true;
	}

	return false;
}

/*
 * removes stuff that bothers me when doing analyzes, e.g. casts, derefs etc
 */
ExpressionPtr InductionVarMapper::removeAnnoyingStuff(ExpressionPtr expr) const {
	// use of variable as argument of ref.new or ref.var
	if(const CallExprPtr exprCall = dynamic_pointer_cast<const CallExpr>(expr))
		if(BASIC.isRefNew(exprCall->getFunctionExpr()) || BASIC.isRefVar(exprCall->getFunctionExpr()))
			return removeAnnoyingStuff(exprCall->getArgument(0));

	if(const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(expr))
		return removeAnnoyingStuff(cast->getSubExpression());

	if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(expr)) {
		if(BASIC.isRefDeref(call->getFunctionExpr()) || BASIC.isRefVar(call->getFunctionExpr()) || BASIC.isRefNew(call->getFunctionExpr()))
			return removeAnnoyingStuff(call->getArgument(0));
	}

	return expr;
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

	// replace variable with loop induction variable if semantically correct
	if(const VariablePtr var = dynamic_pointer_cast<const Variable>(ptr)) {
		if(replacements.find(var) != replacements.end() && replacements[var]){
			ExpressionPtr replacement =  static_pointer_cast<const Expression>(replacements[var]);
//			std::cout << "FUCKE " << var << " -> " << replacements[var] << std::endl;
			//return var;
std::cout << "THIS crashes the compiler: " << var << " -> " << replacement << std::endl << std::endl;
			if(*replacement->getType() == *var->getType())
				return replacement;

			// add a cast expression to the type of the node we are replacing
			return builder.castExpr(var->getType(), replacement);
		}
	}

	// try to replace variables with loop-induction variables where ever possible
	if(const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(ptr)) {
		if(BASIC.isRefAssign(call->getFunctionExpr())) {
			InductionVarMapper subMapper(mgr, replacements);
			ExpressionPtr rhs = call->getArgument(1)->substitute(mgr, subMapper);
			ExpressionPtr lhs = call->getArgument(0);
			// removing caching of the variable to be replaced
			clearCacheEntry(lhs);

			if(isGetId(rhs)) {// an induction variable is assigned to another variable. Use the induction variable instead
				if(replacements.find(rhs) != replacements.end())
					replacements[lhs] = replacements[rhs];
				else
					replacements[lhs] = rhs;
				// remove variable from cache since it's mapping has been changed now
				return builder.getNoOp();
			} else {
				// if we have a replacement for the rhs, use the same also or the lhs
				if(replacements.find(rhs) != replacements.end()) {
					replacements[lhs] = replacements[rhs];
				}
			}

		}

		// maps arguments to parameters
		if(const LambdaExprPtr lambda = dynamic_pointer_cast<const LambdaExpr>(call->getFunctionExpr())){
			ExpressionList args;
			for_range(make_paired_range(lambda->getLambda()->getParameters()->getElements(), call->getArguments()),
					[&](const std::pair<const core::VariablePtr, const core::ExpressionPtr> pair) {
				VariablePtr arg = getVariableArg(pair.second, builder);
				if(replacements.find(arg) != replacements.end() && replacements[arg]) {
					replacements[pair.first] = replacements[arg]->substitute(mgr, *this);
	//				args.push_back(replacements[pair.first].as<ExpressionPtr>());
	//		args.push_back(builder.intLit(42));
				} else {
	//				replacements[pair.first] = pair.second->substitute(mgr, *this); NOT needed now
				}
				args.push_back(replacements[pair.first].as<ExpressionPtr>());
			});

			return call;
//			clearCacheEntry(lambda->getBody());
			InductionVarMapper subMapper(mgr, replacements);
			return builder.callExpr(builder.lambdaExpr(lambda->getType().as<FunctionTypePtr>(), lambda->getLambda()->getParameters(),
					lambda->getBody()->substitute(mgr, subMapper).as<CompoundStmtPtr>()), args);
		}

	}
	if(const DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(ptr)) {
		ExpressionPtr init = decl->getInitialization()->substitute(mgr, *this);

		// remove cast and other annoying stuff
		init = removeAnnoyingStuff(init);
		VariablePtr var = decl->getVariable();

		// plain use of variable as initialization
		if(isGetId(init) || init->getNodeType() != NT_Literal) { //TODO fix this on demand, proper fix does not seem possible
			if(replacements.find(init) != replacements.end() && replacements[init])
				replacements[var] = replacements[init];
			else
				replacements[var] = init;

			clearCacheEntry(var);
//std::cout << "Mapping " << var << " to " << replacements[var]<< std::endl;
			return builder.getNoOp();
		}
	}
	if(const ForStmtPtr loop = dynamic_pointer_cast<const ForStmt>(ptr)) {
		// remove cast and other annoying stuff
		ExpressionPtr start = removeAnnoyingStuff(loop->getStart()->substitute(mgr, *this));
		ExpressionPtr end = removeAnnoyingStuff(loop->getEnd()->substitute(mgr, *this));

		VariablePtr var = loop->getDeclaration()->getVariable();

		StatementPtr body, body1, body2;

		// check if the lover bound is a valuable replacement
		if(isGetId(start) || core::analysis::isCallOf(start, BASIC.getArrayRefElem1D())) { //TODO fix this on demand, proper fix does not seem possible
			if(replacements.find(start) != replacements.end() && replacements[start])
				replacements[var] = replacements[start];
			else
				replacements[var] = start;

			clearCacheEntry(var);
			// construct a body where the induction variable is replaced with the lower bound
			body1 = loop->getBody()->substitute(mgr, *this);
		}

		// check if the upper bound is a valuable replacement
		if(isGetId(end) || core::analysis::isCallOf(end, BASIC.getArrayRefElem1D())) { //TODO fix this on demand, proper fix does not seem possible
			if(replacements.find(end) != replacements.end() && replacements[end])
				replacements[var] = replacements[end];
			else
				replacements[var] = end;

//			InductionVarMapper subMapper(mgr, replacements); WTF??
			clearCache();
			// construct a body where the induction variable is replaced with the upper bound
			body2 = loop->getBody()->substitute(mgr, *this);
		}

		if(body1 && body2) // if both, lower and upper bound are valid replacements, use both variants, one after another
			body = builder.compoundStmt(toVector(body1, body2));
		else if(body1)
			body = body1;
		else if(body2)
			body = body2;
		else
			body = loop->getBody()->substitute(mgr, *this);


//std::cout << "Mapping " << var << " to " << replacements[var]<< std::endl;
		// cannot replace it with noOp since it may be needed, for example as induction variable in for loops
		return builder.forStmt(var, loop->getStart(), loop->getEnd(), loop->getStep(), body);

	}

	return ptr->substitute(mgr, *this);
}

IndexExprEvaluator::IndexExprEvaluator(	const IRBuilder& build, AccessMap& idxAccesses) : IRVisitor<void>(false), builder(build), accesses(idxAccesses),
		rw(ACCESS_TYPE::read) {
	globalAccess = irp::callExpr( any, irp::callExpr( irp::literal("_ocl_unwrap_global"), var("global_var") << *any) << var("index_expr"));
	globalUsed = irp::callExpr( irp::literal("_ocl_unwrap_global"), var("global_var") << *any);
}

void IndexExprEvaluator::visitCallExpr(const CallExprPtr& idx) {
	// add aliases to global variables to list
	if(const LambdaExprPtr lambda = dynamic_pointer_cast<const LambdaExpr>(idx->getFunctionExpr())){
		for_range(make_paired_range(lambda->getLambda()->getParameters()->getElements(), idx->getArguments()),
				[&](const std::pair<const core::VariablePtr, const core::ExpressionPtr> pair) {
			if(globalAliases.find(pair.second) != globalAliases.end()) { // TODO test multiple levels of aliasing
				globalAliases[pair.first] = globalAliases[pair.second];
			} else {
				MatchOpt&& match = globalUsed->matchPointer(pair.second);
				if(match) {
					globalAliases[pair.first] = dynamic_pointer_cast<const Variable>(match->getVarBinding("global_var").getValue());
				}
			}
		});
	}

	// check if call is an access
	if(BASIC.isSubscriptOperator(idx->getFunctionExpr())) {
		VariablePtr globalVar;
		ExpressionPtr idxExpr;

		// check if access is to a global variable
		MatchOpt&& match = globalAccess->matchPointer(idx);

		if(match) {
			globalVar = dynamic_pointer_cast<const Variable>(match->getVarBinding("global_var").getValue());
			assert(globalVar && "_ocl_unwrap_global should only be used to access ocl global variables");

			idxExpr = dynamic_pointer_cast<const Expression>(match->getVarBinding("index_expr").getValue());
			assert(idxExpr && "Cannot extract index expression from access to ocl global variable");
		}

		// check if access is to an alias of a global variable
		if(globalAliases.find(idx->getArgument(0)) != globalAliases.end()) {
			globalVar = globalAliases[idx->getArgument(0)];
			assert(globalVar && "Accessing alias to ocl global variable in a unexpected way");

			idxExpr = dynamic_pointer_cast<const Expression>(idx->getArgument(1));
		}

		if(globalVar) {
			if(accesses.find(globalVar) != accesses.end())
				if(accesses[globalVar].find(idxExpr) != accesses[globalVar].end()) {
					accesses[globalVar][idxExpr] = ACCESS_TYPE(accesses[globalVar][idxExpr] | rw);
					return;
				}

			accesses[globalVar][idxExpr] = rw;
		}
	}
}


void AccessExprCollector::visitCallExpr(const CallExprPtr& call){
	// check if call is an assignment
	if(BASIC.isRefAssign(call->getFunctionExpr())) {
		iee.setAccessType(ACCESS_TYPE::write);
		// visit right hand side of assignment
		visitDepthFirstOnce(call->getArgument(0), iee);
		// visit left hand side of assignment, read expressions overwrite write expressions
		iee.setAccessType(ACCESS_TYPE::read);
		visitDepthFirstOnce(call->getArgument(1), iee);
	}
}

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme

