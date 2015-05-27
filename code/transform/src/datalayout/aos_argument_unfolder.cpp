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

#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/transform/datalayout/aos_argument_unfolder.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;
namespace ia = insieme::analysis;

ExpressionList AosArgumentUnfolder::updateArguments(const ExpressionAddress& oldArg) {
	ExpressionList newArgs;
	pattern::AddressMatchOpt match = varWithOptionalDeref.matchAddress(oldArg);
assert_fail();
	if(match) {
		ExpressionAddress oldVarDecl = getDeclaration(match.get()["variable"].getValue().as<ExpressionAddress>());

		auto varCheck = varsToReplace.find(oldVarDecl);
		if(varCheck != varsToReplace.end()) {
			ExpressionAddress oldVar = varCheck->first;
			CompoundStmtPtr newVars = varCheck->second.as<CompoundStmtPtr>();

			for(StatementPtr newVar : newVars->getStatements()) {
				newArgs.push_back(core::transform::fixTypesGen(mgr, oldArg.getAddressedNode(), oldVar, newVar.as<ExpressionPtr>(), false));
			}
		}
		return newArgs;
	}

	// "unfold" the tuple member access to get the individual pointers
	match = tupleMemberAccess.matchAddress(oldArg);

	if(match) {
		ExpressionAddress oldVarRoot = getDeclaration(match.get()["variable"].getValue().as<ExpressionAddress>());
//		std::cout << "oldDecl: " << oldVarRoot << " " << *oldVarRoot << std::endl;

		auto varCheck = varsToPropagate.find(oldVarRoot);
		if(varCheck != varsToPropagate.end()) {
			IRBuilder builder(mgr);
			ExpressionAddress oldVar = varCheck->first;
			ExpressionPtr newVar = varCheck->second.as<ExpressionPtr>();

//std::cout << "oldVar: " << oldVar << " " << *oldVar << std::endl;
//std::cout << "newVar: " << newVar->getType() << " " << *newVar << std::endl;
			ExpressionPtr tupleIndex = match.get()["tupleIndex"].getValue().as<LiteralPtr>();
			ExpressionPtr arrayIndex = match.get()["arrayIndex"].getValue().as<ExpressionPtr>();
			for(NamedTypePtr newType : newStructType) {

				ExpressionPtr accessTuple = builder.accessComponent(newVar, tupleIndex);
				newArgs.push_back(builder.deref(builder.accessMember(builder.arrayRefElem(accessTuple, arrayIndex), newType->getName())));
			}


		}
	}

	return newArgs;
}

bool AosArgumentUnfolder::updateArgumentAndParam(const ExpressionAddress& oldArg, const VariableAddress& oldParam,
		ExpressionList& newArg, VariableList& newParam, TypeList& paramTys) {

	ExpressionAddress oldVarDecl;
	pattern::AddressMatchOpt match = varWithOptionalDeref.matchAddress(oldArg);

	if(match) {
		oldVarDecl = getDeclaration(match.get()["variable"].getValue().as<ExpressionAddress>());

		auto paramToReplace = varsToReplace.find(oldParam);
		auto argToReplace = varsToReplace.find(oldVarDecl);

		// check for replacements of the old argument and parameter
		bool paramFound = paramToReplace != varsToReplace.end();
		bool argFound = argToReplace != varsToReplace.end();

	//			  assert_true(!(paramFound ^ argFound)) << (paramFound  ? "Found param to replace but no matching argument replacement " :
	//					  "Found argument to replace but no matching parameter replacement")<< " " << *oldParam;

		if(paramFound && argFound) {
			// add new arguments and parameters to the lists

//		std::cout << "toll\n";
			for(StatementPtr arg : argToReplace->second.as<CompoundStmtPtr>()) {
//		std::cout << "ARG: " << *arg << std::endl;
				newArg.push_back(arg.as<ExpressionPtr>());
			}
			for(StatementPtr param : paramToReplace->second.as<CompoundStmtPtr>()) {
				VariablePtr p = param.as<VariablePtr>();
				newParam.push_back(p);
				paramTys.push_back(p->getType());
			}
			return true;
		}
	}

	// "unfold" the tuple member access to get the individual pointers
	match = tupleMemberAccess.matchAddress(oldArg);

	if(match) {
		ExpressionAddress tupleVar = match.get()["variable"].getValue().as<ExpressionAddress>();
		ExpressionAddress oldVar = getDeclaration(tupleVar);

		ExpressionAddress oldVarRoot = getRootVariable(tupleVar.getParentAddress(), tupleVar).as<ExpressionAddress>();

		// check for replacements of the old argument
		auto argToReplace = varsToReplace.find(oldVar);
		bool argFound = argToReplace != varsToReplace.end();

		if(argFound) {
			IRBuilder builder(mgr);
			ExpressionPtr newVar = argToReplace->second.as<ExpressionPtr>();

			pattern::TreePattern structType = pattern::aT(pattern::atom(oldStructType));
			ExpressionPtr tupleIndex = match.get()["tupleIndex"].getValue().as<LiteralPtr>();
			ExpressionPtr arrayIndex = match.get()["arrayIndex"].getValue().as<ExpressionPtr>();

			// check for replacements of the old parameter
			auto paramToReplace = varsToReplace.find(oldParam);
			bool paramFound = paramToReplace != varsToReplace.end();
			if(structType.match(oldArg->getType()) && paramFound) {
				for_range(make_paired_range(newStructType, paramToReplace->second.as<CompoundStmtPtr>()),
						[&](const std::pair<NamedTypePtr, StatementPtr>& cur) {
					NamedTypePtr newType = cur.first;
					StatementPtr param =cur.second;

					VariablePtr p = param.as<VariablePtr>();
					newParam.push_back(p);
					paramTys.push_back(p->getType());

					ExpressionPtr accessTuple = builder.accessComponent(newVar, tupleIndex);
					ExpressionPtr argRef = builder.accessMember(builder.arrayRefElem(accessTuple, arrayIndex), newType->getName());

					if(argRef->getType() != builder.refType(p->getType())) // add ref_reinterpret if necessary
						argRef = builder.refReinterpret(argRef, p->getType());

					newArg.push_back(builder.deref(argRef));
				});
			} else {
				ExpressionPtr accessTuple = builder.accessComponent(newVar, tupleIndex);
				newArg.push_back(builder.deref(builder.arrayRefElem(accessTuple, arrayIndex)));
				newParam.push_back(oldParam);
				paramTys.push_back(oldParam->getType());
			}

			return true;
		}
	}

	// nothing special, keep everything as it is
	newArg.push_back(oldArg);
	newParam.push_back(oldParam);
	paramTys.push_back(oldParam->getType());

	return false;
}

const CallExprPtr AosArgumentUnfolder::updateArgumentsAndParams(CallExprAddress oldCall, const core::CallExprPtr& newCall) {
	const LambdaExprAddress lambdaExpr = oldCall->getFunctionExpr().as<LambdaExprAddress>();
	std::vector<VariableAddress> paramAddrs = lambdaExpr->getParameterList();

	ExpressionList newArgs;
	VariableList newParams;
	TypeList paramTys;

	bool changed = false;

	// determine index of the argument/parameter
	for_range(make_paired_range(oldCall->getArguments(), paramAddrs),
			[&](const std::pair<const ExpressionAddress, const VariableAddress>& pair) {
		const ExpressionAddress& oldArg = pair.first;
		const VariableAddress& oldParam = pair.second;
		ExpressionAddress oldVarDecl;

		ExpressionList newArg;
		VariableList newParam;
		TypeList paramTy;

		changed |= updateArgumentAndParam(oldArg, oldParam, newArg, newParam, paramTy);

		// connect individual lists of arguments/parameters (containing only one element if not replaced) to the full function call
		for(ExpressionPtr na : newArg)
			newArgs.push_back(na);
		for(VariablePtr np : newParam)
			newParams.push_back(np);
		for(TypePtr nt : paramTy)
			paramTys.push_back(nt);
	});

	if(!changed)
		return newCall;

	IRBuilder builder(mgr);
	FunctionTypePtr newFunTy = builder.functionType(paramTys, newCall->getType());
	LambdaExprPtr newLambda = builder.lambdaExpr(newFunTy, newParams, newCall->getFunctionExpr().as<LambdaExprPtr>()->getBody());

//	std::cout << "type " << *newCall->getType() << std::endl;
//		std::cout << "lambda " << *newLambda << std::endl;
//	std::cout << "params " << newParams << std::endl;
//	std::cout << "args " << newArgs << std::endl;
	CallExprPtr call = builder.callExpr(newCall->getType(), newLambda, newArgs);

	return call;
}

AosArgumentUnfolder::AosArgumentUnfolder(NodeManager& mgr, ExprAddressMap& varReplacements, ExprAddressMap& varsToPropagate,
		std::map<NodeAddress, NodePtr>& replacements,
		const StructTypePtr& oldStructType, const StructTypePtr& newStructType)
	: mgr(mgr), varsToReplace(varReplacements), varsToPropagate(varsToPropagate), replacements(replacements),
	  oldStructType(oldStructType), newStructType(newStructType),
	  typePattern(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any))))),
	  variablePattern(pirp::variable(typePattern) // local variable
			| pirp::literal(pirp::refType(typePattern), pattern::any)),// global variable
	  namedVariablePattern(var("variable", variablePattern)),
	  varWithOptionalDeref(optionalDeref(namedVariablePattern)),
	  tupleMemberAccess(pirp::refDeref(pattern::aT(pirp::arrayRefElem1D(
			pirp::tupleMemberAccess(namedVariablePattern, var("tupleIndex", pattern::any), pattern::any), var("arrayIndex", pattern::any))))) {}


const NodePtr AosArgumentUnfolder::mapAddress(const NodePtr& ptr, NodeAddress& prevAddr) {
	if (ptr->getNodeCategory() == core::NodeCategory::NC_Type)
		return ptr;

	if(CallExprAddress oldCall = prevAddr.isa<CallExprAddress>()) {
		LambdaExprAddress lambdaExpr = oldCall->getFunctionExpr().isa<LambdaExprAddress>();

		if(lambdaExpr && !mgr.getLangBasic().isBuiltIn(lambdaExpr)) {
			const CallExprPtr newCall = ptr->substitute(ptr->getNodeManager(), *this, prevAddr).as<CallExprPtr>();
			const CallExprPtr updated = updateArgumentsAndParams(oldCall, newCall);

			if(newCall != updated) {
				replacements[prevAddr] = updated;
//				return updated;
			}

			return ptr;
		}
	}

	return ptr->substitute(ptr->getNodeManager(), *this, prevAddr);
}

const NodeAddress AosArgumentUnfolder::mapPtr(NodePtr& ptr) {
	ptr = core::NodeAddress(mapFromRoot(ptr));
	NodeAddress mapped(ptr);

	mapKeySwitchRoot(varsToReplace, mapped);
	mapKeySwitchRoot(varsToPropagate, mapped);

	return mapped;
}

}
}
}
