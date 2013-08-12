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

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/frontend/ocl/ocl_host_utils.h"

namespace insieme {
namespace frontend {
namespace ocl {

// shortcut

core::ExpressionPtr getVarOutOfCrazyInspireConstruct(const core::ExpressionPtr& arg, const core::IRBuilder& builder) {
// remove stuff added by (void*)&
	core::CallExprPtr stripped = arg.isa<core::CallExprPtr>();

	if (!stripped) {
		return arg;
	}

	auto funExpr = stripped->getFunctionExpr();
	if(BASIC.isScalarToArray(funExpr) || BASIC.isRefDeref(funExpr) || BASIC.isRefReinterpret(funExpr)) {
		return getVarOutOfCrazyInspireConstruct(stripped->getArgument(0), builder);
	}

	return arg;
}


/*
 * Returns the very base type of the passed Expression
 */
const core::TypePtr getBaseType(const core::ExpressionPtr& singleElementExpr) {
	return getBaseType(singleElementExpr->getType());
}

/*
 * Returns the very base type of the passed type
 */
const core::TypePtr getBaseType(const core::TypePtr& singleElementType) {
	core::TypePtr type = singleElementType;
	if(const core::SingleElementTypePtr& se = dynamic_pointer_cast<const core::SingleElementType>(type))
		return getBaseType(se->getElementType());
	if(const core::RefTypePtr& ref = dynamic_pointer_cast<const core::RefType>(type))
		return getBaseType(ref->getElementType());
	return type;
}

/*
 * Returns either the type itself or the element type, in case that it is a referenceType
 */
const core::TypePtr getNonRefType(const core::TypePtr& refType) {
	core::TypePtr type = refType;
	while(const core::RefTypePtr& ref = dynamic_pointer_cast<const core::RefType>(type))
		type = ref->getElementType();
	return type;
}

/*
 * Function to get the type of an Expression
 * If it is a ref-type, it's element type is returned
 */
const core::TypePtr getNonRefType(const core::ExpressionPtr& refExpr) {
	return getNonRefType(refExpr->getType());
}

/*
 * Builds a ref.deref call around an expression if the it is of ref-type
 */
core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr, const core::IRBuilder& builder) {
	// core::ExpressionPtr retExpr = expr;
	if (core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr(refTy->getElementType(), BASIC.getRefDeref(), expr);
	}
	return expr;
}

/*
 * removes the returns 'a if type is ref<'a>, type otherwise
 */
core::TypePtr removeSingleRef(const core::TypePtr& type){
	if (core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
			return refTy->getElementType();
	}
	return type;
}

/*
 * Builds a ref.deref call around an expression if the it is of type ref<ref<'a>>
 */
core::ExpressionPtr removeDoubleRef(const core::ExpressionPtr& expr, const core::IRBuilder& builder){
	if (core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		if(refTy->getElementType()->getNodeType() == core::NT_RefType || refTy->toString().find("array") == string::npos) // on non array types remove also a single ref
				return builder.callExpr(refTy->getElementType(), BASIC.getRefDeref(), expr);
	}
	return expr;
}

/*
 * Returns either the expression itself or the first argument if expression was a call to function
 */
core::ExpressionPtr tryRemove(const core::ExpressionPtr& function, const core::ExpressionPtr& expr, const core::IRBuilder& builder) {
	core::ExpressionPtr e = expr;
	while(const core::CallExprPtr& call = core::dynamic_pointer_cast<const core::CallExpr>(e)) {
		if(call->getFunctionExpr() == function)
			e = call->getArgument(0);
		else
			break;
	}
	return e;
}

/*
 * Returns either the expression itself or the expression inside a nest of ref.new/ref.var calls
 */
core::ExpressionPtr tryRemoveAlloc(const core::ExpressionPtr& expr, const core::IRBuilder& builder) {
	if(const core::CallExprPtr& call = core::dynamic_pointer_cast<const core::CallExpr>(expr)) {
		if(BASIC.isRefNew(call->getFunctionExpr()) || BASIC.isRefVar(call->getFunctionExpr()))
			return tryRemoveAlloc(call->getArgument(0), builder);
	}
	return expr;
}


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
 * Function to copy all annotations form one NodePtr to another
 */
void copyAnnotations(const core::NodePtr& source, core::NodePtr& sink) {
	sink->setAnnotations(source->getAnnotations());
}


bool LambdaSearcher::visitCallExpr(const core::CallExprAddress& call) {
	bool ret = false;
	if(const core::LambdaExprPtr lambda = core::dynamic_pointer_cast<const core::LambdaExpr>(call.getAddressedNode()->getFunctionExpr())) {
		for_range(make_paired_range(lambda->getParameterList(), call->getArguments()),
				[&](const std::pair<core::VariablePtr, core::ExpressionPtr>& cur) {
			if(*lAddr == *cur.first) {
				if(*vAddr == *cur.second)
					ret = true;
				else
					lAddr = core::Address<const core::Variable>::find(getVariableArg(cur.second, builder), root);
			}
		});
	}
	return ret;
}

core::VariableMap refreshVariables(std::vector<core::DeclarationStmtPtr>& localMemDecls, const core::IRBuilder& builder){
	core::VariableMap varMapping;
	for_each(localMemDecls, [&](core::DeclarationStmtPtr& localMemDecl){
		core::visitDepthFirstOnce(localMemDecl->getInitialization(), core::makeLambdaVisitor([&](const core::NodePtr& node) {
			if(core::VariablePtr var = dynamic_pointer_cast<const core::Variable>(node))
			if(varMapping.find(var) == varMapping.end()) // variable does not have a replacement in map now
				varMapping[var] = builder.variable(var->getType());
		}));

		for_each(varMapping, [&](std::pair<core::VariablePtr, core::VariablePtr> replacement) {
			localMemDecl = static_pointer_cast<const core::DeclarationStmt>(
					core::transform::replaceAll(builder.getNodeManager(), localMemDecl, replacement.first, replacement.second));
		});
	});

	return varMapping;
}

void refreshVariables(core::ExpressionPtr& localMemInit, core::VariableMap& varMapping, const core::IRBuilder& builder){
	core::visitDepthFirstOnce(localMemInit, core::makeLambdaVisitor([&](const core::NodePtr& node) {
		if(core::VariablePtr var = dynamic_pointer_cast<const core::Variable>(node))
		if(varMapping.find(var) == varMapping.end()) // variable does not have a replacement in map now
			varMapping[var] = builder.variable(var->getType());
	}));

	for_each(varMapping, [&](std::pair<core::VariablePtr, core::VariablePtr> replacement) {
//		std::cout << "\nreplaceinig " << replacement.first << " with " << replacement.second << std::endl;
		localMemInit = static_pointer_cast<const core::Expression>(
				core::transform::replaceAll(builder.getNodeManager(), localMemInit, replacement.first, replacement.second));
	});
}

/*
 * takes a type ref<array<vector<'b,#l>,1>> and creates ref<array<'b>,1> from it
 */
core::TypePtr vectorArrayTypeToScalarArrayType(core::TypePtr arrayTy, const core::IRBuilder& builder) {
	if(const core::RefTypePtr refTy = dynamic_pointer_cast<const core::RefType>(arrayTy)) {
		if(const core::ArrayTypePtr arrTy = dynamic_pointer_cast<const core::ArrayType>(refTy->getElementType()))
			if(const core::VectorTypePtr vecTy = dynamic_pointer_cast<const core::VectorType>(arrTy->getElementType())) {
				return builder.refType(builder.arrayType(vecTy->getElementType()));
			}
	}

	return arrayTy;
}


/*
 * checkes if the type of expr is type, otherwise a refReinterpred is added around expr and returned;
 */
core::ExpressionPtr tryRefReinterpret(core::ExpressionPtr expr, core::TypePtr type, core::IRBuilder builder) {
	if(!core::types::isSubTypeOf(expr->getType(), type)) {
		core::TypePtr exprTy = expr->getType();

		// if there is a deref around the argument and a refReinterpret is needed, remove it and add it around the reinterpret
		if(core::CallExprPtr deref = dynamic_pointer_cast<const core::CallExpr>(expr)) {
			if(BASIC.isRefDeref(deref->getFunctionExpr())) {
				return builder.deref(builder.callExpr(builder.refType(type), BASIC.getRefReinterpret(), deref.getArgument(0), builder.getTypeLiteral(type)));
			}
		}

		// if there is no deref on the passed argument, it must have a ref type to perform the refReinterpret
		if(core::RefTypePtr refTy = dynamic_pointer_cast<const core::RefType>(type))
			return builder.callExpr(refTy, BASIC.getRefReinterpret(), expr, builder.getTypeLiteral(refTy->getElementType()));
		else
			assert(refTy && "Cannot do a reinterpret on an non-ref kernel argument");
	}

	return expr;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme

