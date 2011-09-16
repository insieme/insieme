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

#include "insieme/frontend/ocl/ocl_host_utils.h"

namespace insieme {
namespace frontend {
namespace ocl {

// shortcut
#define BASIC builder.getNodeManager().basic

core::ExpressionPtr getVarOutOfCrazyInspireConstruct(const core::ExpressionPtr& arg, const core::ASTBuilder& builder) {
	core::CallExprPtr&& stripped = dynamic_pointer_cast<const core::CallExpr>(arg);
// remove stuff added by (void*)&
	if(const core::CallExprPtr& refToAnyref = dynamic_pointer_cast<const core::CallExpr>(arg))
		if(refToAnyref->getFunctionExpr() == BASIC.getRefToAnyRef() )
			if(const core::CallExprPtr& makingRef = dynamic_pointer_cast<const core::CallExpr>(refToAnyref->getArgument(0)))
				stripped = makingRef;

	if(!!stripped && stripped->getFunctionExpr() == BASIC.getScalarToArray())
		return stripped->getArgument(0);
	if(!!stripped && stripped->getFunctionExpr() == BASIC.getRefDeref())
		return stripped->getArgument(0);


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
	// TODO test quickfix
	core::TypePtr type = singleElementType;
	while(const core::SingleElementTypePtr& se = dynamic_pointer_cast<const core::SingleElementType>(type))
		type = se->getElementType();
	return type;
}

/*
 * Function to get the type of an Expression
 * If it is a ref-type, it's element type is returned
 */
const core::TypePtr getNonRefType(const core::ExpressionPtr& refExpr) {
	// TODO test quickfix
	core::TypePtr type = refExpr->getType();
	while (const core::RefTypePtr& ref = dynamic_pointer_cast<const core::RefType>(type))
		type = ref->getElementType();
	return type;
}

/*
 * Returns either the type itself or the element type, in case that it is a referenceType
 */
const core::TypePtr getNonRefType(const core::TypePtr& refType) {
	// TODO test quickfix
	core::TypePtr type = refType;
	while(const core::RefTypePtr& ref = dynamic_pointer_cast<const core::RefType>(type))
		type = ref->getElementType();
	return type;
}

/*
 * Builds a ref.deref call around an expression if the it is of ref-type
 */
core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr, const core::ASTBuilder& builder) {
	// core::ExpressionPtr retExpr = expr;
	if (core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr(refTy->getElementType(), BASIC.getRefDeref(), expr);
	}
	return expr;
}

/*
 * Returns either the expression itself or the first argument if expression was a call to function
 */
core::ExpressionPtr tryRemove(const core::ExpressionPtr& function, const core::ExpressionPtr& expr, const core::ASTBuilder& builder) {
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
core::ExpressionPtr tryRemoveAlloc(const core::ExpressionPtr& expr, const core::ASTBuilder& builder) {
	if(const core::CallExprPtr& call = core::dynamic_pointer_cast<const core::CallExpr>(expr)) {
		if(call->getFunctionExpr() == BASIC.getRefNew() || call->getFunctionExpr() == BASIC.getRefVar())
			return tryRemoveAlloc(call->getArgument(0), builder);
	}
	return expr;
}


/*
 * 'follows' the first argument as long it is a call expression until it reaches a variable. If a variable is found it returns it, otherwise NULL is returned
 * Usefull to get variable out of nests of array and struct accesses
 */
core::VariablePtr getVariableArg(const core::ExpressionPtr& function, const core::ASTBuilder& builder) {
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

bool NullLitSearcher::visitLiteral(const core::LiteralPtr& literal) {
	if(literal == builder.literal(literal->getType(), "0"))
		return true;
	return false;
}

} //namespace ocl
} //namespace frontend
} //namespace insieme
