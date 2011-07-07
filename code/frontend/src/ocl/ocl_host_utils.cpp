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


/*
 * Function to get the type of an Expression
 * If it is a ref-type, it's element type is returned
 */
const core::TypePtr getNonRefType(const core::ExpressionPtr& refExpr) {
	if (const core::RefTypePtr& ref = dynamic_pointer_cast<const core::RefType>(refExpr->getType()))
		return ref->getElementType();
	return refExpr->getType();
}

/*
 * Returns either the type itself or the element type, in case that it is a referenceType
 */
const core::TypePtr getNonRefType(const core::TypePtr& refType) {
	if (const core::RefTypePtr& ref = dynamic_pointer_cast<const core::RefType>(refType))
		return ref->getElementType();
	return refType;
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
 * Function to copy all annotations form one NodePtr to another
 */
void copyAnnotations(const core::NodePtr& source, core::NodePtr& sink) {
	unsigned int i = 0; // normal iterator and for_each loop leads to infinity loop, go
	for (auto I = source->getAnnotations().begin(); i < source->getAnnotations().size(); ++I, ++i)
		sink->addAnnotation(I->second);
}



} //namespace ocl
} //namespace frontend
} //namespace insieme
