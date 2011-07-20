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

#include "insieme/core/ast_node.h"
#include "insieme/core/expressions.h"
#include "insieme/core/types.h"

namespace insieme {
namespace core {
namespace analysis {


/**
 * Tests whether the node referenced by the given pointer is a call to the given function.
 *
 * @param candidate the node to be tested
 * @param function the function to be tested for
 * @return true if so, false otherwise
 */
bool isCallOf(const NodePtr& candidate, const NodePtr& function);

/**
 * Tests whether the given node is representing a NoOP.
 */
bool isNoOp(const StatementPtr& candidate);

/**
 * Tests whether the given node is representing a reference type to the given type.
 *
 * @param candidate the node to be tested
 * @param type the type of the expected element
 * @return true if so, false otherwise
 */
bool isRefOf(const NodePtr& candidate, const NodePtr& type);

/**
 * Tests whether the given node is representing a reference type to a type of the given kind.
 *
 * @param candidate the node to be tested
 * @param kind the kind of the expected element
 * @return true if so, false otherwise
 */
bool isRefOf(const NodePtr& candidate, const NodeType kind);

/**
 * A simple helper utility to test whether the given type is a reference type.
 * In this particular case, the answer can be determined statically.
 *
 * @param refType the type to be tested.
 * @return true - always.
 */
static inline bool isRefType(const RefTypePtr& refType) {
	return true;
}

/**
 * A simple helper utility to test whether the given type is a reference type.
 *
 * @param type the type to be tested.
 * @return true if the given type is a reference type, false otherwise
 */
static inline bool isRefType(const TypePtr& type) {
	return type->getNodeType() == core::NT_RefType;
}

/**
 * Determines whether the given expression has is of a reference type.
 *
 * @param expr the expression to be tested
 * @return true if so, false otherwise
 */
static inline bool hasRefType(const ExpressionPtr& expr) {
	return isRefType(expr->getType());
}

static inline TypePtr getReferencedType(const RefTypePtr& type) {
	return type->getElementType();
}

static inline TypePtr getReferencedType(const TypePtr& type) {
	return getReferencedType(static_pointer_cast<const RefType>(type));
}

/**
 * Tests whether the given type is a type used for type literals. Hence, whether
 * the type is of the from type<'a> where 'a may be a variable or concrete type.
 *
 * @param type the type to be tested
 * @return true if the given type is a type literal type, false otherwise
 */
bool isTypeLiteralType(const GenericTypePtr& type);


/**
 * Tests whether the given type is a type used for type literals. Hence, whether
 * the type is of the from type<'a> where 'a may be a variable or concrete type.
 *
 * @param type the type to be tested
 * @return true if the given type is a type literal type, false otherwise
 */
bool isTypeLiteralType(const TypePtr& type);

} // end namespace utils
} // end namespace core
} // end namespace insieme
