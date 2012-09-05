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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_types.h"

namespace insieme {
namespace core {
namespace analysis {

/**
 * Tests whether the call referenced by the given pointer is a call to the given function.
 *
 * @param candidate the node to be tested
 * @param function the function to be tested for
 * @return true if so, false otherwise
 */
bool isCallOf(const CallExprPtr& candidate, const NodePtr& function);

/**
 * Tests whether the node referenced by the given pointer is a call to the given function.
 *
 * @param candidate the node to be tested
 * @param function the function to be tested for
 * @return true if so, false otherwise
 */
bool isCallOf(const NodePtr& candidate, const NodePtr& function);

/**
 * A utility function to extract an argument from the given call expression.
 *
 * @param call the call from which an argument should be extracted. The caller has to make
 * 			sure that the given node is in deed a call expression.
 * @param index the index of the requested argument
 * @return the extracted argument
 */
static inline core::ExpressionPtr getArgument(const NodePtr& call, int index) {
	assert(call->getNodeType() == core::NT_CallExpr && "Call has to be a call expression!");
	return static_pointer_cast<const CallExpr>(call)->getArgument(index);
}

/**
 * Tests whether the given node is representing a NoOP.
 */
bool isNoOp(const StatementPtr& candidate);

/**
 * Tests whether the given node is of a reference type to the given type.
 * For Types: direct test
 * For Expressions: the expression type is tested
 *
 * @param candidate the node to be tested
 * @param type the type of the expected element
 * @return true if so, false otherwise
 */
bool isRefOf(const NodePtr& candidate, const NodePtr& type);

/**
 * Tests whether the given node is of a reference type to a type of the given kind.
 * For Types: direct test
 * For Expressions: the expression type is tested
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
    if (!type) return false;
    return type->getNodeType() == core::NT_RefType;
}

/**
 * Determines whether the given expression has is of a reference type.
 *
 * @param expr the expression to be tested
 * @return true if so, false otherwise
 */
static inline bool hasRefType(const ExpressionPtr& expr) {
    if (!expr) return false;
	return isRefType(expr->getType());
}

static inline TypePtr getReferencedType(const RefTypePtr& type) {
    if (!type) return NULL;
	return type->getElementType();
}

static inline TypePtr getReferencedType(const TypePtr& type) {
    if (!type) return NULL;
    return getReferencedType(dynamic_pointer_cast<const RefType>(type));
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

/**
 * Tests whether the given node is a constructor expression.
 *
 * @param node the node to be tested
 * @return true if the given node is a constructor, false otherwise
 */
bool isConstructorExpr(const NodePtr& node);

/**
 * Extracts the represented type from the given type literal type.
 *
 * @param type the type literal type
 * @return the represented type
 */
static inline TypePtr getRepresentedType(const TypePtr& type) {
	assert(isTypeLiteralType(type));
	return static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0];
}

/**
 * Collects a set of all variables encountered within the given code fragment (and
 * all nested fragments - including invoked functions).
 *
 * @param code the code fragment to be analysed
 * @return the set of encountered variables.
 */
VariableSet getAllVariables(const NodePtr& code);

/**
 * Extracts the list of free variables from the given code fragment.
 *
 * @param code the code fragment to be analysed
 * @return the list of free variables within the given code fragment
 */
VariableList getFreeVariables(const NodePtr& code);

/**
 * Retrieves the name of variable in the outer scope where it has been declared.
 *
 * @param varlist the list of adresses of renamed variables
 * @return a map of the addresses in varlist to their declaration in the outermost scope
 */
utils::map::PointerMap<VariableAddress, VariableAddress> getRenamedVariableMap(const std::vector<VariableAddress> varlist);

/**
 * Retrieves the name of variable in the outer scope where it has been declared.
 *
 * @param varMap map of the addresses in varlist to their declaration in the outermost scope
 *        will be updated with aliases of the outher scope in place
 */
void getRenamedVariableMap(utils::map::PointerMap<VariableAddress, VariableAddress>& varMap);

/**
 * Tests whether the given type is a volatile type. In that case 
 * the type is of the form volatile<'a> where 'a is a concrete type.
 *
 * @param type the type to be tested
 * @return true if the given type is a type literal volatile, false otherwise
 */
bool isVolatileType(const TypePtr& type);

TypePtr getVolatileType(const TypePtr& type);

/**
 * Locates the address of the first call expression node calling the given function.
 *
 * @param root the root node to start the search with
 * @param fun the function the requested node should call
 * @return the address to the requested call expression node or an invalid address if not found
 */
CallExprAddress findLeftMostOutermostCallOf(const NodeAddress& root, const ExpressionPtr& fun);

} // end namespace utils
} // end namespace core
} // end namespace insieme
