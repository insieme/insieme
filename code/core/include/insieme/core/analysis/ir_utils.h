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
 * Determines whether the given type is generic or not. A type is considered to be generic if it
 * includes type variables. Hence, a function accepting a generic input parameter or a value type
 * including a variable type parameter will be considered generic.
 *
 * @param type the type to be checked
 * @return true if the type is generic, false otherwise
 */
bool isGeneric(const TypePtr& type);

/**
 * A universial utility extracting a list of element types from any given
 * type (e.g. parameter types of an abstract type, element types of a
 * reference, channel, array or vector type, ...)
 *
 * @param type the type which's direct sub-types should be determined.
 * @return the list of identified sub-types
 */
TypeList getElementTypes(const TypePtr& type);

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

// ----------------------------------- Type-Literals ----------------------------

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
 * Tests whether the given literal is a type literal.
 *
 * @param literal the literal to be tested
 * @return true if it is a type literal, false otherwise
 */
static inline bool isTypeLiteral(const LiteralPtr& literal) {
	return literal && isTypeLiteralType(literal->getType());
}

/**
 * Test whether the given node is a type literal
 *
 * @param node the node to be tested
 * @return true if it is a type literal, false otherwise
 */
static inline bool isTypeLiteral(const NodePtr& node) {
	return node && isTypeLiteral(node.isa<LiteralPtr>());
}

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
 * Extracts the represented type from the given type literal expression.
 *
 * @param type the type literal type
 * @return the represented type
 */
static inline TypePtr getRepresentedType(const ExpressionPtr& expr) {
	return getRepresentedType(expr->getType());
}

// ----------------------------------- Type-Parameter ----------------------------

/**
 * Checks whether the given type is a int-type-param literal type.
 *
 * @param type the type to be checked
 * @return true if it is a int-type-param literal type, false otherwise
 */
bool isIntTypeParamType(const TypePtr& type);

/**
 * Checks whether the given literal is an int-type-param literal.
 *
 * @param literal the literal to be tested
 * @return true if so, false otherwise
 */
static inline bool isIntTypeParamLiteral(const LiteralPtr& literal) {
	return literal && isIntTypeParamType(literal->getType());
}

/**
 * Checks whether the given node is an int-type-param literal.
 *
 * @param node the node to be tested
 * @return true if so, false otherwise
 */
static inline bool isIntTypeParamLiteral(const NodePtr& node) {
	return node && isIntTypeParamLiteral(node.isa<LiteralPtr>());
}

/**
 * Extracts the int-type parameter represented by the given int-type-parameter type.
 *
 * @param type the int-type-param type to be processed
 * @return the extracted int-type parameter
 */
static inline IntTypeParamPtr getRepresentedTypeParam(const TypePtr& type) {
	assert(isIntTypeParamType(type));
	return type.as<GenericTypePtr>()->getIntTypeParameter()[0];
}

/**
 * Extract the int-type parameters represented by the given int-type-parameter literal.
 *
 * @param expr the literal / expression to be processed.
 * @return the extracted int-type parameter
 */
static inline IntTypeParamPtr getRepresentedTypeParam(const ExpressionPtr& expr) {
	return getRepresentedTypeParam(expr->getType());
}


/**
 * Tests whether the given node is a constructor expression.
 *
 * @param node the node to be tested
 * @return true if the given node is a constructor, false otherwise
 */
bool isConstructorExpr(const NodePtr& node);

/**
 * Collects a set of all variables encountered within the given code fragment (and
 * all nested fragments - including invoked functions).
 *
 * @param code the code fragment to be analysed
 * @return the set of encountered variables.
 */
VariableSet getAllVariables(const NodePtr& code);

/**
 * Collects a set of all variables encountered within the current scope - hence not including
 * any sub-functions but including recursive function variables.
 *
 * This function should be used whenever introducing a new variable to ensure that there
 * are no collisions.
 *
 * @param code the code fragment to be analysed
 * @return the set of all variables within the current scope
 */
VariableSet getAllVariablesInScope(const NodePtr& code);

/**
 * Extracts the list of free variables from the given code fragment.
 *
 * @param code the code fragment to be analysed
 * @return the list of free variables within the given code fragment
 */
VariableList getFreeVariables(const NodePtr& code);

/**
 * Extracts a list of addresses routed by the given node referencing all
 * free variables.
 *
 * @param code the code fragment to extract free variables from
 * @return the list of addresses referencing those addresses
 */
vector<VariableAddress> getFreeVariableAddresses(const NodePtr& code);

/**
 * Extracts a list of exit point addresses within the given statement. All
 * exit points are either break, continue or return statements.
 *
 * @param stmt the statement for which the exit points should be computed
 * @return the list of exit point addresses rooted by the stmt
 */
vector<StatementAddress> getExitPoints(const StatementPtr& stmt);

/**
 * Extracts a list of exit point addresses within the given statement. All
 * exit points are either break, continue or return statements.
 *
 * @param stmt the statement for which the exit points should be computed
 * @return the list of exit point addresses extending the given stmt address
 */
vector<StatementAddress> getExitPoints(const StatementAddress& stmt);

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

/**
 * Tests whether the given code fragement contains the given element.
 *
 * @param code the IR structure to be inspected recursively
 * @param element the element to be searched
 * @return true if found, false otherwise
 */
bool contains(const NodePtr& code, const NodePtr& element);

/**
 * Tests if the variable is ever asigned or used by reference, if so, is not considered a read only
 * variable
 * is not desdendign into lambdas, only evaluates closest scope
 * @param context the IR structure inspected
 * @param var the variable we are testing
 * @return true if the variable is never assigned or used by reference
 */
bool isReadOnly(const StatementPtr& context, const VariablePtr& var);

/**
 * Tests if the given parameters is a read-only parameter within the given lambda.
 *
 * @param lambda the lambda expression to be tested
 * @param param the parameter to be tested, must be a parameter of the given lambda
 * @return true if the parameter is only read, false if it might be written
 */
bool isReadOnly(const LambdaExprPtr& lambda, const VariablePtr& param);

} // end namespace utils
} // end namespace core
} // end namespace insieme
