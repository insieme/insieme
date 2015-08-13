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

#pragma once

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_types.h"

#include "insieme/core/lang/reference.h"

namespace insieme {
namespace core {
namespace analysis {


	/**
	 * Gets all nodes of the given type in node which are free.
	 * A node is free if it is not enclosed by any node of the types listed in pruneNodes.
	 *
	 * @param stmt the statement to be tested
	 * @return list of free nodes
	 */
	NodeList getFreeNodes(const NodePtr& node, NodeType controlStmt, const vector<NodeType>& pruneNodes);

	/**
	 * Tests whether the given expression is side effect free.
	 * An expression is side effect free if it is composed only of calls to pure functions.
	 *
	 * @param expr the expression to be tested
	 */
	bool isSideEffectFree(const ExpressionPtr& expr);

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
		assert_eq(call->getNodeType(), core::NT_CallExpr) << "Call has to be a call expression!";
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
	 * Determines whether the given node contains parallel code (that is, calls to "parallel").
	 *
	 * @param node the node to be checked
	 * @return true if it contains parallel code, false otherwise
	 */
	bool isParallel(const NodePtr& node);

	/**
	 * A universal utility extracting a list of element types from any given
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
	 * A simple helper utility to test whether the given node is a reference type.
	 *
	 * @param type the type to be tested.
	 * @return true if the given type is a reference type, false otherwise
	 */
	static inline bool isRefType(const NodePtr& type) {
		return lang::ReferenceType::isReferenceType(type);
	}

	/**
	 * Determines whether the given expression has is of a reference type.
	 *
	 * @param expr the expression to be tested
	 * @return true if so, false otherwise
	 */
	static inline bool hasRefType(const ExpressionPtr& expr) {
		return expr && isRefType(expr->getType());
	}

	/**
	 * Obtains the type referenced by the given reference type.
	 *
	 * @param type the type of the reference to be processed
	 * @return the type of the value referenced by the given reference type
	 */
	static inline TypePtr getReferencedType(const NodePtr& type) {
		assert_true(isRefType(type)) << "Cannot get the referenced type of a non ref type.";
		return lang::ReferenceType(type).getElementType();
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
		assert_true(isTypeLiteralType(type));
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
	 * Determines whether the given node is exhibiting free variables or not.
	 *
	 * @param code the code fragment to be analysed
	 * @return true if it is exhibiting free variables, false otherwise
	 */
	bool hasFreeVariables(const NodePtr& code);

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
	 * Tests whether the given code fragment has a free variable satisfying the given filter.
	 */
	bool hasFreeVariable(const NodePtr& code, const std::function<bool(VariablePtr)>& filter);

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
	 * Retrieves the name of variable in all the outer scopes up to his declaration.
	 *
	 * @param var is the variable
	 * @code is the scope
	 * @return a vector of names of the variable from the inner scope up to the declaration
	 */
	std::vector<VariablePtr> getVariableNames(const VariablePtr& var, const NodePtr& code);

	/**
	 * Retrieves the name of variable in the outer scope where it has been declared.
	 *
	 * @param varlist the list of addresses of renamed variables
	 * @return a map of the addresses in varlist to their declaration in the outermost scope
	 */
	utils::map::PointerMap<VariableAddress, VariableAddress> getRenamedVariableMap(const std::vector<VariableAddress> varlist);

	/**
	 * Retrieves the name of variable in the outer scope where it has been declared.
	 *
	 * @param varMap map of the addresses in varlist to their declaration in the outermost scope
	 *        will be updated with aliases of the outer scope in place
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

	/**
	 * Returns the inner type of a volatile type.
	 *
	 * @return inner type of "type"
	 */
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
	 * Tests whether the given code fragment contains the given element.
	 *
	 * @param code the IR structure to be inspected recursively
	 * @param element the element to be searched
	 * @return true if found, false otherwise
	 */
	bool contains(const NodePtr& code, const NodePtr& element);

	/**
	 * Counts the number of occurrences of instances of the given element in the given code fragment.
	 *
	 * @param code the IR structure to be inspected
	 * @param element the element to be searched
	 * @return number of instances of element
	 */
	unsigned countInstances(const NodePtr& code, const NodePtr& element);

	/**
	 * Tests if the variable is ever assigned or used by reference, if so, is not considered a read only
	 * variable
	 * is not descending into lambdas, only evaluates closest scope
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

	/**
	 * 	Test if the parameter is read or written in the scope, if passed by reference
	 * 	to any other scope it will be assumed to be non read-only,
	 * 	no matters what happens inside of that lambda
	 *
	 * @param lambda the lambda expression to be tested
	 * @param param the parameter to be tested, must be a parameter of the given lambda
	 * @return true if the parameter is only read, false if it might be written
	 */
	bool isReadOnlyWithinScope(const StatementPtr& context, const VariablePtr& param);

	/**
	 * test if the expression is the usage of an static variable ( local visibility, global storage)
	 *
	 * @param var, the expression to test
	 * @return whenever is a static
	 */
	bool isStaticVar(const ExpressionPtr& var);

	/**
	 * compare given typePtrs, trying to unroll rectypes
	 */
	bool compareTypes(const TypePtr& a, const TypePtr& b);


	// ----------------------------------- Jobs ----------------------------

	/**
	 * Tests whether the given expression is zero or not.
	 */
	bool isZero(const core::ExpressionPtr& value);


} // end namespace utils
} // end namespace core
} // end namespace insieme
