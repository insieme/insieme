/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
	 * Tests whether the given declaration is side effect free.
	 * A declaration is side effect free if the arguments to its initializing expression are side effect free.
	 *
	 * @param decl the declaration to be tested
	 */
	bool isSideEffectFree(const DeclarationPtr& decl);

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
	 * Obtains the (optional) ref_decl call referenced by the init expression of the given declaration,
	 * if such a call is present. If not, the resulting call will be null.
	 */
	CallExprPtr getRefDeclCall(const DeclarationPtr& decl);

	/**
	 * Tests whether the given call is materializing its result
	 * (this is the case if the function returns a value type but the return value of the call is a reference to that type)
	 */
	bool isMaterializingCall(const NodePtr& candidate);

	/**
	 * Tests whether the given declaration allocates new memory
	 * (this is the case if a plain reference type is declared and not just initialized as an alias to an existing memory location)
	 */
	bool isMaterializingDecl(const NodePtr& candidate);

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
	 * Tests whether the given node is a struct type or an expression of a struct type.
	 *
	 * @return the struct it is representing or a null pointer if it is not a struct
	 */
	StructPtr isStruct(const NodePtr& node);

	/**
	 * Tests whether the given node is a union type or an expression of a union type.
	 *
	 * @return the union it is representing or a null pointer if it is not a union
	 */
	UnionPtr isUnion(const NodePtr& node);

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
	 * Tests whether the given node is reference to a type satisfying the given filter.
	 *
	 * @param candidate the node to be tested
	 * @param filter the filter to be applied on the element type
	 * @return true if so, false otherwise
	 */
	bool isRefOf(const NodePtr& candidate, const std::function<bool(const NodePtr&)>& filter);

	/**
	 * A simple helper utility to test whether the given node is a reference type.
	 *
	 * @param type the type to be tested.
	 * @return true if the given type is a reference type, false otherwise
	 */
	static inline bool isRefType(const NodePtr& type) {
		return lang::isReference(type);
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
	 * @param type the type or expression of the reference to be processed
	 * @return the type of the value referenced by the given reference type or expression
	 */
	static inline TypePtr getReferencedType(const NodePtr& node) {
		if (auto expr = node.isa<ExpressionPtr>()) return getReferencedType(expr->getType());
		assert_true(node) << "Cannot get the referenced type of null pointer";
		assert_true(isRefType(node)) << "Cannot get the referenced type of a non ref type: " << *node;
		return lang::ReferenceType(node).getElementType();
	}

	/**
	 * Tests whether the given declaration statement is an undefined variable declaration or not.
	 *
	 * @return true if the passed declaration is an undefined declaration
	 */
	static inline bool isUndefinedInitalization(const DeclarationStmtPtr& decl) {
		auto& refExt = decl->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
		return refExt.isCallOfRefDecl(decl->getInitialization());
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
	unsigned countInstances(const NodePtr& code, const NodePtr& element, bool limitScope = false);

	/**
	 * Counts the number of occurrences of instances of the given node type in the given code fragment.
	 *
	 * @param code the IR structure to be inspected
	 * @param nodeType the node type to be searched
	 * @return number of instances of element
	 */
	unsigned countInstancesOfNodeType(const NodePtr& code, const NodeType& nodeType, bool limitScope = false);

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
	 * Tests whether the given type has free tag type references or not.
	 *
	 * @param type the type to be tested
	 * @return true if it has free tag type references, false otherwise
	 */
	bool hasFreeTagTypeReferences(const TypePtr& type);

	/**
	 * Obtains the canonical representation of the given type. For instance,
	 * generic parameters will be normalized and recursive types presented
	 * in their most compact form.
	 */
	TypePtr getCanonicalType(const TypePtr& a);

	/**
	 * Compare two types whether they are semantically equivalent.
	 * In particular, this comparison is handling the unrolling of recursive types.
	 */
	bool equalTypes(const TypePtr& a, const TypePtr& b);

	/**
	 * Tests whether the given expression is zero or not.
	 */
	bool isZero(const core::ExpressionPtr& value);

	/**
	 * Returns the lowest common ancestor of all passed node addresses. All Addresses must have the same root node.
	 */
	NodeAddress getLowestCommonAncestor(const std::vector<NodeAddress>& addresses);

	/**
	 * Checks whether there is a loop on the path between top and bottom.
	 */
	bool hasLoopInBetween(const NodeAddress& top, const NodeAddress& bottom);

	// ----------------------------------- Lambda extraction + helpers ----------------------------

	/**
	 * Tests whether the given statement contains a control statement (e.g., NT_BreakStmt).
	 * The search is pruned at given node types listed in pruneStmts.
	 */
	bool hasFreeControlStatement(const StatementPtr& stmt, NodeType controlStmt, const vector<NodeType>& pruneStmts);

	/**
	 * Tests whether the given statement is outline-able or not.
	 */
	bool isOutlineAble(const StatementPtr& stmt, bool allowReturns = false);

	/**
	 * Tests whether the given statement contains a free break statement.
	 */
	bool hasFreeBreakStatement(const StatementPtr& stmt);

	/**
	 * Tests whether the given statement contains a free continue statement.
	 */
	bool hasFreeContinueStatement(const StatementPtr& stmt);

	/**
	 * Tests whether the given statement contains a free return statement.
	 */
	bool hasFreeReturnStatement(const StatementPtr& stmt);

	/**
	 * Returns the first parent of the passed address which has been a function originating from the input code.
	 */
	LambdaExprAddress getLowestUserDefinedFunctionAbove(const NodeAddress& node);

	/**
	 * Returns the lambda corresponding to a given LambdaReferenceAddress.
	 */
	LambdaExprAddress getLambdaFromReference(const LambdaReferenceAddress& reference);

	// ------------------------------- utilities for recursive constructs -------------------------

	/**
	 * Computes equivalent definitions of the given tag type definition only containing
	 * minimal definition group sizes.
	 */
	std::map<TagTypeReferencePtr, TagTypePtr> minimizeRecursiveGroup(const TagTypeDefinitionPtr& def);

	/**
	 * Computes equivalent definitions of the given lambda definitions only containing
	 * minimal definition group sizes.
	 */
	std::map<LambdaReferencePtr, LambdaExprPtr> minimizeRecursiveGroup(const LambdaDefinitionPtr& def);

} // end namespace utils
} // end namespace core
} // end namespace insieme
