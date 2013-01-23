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

#include <boost/optional/optional.hpp>

#include "insieme/utils/map_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {

class Substitution;
typedef boost::optional<Substitution> SubstitutionOpt;

class IRBuilder;

namespace transform {

/**
 * Replaces all occurrences of a specific nodes within the given AST sub-tree with a given replacement.
 *
 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
 * @param root the root of the sub-tree to be manipulated
 * @param toReplace the node to be replaced during this operation
 * @param replacement the node to be used as a substitution for the toReplace node
 */
NodePtr replaceAll(NodeManager& mgr, const NodePtr& root,
		const NodePtr& toReplace, const NodePtr& replacement, bool limitScope = true);

/**
 * Replaces all occurrences of the specified variable within the given sub-tree with the given replacement.
 *
 * @param mgr the manager used for creating and maintaining new node instances if required
 * @param root the root of the sub-tree to be manipulated
 * @param toReplace the variable to be replaced during this operation
 * @param replacement the node to be used as a substitution for the toReplace node
 */
NodePtr replaceAll(NodeManager& mgr, const NodePtr& root,
		const VariablePtr& toReplace, const NodePtr& replacement, bool limitScope = true);

/**
 * Replaces all occurrences of a specific nodes within the given AST sub-tree with a given replacement.
 * The replacements are provided via a map.
 *
 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
 * @param root the root of the sub-tree to be manipulated
 * @param replacements the map mapping nodes to their replacements
 * @return the modified version of the root node
 */
NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const NodeMap& replacements, bool limitScope = true);

/**
 * A generic wrapper for the function provided above. This operation returns the same kind of node
 * pointer is getting passed as an argument.
 */
template<typename T>
core::Pointer<T> replaceAllGen(NodeManager& mgr, const core::Pointer<T>& root, const NodeMap& replacements, bool limitScope = true) {
	return static_pointer_cast<T>(replaceAll(mgr, root, replacements, limitScope));
}

/**
 * A generic wrapper for the function provided above. This operation returns the same kind of node
 * pointer is getting passed as an argument.
 */
template<typename T>
core::Pointer<T> replaceAllGen(NodeManager& mgr, const core::Pointer<T>& root, 
		const NodePtr& toReplace, const NodePtr& replacement, bool limitScope = true) {
	return static_pointer_cast<T>(replaceAll(mgr, root, toReplace, replacement, limitScope));
}


/**
 * Replaces all the nodes addressed within the given map by the associated replacements. The given addresses
 * have to be relative to the same root node and the modified version of this node will be returned.
 *
 * @param mgr the node manager to be used for new nodes created during the replacement operations
 * @param replacements a map linking addressed nodes to their replacements
 * @return the modified version of the common root node of all the given replacements
 */
NodePtr replaceAll(NodeManager& mgr, const std::map<NodeAddress, NodePtr>& replacements);

/**
 * Replaces all occurrences of the variables within the given map and the current scope by the element associated
 * to them.
 *
 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
 * @param root the root of the sub-tree to be manipulated
 * @param replacements the map mapping variables to their replacements
 */
NodePtr replaceVars(NodeManager& mgr, const NodePtr& root,
		const VariableMap& replacements);

NodePtr replaceVars(NodeManager& mgr, const NodePtr& root,
		const VarExprMap& replacements);

/**
 * A generic wrapper for the function provided above. This operation returns the same kind of node
 * pointer is getting passed as an argument.
 */
template<typename T>
core::Pointer<T> replaceVarsGen(NodeManager& mgr, const core::Pointer<T>& root,
		const VariableMap& replacements) {
	return static_pointer_cast<T>(replaceVars(mgr, root, replacements));
}

template<typename T>
core::Pointer<T> replaceVarsGen(NodeManager& mgr, const core::Pointer<T>& root,
		const VarExprMap& replacements) {
	return static_pointer_cast<T>(replaceVars(mgr, root, replacements));
}

/**
 * pre-implemented functors to be used with replaceVarRecursive[Gen]
 */

typedef std::function<StatementPtr(const StatementPtr&)> TypeHandler;
typedef std::function<ExpressionPtr(const ExpressionPtr&, const ExpressionPtr&)> TypeRecoveryHandler;

ExpressionPtr defaultTypeRecovery(const ExpressionPtr& oldExpr, const ExpressionPtr& newExpr);

// functor which updates the type literal inside a call to undefined in a declaration
TypeHandler getVarInitUpdater(NodeManager& manager);

/**
 * Replaces all variables within the given map within the current scope by the associated elements. If
 * variables are passed to functions accepting different types, a new version of the function accepting
 * the correct type will be generated.
 *
 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
 * @param root the root of the sub-tree to be manipulated
 * @param replacements the map mapping variables to their replacements
 * @param limitScope a flag determining if also variables in inner scopes should be considered
 * @param recoveryHandler the function to be used to recover the proper typing of function calls during the replacement
 * @param typeHandler a function to be called if the correct return type cannot be determined by default
 */
NodePtr replaceVarsRecursive(NodeManager& mgr, const NodePtr& root,
		const utils::map::PointerMap<VariablePtr, VariablePtr>& replacements, bool limitScope = true,
		const TypeRecoveryHandler& recoveryHandler = defaultTypeRecovery,
		const TypeHandler& typeHandler = id<ExpressionPtr>());

/**
 * Replaces all variables within the given map within the current scope by the associated elements. If
 * variables are passed to functions accepting different types, a new version of the function accepting
 * the correct type will be generated.
 *
 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
 * @param root the root of the sub-tree to be manipulated
 * @param replacements the map mapping variables to their replacements
 * @param limitScope a flag determining if also variables in inner scopes should be considered
 * @param recoveryHandler the function to be used to recover the proper typing of function calls during the replacement
 * @param typeHandler a function to be called if the correct return type cannot be determined by default
 */
template<typename T>
Pointer<const T> replaceVarsRecursiveGen(NodeManager& mgr, const Pointer<const T>& root,
		const VariableMap& replacements, bool limitScope = true,
		const TypeRecoveryHandler& recoveryHandler = defaultTypeRecovery,
		const TypeHandler& typeHandler = id<StatementPtr>()) {
	return static_pointer_cast<const T>(replaceVarsRecursive(mgr, root, replacements, limitScope, recoveryHandler, typeHandler));
}


/**
 * Replaces all variables within the given map within the current scope by the associated elements. If
 * variables are passed to functions accepting different types, a new version of the function accepting
 * the correct type will be generated. Furthermore it executes recoveryHandler on every call to a lambda.
 * By default this will try to fix type inconsistencies in tuple/struct accesses and deref operations
 *
 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
 * @param root the root of the sub-tree to be manipulated
 * @param replacements the map mapping variables to their replacements
 * @param limitScope a flag determining if also variables in inner scopes should be considered
 * @param typeHandler a function to be called if the correct return type cannot be determined by default
 */
NodePtr fixTypes(NodeManager& mgr, NodePtr root, const VariableMap& replacements, bool limitScope,
		const TypeHandler& typeHandler = id<StatementPtr>() );

/**
 * Replaces all variables within the given map within the current scope by the associated elements. If
 * variables are passed to functions accepting different types, a new version of the function accepting
 * the correct type will be generated. Furthermore it executes recoveryHandler on every call to a lambda.
 * By default this will try to fix type inconsistencies in tuple/struct accesses and deref operations
 *
 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
 * @param root the root of the sub-tree to be manipulated
 * @param replacements the map mapping variables to their replacements
 * @param limitScope a flag determining if also variables in inner scopes should be considered
 * @param typeHandler a function to be called if the correct return type cannot be determined by default
 */
template<typename T>
Pointer<const T> fixTypesGen(NodeManager& mgr, const Pointer<const T> root, const VariableMap& replacements, bool limitScope,
		const TypeHandler& typeHandler = id<StatementPtr>()) {
	return static_pointer_cast<const T>(fixTypes(mgr, root, replacements, limitScope, typeHandler));
}

/**
 * Replaces all occurrences of the type variables and int type parameters within the top level scope of the
 * given root node by the values assigned to them within the given substitution.
 *
 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
 * @param root the root of the sub-tree to be manipulated
 * @param substitution the substitution defining the mapping of variables to their instantiations
 */
NodePtr replaceTypeVars(NodeManager& mgr, const NodePtr& root, const SubstitutionOpt& substitution);

/**
 * Replaces the node specified by the given address and returns the root node of the modified tree.
 *
 * @param manager the manager to be used for maintaining node instances
 * @param toReplace the address of the node to be replaced
 * @param replacement the node to be used as a substitution for the toReplace node
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr replaceNode(NodeManager& manager, const NodeAddress& toReplace, const NodePtr& replacement);

/**
 * Replaces the node specified by the given address and returns the address of the replacement node
 * in the modified tree.
 *
 * @param manager the manager to be used for maintaining node instances
 * @param toReplace the address of the node to be replaced
 * @param replacement the node to be used as a substitution for the toReplace node
 * @return the address of the replacement node in the generated tree
 */
NodeAddress replaceAddress(NodeManager& manager, const NodeAddress& toReplace, const NodePtr& replacement);

} // End transform namespace
} // End core namespace
} // End insieme namespace
