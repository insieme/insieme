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

#include <boost/optional/optional.hpp>

#include "insieme/utils/map_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"

#include "insieme/core/types/substitution.h"

namespace insieme {
namespace core {

	class IRBuilder;

	namespace transform {

		/**
		 * Possible actions for the replacement utility limiter
		 *
		 * Process: simply process the given node
		 * Skip: skip the given node, but continue processing those below it
		 * Prune: prune the tree at this point, skipping the node and everything below it
		 * Interrupt: interrupt replacement, and stop completely
		 */
		enum class ReplaceAction { Process, Skip, Prune, Interrupt };

		std::ostream& operator<<(std::ostream& out, const ReplaceAction& action);

		using ReplaceLimiter = std::function<ReplaceAction(const NodePtr&)>;
		using TransformFunc = std::function<NodePtr(const NodePtr&)>;

		const static ReplaceLimiter globalReplacement = [](const NodePtr& cur) { return ReplaceAction::Process; };
		const static ReplaceLimiter localReplacement = [](const NodePtr& cur) {
			return cur.isa<LambdaExprPtr>() ? ReplaceAction::Prune : ReplaceAction::Process;
		};


		/**
		 * Replaces all occurrences of a specific nodes within the given AST sub-tree with a given replacement.
		 *
		 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
		 * @param root the root of the sub-tree to be manipulated
		 * @param toReplace the node to be replaced during this operation
		 * @param replacement the node to be used as a substitution for the toReplace node
		 * @param limiter customizes the scope of the replacement
		 * @return the modified version of the root node
		 */
		NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const NodePtr& toReplace, const NodePtr& replacement,
		                   const ReplaceLimiter limiter = localReplacement);

		/**
		 * Replaces all occurrences of a specific nodes within the given AST sub-tree with a given replacement.
		 * The replacements are provided via a map.
		 *
		 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
		 * @param root the root of the sub-tree to be manipulated
		 * @param replacements the map mapping nodes to their replacements
		 * @param limiter customizes the scope of the replacement
		 * @return the modified version of the root node
		 */
		NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const NodeMap& replacements, const ReplaceLimiter limiter = localReplacement);

		/**
		 * A generic wrapper for the function provided above. This operation returns the same kind of node
		 * pointer is getting passed as an argument.
		 */
		template <typename T>
		core::Pointer<T> replaceAllGen(NodeManager& mgr, const core::Pointer<T>& root, const NodeMap& replacements,
		                               const ReplaceLimiter limiter = localReplacement) {
			return static_pointer_cast<T>(replaceAll(mgr, root, replacements, limiter));
		}

		/**
		 * A generic wrapper for the function provided above. This operation returns the same kind of node
		 * pointer is getting passed as an argument.
		 */
		template <typename T>
		core::Pointer<T> replaceAllGen(NodeManager& mgr, const core::Pointer<T>& root, const NodePtr& toReplace, const NodePtr& replacement,
		                               const ReplaceLimiter limiter = localReplacement) {
			return static_pointer_cast<T>(replaceAll(mgr, root, toReplace, replacement, limiter));
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
		NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const VariableMap& replacements);

		NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const VarExprMap& replacements);

		/**
		 * A generic wrapper for the function provided above. This operation returns the same kind of node
		 * pointer is getting passed as an argument.
		 */
		template <typename T>
		core::Pointer<T> replaceVarsGen(NodeManager& mgr, const core::Pointer<T>& root, const VariableMap& replacements) {
			return static_pointer_cast<T>(replaceVars(mgr, root, replacements));
		}

		template <typename T>
		core::Pointer<T> replaceVarsGen(NodeManager& mgr, const core::Pointer<T>& root, const VarExprMap& replacements) {
			return static_pointer_cast<T>(replaceVars(mgr, root, replacements));
		}

		/**
		 * pre-implemented functors to be used with replaceVarRecursive[Gen]
		 */

		typedef std::function<StatementPtr(const StatementPtr&)> TypeHandler;
		typedef std::function<ExpressionPtr(const ExpressionPtr&, const ExpressionPtr&)> TypeRecoveryHandler;

		/**
		 * Replaces all occurrences of the type variables and int type parameters within the top level scope of the
		 * given root node by the values assigned to them within the given substitution.
		 *
		 * @param mgr the manager used to maintain new nodes, in case new nodes have to be formed
		 * @param root the root of the sub-tree to be manipulated
		 * @param substitution the substitution defining the mapping of variables to their instantiations
		 */
		NodePtr replaceTypeVars(NodeManager& mgr, const NodePtr& root, const types::SubstitutionOpt& substitution);

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

		namespace detail {
			NodePtr transformBottomUpInternal(const NodePtr& root, const TransformFunc& transformFunc, const ReplaceLimiter& replaceLimiter);
		}

		/**
		 * Applies the transformation function bottom up on the passed IR tree, pruning/skipping/interrupting as per the limiter.
		 * NOTE: caches the transformation, do not depend on stateful mapping
		 */
		template<typename TransformLambda,
			typename LambdaArgType = typename std::remove_const<typename std::remove_reference<typename lambda_traits<TransformLambda>::arg1_type>::type>::type>
		NodePtr transformBottomUp(const NodePtr& root, TransformLambda transformFunc, const ReplaceLimiter& replaceLimiter = localReplacement) {
			TransformFunc adjustedTransformFunc = [&transformFunc](const NodePtr& node) -> NodePtr {
				if(!node.isa<LambdaArgType>()) return node;
				return transformFunc(node.as<LambdaArgType>());
			};

			return detail::transformBottomUpInternal(root, adjustedTransformFunc, replaceLimiter);
		}

		/**
		 * Applies the transformation function bottom up on the passed IR tree, pruning/skipping/interrupting as per the limiter.
		 * NOTE: caches the transformation, do not depend on stateful mapping
		 * Note: make sure you are not changing the type of the root node in the transformation
		 */
		template<typename NodeType, typename TransformLambda>
		NodeType transformBottomUpGen(const NodeType& root, TransformLambda transformFunc, const ReplaceLimiter& replaceLimiter = localReplacement) {
			return transformBottomUp(root, transformFunc, replaceLimiter).template as<NodeType>();
		}

	} // End transform namespace
} // End core namespace
} // End insieme namespace
