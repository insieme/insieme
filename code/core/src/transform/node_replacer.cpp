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
 *
 */
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/core/types/subtyping.h"
#include "insieme/core/types/return_type_deduction.h"

#include "insieme/utils/logging.h"

namespace {

	using namespace insieme::core;
	using namespace insieme::core::transform;
	using namespace insieme::core::types;

	using namespace insieme::utils::map;

	/**
	 * Visitor which replace a specific node of the IR starting from a root node.
	 */
	class NodeReplacer : public CachedNodeMapping {
		NodeManager& manager;
		const PointerMap<NodePtr, NodePtr>& replacements;
		const ReplaceLimiter limiter;
		bool interrupted = false;

	  public:
		NodeReplacer(NodeManager& manager, const PointerMap<NodePtr, NodePtr>& replacements, const ReplaceLimiter limiter)
		    : manager(manager), replacements(replacements), limiter(limiter) {}

	  private:
		virtual const NodePtr resolveElement(const NodePtr& ptr) {
			// we shouldn't do anything if replacement was interrupted
			if(interrupted) { return ptr; }

			// check which action to perform for this node
			ReplaceAction repAction = limiter(ptr);

			if(repAction == ReplaceAction::Interrupt) {
				interrupted = true;
				return ptr;
			} else if(repAction == ReplaceAction::Prune) {
				return ptr;
			}

			if(repAction != ReplaceAction::Skip) {
				// check whether the element has been found
				auto pos = replacements.find(ptr);
				if(pos != replacements.end()) { return pos->second; }
			}

			// recursive replacement has to be continued
			NodePtr res = ptr->substitute(manager, *this);

			// check whether something has changed ...
			if(res == ptr) {
				// => nothing changed
				return ptr;
			}

			// preserve annotations
			utils::migrateAnnotations(ptr, res);

			// done
			return res;
		}
	};

	class TypeVariableReplacer : public CachedNodeMapping {
		NodeManager& manager;
		const SubstitutionOpt& substitution;

	  public:
		TypeVariableReplacer(NodeManager& manager, const SubstitutionOpt& substitution) : manager(manager), substitution(substitution) {
			assert_true(substitution && !substitution->empty()) << "Substitution must not be empty!";
		}

	  private:
		/**
		 * Performs the recursive clone operation on all nodes passed on to this visitor.
		 */
		virtual const NodePtr resolveElement(const NodePtr& ptr) {
			// check whether the element has been found
			if(ptr->getNodeType() == NT_TypeVariable) { return substitution->applyTo(manager, static_pointer_cast<const Type>(ptr)); }

			// handle scope limiting elements
			switch(ptr->getNodeType()) {
			case NT_LambdaExpr:
			case NT_FunctionType:
				// enters a new scope => variable will no longer occur
				return ptr;
			default: {}
			}

			// recursive replacement has to be continued
			NodePtr res = ptr->substitute(manager, *this);

			// fix generic operators ... gen.eq => int.eq if parameters are properly instantiated
			if(res->getNodeType() == NT_CallExpr) {
				auto& basic = manager.getLangBasic();
				CallExprPtr call = res.as<CallExprPtr>();
				if(basic.isGenOp(call->getFunctionExpr())) {
					// see whether operation can be replaced
					auto oldOp = call->getFunctionExpr().as<LiteralPtr>();
					auto opCode = basic.getOperator(oldOp);
					auto newOp = basic.getOperator(call->getArgument(0)->getType(), opCode);

					// if there was a change
					if(newOp && newOp != oldOp) {
						// exchange operator
						IRBuilder builder(manager);
						res = builder.callExpr(call->getType(), newOp, call->getArgumentDeclarations());
					}
				}
			}

			// check whether something has changed ...
			if(res == ptr) {
				// => nothing changed
				return ptr;
			}

			// preserve annotations
			utils::migrateAnnotations(ptr, res);

			// correct type literal name => cosmetic
			if(res->getNodeType() == NT_Literal) {
				const LiteralPtr& literal = static_pointer_cast<const Literal>(res);
				if(analysis::isTypeLiteralType(literal->getType())) {
					const TypePtr& type = analysis::getRepresentedType(literal->getType());

					// update type
					return IRBuilder(manager).getTypeLiteral(type);
				}
			}

			// done
			return res;
		}
	};


	class NodeAddressReplacer : public SimpleNodeMapping {
		const unsigned indexToReplace;
		const NodePtr& replacement;

	  public:
		NodeAddressReplacer(unsigned index, const NodePtr& replacement) : indexToReplace(index), replacement(replacement) {}

	  private:
		/**
		 * Represents an identity-operation except for the one element to be replaced,
		 * which is identified by its index.
		 *
		 * @param index the index of the element to be mapped
		 * @param ptr a pointer to the element to be mapped
		 * @return a pointer to the mapped element
		 */
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			if(indexToReplace == index) { return replacement; }
			return ptr;
		}
	};
}

namespace insieme {
namespace core {
namespace transform {

	std::ostream& operator<<(std::ostream& out, const ReplaceAction& action) {
		switch(action) {
			case ReplaceAction::Interrupt : return out << "ReplaceAction::Interrupt";
			case ReplaceAction::Process : return out << "ReplaceAction::Process";
			case ReplaceAction::Prune : return out << "ReplaceAction::Prune";
			case ReplaceAction::Skip : return out << "ReplaceAction::Skip";
		}
		return out;
	}

	NodePtr applyReplacer(NodeManager& mgr, const NodePtr& root, SimpleNodeMapping& mapper) {
		NodePtr res = NodePtr(NULL);
		if(!root) { return res; }

		// map root node element
		try {
			res = mapper.map(0, root);
		} catch(const std::bad_function_call& e) { LOG(ERROR) << e.what() << ": limiter/mapper invalid"; }

		// preserve annotations
		if(res != root) { utils::migrateAnnotations(root, res); }
		return res;
	}

	NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const NodePtr& toReplace, const NodePtr& replacement, const ReplaceLimiter limiter) {
		// check whether there is something to do
		if(*toReplace == *replacement) { return mgr.get(root); }

		// perform replacement
		NodeMap replacements{{toReplace, replacement}};
		NodeReplacer replacer(mgr, replacements, limiter);
		return applyReplacer(mgr, root, replacer);
	}

	NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const NodeMap& replacements, const ReplaceLimiter limiter) {
		// shortcut for empty replacements
		if(replacements.empty()) { return mgr.get(root); }

		// perform replacement
		NodeReplacer replacer(mgr, replacements, limiter);
		return applyReplacer(mgr, root, replacer);
	}

	NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const VariableMap& replacements) {
		NodeMap repMap = ::transform(
		    replacements, [](const std::pair<VariablePtr, VariablePtr>& p) { return std::make_pair(p.first.as<NodePtr>(), p.second.as<NodePtr>()); });
		return replaceAll(mgr, root, repMap);
	}

	NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const VarExprMap& replacements) {
		NodeMap repMap = ::transform(
		    replacements, [](const std::pair<VariablePtr, ExpressionPtr>& p) { return std::make_pair(p.first.as<NodePtr>(), p.second.as<NodePtr>()); });
		return replaceAll(mgr, root, repMap);
	}

	NodePtr replaceTypeVars(NodeManager& mgr, const NodePtr& root, const SubstitutionOpt& substitution) {
		assert_true(root) << "Root must not be a null pointer!";

		// check whether there is something to do
		if(!substitution || substitution->empty()) { return root; }

		// conduct actual substitution
		auto mapper = ::TypeVariableReplacer(mgr, substitution);
		return applyReplacer(mgr, root, mapper);
	}


	NodePtr replaceAll(NodeManager& mgr, const std::map<NodeAddress, NodePtr>& replacements) {
		typedef std::pair<NodeAddress, NodePtr> Replacement;

		// check preconditions
		assert_false(replacements.empty()) << "Replacements must not be empty!";

		assert(all(replacements, [&](const Replacement& cur) { return cur.first.isValid() && cur.second; })
		       && "Replacements are no valid addresses / pointers!");

		assert(all(replacements, [&](const Replacement& cur) { return *cur.first.getRootNode() == *replacements.begin()->first.getRootNode(); })
		       && "Replacements do not all have the same root node!");


		// convert replacements into edit-able list
		vector<Replacement> steps(replacements.rbegin(), replacements.rend());

		NodePtr res = replaceNode(mgr, steps.front().first, steps.front().second);
		for(auto it = steps.begin() + 1; it != steps.end(); ++it) {
			// conduct current replacements using updated address
			res = replaceNode(mgr, it->first.switchRoot(res), it->second);
		}

		// return final node version
		return res;
	}

	NodePtr replaceNode(NodeManager& manager, const NodeAddress& toReplace, const NodePtr& replacement) {
		assert_true(toReplace.isValid()) << "Invalid node address provided!";

		// short-cut for replacing the root
		if(toReplace.isRoot()) { return replacement; }

		// create result
		NodePtr res = replacement;

		// process the path bottom up => replace one node after another
		auto path = toReplace.getPath();

		// replace bottom up
		unsigned lastPos = path.getIndex();
		while(path.getLength() > 1) {
			// go to parent
			path = path.getPathToParent();

			// conduct replace operation
			auto mapper = NodeAddressReplacer(lastPos, res);
			const NodePtr& cur = path.getAddressedNode();
			res = cur->substitute(manager, mapper);

			// preserve annotations
			utils::migrateAnnotations(cur, res);

			// update last-pos
			lastPos = path.getIndex();
		}

		// done
		return res;
	}

	NodeAddress replaceAddress(NodeManager& manager, const NodeAddress& toReplace, const NodePtr& replacement) {
		if(toReplace.isRoot()) { return NodeAddress(replacement); }
		NodePtr newRoot = replaceNode(manager, toReplace, replacement);
		return toReplace.switchRoot(newRoot);
	}

	namespace detail {
		/**
		 * Mapper which performs transformation on IR starting from a root node.
		 */
		class NodeTransformer : public CachedNodeMapping {
			NodeManager& manager;
			const TransformFunc& transformation;
			const ReplaceLimiter limiter;
			bool interrupted = false;

		  public:
			NodeTransformer(NodeManager& manager, const TransformFunc& transformation, const ReplaceLimiter limiter)
				: manager(manager), transformation(transformation), limiter(limiter) {}

		  private:
			virtual const NodePtr resolveElement(const NodePtr& ptr) {
				// we shouldn't do anything if replacement was interrupted
				if(interrupted) { return ptr; }

				// check which action to perform for this node
				ReplaceAction repAction = limiter(ptr);

				if(repAction == ReplaceAction::Interrupt) {
					interrupted = true;
					return ptr;
				} else if(repAction == ReplaceAction::Prune) {
					return ptr;
				}

				// recursive replacement has to be continued
				NodePtr res = ptr->substitute(manager, *this);

				if(repAction == ReplaceAction::Skip) {
					return res;
				}

				res = transformation(res);

				// check whether something has changed ...
				if(res == ptr) {
					// => nothing changed
					return ptr;
				}

				// preserve annotations
				utils::migrateAnnotations(ptr, res);

				// done
				return res;
			}
		};

		NodePtr transformBottomUpInternal(const NodePtr& root, const TransformFunc& transformFunc, const ReplaceLimiter& replaceLimiter) {
			NodeTransformer transformer(root.getNodeManager(), transformFunc, replaceLimiter);
			return transformer.map(root);
		}
	}

} // End transform namespace
} // End core namespace
} // End insieme namespace
