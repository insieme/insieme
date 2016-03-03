/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
						res = builder.callExpr(call->getType(), newOp, call->getArguments());
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

	namespace {

		CallExprPtr typeDeductionBasedTypeRecovery(const CallExprPtr& call) {
			// check type
			const ExpressionPtr fun = call->getFunctionExpr();
			const ExpressionList args = call->getArguments();

			// check whether the function type has been preserved
			assert_eq(fun->getType()->getNodeType(), NT_FunctionType) << "Call is no longer targeting function after replacement!";

			FunctionTypePtr funType = fun->getType().as<FunctionTypePtr>();

			TypePtr should = deduceReturnType(funType, extractTypes(call->getArguments()), false);

			// see whether a better type has been deduced
			if(!should || *should == *call->getType()) { return call; }

			// use deduced alternative
			return IRBuilder(call->getNodeManager()).callExpr(should, call->getFunctionExpr(), call->getArguments());
		}
	}

	namespace {

		ExpressionPtr defaultCallExprTypeRecovery(const CallExprPtr& call) {
			// check whether target of call is a literal
			NodeManager& manager = call->getNodeManager();
			const auto& basic = manager.getLangBasic();
			const auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
			if(lang::isBuiltIn(call->getFunctionExpr())) {
				IRBuilder builder(manager);

				auto args = call->getArguments();

				const ExpressionPtr& fun = call->getFunctionExpr().as<ExpressionPtr>();

				// deal with standard build-in funs
				if(refExt.isRefMemberAccess(fun) && analysis::isRefType(args[0]->getType())
				   && analysis::getReferencedType(args[0]->getType()).isa<TagTypePtr>()) {
					return builder.refMember(args[0], args[1].as<LiteralPtr>()->getValue());
				}
				if(basic.isCompositeMemberAccess(fun) && args[0]->getType().isa<TagTypePtr>()) {
					return builder.accessMember(args[0], args[1].as<LiteralPtr>()->getValue());
				}
				if(refExt.isRefComponentAccess(fun)) { return builder.refComponent(args[0], args[1]); }
				if(basic.isTupleMemberAccess(fun)) { return builder.accessComponent(args[0], args[1]); }

				// eliminate unnecessary dereferencing
				if(refExt.isRefDeref(fun) && !analysis::isRefType(args[0]->getType())) { return args[0]; }
			}

			// use generic fall-back solution
			return typeDeductionBasedTypeRecovery(call);
		}
	}

	ExpressionPtr no_type_fixes(const ExpressionPtr&, const ExpressionPtr& newExpr) {
		return newExpr;
	}

	ExpressionPtr defaultTypeRecovery(const ExpressionPtr& oldExpr, const ExpressionPtr& newExpr) {
		// rule out everything that is no change
		if(*oldExpr == *newExpr) { return newExpr; }

		// handle call expressions externally
		if(CallExprPtr call = newExpr.isa<CallExprPtr>()) {
			// check old function / argument types have changed
			if(CallExprPtr oldCall = oldExpr.isa<CallExprPtr>()) {
				bool changed = call.size() != oldCall.size(); // check number of parameters
				changed = changed || (oldCall->getFunctionExpr()->getType() != call->getFunctionExpr()->getType());

				// check argument types
				for(std::size_t i = 0; !changed && i < call.size(); i++) {
					changed = !types::isSubTypeOf(call[i]->getType(), oldCall[i]->getType());
				}

				// if nothing has changed, consider new type to be accurate
				if(!changed) { return newExpr; }
			}

			return defaultCallExprTypeRecovery(call);
		}

		// handle lambda expressions
		if(LambdaExprPtr lambda = newExpr.isa<LambdaExprPtr>()) {
			auto newParams = lambda->getLambda()->getParameters();

			// see whether old version also has been a lambda
			if(LambdaExprPtr oldLambda = oldExpr.isa<LambdaExprPtr>()) {
				// check whether any of the lambdas in the definition has changed its parameter list or type
				LambdaDefinitionPtr oldDef = oldLambda->getDefinition();
				LambdaDefinitionPtr newDef = lambda->getDefinition();

				// if there is no change or a fundamental change => abort
				if(oldDef == newDef || oldDef.size() != newDef.size()) { return newExpr; }

				// see whether types are altered
				bool altered = false;
				for(decltype(oldDef.size()) i = 0; !altered && i < oldDef.size(); i++) {
					LambdaPtr oldEntry = oldDef[i]->getLambda();
					LambdaPtr newEntry = newDef[i]->getLambda();

					auto oldParams = oldEntry->getParameters();
					auto newParams = newEntry->getParameters();

					// test whether the types of the parameters have changed
					if(*oldParams == *newParams || extractTypes(oldParams->getElements()) == extractTypes(newParams->getElements())) { continue; }

					// types have changed => lambdas need to be updated
					altered = true;
				}

				// if there is no change in the types => nothing to fix
				if(!altered) { return newExpr; }


				// ---------- re-build lambda -------------
				IRBuilder builder(newExpr->getNodeManager());

				// 1. rebuild definition
				NodeMap recVarMap;
				LambdaBindingMap bindings;

				for(decltype(oldDef.size()) i = 0; i < oldDef.size(); i++) {
					LambdaBindingPtr oldBinding = oldDef[i];
					LambdaBindingPtr newBinding = newDef[i];

					// check whether something has changed
					if(oldBinding == newBinding) {
						bindings.insert({ newBinding->getReference(), newBinding->getLambda() });
						continue;
					}

					// rebuild lambda using parameters and body ..
					auto newLambda = newBinding->getLambda();

					// re-build function type
					FunctionKind kind = newLambda->getType()->getKind();
					FunctionTypePtr funType;
					{
						auto paramTypes = extractTypes(newLambda->getParameters()->getElements());
						if(kind == FK_CONSTRUCTOR || kind == FK_DESTRUCTOR) {
							funType = builder.functionType(paramTypes, paramTypes[0], kind);
						} else {
							funType = builder.functionType(paramTypes, newLambda->getType()->getReturnType(), kind);
						}
					}

					// re-build lambda
					LambdaPtr lambda = builder.lambda(funType, newLambda->getParameters(), newLambda->getBody());

					// re-build recursive variable
					LambdaReferencePtr lambdaRef = builder.lambdaReference(funType, newBinding->getReference()->getName());
					recVarMap[newBinding->getReference()] = lambdaRef;

					// add new binding
					bindings.insert({ lambdaRef, lambda });
				}

				// 2. update recursive variables
				if(oldLambda->isRecursive()) {
					// update all lambda bodies to reflect new recursive variables
					for(auto& cur : bindings) {
						auto newBody = replaceAllGen(newExpr->getNodeManager(), cur.second->getBody(), recVarMap, transform::globalReplacement);
						cur.second = builder.lambda(cur.second->getType(), cur.second->getParameters(), newBody);
					}
				}

				// re-build definition
				LambdaDefinitionPtr resDef = builder.lambdaDefinition(bindings);

				// 3. re-build lambda expression
				return builder.lambdaExpr(recVarMap[lambda->getReference()].as<LambdaReferencePtr>(), resDef);
			}
		}

		return newExpr;
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
