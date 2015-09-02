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

#include "insieme/core/transform/instantiate.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/address_mapper.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace transform {

	namespace {
		typedef std::pair<NodeMap, bool> InstContext;

		class TypeInstantiator : public NodeMapping<InstContext> {
		  private:
			std::function<bool(const NodePtr& node)> skip;

			template <template <typename> class Ptr, class T, typename Result = Ptr<T>>
			Result apply(const NodeMap& map, const Ptr<T>& ptr) {
				if(map.find(ptr) != map.end()) { return (map.find(ptr)->second).template as<Result>(); }
				return ptr;
			}

			LambdaExprPtr generateSubstituteLambda(NodeManager& nodeMan, const LambdaExprPtr& funExp, NodeMap& nodeMap) {
				// create limiter based on skip
				ReplaceLimiter limiter = [&](const NodePtr& ptr) {
					return (ptr.isa<core::LambdaExprPtr>() || skip(ptr)) ? ReplaceAction::Prune : ReplaceAction::Process;
				};

				// using the derived mapping, we generate a new lambda expression with instantiated type params
				IRBuilder builder(nodeMan);
				auto funType = funExp->getFunctionType();
				auto replacedBody = funExp->getBody();
				// only apply replacement to body of lambdas which we are not supposed to skip
				if(!skip(funExp)) { replacedBody = replaceAllGen(nodeMan, funExp->getBody(), nodeMap, limiter); }
				// tricky: we apply the same mapping recursively on the *new* body
				InstContext newContext = {nodeMap, skip(funExp)};
				auto newBody = replacedBody->substitute(nodeMan, *this, newContext);

				// we now need to determine the new return type to use
				TypePtr newReturnType = funType->getReturnType();
				// generate a new return type if necessary
				if(!skip(funExp) && analysis::isGeneric(newReturnType)) {
					// constructors and destructors can not be automatically typed!
					if(funType->getKind() != FK_CONSTRUCTOR && funType->getKind() != FK_DESTRUCTOR) {
						newReturnType = analysis::autoReturnType(nodeMan, newBody);
						newReturnType = replaceAllGen(nodeMan, newReturnType, nodeMap, limiter);
					}
				}
				LOG(DEBUG) << "body: " << *funExp->getBody() << "\n";
				LOG(DEBUG) << "newBody: " << *newBody << "\n";
				LOG(DEBUG) << "newReturnType nodemap: " << nodeMap << "\n";
				LOG(DEBUG) << "NewReturnType: " << *newReturnType << "\n";

				// now we can generate the substitution
				auto replacedFunType = replaceAllGen(nodeMan, funType, nodeMap, limiter);
				auto newFunType = builder.functionType(replacedFunType->getParameterTypes(), newReturnType, replacedFunType->getKind());

				// and our new parameters
				auto newParamList =
				    ::transform(funExp->getLambda()->getParameterList(), [&](const VariablePtr& t) { return replaceAllGen(nodeMan, t, nodeMap, limiter); });

				// finally, build the new lambda
				auto newLambda = funExp;
				if(newFunType != funType) {
					// function type changed due to instantiation, we need to build a new one
					newLambda = builder.lambdaExpr(newFunType, newParamList, newBody);
					if(funExp->isRecursive()) {
						assert_eq(funExp->getDefinition()->getDefinitions().size(), 1) << "Instantiation not yet implemented for mutually recursive functions";
						auto instantiatedOldRecVar = replaceAllGen(nodeMan, funExp->getVariable(), nodeMap, limiter);
						auto newRecVar = newLambda->getVariable();
						auto newerBody = replaceAllGen(nodeMan, newBody, {{instantiatedOldRecVar, newRecVar}});
						LOG(DEBUG) << "Recursive fun instantiation";
						LOG(DEBUG) << " -- instantiatedOldRecVar:" << *instantiatedOldRecVar;
						LOG(DEBUG) << " -- newRecVar:" << *newRecVar;
						LOG(DEBUG) << " -- newBody:\n" << dumpDetailColored(newBody);
						LOG(DEBUG) << " -- newerBody:\n" << dumpDetailColored(newerBody);
						auto oldBodyAddr = LambdaExprAddress(newLambda)->getBody();
						newLambda = replaceAddress(nodeMan, oldBodyAddr, newerBody).getRootNode().as<LambdaExprPtr>();
					}
				} else {
					// function type is unchanged, we just need to replace the body (prevent breaking rec lambdas)
					auto oldBodyAddr = LambdaExprAddress(funExp)->getBody();
					newLambda = replaceAddress(nodeMan, oldBodyAddr, newBody).getRootNode().as<LambdaExprPtr>();
				}

				// migrate migratable annotations before returning
				utils::migrateAnnotations(funExp, newLambda);
				return newLambda;
			}

		  public:
			TypeInstantiator(std::function<bool(const NodePtr& node)> skip = [](const NodePtr& node) { return false; }) : skip(skip) {}

			virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr, InstContext& context) override {
				auto prevNodeMap = context.first;
				auto& nodeMan = ptr->getNodeManager();
				IRBuilder builder(nodeMan);
				LOG(DEBUG) << "============ MAP: " << dumpOneLine(ptr) << " |==| " << *ptr << "\n";
				LOG(DEBUG) << "===> CONTEXT: " << context.second << "\n";
				// we are looking for CallExprs which have a concrete type params in the arguments,
				// but a variable type param in the lambda which is called
				if(ptr.isa<CallExprPtr>()) {
					LOG(DEBUG) << "============ CALL: " << *ptr << "\n";
					auto call = ptr.as<CallExprPtr>();
					auto fun = call->getFunctionExpr();
					auto args = call->getArguments();
					LOG(DEBUG) << " |-> isBuiltin: " << core::lang::isBuiltIn(fun) << "\n";

					auto newLambdaExp = fun;
					NodeMap fullNodeMap = prevNodeMap;

					// generate replacement node map
					core::types::SubstitutionOpt&& map = core::types::getTypeVariableInstantiation(nodeMan, call);
					NodeMap nodeMap;
					if(map) { nodeMap.insertAll(map->getMapping()); }

					// apply prev node map to RHS of new node map
					for(auto& p : nodeMap) {
						fullNodeMap[p.first] = apply(prevNodeMap, p.second);
					}

					if(fullNodeMap.empty()) {
						LOG(DEBUG) << "nodeMap EMPTY\n";
						InstContext newContext = {fullNodeMap, false};
						LOG(DEBUG) << "===> GENERATED CALL CONTEXT: " << skip(fun) << "\n";
						return ptr->substitute(nodeMan, *this, newContext);
					}
					LOG(DEBUG) << "====== prev nodemap: " << prevNodeMap << "\n";
					LOG(DEBUG) << "====== new  nodemap: " << nodeMap << "\n";
					LOG(DEBUG) << "====== full nodemap: " << fullNodeMap << "\n";

					// if it's a lambda, we need to substitute it
					if(fun.isa<LambdaExprPtr>() && !skip(fun)) {
						// possibly do early check here and skip deriving of var instantiation
						auto funExp = fun.as<LambdaExprPtr>();
						auto funType = funExp->getFunctionType();
						LOG(DEBUG) << "--> call gsl...\n";
						newLambdaExp = generateSubstituteLambda(nodeMan, funExp, fullNodeMap);
					}

					// handle higher order functions: perform replacements in arguments which are lambda expressions
					ExpressionList newArgs;
					std::transform(args.cbegin(), args.cend(), std::back_inserter(newArgs), [&](const ExpressionPtr& t) -> ExpressionPtr {
						LOG(DEBUG) << "\n\nARG: " << *t << "\n";
						if(t.isa<LambdaExprPtr>() && !skip(t)) {
							LOG(DEBUG) << "LambdaExprPtr\n";
							auto le = t.as<LambdaExprPtr>();
							return generateSubstituteLambda(nodeMan, le, fullNodeMap);
						}
						InstContext newContext = {fullNodeMap, context.second || skip(t)};
						LOG(DEBUG) << "--> call map...\n";
						return this->map(t, newContext);
					});

					// build a new return type if necessary
					auto newReturnType = call->getType();
					if(core::analysis::isGeneric(newReturnType)) {
						newReturnType = newLambdaExp->getType().as<FunctionTypePtr>()->getReturnType().as<TypePtr>();
						LOG(DEBUG) << "===> CHECK CALL CONTEXT: " << context.second << "\n";
						if(!context.second) { newReturnType = replaceAllGen(nodeMan, newReturnType, fullNodeMap); }
					}

					// finally, generate the typed call
					auto newCall = builder.callExpr(newReturnType, newLambdaExp, newArgs);
					utils::migrateAnnotations(call, newCall);
					LOG(DEBUG) << "============ NEWCALL: " << *newCall << "\n --> ret type: " << newReturnType << "\n";
					return newCall;
				}
				InstContext newContext = {prevNodeMap, context.second};
				return ptr->substitute(nodeMan, *this, newContext);
			}
		};
	}

	namespace detail {
		NodePtr instantiate(const NodePtr& root, std::function<bool(const core::NodePtr& node)> skip) {
			TypeInstantiator inst(skip);
			InstContext emtpy = {{}, false};
			return inst.map(root, emtpy);
		}
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
