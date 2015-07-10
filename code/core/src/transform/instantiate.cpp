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

namespace insieme {
namespace core {
namespace transform {

namespace {
	typedef std::pair<NodeMap,bool> InstContext;

	class TypeInstantiator : public NodeMapping<InstContext> {
	public:
		enum class InstantiationOption { TYPE_VARIABLES, INT_TYPE_PARAMS, BOTH };

	private:
		InstantiationOption opt;
		std::function<bool(const NodePtr& node)> skip;

		template<template <typename> class Ptr, class T, typename Result = Ptr<T>>
			Result apply(const NodeMap& map, const Ptr<T>& ptr) {
				if(map.find(ptr) != map.end()) {
					return (map.find(ptr)->second).template as<Result>();
				} 
				return ptr;
		}

		LambdaExprPtr generateSubstituteLambda(
				NodeManager& nodeMan, const LambdaExprPtr& funExp, NodeMap& nodeMap) {
			
			// create limiter based on skip
			ReplaceLimiter limiter = [&](const NodePtr& ptr) { 
				return (ptr.isa<core::LambdaExprPtr>() || skip(ptr)) ? ReplaceAction::Prune : ReplaceAction::Process;
			};

			// using the derived mapping, we generate a new lambda expression with instantiated type params
			auto builder = IRBuilder(nodeMan);
			auto funType = funExp->getFunctionType();
			auto replacedBody = funExp->getBody();
			// only apply replacement to body of lambdas which we are not supposed to skip
			if(!skip(funExp)) {
				replacedBody = core::transform::replaceAllGen(nodeMan, funExp->getBody(), nodeMap, limiter);
			}
			// tricky: we apply the same mapping recursively on the *new* body
			InstContext newContext = {nodeMap, skip(funExp)};
			auto newBody = replacedBody->substitute(nodeMan, *this, newContext);
			
			// we now need to determine the new return type to use
			TypePtr newReturnType = funType->getReturnType();
			// constructors can not be automatically typed!
			if(!skip(funExp) && funType->getKind() != FK_CONSTRUCTOR && funType->getKind() != FK_DESTRUCTOR) {
				newReturnType = analysis::autoReturnType(nodeMan, newBody);
				newReturnType = core::transform::replaceAllGen(nodeMan, newReturnType, nodeMap, limiter);
			}
			std::cout << "body: " << *funExp->getBody() << "\n";
			std::cout << "newBody: " << *newBody << "\n";
			std::cout << "newReturnType nodemap: " << nodeMap << "\n";
			std::cout << "NewReturnType: " << *newReturnType << "\n";

			// now we can generate the substitution
			auto replacedFunType = core::transform::replaceAllGen(nodeMan, funType, nodeMap, limiter);
			auto newFunType = 
				builder.functionType(replacedFunType->getParameterTypes(), newReturnType, replacedFunType->getKind());

			// and our new parameters
			auto newParamList = ::transform(funExp->getLambda()->getParameterList(), [&](const VariablePtr& t) {
				return core::transform::replaceAllGen(nodeMan, t, nodeMap, limiter);
			});

			// finally, build the new lambda 
			auto newLambda = funExp;
			if(newFunType != funType) {
				// function type changed due to instantiation, we need to build a new one
				newLambda = builder.lambdaExpr(newFunType, newParamList, newBody);
				assert_false(funExp->isRecursive()) << "Instantiation of recursive functions not supported yet.";
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
		TypeInstantiator(InstantiationOption opt, std::function<bool(const NodePtr& node)>
			skip = [](const NodePtr& node){ return false; } ) : opt(opt), skip(skip) { }

		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr, InstContext& context) override {
			auto prevNodeMap = context.first;
			auto& nodeMan = ptr->getNodeManager();
			auto builder = IRBuilder(nodeMan);
			std::cout << "============ MAP: " << dumpOneLine(ptr) << " |==| " << *ptr << "\n";
			std::cout << "===> CONTEXT: " << context.second << "\n";
			// we are looking for CallExprs which have a concrete type params in the arguments,
			// but a variable type param in the lambda which is called
			if(ptr.isa<CallExprPtr>()) {
				std::cout << "============ CALL: " << *ptr << "\n";
				auto call = ptr.as<CallExprPtr>();
				auto fun = call->getFunctionExpr();
				auto args = call->getArguments();
				std::cout << " |-> isBuiltin: " << core::lang::isBuiltIn(fun) << "\n";
				
				auto newLambdaExp = fun;
				NodeMap fullNodeMap = prevNodeMap;

				// generate replacement node map
				core::types::SubstitutionOpt&& map = core::types::getTypeVariableInstantiation(nodeMan, call);
				NodeMap nodeMap;
				if(map) {
					if(opt == InstantiationOption::TYPE_VARIABLES || opt == InstantiationOption::BOTH) {
						nodeMap.insertAll(map->getMapping());
					}
					if(opt == InstantiationOption::INT_TYPE_PARAMS || opt == InstantiationOption::BOTH) {
						nodeMap.insertAll(map->getIntTypeParamMapping());
					}
				}

				// apply prev node map to RHS of new node map
				for(auto& p : nodeMap) {
					fullNodeMap[p.first] = apply(prevNodeMap, p.second);
				}

				if(fullNodeMap.empty()) { 
					std::cout << "nodeMap EMPTY\n";
					InstContext newContext = { fullNodeMap, false };
					std::cout << "===> GENERATED CALL CONTEXT: " << skip(fun) << "\n";
					return ptr->substitute(nodeMan, *this, newContext);
				}
				std::cout << "====== prev nodemap: " << prevNodeMap << "\n";
				std::cout << "====== new  nodemap: " << nodeMap << "\n";
				std::cout << "====== full nodemap: " << fullNodeMap << "\n";

				// if it's a lambda, we need to substitute it
				if(fun.isa<LambdaExprPtr>() && !skip(fun)) {
					// possibly do early check here and skip deriving of var instantiation
					auto funExp = fun.as<LambdaExprPtr>();
					auto funType = funExp->getFunctionType();
					std::cout << "--> call gsl...\n";
					newLambdaExp = generateSubstituteLambda(nodeMan, funExp, fullNodeMap);
				}

				// handle higher order functions: perform replacements in arguments which are lambda expressions
				ExpressionList newArgs;
				std::transform(args.cbegin(), args.cend(), std::back_inserter(newArgs), 
					[&](const ExpressionPtr& t) -> ExpressionPtr {
						std::cout << "\n\nARG: " << *t << "\n";
						if(t.isa<LambdaExprPtr>() && !skip(t)) {
							std::cout << "LambdaExprPtr\n";
							auto le = t.as<LambdaExprPtr>();
							return generateSubstituteLambda(nodeMan, le, fullNodeMap);
						}
						InstContext newContext = {fullNodeMap, context.second || skip(t)};
						std::cout << "--> call map...\n";
						return this->map(t, newContext);
					});

				// build a new return type if necessary
				auto newReturnType = call->getType();
				if(core::analysis::isGeneric(newReturnType)) {
					newReturnType = newLambdaExp->getType().as<FunctionTypePtr>()->getReturnType().as<TypePtr>();
					std::cout << "===> CHECK CALL CONTEXT: " << context.second << "\n";
					if(!context.second) {
						newReturnType = replaceAllGen(nodeMan, newReturnType, fullNodeMap);
					}
				}

				// finally, generate the typed call
				auto newCall = builder.callExpr(newReturnType, newLambdaExp, newArgs);
				utils::migrateAnnotations(call, newCall);
				std::cout << "============ NEWCALL: " << *newCall << "\n --> ret type: " << newReturnType << "\n";
				return newCall;
			}
			InstContext newContext = {prevNodeMap, context.second};
			return ptr->substitute(nodeMan, *this, newContext);
		}
	};
}

NodePtr instantiateIntTypeParams(const NodePtr& root, std::function<bool(const core::NodePtr& node)> skip) {
	TypeInstantiator inst(TypeInstantiator::InstantiationOption::INT_TYPE_PARAMS, skip);
	InstContext emtpy = { {}, false };
	return inst.map(root, emtpy);
}

NodePtr instantiateTypeVariables(const NodePtr& root, std::function<bool(const core::NodePtr& node)> skip) {
	TypeInstantiator inst(TypeInstantiator::InstantiationOption::TYPE_VARIABLES, skip);
	InstContext emtpy = { {}, false };
	return inst.map(root, emtpy);
}

NodePtr instantiateTypes(const NodePtr& root, std::function<bool(const core::NodePtr& node)> skip) {
	TypeInstantiator inst(TypeInstantiator::InstantiationOption::BOTH, skip);
	InstContext emtpy = { {}, false };
	return inst.map(root, emtpy);
}

} // end namespace transform
} // end namespace core
} // end namespace insieme
