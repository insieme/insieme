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
#include "insieme/core/transform/address_mapper.h"
#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace core {
namespace transform {


namespace {
	template<class VarTypePtr, class ConcreteTypePtr, class VarTypeAddress> 
	class TypeInstantiator : public SimpleNodeMapping {
	private:
		template<class T>
		static bool containsType(const TypePtr& t) {
			return visitDepthFirstOnceInterruptible(t, [](const T& i) {
				return true;
			}, true, true);
		}

		template<class T>
		static vector<vector<VarTypeAddress>> collectTypeParamList(const T& parameterTypes) {
			vector<vector<VarTypeAddress>> ret;
			for(const auto& param : parameterTypes) {
				auto cur = vector<VarTypeAddress>();
				visitDepthFirst(NodeAddress(param), [&](const VarTypeAddress& i) {
					cur.push_back(i);
				}, true, true);
				ret.push_back(cur);
			}
			return ret;
		}

	public:
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) override {
			auto& nodeMan = ptr->getNodeManager();
			auto builder = IRBuilder(nodeMan);
			// we are looking for CallExprs which have a concrete type params in the arguments,
			// but a variable type param in the lambda which is called
			if(ptr.isa<CallExprPtr>()) {
				auto call = ptr.as<CallExprPtr>();
				auto args = call->getArguments();
				if(std::any_of(args.cbegin(), args.cend(), [&](const ExpressionPtr& expr) {
					return containsType<ConcreteTypePtr>(expr->getType()); 
				})) {
					auto fun = call->getFunctionExpr();
					if(fun.isa<LambdaExprPtr>()) {
						auto funExp = fun.as<LambdaExprPtr>();
						auto funType = funExp->getFunctionType();
						if(containsType<VarTypePtr>(funType)) {
							auto variableTypeAddrList = collectTypeParamList(funType->getParameterTypeList());

							// we have gathered the addresses of variable type pointers within each parameter
							// now we try to derive a mapping for each of them to a concrete instantiation from the call site
							NodeMap nodeMap;
							for(unsigned paramIndex = 0; paramIndex < variableTypeAddrList.size(); ++paramIndex) {
								auto paramAddr = variableTypeAddrList[paramIndex];
								auto argTypePtr = call->getArgument(paramIndex)->getType();
								for(auto&& addr : paramAddr) {
									auto mappedAddr = addr.switchRootAndType(argTypePtr);
									nodeMap.insert(std::make_pair(addr.getAddressedNode(), mappedAddr.getAddressedNode()));
								}
							}
							//std::cout << "THE MAP: " << nodeMap << std::endl;

							// using the derived mapping, we generate a new lambda expression with instantiated type params
							// tricky: we apply the same mapping recursively on the *new* body
							auto newBody = core::transform::replaceAllGen(nodeMan, funExp->getBody(), nodeMap, true)->substitute(nodeMan, *this);
							auto newFunType = core::transform::replaceAllGen(nodeMan, funType, nodeMap, true);
							auto newParamList = ::transform(funExp->getLambda()->getParameterList(), [&](const VariablePtr& t) { 
								return core::transform::replaceAllGen(nodeMan, t, nodeMap); 
							});
							auto newLambdaExp = builder.lambdaExpr(newFunType, newParamList, newBody);

							return builder.callExpr(newLambdaExp, call->getArguments());
						}
					}
				}
			}
			return ptr->substitute(nodeMan, *this);
		}
	};
}

NodePtr instantiateIntTypeParams(const NodePtr& root) {
	TypeInstantiator<VariableIntTypeParamPtr, ConcreteIntTypeParamPtr, VariableIntTypeParamAddress> inst; 
	return inst.map(root);
}

NodePtr instantiateTypeVariables(const NodePtr& root) {
	TypeInstantiator<TypeVariablePtr, TypePtr, TypeVariableAddress> inst; 
	return inst.map(root);
}

NodePtr instantiateTypes(const NodePtr& root) {
	return instantiateTypeVariables(instantiateIntTypeParams(root));
}

} // end namespace transform
} // end namespace core
} // end namespace insieme
