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
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/types/type_variable_deduction.h"

namespace insieme {
namespace core {
namespace transform {


namespace {
	class TypeInstantiator : public SimpleNodeMapping {
	public:
		enum class InstantiationOption { TYPE_VARIABLES, INT_TYPE_PARAMS, BOTH };

	private:
		InstantiationOption opt;
		std::function<bool(const NodePtr& node)> skip;

		LambdaExprPtr generateSubstituteLambda(NodeManager& nodeMan, const LambdaExprPtr& funExp, const NodeMap& nodeMap) {
			// using the derived mapping, we generate a new lambda expression with instantiated type params
			// tricky: we apply the same mapping recursively on the *new* body
			auto builder = IRBuilder(nodeMan);
			auto funType = funExp->getFunctionType();
			auto newBody = core::transform::replaceAllGen(nodeMan, funExp->getBody(), nodeMap, true, skip)->substitute(nodeMan, *this);
			auto newFunType = core::transform::replaceAllGen(nodeMan, funType, nodeMap, true, skip);
			auto newParamList = ::transform(funExp->getLambda()->getParameterList(), [&](const VariablePtr& t) {
				return core::transform::replaceAllGen(nodeMan, t, nodeMap, true, skip);
			});
			auto newLambda = builder.lambdaExpr(newFunType, newParamList, newBody);
			utils::migrateAnnotations(funExp, newLambda);
			return newLambda;
		}

	public:
		TypeInstantiator(InstantiationOption opt, std::function<bool(const NodePtr& node)>
			skip = [](const NodePtr& node){ return false; } ) : opt(opt), skip(skip) { }

		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) override {
			auto& nodeMan = ptr->getNodeManager();
			auto builder = IRBuilder(nodeMan);
			// we are looking for CallExprs which have a concrete type params in the arguments,
			// but a variable type param in the lambda which is called
			if(ptr.isa<CallExprPtr>()) {
				auto call = ptr.as<CallExprPtr>();
				auto fun = call->getFunctionExpr();
				auto args = call->getArguments();

				// if we find a node that should not be touched we just return the unmodified node
				if(skip(fun)) return ptr;

				if(fun.isa<LambdaExprPtr>()) {
					// possibly do early check here and skip deriving of var instantiation
					auto funExp = fun.as<LambdaExprPtr>();
					auto funType = funExp->getFunctionType();

					core::types::SubstitutionOpt&& map = core::types::getTypeVariableInstantiation(nodeMan, call);
					NodeMap nodeMap;
					if(opt == InstantiationOption::TYPE_VARIABLES || opt == InstantiationOption::BOTH) nodeMap.insertAll(map->getMapping());
					if(opt == InstantiationOption::INT_TYPE_PARAMS || opt == InstantiationOption::BOTH) nodeMap.insertAll(map->getIntTypeParamMapping());
					if(nodeMap.empty()) return ptr->substitute(nodeMan, *this);

					auto newLambdaExp = generateSubstituteLambda(nodeMan, funExp, nodeMap);

					// handle higher order functions by performing replacements in arguments which are lambda expressions
					ExpressionList newArgs;
					std::transform(args.cbegin(), args.cend(), std::back_inserter(newArgs), [&](const ExpressionPtr& t) -> ExpressionPtr {
						if(t.isa<LambdaExprPtr>()) {
							auto le = t.as<LambdaExprPtr>();
							return generateSubstituteLambda(nodeMan, le, nodeMap);
						}
						return t;
					});

					auto newCall = builder.callExpr(newLambdaExp, newArgs);
					utils::migrateAnnotations(call, newCall);
					return newCall;
				}
			}
			return ptr->substitute(nodeMan, *this);
		}
	};
}

NodePtr instantiateIntTypeParams(const NodePtr& root, std::function<bool(const core::NodePtr& node)> skip) {
	TypeInstantiator inst(TypeInstantiator::InstantiationOption::INT_TYPE_PARAMS, skip);
	return inst.map(root);
}

NodePtr instantiateTypeVariables(const NodePtr& root, std::function<bool(const core::NodePtr& node)> skip) {
	TypeInstantiator inst(TypeInstantiator::InstantiationOption::TYPE_VARIABLES, skip);
	return inst.map(root);
}

NodePtr instantiateTypes(const NodePtr& root, std::function<bool(const core::NodePtr& node)> skip) {
	TypeInstantiator inst(TypeInstantiator::InstantiationOption::BOTH, skip);
	return inst.map(root);
}

} // end namespace transform
} // end namespace core
} // end namespace insieme
