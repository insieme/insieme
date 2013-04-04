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

#include "insieme/core/types/return_type_deduction.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/types/unification.h"
#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace types {


// -------------------------------------------------------------------------------------------------------------------------
//                                                    Return type deduction
// -------------------------------------------------------------------------------------------------------------------------


namespace {

	class FreshVariableSubstitution : public NodeMapping {

		NodeManager& manager;

		// sets of used variables
		TypeSet& varSet;
		IntTypeParamSet& paramSet;

		// a container for "remembering" replacements
		utils::map::PointerMap<TypeVariablePtr, TypeVariablePtr> varMap;
		utils::map::PointerMap<VariableIntTypeParamPtr, VariableIntTypeParamPtr> paramMap;

		// some utilities for generating variables
		unsigned varCounter;
		unsigned paramCounter;

	public:

		FreshVariableSubstitution(NodeManager& manager, TypeSet& varSet, IntTypeParamSet& paramSet)
			: NodeMapping(), manager(manager), varSet(varSet), paramSet(paramSet), varCounter(0), paramCounter(0) {};

		virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
			// only handle type variables
			NodeType curType = ptr->getNodeType();
			if (curType != NT_TypeVariable && curType != NT_VariableIntTypeParam) {
				return ptr->substitute(manager, *this);
			}


			switch(curType) {
				case NT_TypeVariable: {

					// cast type variable
					TypeVariablePtr cur = static_pointer_cast<const TypeVariable>(ptr);

					// search for parameter ...
					auto pos = varMap.find(cur);
					if (pos != varMap.end()) {
						// found => return result
						return pos->second;
					}

					// create new variable substitution
					TypeVariablePtr res = getFreshVar();
					varMap.insert(std::make_pair(cur, res));
					varSet.insert(res);
					return res;
				}

				case NT_VariableIntTypeParam: {

					// cast int type parameter
					VariableIntTypeParamPtr cur = static_pointer_cast<const VariableIntTypeParam>(ptr);

					// search for parameter ...
					auto pos = paramMap.find(cur);
					if (pos != paramMap.end()) {
						// found => return result
						return pos->second;
					}

					// create fresh parameter ...
					VariableIntTypeParamPtr res = getFreshParam();
					paramMap.insert(std::make_pair(cur, res));
					paramSet.insert(res);
					return res;
				}
				default:
					assert(false && "Should be impossible to reach!");
			}
			return ptr;
		}

	private:

		TypeVariablePtr getFreshVar() {
			TypeVariablePtr res;
			do {
				res = TypeVariable::get(manager, "V" + toString(++varCounter));
			} while(varSet.find(res) != varSet.end());
			return res;
		}

		VariableIntTypeParamPtr getFreshParam() {
			VariableIntTypeParamPtr res;
			do {
				res = VariableIntTypeParam::get(manager, 'a' + (paramCounter++));
			} while (paramSet.find(res) != paramSet.end());
			return res;
		}

	};


	void collectAllTypeVariables(const TypeList& types, TypeSet& varSet, IntTypeParamSet& paramSet) {

		// assemble type-variable collecting visitor
		auto visitor = makeLambdaVisitor([&](const NodePtr& cur) {
			// collect all type variables
			if (cur->getNodeType() == NT_TypeVariable) {
				varSet.insert(static_pointer_cast<const Type>(cur));
			}

			// collect variable int-type parameters
			if (cur->getNodeType() == NT_VariableIntTypeParam) {
				paramSet.insert(static_pointer_cast<const VariableIntTypeParam>(cur));
			}

		}, true);

		// collect type variables
		for_each(types, [&](const TypePtr& cur) {
			visitDepthFirstOnce(cur, visitor);
		});

	}

	template<typename T>
	Pointer<T> makeTypeVariablesUnique(const Pointer<T>& target, TypeSet& usedTypes, IntTypeParamSet& paramSet) {
		NodeManager& manager = target->getNodeManager();
		FreshVariableSubstitution mapper(manager, usedTypes, paramSet);
		return static_pointer_cast<T>(target->substitute(manager, mapper));
	}

}

TypePtr tryDeduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes) {

	NodeManager& manager = funType->getNodeManager();

	// try deducing variable instantiations the argument types
	auto varInstantiation = types::getTypeVariableInstantiation(manager, funType->getParameterTypes()->getTypes(), argumentTypes);

	// check whether derivation was successful
	if (!varInstantiation) {
		std::stringstream msg;
		msg << "Cannot match arguments ("
				<< join(", ", argumentTypes, print<deref<TypePtr>>())
				<< ") to parameters ("
				<< join(", ", funType->getParameterTypes(), print<deref<TypePtr>>())
				<< ")";
		throw ReturnTypeDeductionException(msg.str());
	}

	// extract return type
	const TypePtr& resType = funType->getReturnType();

	// compute and return the expected return type
	return varInstantiation->applyTo(manager, resType);
}

TypePtr deduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes, bool unitOnFail) {
	try {

		// try deducing the return type ...
		return tryDeduceReturnType(funType, argumentTypes);

	} catch (const ReturnTypeDeductionException&) {
		// didn't work => print a warning
		LOG(DEBUG) << "Unable to deduce return type for call to function of type "
				<< toString(*funType) << " using arguments " << join(", ", argumentTypes, print<deref<TypePtr>>());
	}
	// return null ptr
	return unitOnFail ? funType->getNodeManager().getLangBasic().getUnit() : TypePtr();
}


} // end namespace types
} // end namespace core
} // end namespace insieme
