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

		class FreshVariableSubstitution : public SimpleNodeMapping {
			NodeManager& manager;

			// sets of used variables
			TypeSet& varSet;

			// a container for "remembering" replacements
			utils::map::PointerMap<TypeVariablePtr, TypeVariablePtr> varMap;

			// some utilities for generating variables
			unsigned varCounter;

		  public:
			FreshVariableSubstitution(NodeManager& manager, TypeSet& varSet) : SimpleNodeMapping(), manager(manager), varSet(varSet), varCounter(0){};

			virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
				// only handle type variables
				NodeType curType = ptr->getNodeType();
				if(curType != NT_TypeVariable) { return ptr->substitute(manager, *this); }


				// cast type variable
				TypeVariablePtr cur = static_pointer_cast<const TypeVariable>(ptr);

				// search for parameter ...
				auto pos = varMap.find(cur);
				if(pos != varMap.end()) {
					// found => return result
					return pos->second;
				}

				// create new variable substitution
				TypeVariablePtr res = getFreshVar();
				varMap.insert(std::make_pair(cur, res));
				varSet.insert(res);
				return res;
			}

		  private:
			TypeVariablePtr getFreshVar() {
				TypeVariablePtr res;
				do {
					res = TypeVariable::get(manager, "V" + toString(++varCounter));
				} while(varSet.find(res) != varSet.end());
				return res;
			}
		};


		template <typename T>
		Pointer<T> makeTypeVariablesUnique(const Pointer<T>& target, TypeSet& usedTypes) {
			NodeManager& manager = target->getNodeManager();
			FreshVariableSubstitution mapper(manager, usedTypes);
			return static_pointer_cast<T>(target->substitute(manager, mapper));
		}
	}

	TypePtr tryDeduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes) {
		NodeManager& manager = funType->getNodeManager();

		// try deducing variable instantiations the argument types
		auto varInstantiation = types::getTypeVariableInstantiation(manager, funType->getParameterTypes()->getTypes(), argumentTypes);

		// check whether derivation was successful
		if(!varInstantiation) {
			std::stringstream msg;
			msg << "Cannot match arguments (" << join(", ", argumentTypes, print<deref<TypePtr>>()) << ") \n"
				   "         to parameters ("
			    << join(", ", funType->getParameterTypes(), print<deref<TypePtr>>()) << ")";
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

		} catch(const ReturnTypeDeductionException& e) {
			// didn't work => print a warning
			LOG(DEBUG) << "Unable to deduce return type for call to function of type " << toString(*funType) << " using arguments "
			           << join(", ", argumentTypes, print<deref<TypePtr>>());

			LOG(DEBUG) << "Exception:\n" << e.what();
		}
		// return null ptr
		return unitOnFail ? funType->getNodeManager().getLangBasic().getUnit() : TypePtr();
	}


} // end namespace types
} // end namespace core
} // end namespace insieme
