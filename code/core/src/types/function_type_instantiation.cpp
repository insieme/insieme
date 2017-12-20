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

#include "insieme/core/types/function_type_instantiation.h"

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

	FunctionTypePtr tryInstantiateFunctionType(const FunctionTypePtr& funType, const TypeList& argumentTypes) {
		NodeManager& manager = funType->getNodeManager();

		// try deducing variable instantiations the argument types
		auto varInstantiation = types::getTypeVariableInstantiation(manager, funType->getParameterTypes()->getTypes(), argumentTypes);

		// check whether derivation was successful
		if(!varInstantiation) return nullptr;

		// instantiate function
		return varInstantiation->applyTo(manager, funType);
	}

} // end namespace types
} // end namespace core
} // end namespace insieme
