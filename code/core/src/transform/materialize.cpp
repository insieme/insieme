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

#include "insieme/core/transform/materialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/core/lang/reference.h"

namespace insieme {
namespace core {
namespace transform {


	TypePtr materialize(const TypePtr& type) {
		// check for null pointer
		if(!type) return type;

		// check for unit
		if(type->getNodeManager().getLangBasic().isUnit(type)) return type;

		// do not materialize references
		if(lang::isCppReference(type)||lang::isCppRValueReference(type)) {
			return type;
		}

		// wrap type into plain ref
		IRBuilder builder(type->getNodeManager());
		return builder.refType(type);
	}

	DeclarationPtr materialize(const ExpressionPtr& value) {
		auto& mgr = value->getNodeManager();

		// we don't materialize if the expression is a constructor call or an init expression
		if(analysis::isConstructorCall(value) || value.isa<InitExprPtr>()) {
			return Declaration::get(mgr, value->getType(), value);
		}

		return Declaration::get(mgr, materialize(value->getType()), value);
	}

	TypePtr dematerialize(const TypePtr& type) {
		// check for null pointer
		if(!type) return type;

		// check for unit
		if(type->getNodeManager().getLangBasic().isUnit(type)) return type;

		// make sure we are dealing with a reference here
		if (!analysis::isRefType(type)) return type;

		// do not materialize references
		if(lang::isCppReference(type)||lang::isCppRValueReference(type)) {
			return type;
		}

		// strip surrounding ref
		return analysis::getReferencedType(type);
	}


	LambdaIngredients materialize(const LambdaIngredients& in) {

		LambdaIngredients out;

		// shortcut for empty parameter list
		if (in.params.empty()) {
			out.body = in.body;
			return out;
		}

		// materialize parameters and body
		NodeManager& mgr = in.body->getNodeManager();
		IRBuilder builder(mgr);

		// replace all variables in the body by their materialized version
		insieme::utils::map::PointerMap<VariablePtr,ExpressionPtr> substitute;
		for(const auto& cur : in.params) {

			// get materialized type
			auto paramType = materialize(cur->getType());

			// create the new parameters with an extra ref
			auto newParam = builder.variable(paramType, cur->getID());

			// migrate annotations
			utils::migrateAnnotations(cur, newParam);

			// record changes
			out.params.push_back(newParam);

			//only do an implicit deref of the parameter if it has actually been materialized
			if (paramType != cur->getType()) {
				substitute[cur] = builder.deref(newParam);
			}
		}
		out.body = transform::replaceVarsGen(mgr, in.body, substitute);

		// done
		return out;
	}

	ExpressionPtr castInitializationIfNotMaterializing(const TypePtr& initializedType, const ExpressionPtr& init) {
		const auto& initType = init->getType();
		// we need to add a cast if both types are reference, differ in their kind, but the initializedType is not the materialization of the type of init
		if(lang::isReference(initializedType) && lang::isReference(initType)
				&& lang::ReferenceType(initializedType).getKind() != lang::ReferenceType(initType).getKind()
				&& !analysis::isMaterializationOf(initializedType, initType)) {
			return lang::buildRefKindCast(init, lang::getReferenceKind(initializedType));
		}
		return init;
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
