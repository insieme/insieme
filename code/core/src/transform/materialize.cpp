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

#include "insieme/core/transform/materialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"

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

		// wrap type into parameter
		IRBuilder builder(type->getNodeManager());
		return builder.refType(type);
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

} // end namespace transform
} // end namespace core
} // end namespace insieme
