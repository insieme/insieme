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

#include "insieme/core/transform/instantiate.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/address_mapper.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace transform {

	namespace {

		class Instantiator : public CachedNodeMapping {

			const NodeFilter& skip;

		public:

			Instantiator(const NodeFilter& skip) : skip(skip) {}

			virtual const NodePtr resolveElement(const NodePtr& ptr) override {
				NodeManager& mgr = ptr->getNodeManager();
				IRBuilder builder(mgr);

				// check skip-filter
				if(skip(ptr)) return ptr;

				// for non-calls ...
				auto call = ptr.isa<CallExprPtr>();
				if(!call) {
					// ... just descend recursively
					return ptr->substitute(mgr, *this);
				}

				// for calls: check type variable instantiations
				auto fun = call->getFunctionExpr();

				// we don't instantiate calls with Literals as their callee
				if(fun->getNodeType() == NT_Literal) {
					// ... just descend recursively
					return ptr->substitute(mgr, *this);
				}

				// instantiate type parameters
				auto substitution = types::getTypeVariableInstantiation(mgr, call);

				// skip invalid function calls
				if(!substitution) return ptr;

				// apply substitution on function
				auto newFun = (fun.isa<CallExprPtr>() || skip(fun)) ? fun : (*substitution).applyTo(fun);

				// TODO: apply substitution on arguments (for higher-order functions)
				auto newArgs = call->getArgumentList();

				// build replacement call
				NodePtr res = builder.callExpr(call->getType(), newFun, newArgs);

				// move annotations
				transform::utils::migrateAnnotations(call, res);

				// continue instantiating nested structures
				return res->substitute(mgr, *this);
			}

		};

	}

	namespace detail {
		NodePtr instantiate(const NodePtr& root, const NodeFilter& skip) {
			return Instantiator(skip).map(root);
		}
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
