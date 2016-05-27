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

				// instantiate type parameters
				auto substitution = types::getTypeVariableInstantiation(mgr, call);

				// skip invalid function calls
				if(!substitution) return ptr;

				// apply substitution on function
				auto newFun = (fun.isa<CallExprPtr>() || skip(fun)) ? fun : (*substitution).applyTo(fun);

				// TODO: apply substitution on arguments (for higher-order functions)
				auto newArgs = call->getArgumentList();

				// build replacement call
				NodePtr res = CallExpr::get(mgr, call->getType(), newFun, newArgs);

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
