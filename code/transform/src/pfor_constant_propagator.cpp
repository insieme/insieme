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

#include "insieme/transform/pfor_constant_propagator.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace transform {

using namespace core;

/**
 * Visitor that checks and performs constant propagation in bodies of for loops that are called in pfor calls with fixed parameters (start, end, step)
 */
const NodePtr PForConstantPropagator::resolveElement(const NodePtr& ptr) {

	// if we reach a type stop recursion
	if (ptr->getNodeCategory() == NC_Type || ptr->getNodeCategory() == NC_IntTypeParam) {
		return ptr;
	}

	// recursive replacement has to be continued
	NodePtr res = ptr->substitute(ptr->getNodeManager(), *this);

	NodeAddress addr(res);
	auto& basic = ptr->getNodeManager().getLangBasic();

	if(addr.isa<CallExprAddress>()) {

		const CallExprAddress& call = addr.as<CallExprAddress>();

		if(basic.isPFor(call->getFunctionExpr().getAddressedNode())) {

			const ExpressionAddress start = call->getArgument(1);
			const ExpressionAddress end = call->getArgument(2);
			const ExpressionAddress step = call->getArgument(3);
			NodeAddress body = call->getArgument(4);

			if (body->getNodeType() == NT_BindExpr) {
				body = body.as<BindExprAddress>()->getCall()->getFunctionExpr();
			}

			if (body->getNodeType() == NT_LambdaExpr) {
				ParametersAddress params = body.as<LambdaExprAddress>()->getParameterList();
				assert(params.size() > 2 && "Expecting at least 3 parameters (start, end, stride)!");
//				dumpColor(params);
				const NodeAddress startVar = params[params.size()-3];
				const NodeAddress endVar = params[params.size()-2];
				const NodeAddress stepVar = params[params.size()-1];
				body = body.as<LambdaExprAddress>()->getBody();

				NodeMap replacements;

				// TODO: fix apparent bug causing semantic errors for bt when replacing starting value (maybe it is wrongly working on pointers instead of addresses?)
				if(arithmetic::toFormula(start).isConstant())
					replacements[startVar] = start;
				if(arithmetic::toFormula(end).isConstant())
					replacements[endVar] = end;
				if(arithmetic::toFormula(step).isConstant())
					replacements[stepVar] = step;

				if(!replacements.empty()) {
					const NodePtr newBody = core::transform::replaceAll(ptr->getNodeManager(), body, replacements);
					return core::transform::replaceAddress(ptr->getNodeManager(), body, newBody).getRootNode();
				}

				return res;
			}
		}
	}

	// check whether something has changed ...
	if (res == ptr) {
		// => nothing changed
		return ptr;
	}

	// preserve annotations
	core::transform::utils::migrateAnnotations(ptr, res);

	// done
	return res;
}

} // end namespace transform
} // end namespace insieme
