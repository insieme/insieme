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

#include "insieme/frontend/utils/conversion_utils.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace frontend {
namespace utils {

	core::ExpressionPtr fixTempMemoryInInitExpression(const core::ExpressionPtr& variable, const core::ExpressionPtr& initExp) {
		auto& mgr = initExp->getNodeManager();
		auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
		// if the init expr is a constructor call
		if(core::analysis::isConstructorCall(initExp)) {
			core::CallExprAddress call(initExp.as<core::CallExprPtr>());
			assert_ge(call->getArguments().size(), 1) << "Ill-formed constructor call. Missing this argument";
			if(refExt.isCallOfRefTemp(call->getArgument(0))) {
				// we replace the first parameter (which has been created as ref_temp) by the variable to initialize
				return core::transform::replaceNode(
					       initExp->getNodeManager(), call->getArgument(0),
					       core::lang::buildRefCast(variable, call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->getParameterType(0)))
					.as<core::ExpressionPtr>();
			}
		}
		return initExp;
	}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme

