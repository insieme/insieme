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
 *
 */
#include "insieme/transform/functions/transformations.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/simplify.h"

namespace insieme {
namespace transform {
namespace functions {

	RecursiveFunctionUnrolling::RecursiveFunctionUnrolling(const parameter::Value& value)
	    : Transformation(RecursiveFunctionUnrollingType::getInstance(), value), unrolling(parameter::getValue<unsigned>(value)) {
		if(unrolling < 2) { throw InvalidParametersException("Unrolling factor must be at least 2!"); }
	}

	core::NodeAddress RecursiveFunctionUnrolling::apply(const core::NodeAddress& targetAddress) const {
		auto target = targetAddress.as<core::NodePtr>();

		if(target->getNodeType() != core::NT_LambdaExpr) { throw InvalidTargetException("Can only be applied to lambdas!"); }

		// start by obtaining the lambda
		core::LambdaExprPtr lambda = target.as<core::LambdaExprPtr>();

		// can only be applied to recursive functions
		if(!lambda->isRecursive()) { throw InvalidTargetException("Can only be applied on recursive functions!"); }

		// fix recursive variable usage
		lambda = core::transform::correctRecursiveLambdaVariableUsage(lambda->getNodeManager(), lambda);

		// unroll lambda
		lambda = lambda->unroll(unrolling);

		// simplify resulting expression
		auto res = core::transform::simplify(lambda->getNodeManager(), lambda);
		return core::transform::replaceAddress(target->getNodeManager(), targetAddress, res);
	}

} // end namespace functions
} // end namespace transform
} // end namespace insieme
