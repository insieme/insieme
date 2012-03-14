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

#include "insieme/transform/rulebased/transformations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

namespace insieme {
namespace transform {
namespace rulebased {

	using namespace pattern;
	using namespace pattern::generator;
	using namespace pattern::generator::irg;

	TransformationPtr makeLoopUnrolling(size_t factor) {
		return std::make_shared<LoopUnrolling>(parameter::makeValue<unsigned>(factor));
	}


	MatchExpressionPtr deltaRange(const string& var_start, const string& var_end, const string& var_step) {
		return std::make_shared<expression::Constructor<ptr_target>>([=](const Match<ptr_target>& match)->MatchValue<ptr_target> {
			core::NodeManager& manager = match.getRoot()->getNodeManager();
			core::IRBuilder builder(manager);

			// resolve start/end variables
			assert(match.isVarBound(var_start) && "Start variable not bound to value!");
			assert(match.isVarBound(var_end)   && "End variable not bound to value!");
			assert(match.isVarBound(var_step)  && "Step variable not bound to value!");

			const auto& start_value = match.getVarBinding(var_start).getValue();
			const auto& end_value   = match.getVarBinding(var_end).getValue();
			const auto& step_value  = match.getVarBinding(var_step).getValue();

			assert(start_value->getNodeCategory() == core::NC_Expression && "Start variable must be bound to an expression!");
			assert(end_value->getNodeCategory()   == core::NC_Expression && "End variable must be bound to an expression");
			assert(step_value->getNodeCategory()  == core::NC_Expression && "Step variable must be bound to an expression");


			try {

				auto start_formula = core::arithmetic::toPiecewise(start_value.as<core::ExpressionPtr>());
				auto end_formula   = core::arithmetic::toPiecewise(end_value.as<core::ExpressionPtr>());
				auto step_formula  = core::arithmetic::toPiecewise(step_value.as<core::ExpressionPtr>());

				auto diff_formula = end_formula - start_formula;

				// check whether values are constants
				if (!(diff_formula.isFormula() && diff_formula.toFormula().isInteger())) {
					throw InvalidTargetException("Number of iterations is not constant!");
				}
				if (!(step_formula.isFormula() && step_formula.toFormula().isInteger())) {
					throw InvalidTargetException("Step size is not constant!");
				}

				int diff = diff_formula.toFormula().getConstantValue();
				int step = step_formula.toFormula().getConstantValue();

				vector<MatchValue<ptr_target>> res;
				for(int i=0; i<diff; i+= step) {
					core::NodePtr expr = builder.stringValue(toString(i));
					res.push_back(MatchValue<ptr_target>(expr));
				}

				return res;

			} catch (const core::arithmetic::NotAFormulaException& nfe) {
				// fail transformation
				throw InvalidTargetException("Loop boundaries or step size is not a formula!");
			}
		}, format("[0,..,%s-%s : %s)", var_end.c_str(), var_start.c_str(), var_step.c_str()));
	}

	TransformationPtr makeTotalLoopUnrolling() {
		return std::make_shared<TotalLoopUnrolling>();
	}

} // end namespace rulebased
} // end namespace transform
} // end namespace insieme
