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

#include "insieme/analysis/features/effort_estimation.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/lang/lang.h"

namespace insieme {
namespace analysis {
namespace features {

	namespace {
		struct EffortAnnotation : public core::value_annotation::copy_on_migration {
			EffortEstimationType effort;
			EffortAnnotation(const EffortEstimationType& effort) : effort(effort) {}
			bool operator==(const EffortAnnotation& other) const {
				return effort == other.effort;
			}
		};

		EffortEstimationType EFFORT_SIMPLE_OP = 1;
		EffortEstimationType EFFORT_BRANCH = 2;
		EffortEstimationType EFFORT_LOOP = 5;
		EffortEstimationType EFFORT_LOOP_ITERATIONS = 100;
		EffortEstimationType EFFORT_LOOP_COMPARISON = 1;
		EffortEstimationType EFFORT_LOOP_STEP = 1;
		EffortEstimationType EFFORT_BUILTIN = 1;
		EffortEstimationType EFFORT_FUN_CALL = 5;

		EffortEstimationType estimateEffortInternal(const core::NodePtr& function) {

			// get cached value from annotation if present
			if(function.hasAttachedValue<EffortAnnotation>()) {
				return function.getAttachedValue<EffortAnnotation>().effort;
			}

			EffortEstimationType effort = 0;
			bool debug = false;
			core::visitDepthFirstPrunable(function, [&](const core::NodePtr& node) {
				switch(node->getNodeType()) {
				case core::NT_BreakStmt:
				case core::NT_ContinueStmt:
				case core::NT_ReturnStmt:
				case core::NT_DeclarationStmt:
					if(debug) std::cout << "Simple operation" << std::endl;
					effort += EFFORT_SIMPLE_OP;
					break;
				case core::NT_IfStmt:
				case core::NT_SwitchStmt:
					if(debug) std::cout << "Branch operation" << std::endl;
					effort += EFFORT_BRANCH;
					break;

				case core::NT_ForStmt: {
					if(debug) std::cout << "For loop" << std::endl;
					const auto& forStmt = node.as<core::ForStmtPtr>();
					unsigned long long loopIters = EFFORT_LOOP_ITERATIONS;
					try {
						auto formula = core::arithmetic::toFormula(forStmt->getEnd()) - core::arithmetic::toFormula(forStmt->getStart());
						auto stepFormula = core::arithmetic::toFormula(forStmt->getStep());
						if(formula.isInteger() && stepFormula.isInteger()) {
							loopIters = (unsigned long long) (formula.getIntegerValue() / stepFormula.getIntegerValue());
						}
					} catch (const core::arithmetic::NotAFormulaException& ex) {
						//ignore
					}
					effort += EFFORT_LOOP + loopIters * (estimateEffortInternal(forStmt->getBody()) + EFFORT_LOOP_STEP + EFFORT_LOOP_COMPARISON);
					return core::Prune;
				}

				case core::NT_WhileStmt:
					if(debug) std::cout << "While loop" << std::endl;
					effort += EFFORT_LOOP + EFFORT_LOOP_ITERATIONS * (estimateEffortInternal(node.as<core::WhileStmtPtr>()->getBody()) + EFFORT_LOOP_COMPARISON);
					return core::Prune;

				case core::NT_CallExpr: {
					const auto& call = node.as<core::CallExprPtr>();
					const auto& callee = call->getFunctionExpr();
					if(callee.isa<core::LambdaReferencePtr>()) {
						if(debug) std::cout << "Recursive function call" << std::endl;
						effort += EFFORT_FUN_CALL;

					} else if(core::lang::isBuiltIn(callee) || core::lang::isDerived(callee)) {
						if(debug) std::cout << "Function call to builtin/derived " << callee << std::endl;
						effort += EFFORT_BUILTIN;
						for(const auto& arg : call->getArgumentList()) {
							effort += estimateEffortInternal(arg);
						}
						return core::Prune;

					} else {
						if(debug) std::cout << "Function call" << std::endl;
						effort += EFFORT_FUN_CALL;
					}
					break;
				}

				default:
					; //no effort
				}
				return core::Descent;
			});

			// attach annotation with calculated value
			function.attachValue(EffortAnnotation(effort));

			return effort;
		}
	}

	EffortEstimationType estimateEffort(const core::NodePtr& node) {
		return estimateEffortInternal(node);
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme

