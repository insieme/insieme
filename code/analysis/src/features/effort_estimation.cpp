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

#include "insieme/analysis/features/effort_estimation.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/lang/lang.h"

namespace insieme {
namespace analysis {
namespace features {

	namespace {
		unsigned long long EFFORT_SIMPLE_OP = 1;
		unsigned long long EFFORT_BRANCH = 2;
		unsigned long long EFFORT_LOOP = 5;
		unsigned long long EFFORT_LOOP_ITERATIONS = 100;
		unsigned long long EFFORT_LOOP_COMPARISON = 1;
		unsigned long long EFFORT_LOOP_STEP = 1;
		unsigned long long EFFORT_BUILTIN = 1;
		unsigned long long EFFORT_FUN_CALL = 5;

		unsigned long long estimateEffortInternal(const core::NodePtr& function) {
			unsigned long long effort = 0;
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
					} catch (core::arithmetic::NotAFormulaException ex) {
						//ignore
					}
					effort += EFFORT_LOOP + loopIters * (estimateEffortInternal(forStmt->getBody()) + EFFORT_LOOP_STEP + EFFORT_LOOP_COMPARISON);
					return true;
				}

				case core::NT_WhileStmt:
					if(debug) std::cout << "While loop" << std::endl;
					effort += EFFORT_LOOP + EFFORT_LOOP_ITERATIONS * (estimateEffortInternal(node.as<core::WhileStmtPtr>()->getBody()) + EFFORT_LOOP_COMPARISON);
					return true;

				case core::NT_CallExpr: {
					const auto& callee = node.as<core::CallExprPtr>()->getFunctionExpr();
					if(callee.isa<core::LambdaReferencePtr>()) {
						if(debug) std::cout << "Recursive function call" << std::endl;
						return true;

					} else if(core::lang::isBuiltIn(callee)) {
						if(debug) std::cout << "Function call to builtin" << std::endl;
						effort += EFFORT_BUILTIN;

					} else {
						if(debug) std::cout << "Function call" << std::endl;
						effort += EFFORT_FUN_CALL + estimateEffortInternal(callee);
					}
					break;
				}

				default:
					; //no effort
				}
				return false;
			});
			return effort;
		}
	}

	unsigned long long estimateEffort(const core::LambdaExprPtr& function) {
		return estimateEffortInternal(function->getBody());
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme

