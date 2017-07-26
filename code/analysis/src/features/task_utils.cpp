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

#include "insieme/analysis/features/task_utils.h"

#include <algorithm>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/analysis/ir_utils.h"


namespace insieme {
namespace analysis {
namespace features {

	std::vector<core::NodeAddress> getParallelTasks(const core::NodeAddress& root) {
		core::IRBuilder builder(root->getNodeManager());
		auto desiredThreadNumRange = builder.getThreadNumRange(1, 1);

		std::vector<core::NodeAddress> parallelCalls;

		//find all tasks and return their parallel call node
		core::visitDepthFirstOnce(root, [&](const core::JobExprAddress& job) {
			if(job->getThreadNumRange().getAddressedNode() == desiredThreadNumRange) {
				parallelCalls.push_back(job.getParentAddress(2)); //parallel call is two levels up
			}
		});

		return parallelCalls;
	}

	void getRecursiveTasksInternal(const core::NodeAddress& root,
																 std::unordered_set<core::LambdaReferenceAddress> visitedReferences,
																 const std::vector<core::NodeAddress>& parallelCalls,
																 std::vector<std::pair<core::NodeAddress, core::NodeAddress>>& results) {
		core::visitDepthFirstPrunable(root, [&](const core::CallExprAddress& call) {
			//here we found a call to a LambdaExpression
			if(const auto& lambdaExpr = call->getFunctionExpr().isa<core::LambdaExprAddress>()) {
				if(!core::lang::isBuiltIn(lambdaExpr)) {
					visitedReferences.insert(lambdaExpr->getReference());
				}
			}

			//calls to LambdaReferences
			if(const auto& callee = call->getFunctionExpr().isa<core::LambdaReferenceAddress>()) {
				//find if we already have visited the definition of this lambda once
				auto visited = std::find_if(visitedReferences.cbegin(), visitedReferences.cend(),
																		[&callee](const auto& currentReference) { return *currentReference == *callee; });
				if(visited != visitedReferences.cend()) {
					const auto& visitedAddress = (*visited).getParentAddress();
					//check whether there is a parallel call in between
					const auto& above = visitedAddress.getDepth() < call.getDepth() ? visitedAddress : call;
					const auto& below = visitedAddress.getDepth() > call.getDepth() ? visitedAddress : call;
					if(any(parallelCalls, [&](const auto& parallelCall) { return core::isChildOf(above, parallelCall) && core::isChildOf(parallelCall, below); })) {
						//this is a task recursive call
						results.push_back({visitedAddress, callee});
					}
					return true;
				}
				getRecursiveTasksInternal(core::analysis::getLambdaFromReference(callee)->getBody(), visitedReferences, parallelCalls, results);
				return true;
			}
			return false;
		});
	}

	std::vector<std::pair<core::NodeAddress, core::NodeAddress>> getRecursiveTasks(const core::LambdaExprAddress& root,
	                                                                               const std::vector<core::NodeAddress>& parallelCalls) {
		std::unordered_set<core::LambdaReferenceAddress> visitedReferences;
		visitedReferences.insert(root->getReference());

		std::vector<std::pair<core::NodeAddress, core::NodeAddress>> results;

		getRecursiveTasksInternal(root, visitedReferences, parallelCalls, results);

		return results;
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme
