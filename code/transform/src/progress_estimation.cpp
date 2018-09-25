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

#include "insieme/transform/progress_estimation.h"

#include <string>

#include "insieme/analysis/features/effort_estimation.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/lang/lang.h"
#include "insieme/core/transform/node_mapper_utils.h"


namespace insieme {
namespace transform {

	using EffortType = analysis::features::EffortEstimationType;

	namespace {
		core::CallExprPtr buildProgressReportingCallInternal(const core::IRBuilder& builder,
		                                                     const core::LiteralPtr& reportingLiteral,
		                                                     const EffortType progress) {
			return builder.callExpr(reportingLiteral, builder.literal(builder.getLangBasic().getUInt16(), toString(progress)));
		}

		class ProgressMapper : public core::transform::CachedNodeMapping {

			core::NodeManager& mgr;
			core::IRBuilder builder;
			const ProgressEstomationExtension& progressExtension;

			const EffortType progressReportingLimit;

			const EffortType EFFORT_WHILE_LOOP_BRANCHING = 2;
			const EffortType EFFORT_FOR_LOOP_INITILIZATION = 1;
			const EffortType EFFORT_FOR_LOOP_BRANCHING = 2;
			const EffortType EFFORT_FOR_LOOP_CONDITION = 1;
			const EffortType EFFORT_FOR_LOOP_INCREMENT = 1;
			const EffortType EFFORT_IF_BRANCHING = 1;

		  public:
			ProgressMapper(core::NodeManager& manager, const EffortType progressReportingLimit) : mgr(manager), builder(manager),
					progressExtension(manager.getLangExtension<ProgressEstomationExtension>()),
					progressReportingLimit(progressReportingLimit) { }

		  private:
			core::CallExprPtr buildProgressReportingCall(const EffortType progress) const {
				return buildProgressReportingCallInternal(builder, progressExtension.getProgressReportingLiteral(), progress);
			}

			const core::NodePtr resolveElementInternal(const core::NodePtr& node, const EffortType startProgressOffset = 0) {
				const auto& nodeType = node.getNodeType();
				// prune calls to builtins
				if (nodeType == core::NT_CallExpr) {
					const auto& callee = node.as<core::CallExprPtr>()->getFunctionExpr();
					if (core::lang::isBuiltIn(callee) || core::lang::isDerived(callee)) {
						return node;
					}
				}

				// only process CompoundStmts here
				if(nodeType != core::NT_CompoundStmt) {
					return node->substitute(mgr, *this);
				}

				const core::CompoundStmtPtr& compound = node.as<core::CompoundStmtPtr>();
				core::StatementList stmts(compound.getStatements());

				EffortType progress = startProgressOffset;

				auto insertProgressReportingCall = [&](core::StatementList::iterator& it) {
					if(progress != 0) {
						it = stmts.insert(it, buildProgressReportingCall(progress));
						++it;
						progress = 0;
					}
				};

				const auto exitPoints = core::analysis::getExitPoints(compound);
				auto isExitPoint = [&](const core::StatementPtr& stmt) {
					return ::any(exitPoints, [&](const auto& exitPoint) { return exitPoint.getDepth() == 2 && exitPoint.getAddressedNode() == stmt; });
				};

				for(auto it = stmts.begin(); it < stmts.end(); ++it) {
					auto stmt = *it;

					// certain nodes get a special handling
					if(const auto& whileStmt = stmt.isa<core::WhileStmtPtr>()) {
						// report the progress until here before the loop starts
						insertProgressReportingCall(it);
						// get the loop overhead
						const auto loopOverhead = analysis::features::estimateEffort(whileStmt->getCondition()) + EFFORT_WHILE_LOOP_BRANCHING;
						// now build a new loop and replace the old one
						const auto newCondition = whileStmt->getCondition().substitute(mgr, *this);
						const auto newBody = resolveElementInternal(whileStmt->getBody(), loopOverhead).as<core::StatementPtr>();
						*it = builder.whileStmt(newCondition, newBody);
						continue;

					} else if(const auto& forStmt = stmt.isa<core::ForStmtPtr>()) {
						// report the progress until here before the loop starts + the initialization
						progress += EFFORT_FOR_LOOP_INITILIZATION;
						insertProgressReportingCall(it);
						// get the loop overhead
						const auto loopOverhead = EFFORT_FOR_LOOP_BRANCHING + EFFORT_FOR_LOOP_CONDITION + EFFORT_FOR_LOOP_INCREMENT;
						// now build a new loop and replace the old one
						const auto newBody = resolveElementInternal(forStmt->getBody(), loopOverhead).as<core::StatementPtr>();
						*it = builder.forStmt(forStmt->getDeclaration(), forStmt->getEnd(), forStmt->getStep(), newBody);
						continue;

					} else if(const auto& ifStmt = stmt.isa<core::IfStmtPtr>()) {
						// add the condition overhead to both branches
						progress += EFFORT_IF_BRANCHING + analysis::features::estimateEffort(ifStmt->getCondition());
						// now build a new if stmt and replace the old one
						const auto newCondition = ifStmt->getCondition().substitute(mgr, *this);
						const auto newThenBody = resolveElementInternal(ifStmt->getThenBody(), progress).as<core::StatementPtr>();
						const auto newElseBody = resolveElementInternal(ifStmt->getElseBody(), progress).as<core::StatementPtr>();
						*it = builder.ifStmt(newCondition, newThenBody, newElseBody);
						progress = 0;
						continue;
					}

					const auto stmtProgress = analysis::features::estimateEffort(stmt);

					// if the progress for the sub statement is high enough, we recursively handle this sub statement but report the current progress beforehand
					if(stmtProgress > progressReportingLimit) {
						insertProgressReportingCall(it);
						*it = resolveElement(stmt).as<core::StatementPtr>();
						continue;
					}

					// insert reporting call before each exit point or if the progress until now and the stmtProgress are more than our limit
					if(isExitPoint(stmt) || progress + stmtProgress > progressReportingLimit) {
						insertProgressReportingCall(it);
					}
					progress += stmtProgress;
				}

				// insert a reporting call in the end if there isn't already one and we don't have an exit point there
				if(!stmts.empty()) {
					const auto& lastStmt = stmts.back();
					if(!progressExtension.isCallOfProgressReportingLiteral(lastStmt)) {
						if(!isExitPoint(lastStmt) && progress != 0) {
							stmts.push_back(buildProgressReportingCall(progress));
						}
					}

					// if we are processing an empty compound which has a progress (i.e. because it got some start progress from it's parent), report it
				} else if(progress != 0) {
					stmts.push_back(buildProgressReportingCall(progress));
				}

				return builder.compoundStmt(stmts);
			}

			virtual const core::NodePtr resolveElement(const core::NodePtr& node) override {
				return resolveElementInternal(node);
			}
		};
	}


	core::CallExprPtr buildProgressReportingCall(core::NodeManager& manager, const EffortType progress) {
		const auto& ext = manager.getLangExtension<ProgressEstomationExtension>();
		return buildProgressReportingCallInternal(core::IRBuilder(manager), ext.getProgressReportingLiteral(), progress);
	}

	analysis::features::EffortEstimationType getReportedProgress(const core::NodePtr& node) {
		const auto& ext = node.getNodeManager().getLangExtension<ProgressEstomationExtension>();
		if(!ext.isCallOfProgressReportingLiteral(node)) return 0;
		auto uintValue = core::analysis::getArgument(node, 0).as<core::LiteralPtr>();
		return std::stoull(uintValue->getValue()->getValue());
	}

	core::NodePtr applyProgressEstimation(const core::NodePtr& node, const EffortType progressReportingLimit) {
		return ProgressMapper(node.getNodeManager(), progressReportingLimit).map(node);
	}

} // end namespace transform
} // end namespace insieme

