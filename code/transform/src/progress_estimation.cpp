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

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/transform/node_replacer.h"


namespace insieme {
namespace transform {

	namespace {

		ProgressReportingType EFFORT_SIMPLE_OP = 1;
		ProgressReportingType EFFORT_BRANCH = 2;
		ProgressReportingType EFFORT_BUILTIN = 1;
		ProgressReportingType EFFORT_FUN_CALL = 5;
		ProgressReportingType EFFORT_FOR_LOOP_ITERATOR = 1;
		ProgressReportingType EFFORT_FOR_LOOP_CONDITION = 1;
		ProgressReportingType EFFORT_FOR_LOOP_ITERATOR_UPDATE = 1;

		struct UnreportedProgressAnnotation : public core::value_annotation::copy_on_migration {
			ProgressReportingType progress;
			UnreportedProgressAnnotation(const ProgressReportingType& progress) : progress(progress) {}
			bool operator==(const UnreportedProgressAnnotation& other) const {
				return progress == other.progress;
			}
		};

		ProgressReportingType getNodeEffort(const core::NodePtr& node) {
			// get cached value from annotation if present
			if(node.hasAttachedValue<UnreportedProgressAnnotation>()) {
				return node.getAttachedValue<UnreportedProgressAnnotation>().progress;
			}

			// otherwise the effort depends on the node type
			switch(node->getNodeType()) {
				case core::NT_BreakStmt:
				case core::NT_ContinueStmt:
				case core::NT_ReturnStmt:
				case core::NT_DeclarationStmt:
					return EFFORT_SIMPLE_OP;

				case core::NT_SwitchStmt:
					return EFFORT_BRANCH;

				case core::NT_CallExpr: {
					const auto& callee = node.as<core::CallExprPtr>()->getFunctionExpr();
					if(core::lang::isBuiltIn(callee) || core::lang::isDerived(callee)) {
						return EFFORT_BUILTIN;
					} else {
						return EFFORT_FUN_CALL;
					}
				}

				default:
					return 0;
			}
		}

		ProgressReportingType getUnreportedProgress(const core::NodePtr& parent) {
			ProgressReportingType progress = 0;

			core::visitDepthFirstPrunable(parent, [&](const core::NodePtr& node) {
				// don't process or descend into types
				if(node.getNodeCategory() == core::NC_Type) {
					return core::Prune;
				}

				// add the unreported progress for every child node
				progress += getNodeEffort(node);

				// but don't descend into lambda expressions. These already have their unreported progress attached and we already added that
				if(node.getNodeType() == core::NT_LambdaExpr) {
					return core::Prune;
				}
				return core::Descent;
			});

			return progress;
		}

		core::CompoundStmtPtr handleCompound(ProgressReportingType& progress, const core::CompoundStmtPtr& compound, const ProgressReportingType progressReportingLimit, bool isRoot) {
			core::NodeManager& mgr = compound.getNodeManager();
			core::IRBuilder builder(mgr);
			core::StatementList stmts(compound.getStatements());

			// if the current progress is 0, we insert a new statement into the list and update the iterator
			auto insertProgressReportingCall = [&](core::StatementList::iterator& it) {
				if(progress != 0) {
					it = stmts.insert(it, buildProgressReportingCall(mgr, progress));
					++it;
					progress = 0;
				}
			};

			const auto exitPoints = core::analysis::getExitPoints(compound);
			auto isExitPoint = [&exitPoints](const core::StatementPtr& stmt) {
				return ::any(exitPoints, [&stmt](const auto& exitPoint) { return exitPoint.getDepth() == 2 && exitPoint.getAddressedNode() == stmt; });
			};

			for(auto it = stmts.begin(); it < stmts.end(); ++it) {
				auto stmt = *it;

				// special handling for compounds
				if(const auto& innerCompound = stmt.isa<core::CompoundStmtPtr>()) {
					// compound statement children are handeled recursively, counting from the current progress
					*it = handleCompound(progress, innerCompound, progressReportingLimit, false);

					// special handling for for loops
				} else if(const auto& forLoop = stmt.isa<core::ForStmtPtr>()) {
					// we have to report the current progress before entering the for loop. We also add some overhead for the loop iterator variable
					progress += EFFORT_FOR_LOOP_ITERATOR;
					insertProgressReportingCall(it);
					// now we handle the body. Each iteration gets a start progress offset which accounts for evaluating the condition as well as updating the iterator
					ProgressReportingType bodyProgress = EFFORT_BRANCH + EFFORT_FOR_LOOP_CONDITION + EFFORT_FOR_LOOP_ITERATOR_UPDATE;
					const auto newBody = handleCompound(bodyProgress, forLoop->getBody(), progressReportingLimit, true); // enforce reporting of progress at exit points
					*it = core::transform::replaceNode(mgr, core::ForStmtAddress(forLoop)->getBody(), newBody).as<core::ForStmtPtr>();

					// special handling for while loops
				} else if(const auto& whileLoop = stmt.isa<core::WhileStmtPtr>()) {
					// we have to report the current progress before entering the while loop
					insertProgressReportingCall(it);
					// now we handle the body. Each iteration gets a start progress offset which accounts the unreported progress of the condition + some branching overhead
					ProgressReportingType bodyProgress = EFFORT_BRANCH + getUnreportedProgress(whileLoop->getCondition());
					const auto newBody = handleCompound(bodyProgress, whileLoop->getBody(), progressReportingLimit, true); // enforce reporting of progress at exit points
					*it = core::transform::replaceNode(mgr, core::WhileStmtAddress(whileLoop)->getBody(), newBody).as<core::WhileStmtPtr>();

					// special handling for if/else stmts
				} else if(const auto& ifStmt = stmt.isa<core::IfStmtPtr>()) {
					// acount for the branching overhead and the unreported progress of the condition
					progress += EFFORT_BRANCH + getUnreportedProgress(ifStmt->getCondition());
					// now handle both branches with a start offset of the progress we have at the moment
					ProgressReportingType bodyProgress = progress;
					core::NodeMap replacements;
					replacements[core::IfStmtAddress(ifStmt)->getThenBody()] = handleCompound(bodyProgress, ifStmt->getThenBody(), progressReportingLimit, true); // enforce reporting of progress at exit points
					bodyProgress = progress;
					replacements[core::IfStmtAddress(ifStmt)->getElseBody()] = handleCompound(bodyProgress, ifStmt->getElseBody(), progressReportingLimit, true); // enforce reporting of progress at exit points
					*it = core::transform::replaceAllGen(mgr, ifStmt, replacements);
					// the progress has been reported in either branch
					progress = 0;

					// all other nodes are processed the same
				} else {
					const auto stmtProgress = getUnreportedProgress(stmt);

					// if the current node is an exit point of the surrounding stmt
					if(isExitPoint(stmt)) {
						// we add the progress for that exit point as well before reporting our current progress
						progress += stmtProgress;
						insertProgressReportingCall(it);

					} else {
						// if the progress until now and the stmtProgress are more than our limit
						if(progress + stmtProgress > progressReportingLimit) {
							// we report the progress until here
							insertProgressReportingCall(it);
						}
						progress += stmtProgress;
					}
				}
			}

			// if we do have some unreported progress left and this is the root compound, we report it here
			if(isRoot && progress != 0) {
				stmts.push_back(buildProgressReportingCall(mgr, progress));
			}

			return builder.compoundStmt(stmts);
		}

		core::CompoundStmtPtr inlineSmallLambdaBodies(ProgressReportingType& progress, const core::CompoundStmtPtr& compound, const ProgressReportingType progressReportingLimit) {
			auto& mgr = compound.getNodeManager();
			const auto& ext = mgr.getLangExtension<ProgressEstomationExtension>();
			// only proceed if we could at least have a reporting call and a return stmt
			if(compound.size() >= 2) {
				// if we only have one reporting call
				if(core::analysis::countInstances(compound, ext.getProgressReportingLiteral(), true) == 1) {
					const auto& reportingCall = *(compound.end() - 2);
					// if that is the penultimate stmt and the last one is a return
					if(ext.isCallOfProgressReportingLiteral(reportingCall) && (*(compound.end() - 1)).isa<core::ReturnStmtPtr>()) {
						const auto reportedProgress = getReportedProgress(reportingCall);
						// and the reported progress is below the reporting limit, we inline this progress (i.e. remove the reporting call and increase the unreported effort)
						if(reportedProgress < progressReportingLimit) {
							progress += reportedProgress;
							core::StatementList stmts(compound.begin(), compound.end());
							stmts.erase(stmts.end() - 2);
							return core::IRBuilder(mgr).compoundStmt(stmts);
						}
					}
				}
			}
			return compound;
		}

		core::NodePtr applyProgressEstimationImpl(const core::NodePtr& node, const ProgressReportingType progressReportingLimit) {
			// first we transform all the lambdas bottom to top. We add reporting calls and annotate the unreported progress to them
			auto res = core::transform::transformBottomUp(node, [&](const core::LambdaExprPtr& lambda) {
				auto res = lambda;
				ProgressReportingType progress = 0;

				// we do not modify derived lambdas
				if(core::lang::isDerived(lambda)) {
					progress = EFFORT_BUILTIN;

					// every other lambda is processed though
				} else {
					// we create a new body with inserted reporting calls if necessary
					auto newBody = handleCompound(progress, lambda->getBody(), progressReportingLimit, false);
					newBody = inlineSmallLambdaBodies(progress, newBody, progressReportingLimit);
					res = core::transform::replaceNode(res.getNodeManager(), core::LambdaExprAddress(res)->getBody(), newBody).as<core::LambdaExprPtr>();
				}

				// and we attach the unreported progress to the lambda itself
				res.attachValue(UnreportedProgressAnnotation{progress});
				return res;
			}, core::transform::globalReplacement);

			// this is to ensure that if we are handling a compound only (mostly for testing), we also report the progress at it's end
			if(res.getNodeType() == core::NT_CompoundStmt) {
				ProgressReportingType progress = 0;
				return handleCompound(progress, res.as<core::CompoundStmtPtr>(), progressReportingLimit, true);
			}

			return res;
		}
	}


	core::CallExprPtr buildProgressReportingCall(core::NodeManager& manager, const ProgressReportingType progress) {
		core::IRBuilder builder(manager);
		const auto& ext = manager.getLangExtension<ProgressEstomationExtension>();
		return builder.callExpr(ext.getProgressReportingLiteral(), builder.literal(builder.getLangBasic().getUInt16(), toString(progress)));
	}

	ProgressReportingType getReportedProgress(const core::NodePtr& node) {
		const auto& ext = node.getNodeManager().getLangExtension<ProgressEstomationExtension>();
		if(!ext.isCallOfProgressReportingLiteral(node)) return 0;
		auto uintValue = core::analysis::getArgument(node, 0).as<core::LiteralPtr>();
		return std::stoull(uintValue->getValue()->getValue());
	}

	core::NodePtr applyProgressEstimation(const core::NodePtr& node, const ProgressReportingType progressReportingLimit) {
		return applyProgressEstimationImpl(node, progressReportingLimit);
	}

} // end namespace transform
} // end namespace insieme

